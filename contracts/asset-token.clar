;; asset-token
;; A non-fungible token contract for representing ownership of registered physical assets
;; This contract works in conjunction with an asset registry to provide verifiable proof of ownership

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-ASSET-NOT-REGISTERED (err u101))
(define-constant ERR-TOKEN-ALREADY-EXISTS (err u102))
(define-constant ERR-TOKEN-NOT-FOUND (err u103))
(define-constant ERR-TRANSFER-FAILED (err u104))
(define-constant ERR-BURN-FAILED (err u105))
(define-constant ERR-REGISTRY-ADDRESS-NOT-SET (err u106))
(define-constant ERR-TRANSFER-TO-SELF (err u107))
(define-constant ERR-INVALID-RECEIVER (err u108))

;; Constants
(define-constant CONTRACT-OWNER tx-sender)

;; Data variables
(define-data-var registry-contract-address principal 'ST000000000000000000002AMW42H)

;; Data maps
;; Maps token IDs to owner principals
(define-map token-owners (string-ascii 64) principal)

;; Maps token IDs to asset metadata
(define-map token-metadata 
  (string-ascii 64) 
  {
    asset-id: (string-ascii 64),
    registration-date: uint,
    asset-type: (string-ascii 32),
    asset-description: (string-ascii 256)
  }
)

;; Maps token IDs to asset URIs (e.g., pointing to images or additional metadata)
(define-map token-uris (string-ascii 64) (string-utf8 256))

;; Maps owners to a count of their tokens
(define-map owner-token-count principal uint)

;; Maps owner and index to token ID for enumeration
(define-map owned-tokens 
  { owner: principal, index: uint } 
  (string-ascii 64)
)

;; Maps token IDs to indices in owner's collection
(define-map token-index (string-ascii 64) uint)

;; Maps token IDs to transfer history
(define-map transfer-history 
  (string-ascii 64) 
  (list 25 { from: principal, to: principal, timestamp: uint })
)

;; Private functions

;; Helper function to check if an asset is registered in the asset registry
;; This would make a contract call to the associated registry contract
(define-private (is-asset-registered (asset-id (string-ascii 64)))
  (match (contract-call? (unwrap! (var-get registry-contract-address) ERR-REGISTRY-ADDRESS-NOT-SET)
                       is-asset-registered asset-id)
    success (ok success)
    error ERR-ASSET-NOT-REGISTERED
  )
)

;; Helper function to add token to owner's collection
(define-private (add-token-to-owner (token-id (string-ascii 64)) (owner principal))
  (let ((current-count (default-to u0 (map-get? owner-token-count owner))))
    ;; Increment owner's token count
    (map-set owner-token-count owner (+ current-count u1))
    ;; Add token to owner's collection at the end of their list
    (map-set owned-tokens { owner: owner, index: current-count } token-id)
    ;; Store the index of this token in the owner's collection
    (map-set token-index token-id current-count)
  )
)

;; Helper function to remove token from owner's collection
(define-private (remove-token-from-owner (token-id (string-ascii 64)) (owner principal))
  (let ((current-count (default-to u0 (map-get? owner-token-count owner)))
        (token-idx (default-to u0 (map-get? token-index token-id)))
        (last-idx (- current-count u1)))
    ;; Only proceed if owner has tokens
    (if (> current-count u0)
      (begin
        ;; If this isn't the last token, move the last token to the removed token's position
        (if (< token-idx last-idx)
          (let ((last-token-id (unwrap-panic (map-get? owned-tokens { owner: owner, index: last-idx }))))
            ;; Move last token to current position
            (map-set owned-tokens { owner: owner, index: token-idx } last-token-id)
            ;; Update index for the moved token
            (map-set token-index last-token-id token-idx)
          )
          true
        )
        ;; Remove the last token position (now duplicated or no longer needed)
        (map-delete owned-tokens { owner: owner, index: last-idx })
        ;; Decrement owner's token count
        (map-set owner-token-count owner (- current-count u1))
        ;; Delete the index mapping for this token
        (map-delete token-index token-id)
        true
      )
      false
    )
  )
)

;; Helper function to log a transfer in the history
(define-private (log-transfer (token-id (string-ascii 64)) (from principal) (to principal))
  (let ((current-history (default-to (list) (map-get? transfer-history token-id)))
        (new-entry { from: from, to: to, timestamp: block-height }))
    ;; Add the new transfer entry to the history
    (map-set transfer-history token-id (append current-history new-entry))
  )
)

;; Read-only functions

;; Get the total supply of tokens
(define-read-only (get-total-supply)
  (fold + (map-get? owner-token-count true) u0)
)

;; Get the current owner of a token
(define-read-only (get-owner-of (token-id (string-ascii 64)))
  (ok (map-get? token-owners token-id))
)

;; Get token metadata
(define-read-only (get-token-metadata (token-id (string-ascii 64)))
  (ok (map-get? token-metadata token-id))
)

;; Get token URI
(define-read-only (get-token-uri (token-id (string-ascii 64)))
  (ok (map-get? token-uris token-id))
)

;; Get token transfer history
(define-read-only (get-transfer-history (token-id (string-ascii 64)))
  (ok (map-get? transfer-history token-id))
)

;; Get the number of tokens owned by an address
(define-read-only (get-balance-of (owner principal))
  (ok (default-to u0 (map-get? owner-token-count owner)))
)

;; Get token ID at a specific index in an owner's collection
(define-read-only (get-token-of-owner-by-index (owner principal) (index uint))
  (ok (map-get? owned-tokens { owner: owner, index: index }))
)

;; Check if a token exists
(define-read-only (token-exists (token-id (string-ascii 64)))
  (is-some (map-get? token-owners token-id))
)

;; Public functions

;; Set or update the registry contract address
;; Only contract owner can update this
(define-public (set-registry-contract-address (new-address principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (ok (var-set registry-contract-address new-address))
  )
)

;; Mint a new token for a registered asset
;; Can only be called by the contract owner or the registry contract
(define-public (mint (asset-id (string-ascii 64)) 
                     (owner principal)
                     (asset-type (string-ascii 32))
                     (asset-description (string-ascii 256))
                     (asset-uri (string-utf8 256)))
  (let ((token-id asset-id))  ;; Using asset-id as token-id for simplicity
    (begin
      ;; Ensure caller is authorized
      (asserts! (or (is-eq tx-sender CONTRACT-OWNER)
                    (is-eq tx-sender (var-get registry-contract-address)))
                ERR-NOT-AUTHORIZED)
      
      ;; Check if asset is registered
      (asserts! (is-ok (is-asset-registered asset-id)) ERR-ASSET-NOT-REGISTERED)
      
      ;; Check if token already exists
      (asserts! (not (token-exists token-id)) ERR-TOKEN-ALREADY-EXISTS)
      
      ;; Set token owner
      (map-set token-owners token-id owner)
      
      ;; Set token metadata
      (map-set token-metadata token-id 
        {
          asset-id: asset-id,
          registration-date: block-height,
          asset-type: asset-type,
          asset-description: asset-description
        }
      )
      
      ;; Set token URI
      (map-set token-uris token-id asset-uri)
      
      ;; Initialize transfer history with the mint operation
      (log-transfer token-id 'ST000000000000000000002AMW42H owner)
      
      ;; Add token to owner's collection
      (add-token-to-owner token-id owner)
      
      (ok token-id)
    )
  )
)

;; Transfer token to a new owner
(define-public (transfer (token-id (string-ascii 64)) (to principal))
  (let ((owner (default-to 'ST000000000000000000002AMW42H (map-get? token-owners token-id))))
    (begin
      ;; Check if token exists
      (asserts! (token-exists token-id) ERR-TOKEN-NOT-FOUND)
      
      ;; Check if sender is the owner
      (asserts! (is-eq tx-sender owner) ERR-NOT-AUTHORIZED)
      
      ;; Check if not transferring to self
      (asserts! (not (is-eq to owner)) ERR-TRANSFER-TO-SELF)
      
      ;; Check valid recipient (not sending to zero address)
      (asserts! (not (is-eq to 'ST000000000000000000002AMW42H)) ERR-INVALID-RECEIVER)
      
      ;; Remove token from current owner
      (remove-token-from-owner token-id owner)
      
      ;; Set new owner
      (map-set token-owners token-id to)
      
      ;; Add token to new owner's collection
      (add-token-to-owner token-id to)
      
      ;; Log the transfer
      (log-transfer token-id owner to)
      
      (ok true)
    )
  )
)

;; Burn a token (only owner or contract owner can burn)
(define-public (burn (token-id (string-ascii 64)))
  (let ((owner (default-to 'ST000000000000000000002AMW42H (map-get? token-owners token-id))))
    (begin
      ;; Check if token exists
      (asserts! (token-exists token-id) ERR-TOKEN-NOT-FOUND)
      
      ;; Check if sender is the owner or contract owner
      (asserts! (or (is-eq tx-sender owner)
                    (is-eq tx-sender CONTRACT-OWNER))
                ERR-NOT-AUTHORIZED)
      
      ;; Remove token from owner's collection
      (remove-token-from-owner token-id owner)
      
      ;; Delete token data (we keep metadata and history for record purposes)
      (map-delete token-owners token-id)
      
      ;; Log the burn operation
      (log-transfer token-id owner 'ST000000000000000000002AMW42H)
      
      (ok true)
    )
  )
)

;; Update token metadata (only contract owner can update)
(define-public (update-token-metadata (token-id (string-ascii 64))
                                      (asset-type (string-ascii 32))
                                      (asset-description (string-ascii 256)))
  (begin
    ;; Check if caller is authorized
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    
    ;; Check if token exists
    (asserts! (token-exists token-id) ERR-TOKEN-NOT-FOUND)
    
    ;; Get current metadata
    (let ((current-metadata (unwrap-panic (map-get? token-metadata token-id))))
      ;; Update metadata while preserving immutable fields
      (map-set token-metadata token-id 
        {
          asset-id: (get asset-id current-metadata),
          registration-date: (get registration-date current-metadata),
          asset-type: asset-type,
          asset-description: asset-description
        }
      )
      
      (ok true)
    )
  )
)

;; Update token URI (only contract owner can update)
(define-public (update-token-uri (token-id (string-ascii 64)) (new-uri (string-utf8 256)))
  (begin
    ;; Check if caller is authorized
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    
    ;; Check if token exists
    (asserts! (token-exists token-id) ERR-TOKEN-NOT-FOUND)
    
    ;; Update URI
    (map-set token-uris token-id new-uri)
    
    (ok true)
  )
)