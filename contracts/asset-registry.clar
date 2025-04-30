;; asset-registry
;; 
;; This contract serves as the core registry for all physical assets in the Chainmint platform.
;; It allows authorized verifiers to register new assets, assigning each a unique identifier and
;; storing critical metadata like description, location, and physical characteristics.
;; The registry implements a multi-tier verification system where assets can progress through
;; states from "pending" to "verified," requiring signatures from authorized verifiers.

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-ASSET-NOT-FOUND (err u101))
(define-constant ERR-INVALID-STATE (err u102))
(define-constant ERR-ALREADY-VERIFIED (err u103))
(define-constant ERR-ALREADY-REGISTERED (err u104))
(define-constant ERR-INVALID-METADATA (err u105))

;; Asset verification states
(define-constant STATE-PENDING u1)
(define-constant STATE-VERIFIED u2)
(define-constant STATE-REJECTED u3)

;; Data space definitions
;; Admin principal that can manage verifiers
(define-data-var contract-owner principal tx-sender)

;; Map of authorized verifiers
(define-map verifiers principal bool)

;; Counter for generating unique asset IDs
(define-data-var asset-id-counter uint u0)

;; Main asset registry data structure
(define-map assets
  uint  ;; asset-id
  {
    owner: principal,
    description: (string-utf8 500),
    location: (string-utf8 100),
    characteristics: (string-utf8 1000),
    state: uint,
    registration-date: uint,
    verification-date: (optional uint),
    verifier: (optional principal)
  }
)

;; Map to store additional metadata for assets (could be extended with more fields)
(define-map asset-metadata
  uint  ;; asset-id
  {
    image-uri: (optional (string-utf8 256)),
    external-link: (optional (string-utf8 256)),
    additional-data: (optional (string-utf8 1000))
  }
)

;; Private functions

;; Check if the caller is the contract owner
(define-private (is-contract-owner)
  (is-eq tx-sender (var-get contract-owner))
)

;; Check if the caller is an authorized verifier
(define-private (is-verifier (caller principal))
  (default-to false (map-get? verifiers caller))
)

;; Generate a new unique asset ID
(define-private (generate-asset-id)
  (let ((current-id (var-get asset-id-counter)))
    (var-set asset-id-counter (+ current-id u1))
    current-id
  )
)

;; Read-only functions

;; Check if a principal is an authorized verifier
(define-read-only (is-authorized-verifier (verifier-principal principal))
  (default-to false (map-get? verifiers verifier-principal))
)

;; Get asset details by ID
(define-read-only (get-asset (asset-id uint))
  (map-get? assets asset-id)
)

;; Get asset metadata by ID
(define-read-only (get-asset-metadata (asset-id uint))
  (map-get? asset-metadata asset-id)
)

;; Get the current owner of the contract
(define-read-only (get-contract-owner)
  (var-get contract-owner)
)

;; Public functions

;; Set or update the contract owner
(define-public (set-contract-owner (new-owner principal))
  (begin
    (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
    (ok (var-set contract-owner new-owner))
  )
)

;; Add a new verifier
(define-public (add-verifier (verifier-principal principal))
  (begin
    (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
    (ok (map-set verifiers verifier-principal true))
  )
)

;; Remove a verifier
(define-public (remove-verifier (verifier-principal principal))
  (begin
    (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
    (ok (map-delete verifiers verifier-principal))
  )
)

;; Register a new asset (initial state is always "pending")
(define-public (register-asset
  (description (string-utf8 500))
  (location (string-utf8 100))
  (characteristics (string-utf8 1000))
  (image-uri (optional (string-utf8 256)))
  (external-link (optional (string-utf8 256)))
  (additional-data (optional (string-utf8 1000)))
)
  (let 
    (
      (asset-id (generate-asset-id))
      (block-height block-height)
    )
    ;; Validate inputs
    (asserts! (> (len description) u0) ERR-INVALID-METADATA)
    (asserts! (> (len location) u0) ERR-INVALID-METADATA)
    (asserts! (> (len characteristics) u0) ERR-INVALID-METADATA)
    
    ;; Store the main asset data
    (map-set assets asset-id {
      owner: tx-sender,
      description: description,
      location: location,
      characteristics: characteristics,
      state: STATE-PENDING,
      registration-date: block-height,
      verification-date: none,
      verifier: none
    })
    
    ;; Store additional metadata
    (map-set asset-metadata asset-id {
      image-uri: image-uri,
      external-link: external-link,
      additional-data: additional-data
    })
    
    (ok asset-id)
  )
)

;; Verify an asset (can only be done by authorized verifiers)
(define-public (verify-asset (asset-id uint))
  (let 
    (
      (asset (unwrap! (map-get? assets asset-id) ERR-ASSET-NOT-FOUND))
      (current-block-height block-height)
    )
    ;; Authorization check
    (asserts! (is-verifier tx-sender) ERR-NOT-AUTHORIZED)
    
    ;; State validation
    (asserts! (is-eq (get state asset) STATE-PENDING) ERR-INVALID-STATE)
    
    ;; Update the asset state
    (map-set assets asset-id (merge asset {
      state: STATE-VERIFIED,
      verification-date: (some current-block-height),
      verifier: (some tx-sender)
    }))
    
    (ok true)
  )
)

;; Reject an asset (can only be done by authorized verifiers)
(define-public (reject-asset (asset-id uint))
  (let 
    (
      (asset (unwrap! (map-get? assets asset-id) ERR-ASSET-NOT-FOUND))
      (current-block-height block-height)
    )
    ;; Authorization check
    (asserts! (is-verifier tx-sender) ERR-NOT-AUTHORIZED)
    
    ;; State validation
    (asserts! (is-eq (get state asset) STATE-PENDING) ERR-INVALID-STATE)
    
    ;; Update the asset state
    (map-set assets asset-id (merge asset {
      state: STATE-REJECTED,
      verification-date: (some current-block-height),
      verifier: (some tx-sender)
    }))
    
    (ok true)
  )
)

;; Update asset metadata (only the owner can do this)
(define-public (update-asset-metadata
  (asset-id uint)
  (image-uri (optional (string-utf8 256)))
  (external-link (optional (string-utf8 256)))
  (additional-data (optional (string-utf8 1000)))
)
  (let 
    (
      (asset (unwrap! (map-get? assets asset-id) ERR-ASSET-NOT-FOUND))
    )
    ;; Authorization check
    (asserts! (is-eq (get owner asset) tx-sender) ERR-NOT-AUTHORIZED)
    
    ;; Update the metadata
    (map-set asset-metadata asset-id {
      image-uri: image-uri,
      external-link: external-link,
      additional-data: additional-data
    })
    
    (ok true)
  )
)

;; Update asset details (can only be done by owner and only if asset is not yet verified)
(define-public (update-asset-details
  (asset-id uint)
  (description (string-utf8 500))
  (location (string-utf8 100))
  (characteristics (string-utf8 1000))
)
  (let 
    (
      (asset (unwrap! (map-get? assets asset-id) ERR-ASSET-NOT-FOUND))
    )
    ;; Authorization check
    (asserts! (is-eq (get owner asset) tx-sender) ERR-NOT-AUTHORIZED)
    
    ;; State validation
    (asserts! (is-eq (get state asset) STATE-PENDING) ERR-INVALID-STATE)
    
    ;; Validate inputs
    (asserts! (> (len description) u0) ERR-INVALID-METADATA)
    (asserts! (> (len location) u0) ERR-INVALID-METADATA)
    (asserts! (> (len characteristics) u0) ERR-INVALID-METADATA)
    
    ;; Update the asset details
    (map-set assets asset-id (merge asset {
      description: description,
      location: location,
      characteristics: characteristics
    }))
    
    (ok true)
  )
)

;; Transfer asset ownership (can only be done by the current owner)
(define-public (transfer-asset (asset-id uint) (new-owner principal))
  (let 
    (
      (asset (unwrap! (map-get? assets asset-id) ERR-ASSET-NOT-FOUND))
    )
    ;; Authorization check
    (asserts! (is-eq (get owner asset) tx-sender) ERR-NOT-AUTHORIZED)
    
    ;; Update the owner
    (map-set assets asset-id (merge asset {
      owner: new-owner
    }))
    
    (ok true)
  )
)