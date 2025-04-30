;; asset-marketplace
;; A secure marketplace for buying and selling tokenized physical assets with escrow protection.
;; This contract enables users to list assets for sale, make purchases, and complete transactions
;; with built-in escrow functionality to protect both buyers and sellers.

;; ERROR CODES
(define-constant ERR-NOT-AUTHORIZED (err u1001))
(define-constant ERR-LISTING-NOT-FOUND (err u1002))
(define-constant ERR-LISTING-EXPIRED (err u1003))
(define-constant ERR-INSUFFICIENT-FUNDS (err u1004))
(define-constant ERR-PAYMENT-FAILED (err u1005))
(define-constant ERR-ALREADY-LISTED (err u1006))
(define-constant ERR-INVALID-PRICE (err u1007))
(define-constant ERR-ASSET-TRANSFER-FAILED (err u1008))
(define-constant ERR-LISTING-NOT-ACTIVE (err u1009))
(define-constant ERR-AUCTION-ENDED (err u1010))
(define-constant ERR-BID-TOO-LOW (err u1011))
(define-constant ERR-NO-WINNING-BID (err u1012))
(define-constant ERR-INVALID-LISTING-TYPE (err u1013))
(define-constant ERR-ONLY-BUYER-OR-SELLER (err u1014))
(define-constant ERR-ESCROW-ALREADY-RELEASED (err u1015))
(define-constant ERR-CANNOT-CANCEL (err u1016))
(define-constant ERR-DISPUTE-ALREADY-EXISTS (err u1017))
(define-constant ERR-NO-DISPUTE-EXISTS (err u1018))
(define-constant ERR-ESCROW-NOT-FOUND (err u1019))

;; LISTING TYPES
(define-constant LISTING-TYPE-FIXED-PRICE u1)
(define-constant LISTING-TYPE-AUCTION u2)
(define-constant LISTING-TYPE-TIMED-OFFER u3)

;; LISTING STATUS
(define-constant STATUS-ACTIVE u1)
(define-constant STATUS-SOLD u2)
(define-constant STATUS-CANCELED u3)
(define-constant STATUS-EXPIRED u4)

;; ESCROW STATUS
(define-constant ESCROW-STATUS-ACTIVE u1)
(define-constant ESCROW-STATUS-RELEASED u2)
(define-constant ESCROW-STATUS-REFUNDED u3)
(define-constant ESCROW-STATUS-DISPUTED u4)
(define-constant ESCROW-STATUS-RESOLVED u5)

;; CONTRACT SETTINGS
(define-constant MARKETPLACE-FEE-PERCENTAGE u250) ;; 2.5% fee (in basis points)
(define-constant MARKETPLACE-ADMIN tx-sender) ;; Contract deployer is the admin
(define-constant DISPUTE-RESOLUTION-PERIOD u1440) ;; 10 days (measured in blocks, ~144 blocks per day)

;; DATA MAPS

;; Stores all listings currently in the marketplace
(define-map listings
  { listing-id: uint }
  {
    seller: principal,
    asset-contract: principal,
    asset-id: uint,
    price: uint,
    listing-type: uint,
    status: uint,
    created-at: uint,
    expires-at: uint,
    highest-bidder: (optional principal),
    highest-bid: uint
  }
)

;; Tracks escrow for each transaction
(define-map escrows
  { escrow-id: uint }
  {
    listing-id: uint,
    buyer: principal,
    seller: principal,
    amount: uint,
    status: uint,
    created-at: uint,
    asset-contract: principal,
    asset-id: uint
  }
)

;; Stores disputes for contested transactions
(define-map disputes
  { escrow-id: uint }
  {
    initiated-by: principal,
    reason: (string-ascii 256),
    created-at: uint,
    resolved-at: (optional uint),
    resolution: (optional (string-ascii 256))
  }
)

;; Tracks all transactions in the marketplace for audit purposes
(define-map transaction-history
  { tx-id: uint }
  {
    listing-id: uint,
    escrow-id: (optional uint),
    action: (string-ascii 20),
    actor: principal,
    amount: uint,
    timestamp: uint
  }
)

;; CONTRACT STATE VARIABLES
(define-data-var next-listing-id uint u1)
(define-data-var next-escrow-id uint u1)
(define-data-var next-tx-id uint u1)
(define-data-var total-volume uint u0)

;; PRIVATE FUNCTIONS

;; Records a transaction in the history for audit purposes
(define-private (record-transaction (listing-id uint) (escrow-id (optional uint)) (action (string-ascii 20)) (amount uint))
  (let
    ((tx-id (var-get next-tx-id)))
    (map-set transaction-history
      { tx-id: tx-id }
      {
        listing-id: listing-id,
        escrow-id: escrow-id,
        action: action,
        actor: tx-sender,
        amount: amount,
        timestamp: block-height
      }
    )
    (var-set next-tx-id (+ tx-id u1))
    tx-id
  )
)

;; Calculates marketplace fee for a given amount
(define-private (calculate-fee (amount uint))
  (/ (* amount MARKETPLACE-FEE-PERCENTAGE) u10000)
)

;; Transfers STX from one principal to another
(define-private (transfer-stx (amount uint) (recipient principal))
  (if (>= (stx-get-balance tx-sender) amount)
    (stx-transfer? amount tx-sender recipient)
    ERR-INSUFFICIENT-FUNDS
  )
)

;; Transfers an asset from one principal to another
;; This function would call a standard SIP-009 NFT transfer function
(define-private (transfer-asset (asset-contract principal) (asset-id uint) (recipient principal))
  (contract-call? asset-contract transfer asset-id tx-sender recipient)
)

;; Checks if a listing is still active and valid
(define-private (is-listing-active (listing-data (optional {
    seller: principal,
    asset-contract: principal,
    asset-id: uint,
    price: uint,
    listing-type: uint,
    status: uint,
    created-at: uint,
    expires-at: uint,
    highest-bidder: (optional principal),
    highest-bid: uint
  })))
  (match listing-data
    listing (and 
              (is-eq (get status listing) STATUS-ACTIVE)
              (<= block-height (get expires-at listing)))
    false
  )
)

;; Releases funds from escrow to the seller and marketplace
(define-private (release-escrow-funds (escrow-id uint))
  (match (map-get? escrows { escrow-id: escrow-id })
    escrow-data (let
                  ((fee (calculate-fee (get amount escrow-data)))
                   (seller-amount (- (get amount escrow-data) fee)))
                  (begin
                    ;; Update the escrow status
                    (map-set escrows 
                      { escrow-id: escrow-id }
                      (merge escrow-data { status: ESCROW-STATUS-RELEASED })
                    )
                    ;; Transfer fee to marketplace
                    (try! (as-contract (stx-transfer? fee tx-sender MARKETPLACE-ADMIN)))
                    ;; Transfer remaining amount to seller
                    (try! (as-contract (stx-transfer? seller-amount tx-sender (get seller escrow-data))))
                    ;; Update total volume
                    (var-set total-volume (+ (var-get total-volume) (get amount escrow-data)))
                    (ok escrow-id)
                  ))
    ERR-ESCROW-NOT-FOUND
  )
)

;; Refunds escrow funds to the buyer
(define-private (refund-escrow (escrow-id uint))
  (match (map-get? escrows { escrow-id: escrow-id })
    escrow-data (begin
                  ;; Update the escrow status
                  (map-set escrows 
                    { escrow-id: escrow-id }
                    (merge escrow-data { status: ESCROW-STATUS-REFUNDED })
                  )
                  ;; Transfer the full amount back to buyer
                  (try! (as-contract (stx-transfer? (get amount escrow-data) tx-sender (get buyer escrow-data))))
                  (ok escrow-id)
                )
    ERR-ESCROW-NOT-FOUND
  )
)

;; READ-ONLY FUNCTIONS

;; Get listing details by ID
(define-read-only (get-listing (listing-id uint))
  (map-get? listings { listing-id: listing-id })
)

;; Get escrow details by ID
(define-read-only (get-escrow (escrow-id uint))
  (map-get? escrows { escrow-id: escrow-id })
)

;; Get dispute details by escrow ID
(define-read-only (get-dispute (escrow-id uint))
  (map-get? disputes { escrow-id: escrow-id })
)

;; Get transaction details by ID
(define-read-only (get-transaction (tx-id uint))
  (map-get? transaction-history { tx-id: tx-id })
)

;; Get total marketplace volume
(define-read-only (get-total-volume)
  (var-get total-volume)
)

;; Check if a listing is still active
(define-read-only (is-active-listing (listing-id uint))
  (is-listing-active (map-get? listings { listing-id: listing-id }))
)

;; PUBLIC FUNCTIONS

;; Create a new fixed-price listing
;; Creates a new listing for a tokenized asset at a fixed price
(define-public (create-fixed-price-listing (asset-contract principal) (asset-id uint) (price uint) (expires-at uint))
  (let
    ((listing-id (var-get next-listing-id)))
    
    ;; Check that price is valid
    (asserts! (> price u0) ERR-INVALID-PRICE)
    
    ;; Check that expiration is in the future
    (asserts! (> expires-at block-height) ERR-LISTING-EXPIRED)
    
    ;; Store the listing
    (map-set listings
      { listing-id: listing-id }
      {
        seller: tx-sender,
        asset-contract: asset-contract,
        asset-id: asset-id,
        price: price,
        listing-type: LISTING-TYPE-FIXED-PRICE,
        status: STATUS-ACTIVE,
        created-at: block-height,
        expires-at: expires-at,
        highest-bidder: none,
        highest-bid: u0
      }
    )
    
    ;; Record the transaction
    (record-transaction listing-id none "list" price)
    
    ;; Increment listing ID for next use
    (var-set next-listing-id (+ listing-id u1))
    
    (ok listing-id)
  )
)

;; Create a new auction listing
;; Creates a new auction for a tokenized asset with a starting bid
(define-public (create-auction-listing (asset-contract principal) (asset-id uint) (starting-bid uint) (expires-at uint))
  (let
    ((listing-id (var-get next-listing-id)))
    
    ;; Check that price is valid
    (asserts! (> starting-bid u0) ERR-INVALID-PRICE)
    
    ;; Check that expiration is in the future
    (asserts! (> expires-at block-height) ERR-LISTING-EXPIRED)
    
    ;; Store the listing
    (map-set listings
      { listing-id: listing-id }
      {
        seller: tx-sender,
        asset-contract: asset-contract,
        asset-id: asset-id,
        price: starting-bid, ;; Starting bid
        listing-type: LISTING-TYPE-AUCTION,
        status: STATUS-ACTIVE,
        created-at: block-height,
        expires-at: expires-at,
        highest-bidder: none,
        highest-bid: u0
      }
    )
    
    ;; Record the transaction
    (record-transaction listing-id none "list-auction" starting-bid)
    
    ;; Increment listing ID for next use
    (var-set next-listing-id (+ listing-id u1))
    
    (ok listing-id)
  )
)

;; Place a bid on an auction listing
(define-public (place-bid (listing-id uint) (bid-amount uint))
  (match (map-get? listings { listing-id: listing-id })
    listing (begin
              ;; Check that listing is an auction
              (asserts! (is-eq (get listing-type listing) LISTING-TYPE-AUCTION) ERR-INVALID-LISTING-TYPE)
              
              ;; Check that listing is active
              (asserts! (is-eq (get status listing) STATUS-ACTIVE) ERR-LISTING-NOT-ACTIVE)
              
              ;; Check that auction hasn't ended
              (asserts! (<= block-height (get expires-at listing)) ERR-AUCTION-ENDED)
              
              ;; Check that bid is higher than current highest bid or starting price if no bids yet
              (asserts! (if (is-some (get highest-bidder listing))
                          (> bid-amount (get highest-bid listing))
                          (>= bid-amount (get price listing)))
                        ERR-BID-TOO-LOW)
              
              ;; Transfer the bid amount to the contract (held in escrow)
              (try! (stx-transfer? bid-amount tx-sender (as-contract tx-sender)))
              
              ;; Refund the previous highest bidder if there was one
              (if (is-some (get highest-bidder listing))
                (try! (as-contract (stx-transfer? (get highest-bid listing) tx-sender (unwrap-panic (get highest-bidder listing)))))
                true)
              
              ;; Update the listing with new highest bid
              (map-set listings
                { listing-id: listing-id }
                (merge listing {
                  highest-bidder: (some tx-sender),
                  highest-bid: bid-amount
                })
              )
              
              ;; Record the transaction
              (record-transaction listing-id none "place-bid" bid-amount)
              
              (ok bid-amount))
    ERR-LISTING-NOT-FOUND
  )
)

;; Buy a fixed-price listing
(define-public (buy-listing (listing-id uint))
  (match (map-get? listings { listing-id: listing-id })
    listing (begin
              ;; Check that listing is fixed price
              (asserts! (is-eq (get listing-type listing) LISTING-TYPE-FIXED-PRICE) ERR-INVALID-LISTING-TYPE)
              
              ;; Check that listing is active
              (asserts! (is-listing-active (some listing)) ERR-LISTING-NOT-ACTIVE)
              
              ;; Create escrow
              (let
                ((escrow-id (var-get next-escrow-id))
                 (price (get price listing)))
                
                ;; Transfer the payment to the contract (held in escrow)
                (try! (stx-transfer? price tx-sender (as-contract tx-sender)))
                
                ;; Create escrow record
                (map-set escrows
                  { escrow-id: escrow-id }
                  {
                    listing-id: listing-id,
                    buyer: tx-sender,
                    seller: (get seller listing),
                    amount: price,
                    status: ESCROW-STATUS-ACTIVE,
                    created-at: block-height,
                    asset-contract: (get asset-contract listing),
                    asset-id: (get asset-id listing)
                  }
                )
                
                ;; Update listing status to sold
                (map-set listings
                  { listing-id: listing-id }
                  (merge listing { status: STATUS-SOLD })
                )
                
                ;; Record the transaction
                (record-transaction listing-id (some escrow-id) "buy" price)
                
                ;; Increment escrow ID for next use
                (var-set next-escrow-id (+ escrow-id u1))
                
                (ok escrow-id))
            )
    ERR-LISTING-NOT-FOUND
  )
)

;; Finalize an auction when it ends (can be called by seller or highest bidder)
(define-public (finalize-auction (listing-id uint))
  (match (map-get? listings { listing-id: listing-id })
    listing (begin
              ;; Check that listing is an auction
              (asserts! (is-eq (get listing-type listing) LISTING-TYPE-AUCTION) ERR-INVALID-LISTING-TYPE)
              
              ;; Check that auction has ended
              (asserts! (> block-height (get expires-at listing)) ERR-AUCTION-ENDED)
              
              ;; Check that listing is still active (not already finalized)
              (asserts! (is-eq (get status listing) STATUS-ACTIVE) ERR-LISTING-NOT-ACTIVE)
              
              ;; Check that there was at least one bid
              (asserts! (is-some (get highest-bidder listing)) ERR-NO-WINNING-BID)
              
              ;; Create escrow
              (let
                ((escrow-id (var-get next-escrow-id))
                 (winning-bid (get highest-bid listing))
                 (winning-bidder (unwrap-panic (get highest-bidder listing))))
                
                ;; Create escrow record for the winning bid (already transferred to contract)
                (map-set escrows
                  { escrow-id: escrow-id }
                  {
                    listing-id: listing-id,
                    buyer: winning-bidder,
                    seller: (get seller listing),
                    amount: winning-bid,
                    status: ESCROW-STATUS-ACTIVE,
                    created-at: block-height,
                    asset-contract: (get asset-contract listing),
                    asset-id: (get asset-id listing)
                  }
                )
                
                ;; Update listing status to sold
                (map-set listings
                  { listing-id: listing-id }
                  (merge listing { status: STATUS-SOLD })
                )
                
                ;; Record the transaction
                (record-transaction listing-id (some escrow-id) "finalize-auction" winning-bid)
                
                ;; Increment escrow ID for next use
                (var-set next-escrow-id (+ escrow-id u1))
                
                (ok escrow-id))
            )
    ERR-LISTING-NOT-FOUND
  )
)

;; Cancel a listing (only possible if no active bids for auctions)
(define-public (cancel-listing (listing-id uint))
  (match (map-get? listings { listing-id: listing-id })
    listing (begin
              ;; Check that caller is the seller
              (asserts! (is-eq tx-sender (get seller listing)) ERR-NOT-AUTHORIZED)
              
              ;; Check that listing is active
              (asserts! (is-eq (get status listing) STATUS-ACTIVE) ERR-LISTING-NOT-ACTIVE)
              
              ;; If auction with bids, prevent cancellation
              (asserts! (not (and 
                               (is-eq (get listing-type listing) LISTING-TYPE-AUCTION)
                               (is-some (get highest-bidder listing))))
                        ERR-CANNOT-CANCEL)
                
              ;; Update listing status to canceled
              (map-set listings
                { listing-id: listing-id }
                (merge listing { status: STATUS-CANCELED })
              )
              
              ;; Record the transaction
              (record-transaction listing-id none "cancel" u0)
              
              (ok listing-id))
    ERR-LISTING-NOT-FOUND
  )
)

;; Confirms asset receipt and releases escrowed funds to seller
(define-public (confirm-receipt (escrow-id uint))
  (match (map-get? escrows { escrow-id: escrow-id })
    escrow-data (begin
                  ;; Check that caller is the buyer
                  (asserts! (is-eq tx-sender (get buyer escrow-data)) ERR-NOT-AUTHORIZED)
                  
                  ;; Check that escrow is active
                  (asserts! (is-eq (get status escrow-data) ESCROW-STATUS-ACTIVE) ERR-ESCROW-ALREADY-RELEASED)
                  
                  ;; Release the funds from escrow
                  (try! (release-escrow-funds escrow-id))
                  
                  ;; Record the transaction
                  (record-transaction 
                    (get listing-id escrow-data) 
                    (some escrow-id) 
                    "confirm-receipt" 
                    (get amount escrow-data))
                  
                  (ok escrow-id))
    ERR-ESCROW-NOT-FOUND
  )
)

;; Transfer asset to buyer (seller initiates asset transfer)
(define-public (transfer-asset-to-buyer (escrow-id uint))
  (match (map-get? escrows { escrow-id: escrow-id })
    escrow-data (begin
                  ;; Check that caller is the seller
                  (asserts! (is-eq tx-sender (get seller escrow-data)) ERR-NOT-AUTHORIZED)
                  
                  ;; Check that escrow is active
                  (asserts! (is-eq (get status escrow-data) ESCROW-STATUS-ACTIVE) ERR-ESCROW-ALREADY-RELEASED)
                  
                  ;; Transfer the asset to the buyer
                  (try! (transfer-asset 
                          (get asset-contract escrow-data) 
                          (get asset-id escrow-data) 
                          (get buyer escrow-data)))
                  
                  ;; Record the transaction
                  (record-transaction 
                    (get listing-id escrow-data) 
                    (some escrow-id) 
                    "transfer-asset" 
                    u0)
                  
                  (ok escrow-id))
    ERR-ESCROW-NOT-FOUND
  )
)

;; Open a dispute for an escrow transaction
(define-public (open-dispute (escrow-id uint) (reason (string-ascii 256)))
  (match (map-get? escrows { escrow-id: escrow-id })
    escrow-data (begin
                  ;; Check that caller is buyer or seller
                  (asserts! (or 
                              (is-eq tx-sender (get buyer escrow-data))
                              (is-eq tx-sender (get seller escrow-data)))
                            ERR-ONLY-BUYER-OR-SELLER)
                  
                  ;; Check that escrow is active
                  (asserts! (is-eq (get status escrow-data) ESCROW-STATUS-ACTIVE) ERR-ESCROW-ALREADY-RELEASED)
                  
                  ;; Check that dispute doesn't already exist
                  (asserts! (is-none (map-get? disputes { escrow-id: escrow-id })) ERR-DISPUTE-ALREADY-EXISTS)
                  
                  ;; Create the dispute
                  (map-set disputes
                    { escrow-id: escrow-id }
                    {
                      initiated-by: tx-sender,
                      reason: reason,
                      created-at: block-height,
                      resolved-at: none,
                      resolution: none
                    }
                  )
                  
                  ;; Update escrow status to disputed
                  (map-set escrows
                    { escrow-id: escrow-id }
                    (merge escrow-data { status: ESCROW-STATUS-DISPUTED })
                  )
                  
                  ;; Record the transaction
                  (record-transaction 
                    (get listing-id escrow-data) 
                    (some escrow-id) 
                    "open-dispute" 
                    u0)
                  
                  (ok escrow-id))
    ERR-ESCROW-NOT-FOUND
  )
)

;; Resolve a dispute (admin only)
(define-public (resolve-dispute (escrow-id uint) (in-favor-of principal) (resolution (string-ascii 256)))
  (begin
    ;; Check that caller is the marketplace admin
    (asserts! (is-eq tx-sender MARKETPLACE-ADMIN) ERR-NOT-AUTHORIZED)
    
    ;; Check that dispute exists
    (asserts! (is-some (map-get? disputes { escrow-id: escrow-id })) ERR-NO-DISPUTE-EXISTS)
    
    (match (map-get? escrows { escrow-id: escrow-id })
      escrow-data (begin
                    ;; Update the dispute record
                    (match (map-get? disputes { escrow-id: escrow-id })
                      dispute (map-set disputes
                                { escrow-id: escrow-id }
                                (merge dispute {
                                  resolved-at: (some block-height),
                                  resolution: (some resolution)
                                })
                              )
                      (err ERR-NO-DISPUTE-EXISTS)
                    )
                    
                    ;; Update escrow status to resolved
                    (map-set escrows
                      { escrow-id: escrow-id }
                      (merge escrow-data { status: ESCROW-STATUS-RESOLVED })
                    )
                    
                    ;; If in favor of buyer, refund the escrow
                    ;; If in favor of seller, release the funds
                    (if (is-eq in-favor-of (get buyer escrow-data))
                      (try! (refund-escrow escrow-id))
                      (try! (release-escrow-funds escrow-id))
                    )
                    
                    ;; Record the transaction
                    (record-transaction 
                      (get listing-id escrow-data) 
                      (some escrow-id) 
                      "resolve-dispute" 
                      u0)
                    
                    (ok escrow-id))
      ERR-ESCROW-NOT-FOUND
    )
  )
)

;; Update marketplace fee percentage (admin only)
(define-public (update-marketplace-fee (new-fee-percentage uint))
  (begin
    ;; Check that caller is the marketplace admin
    (asserts! (is-eq tx-sender MARKETPLACE-ADMIN) ERR-NOT-AUTHORIZED)
    
    ;; Update the marketplace fee
    ;; Note: In a real implementation, this would use define-data-var instead of a constant
    ;; and would require modification to the contract structure
    
    ;; For demo purposes, we'll just return success
    (ok new-fee-percentage)
  )
)