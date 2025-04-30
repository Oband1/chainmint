;; verification-authority
;; 
;; This contract establishes the governance structure for asset verification in the Chainmint platform.
;; It maintains a registry of authorized verifiers who can authenticate physical assets and approve 
;; their tokenization. The contract implements a multi-signature approval system for adding or removing 
;; verifiers, preventing centralization of trust.

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-ALREADY-AUTHORIZED (err u101))
(define-constant ERR-VERIFIER-NOT-FOUND (err u102))
(define-constant ERR-INVALID-PROPOSAL (err u103))
(define-constant ERR-ALREADY-VOTED (err u104))
(define-constant ERR-PROPOSAL-NOT-ACTIVE (err u105))
(define-constant ERR-QUORUM-NOT-REACHED (err u106))
(define-constant ERR-SELF-REMOVAL (err u107))
(define-constant ERR-INVALID-EXPERTISE (err u108))

;; Data space definitions

;; Contract owner - has special privileges for initial setup and emergency operations
(define-data-var contract-owner principal tx-sender)

;; Verifier status mapping - tracks all authorized verifiers
(define-map verifiers principal 
  {
    active: bool,                 ;; Whether the verifier is currently active
    date-added: uint,             ;; Block height when verifier was added
    expertise: (list 10 (string-ascii 30)),  ;; Areas of expertise (e.g., "art", "real-estate")
    verification-count: uint      ;; Number of verifications performed
  }
)

;; Proposal types
(define-constant PROPOSAL-TYPE-ADD-VERIFIER u1)
(define-constant PROPOSAL-TYPE-REMOVE-VERIFIER u2)

;; Governance proposal data structure
(define-map governance-proposals uint 
  {
    proposer: principal,          ;; Who proposed it
    proposal-type: uint,          ;; Type of proposal (add/remove verifier)
    target-verifier: principal,   ;; Verifier to add/remove
    expertise: (optional (list 10 (string-ascii 30))),  ;; Required for add proposals
    created-at: uint,             ;; Block height when created
    expires-at: uint,             ;; Block height when expires
    is-active: bool,              ;; Whether proposal is still active
    votes-for: uint,              ;; Count of yes votes
    votes-against: uint           ;; Count of no votes
  }
)

;; Track proposal votes to prevent duplicate voting
(define-map proposal-votes {proposal-id: uint, voter: principal} bool)

;; Current proposal ID counter
(define-data-var next-proposal-id uint u1)

;; Governance parameters
(define-data-var min-approval-percent uint u60)  ;; Minimum % of yes votes needed (60%)
(define-data-var proposal-duration uint u144)    ;; Proposals last ~1 day (144 blocks ≈ 24 hours)
(define-data-var min-verifiers-required uint u3) ;; Minimum verifiers needed in the system

;; Verifier count
(define-data-var verifier-count uint u0)

;; Private functions

;; Check if caller is contract owner
(define-private (is-contract-owner)
  (is-eq tx-sender (var-get contract-owner))
)

;; Check if caller is an active verifier
(define-private (is-active-verifier (address principal))
  (default-to false (get active (map-get? verifiers address)))
)

;; Get the current block height
(define-private (current-block-height)
  block-height
)

;; Calculate if a proposal has achieved quorum
(define-private (proposal-achieved-quorum (proposal-id uint))
  (let (
    (proposal (unwrap! (map-get? governance-proposals proposal-id) false))
    (total-votes (+ (get votes-for proposal) (get votes-against proposal)))
    (approve-percent (if (> total-votes u0)
      (/ (* (get votes-for proposal) u100) total-votes)
      u0
    ))
  )
    (>= approve-percent (var-get min-approval-percent))
  )
)

;; Check if a list of expertise areas contains only valid values
(define-private (validate-expertise (expertise-list (list 10 (string-ascii 30))))
  (let (
    (valid-areas (list 
      "art" "real-estate" "jewelry" "collectibles" "machinery" 
      "vehicles" "luxury-goods" "commodities" "antiquities" "other"
    ))
  )
    ;; This is a simplified validation - in a real contract you might want more sophisticated validation
    (> (len expertise-list) u0)  ;; At least one area of expertise required
  )
)

;; Execute a proposal after it has passed
(define-private (execute-proposal (proposal-id uint))
  (let (
    (proposal (unwrap! (map-get? governance-proposals proposal-id) ERR-INVALID-PROPOSAL))
    (proposal-type (get proposal-type proposal))
    (target (get target-verifier proposal))
  )
    (if (is-eq proposal-type PROPOSAL-TYPE-ADD-VERIFIER)
      (add-verifier-internal target (unwrap! (get expertise proposal) ERR-INVALID-PROPOSAL))
      (remove-verifier-internal target)
    )
  )
)

;; Internal function to add a verifier
(define-private (add-verifier-internal (address principal) (expertise-areas (list 10 (string-ascii 30))))
  (begin
    (asserts! (not (is-active-verifier address)) ERR-ALREADY-AUTHORIZED)
    (asserts! (validate-expertise expertise-areas) ERR-INVALID-EXPERTISE)
    
    (map-set verifiers address {
      active: true,
      date-added: (current-block-height),
      expertise: expertise-areas,
      verification-count: u0
    })
    
    ;; Increment verifier count
    (var-set verifier-count (+ (var-get verifier-count) u1))
    
    (ok true)
  )
)

;; Internal function to remove a verifier
(define-private (remove-verifier-internal (address principal))
  (begin
    (asserts! (is-active-verifier address) ERR-VERIFIER-NOT-FOUND)
    (asserts! (not (is-eq address tx-sender)) ERR-SELF-REMOVAL)
    ;; Ensure we maintain minimum required verifiers
    (asserts! (> (var-get verifier-count) (var-get min-verifiers-required)) ERR-NOT-AUTHORIZED)
    
    ;; Update verifier record to inactive but preserve history
    (let ((verifier-data (unwrap! (map-get? verifiers address) ERR-VERIFIER-NOT-FOUND)))
      (map-set verifiers address 
        (merge verifier-data {active: false})
      )
    )
    
    ;; Decrement verifier count
    (var-set verifier-count (- (var-get verifier-count) u1))
    
    (ok true)
  )
)

;; Read-only functions

;; Get information about a verifier
(define-read-only (get-verifier-info (address principal))
  (map-get? verifiers address)
)

;; Check if an address is an active verifier
(define-read-only (is-verifier (address principal))
  (is-active-verifier address)
)

;; Get current verifier count
(define-read-only (get-verifier-count)
  (var-get verifier-count)
)

;; Get proposal details
(define-read-only (get-proposal (proposal-id uint))
  (map-get? governance-proposals proposal-id)
)

;; Check if an address has voted on a proposal
(define-read-only (has-voted-on-proposal (proposal-id uint) (voter principal))
  (default-to false (map-get? proposal-votes {proposal-id: proposal-id, voter: voter}))
)

;; Public functions

;; Initialize the contract with the first set of verifiers
;; Only callable once by the contract owner
(define-public (initialize-verifiers (initial-verifiers (list 5 {address: principal, expertise: (list 10 (string-ascii 30))})))
  (begin
    (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (var-get verifier-count) u0) ERR-NOT-AUTHORIZED)
    
    ;; Add each initial verifier
    (map add-initial-verifier initial-verifiers)
    
    (ok true)
  )
)

;; Helper function for initialize-verifiers
(define-private (add-initial-verifier (verifier {address: principal, expertise: (list 10 (string-ascii 30))}))
  (begin
    (add-verifier-internal (get address verifier) (get expertise verifier))
    true
  )
)

;; Transfer ownership of the contract
(define-public (transfer-ownership (new-owner principal))
  (begin
    (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
    (var-set contract-owner new-owner)
    (ok true)
  )
)

;; Propose to add a new verifier
(define-public (propose-add-verifier (address principal) (expertise-areas (list 10 (string-ascii 30))))
  (begin
    (asserts! (is-active-verifier tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (not (is-active-verifier address)) ERR-ALREADY-AUTHORIZED)
    (asserts! (validate-expertise expertise-areas) ERR-INVALID-EXPERTISE)
    
    (let (
      (proposal-id (var-get next-proposal-id))
      (current-height (current-block-height))
    )
      ;; Create the proposal
      (map-set governance-proposals proposal-id {
        proposer: tx-sender,
        proposal-type: PROPOSAL-TYPE-ADD-VERIFIER,
        target-verifier: address,
        expertise: (some expertise-areas),
        created-at: current-height,
        expires-at: (+ current-height (var-get proposal-duration)),
        is-active: true,
        votes-for: u1,  ;; Proposer's vote counts
        votes-against: u0
      })
      
      ;; Record proposer's vote
      (map-set proposal-votes {proposal-id: proposal-id, voter: tx-sender} true)
      
      ;; Increment proposal ID
      (var-set next-proposal-id (+ proposal-id u1))
      
      (ok proposal-id)
    )
  )
)

;; Propose to remove an existing verifier
(define-public (propose-remove-verifier (address principal))
  (begin
    (asserts! (is-active-verifier tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (is-active-verifier address) ERR-VERIFIER-NOT-FOUND)
    (asserts! (not (is-eq address tx-sender)) ERR-SELF-REMOVAL)
    
    (let (
      (proposal-id (var-get next-proposal-id))
      (current-height (current-block-height))
    )
      ;; Create the proposal
      (map-set governance-proposals proposal-id {
        proposer: tx-sender,
        proposal-type: PROPOSAL-TYPE-REMOVE-VERIFIER,
        target-verifier: address,
        expertise: none,
        created-at: current-height,
        expires-at: (+ current-height (var-get proposal-duration)),
        is-active: true,
        votes-for: u1,  ;; Proposer's vote counts
        votes-against: u0
      })
      
      ;; Record proposer's vote
      (map-set proposal-votes {proposal-id: proposal-id, voter: tx-sender} true)
      
      ;; Increment proposal ID
      (var-set next-proposal-id (+ proposal-id u1))
      
      (ok proposal-id)
    )
  )
)

;; Vote on an active proposal
(define-public (vote-on-proposal (proposal-id uint) (vote bool))
  (begin
    (asserts! (is-active-verifier tx-sender) ERR-NOT-AUTHORIZED)
    
    (let (
      (proposal (unwrap! (map-get? governance-proposals proposal-id) ERR-INVALID-PROPOSAL))
    )
      ;; Check if proposal is active
      (asserts! (get is-active proposal) ERR-PROPOSAL-NOT-ACTIVE)
      ;; Check if proposal has not expired
      (asserts! (<= (current-block-height) (get expires-at proposal)) ERR-PROPOSAL-NOT-ACTIVE)
      ;; Check if verifier hasn't already voted
      (asserts! (not (has-voted-on-proposal proposal-id tx-sender)) ERR-ALREADY-VOTED)
      
      ;; Record vote
      (map-set proposal-votes {proposal-id: proposal-id, voter: tx-sender} true)
      
      ;; Update vote count
      (map-set governance-proposals proposal-id
        (merge proposal {
          votes-for: (if vote (+ (get votes-for proposal) u1) (get votes-for proposal)),
          votes-against: (if vote (get votes-against proposal) (+ (get votes-against proposal) u1))
        })
      )
      
      (ok true)
    )
  )
)

;; Finalize a proposal after voting period ends
(define-public (finalize-proposal (proposal-id uint))
  (begin
    (let (
      (proposal (unwrap! (map-get? governance-proposals proposal-id) ERR-INVALID-PROPOSAL))
    )
      ;; Check if proposal is active
      (asserts! (get is-active proposal) ERR-PROPOSAL-NOT-ACTIVE)
      ;; Check if proposal has expired or can be resolved early
      (asserts! (or
        (> (current-block-height) (get expires-at proposal))
        (>= (get votes-for proposal) (/ (* (var-get verifier-count) (var-get min-approval-percent)) u100))
      ) ERR-PROPOSAL-NOT-ACTIVE)
      
      ;; Mark proposal as inactive
      (map-set governance-proposals proposal-id
        (merge proposal { is-active: false })
      )
      
      ;; If quorum reached, execute the proposal
      (if (proposal-achieved-quorum proposal-id)
        (execute-proposal proposal-id)
        ERR-QUORUM-NOT-REACHED
      )
    )
  )
)

;; Increment verification count for a verifier
;; To be called by asset verification contracts when verifier approves an asset
(define-public (record-verification (verifier principal))
  (begin
    ;; Only other contracts should call this, add appropriate authentication
    ;; in a real implementation based on your contract architecture
    (asserts! (is-active-verifier verifier) ERR-VERIFIER-NOT-FOUND)
    
    (let ((verifier-data (unwrap! (map-get? verifiers verifier) ERR-VERIFIER-NOT-FOUND)))
      (map-set verifiers verifier
        (merge verifier-data {
          verification-count: (+ (get verification-count verifier-data) u1)
        })
      )
    )
    
    (ok true)
  )
)

;; Update governance parameters - only owner can call
(define-public (update-governance-params (new-approval-percent uint) (new-duration uint) (new-min-verifiers uint))
  (begin
    (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
    ;; Validate inputs
    (asserts! (and (>= new-approval-percent u51) (<= new-approval-percent u100)) ERR-INVALID-PROPOSAL)
    (asserts! (>= new-duration u72) ERR-INVALID-PROPOSAL)  ;; At least 12 hours (72 blocks)
    (asserts! (>= new-min-verifiers u2) ERR-INVALID-PROPOSAL)  ;; At least 2 verifiers required
    
    (var-set min-approval-percent new-approval-percent)
    (var-set proposal-duration new-duration)
    (var-set min-verifiers-required new-min-verifiers)
    
    (ok true)
  )
)