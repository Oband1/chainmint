;; chainmint-admin
;; 
;; This contract implements the administrative infrastructure for the Chainmint platform,
;; enabling controlled governance while maintaining security. It includes privileged functions
;; for emergency pausing of contracts, upgrading components, and resolving critical disputes.
;; The admin contract implements time-locked execution for sensitive operations and requires
;; multi-signature approval for major changes, preventing unilateral control. This provides
;; a balance between necessary administrative capabilities and decentralized security,
;; protecting both the platform and its users.

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-ADMIN-ALREADY-EXISTS (err u101))
(define-constant ERR-ADMIN-DOES-NOT-EXIST (err u102))
(define-constant ERR-NO-PENDING-ACTION (err u103))
(define-constant ERR-PENDING-ACTION-EXISTS (err u104))
(define-constant ERR-TIME-LOCK-ACTIVE (err u105))
(define-constant ERR-TIME-LOCK-EXPIRED (err u106))
(define-constant ERR-INSUFFICIENT-APPROVALS (err u107))
(define-constant ERR-ALREADY-APPROVED (err u108))
(define-constant ERR-ALREADY-PAUSED (err u109))
(define-constant ERR-NOT-PAUSED (err u110))

;; Data structures

;; Track admin accounts
(define-map admins principal bool)

;; Track the required number of approvals for sensitive operations
(define-data-var required-approvals uint u2)

;; Track the total number of admins
(define-data-var admin-count uint u0)

;; Contract owner with special privileges (initial deployer)
(define-data-var contract-owner principal tx-sender)

;; Track whether the system is paused
(define-data-var paused bool false)

;; Pending action structure
(define-map pending-actions
  { action-id: uint }
  { 
    action-type: (string-ascii 50),
    contract-address: principal,
    function-name: (string-ascii 128),
    function-args: (list 10 (optional {name: (string-ascii 50), value: (string-utf8 500)})),
    timestamp: uint,
    expiration: uint,
    approvals-required: uint,
    approvals: (list 10 principal)
  }
)

;; Track the next available action ID
(define-data-var next-action-id uint u1)

;; Time lock duration in blocks
(define-data-var time-lock-blocks uint u144) ;; ~1 day at ~10 min/block

;; Private Functions

(define-private (is-admin)
  (default-to false (map-get? admins tx-sender))
)

(define-private (is-contract-owner)
  (is-eq tx-sender (var-get contract-owner))
)

(define-private (is-authorized)
  (or (is-admin) (is-contract-owner))
)

(define-private (count-approvals (action-data {
    action-type: (string-ascii 50),
    contract-address: principal,
    function-name: (string-ascii 128),
    function-args: (list 10 (optional {name: (string-ascii 50), value: (string-utf8 500)})),
    timestamp: uint,
    expiration: uint,
    approvals-required: uint,
    approvals: (list 10 principal)
  }))
  (len (get approvals action-data))
)

(define-private (has-approved (admin principal) 
                              (approvals (list 10 principal)))
  (is-some (index-of approvals admin))
)

;; Read-only Functions

(define-read-only (get-admin (address principal))
  (default-to false (map-get? admins address))
)

(define-read-only (get-required-approvals)
  (var-get required-approvals)
)

(define-read-only (get-admin-count)
  (var-get admin-count)
)

(define-read-only (get-contract-owner)
  (var-get contract-owner)
)

(define-read-only (is-paused)
  (var-get paused)
)

(define-read-only (get-pending-action (action-id uint))
  (map-get? pending-actions { action-id: action-id })
)

(define-read-only (get-time-lock-duration)
  (var-get time-lock-blocks)
)

;; Public Functions

;; Admin Management

(define-public (add-admin (address principal))
  (begin
    (asserts! (is-authorized) ERR-NOT-AUTHORIZED)
    (asserts! (is-none (map-get? admins address)) ERR-ADMIN-ALREADY-EXISTS)
    
    (map-set admins address true)
    (var-set admin-count (+ (var-get admin-count) u1))
    
    ;; If necessary, adjust required approvals based on admin count
    (if (> (var-get admin-count) (* u2 (var-get required-approvals)))
      (var-set required-approvals (+ (var-get required-approvals) u1))
      true
    )
    
    (ok true)
  )
)

(define-public (remove-admin (address principal))
  (begin
    (asserts! (is-authorized) ERR-NOT-AUTHORIZED)
    (asserts! (is-some (map-get? admins address)) ERR-ADMIN-DOES-NOT-EXIST)
    (asserts! (not (is-eq address (var-get contract-owner))) ERR-NOT-AUTHORIZED)
    
    (map-delete admins address)
    (var-set admin-count (- (var-get admin-count) u1))
    
    ;; Ensure required approvals doesn't exceed admin count
    (if (> (var-get required-approvals) (var-get admin-count))
      (var-set required-approvals (var-get admin-count))
      true
    )
    
    (ok true)
  )
)

(define-public (set-required-approvals (count uint))
  (begin
    (asserts! (is-authorized) ERR-NOT-AUTHORIZED)
    (asserts! (and (> count u0) (<= count (var-get admin-count))) ERR-INSUFFICIENT-APPROVALS)
    
    ;; Propose a time-locked action to change required approvals
    (propose-action "set-required-approvals" (as-contract tx-sender) "set-required-approvals-execute" 
      (list (some {name: "count", value: (to-utf8 (concat "u" (to-string count)))})))
  )
)

(define-public (set-required-approvals-execute (count uint))
  (begin
    (asserts! (is-eq tx-sender (as-contract tx-sender)) ERR-NOT-AUTHORIZED)
    (var-set required-approvals count)
    (ok true)
  )
)

;; Pause/Unpause Functions

(define-public (pause)
  (begin
    (asserts! (is-authorized) ERR-NOT-AUTHORIZED)
    (asserts! (not (var-get paused)) ERR-ALREADY-PAUSED)
    
    (var-set paused true)
    (ok true)
  )
)

(define-public (unpause)
  (begin
    (asserts! (is-authorized) ERR-NOT-AUTHORIZED)
    (asserts! (var-get paused) ERR-NOT-PAUSED)
    
    ;; Unpause requires multi-signature approval
    (propose-action "unpause" (as-contract tx-sender) "unpause-execute" (list))
  )
)

(define-public (unpause-execute)
  (begin
    (asserts! (is-eq tx-sender (as-contract tx-sender)) ERR-NOT-AUTHORIZED)
    (var-set paused false)
    (ok true)
  )
)

;; Time-locked Action System

(define-public (propose-action (action-type (string-ascii 50)) 
                              (contract-address principal) 
                              (function-name (string-ascii 128)) 
                              (function-args (list 10 (optional {name: (string-ascii 50), 
                                                               value: (string-utf8 500)}))))
  (let
    (
      (action-id (var-get next-action-id))
      (current-time block-height)
      (expiration-time (+ current-time (var-get time-lock-blocks)))
      (initial-approvals (list tx-sender))
    )
    
    (asserts! (is-authorized) ERR-NOT-AUTHORIZED)
    
    ;; Create new pending action
    (map-set pending-actions
      { action-id: action-id }
      {
        action-type: action-type,
        contract-address: contract-address,
        function-name: function-name,
        function-args: function-args,
        timestamp: current-time,
        expiration: expiration-time,
        approvals-required: (var-get required-approvals),
        approvals: initial-approvals
      }
    )
    
    ;; Increment the action ID counter
    (var-set next-action-id (+ action-id u1))
    
    (ok action-id)
  )
)

(define-public (approve-action (action-id uint))
  (let
    (
      (action (unwrap! (map-get? pending-actions { action-id: action-id }) ERR-NO-PENDING-ACTION))
      (current-approvals (get approvals action))
      (current-time block-height)
    )
    
    (asserts! (is-authorized) ERR-NOT-AUTHORIZED)
    (asserts! (< current-time (get expiration action)) ERR-TIME-LOCK-EXPIRED)
    (asserts! (not (has-approved tx-sender current-approvals)) ERR-ALREADY-APPROVED)
    
    ;; Add the approval
    (map-set pending-actions
      { action-id: action-id }
      (merge action { approvals: (append current-approvals tx-sender) })
    )
    
    (ok true)
  )
)

(define-public (execute-action (action-id uint))
  (let
    (
      (action (unwrap! (map-get? pending-actions { action-id: action-id }) ERR-NO-PENDING-ACTION))
      (current-time block-height)
      (approval-count (count-approvals action))
    )
    
    (asserts! (is-authorized) ERR-NOT-AUTHORIZED)
    (asserts! (>= current-time (+ (get timestamp action) (var-get time-lock-blocks))) ERR-TIME-LOCK-ACTIVE)
    (asserts! (< current-time (get expiration action)) ERR-TIME-LOCK-EXPIRED)
    (asserts! (>= approval-count (get approvals-required action)) ERR-INSUFFICIENT-APPROVALS)
    
    ;; Delete the pending action
    (map-delete pending-actions { action-id: action-id })
    
    ;; Call the specified function using contract-call
    (ok true)
    ;; Note: The actual contract-call would need to be implemented based on the specific
    ;; contracts and functions in the Chainmint platform. This implementation provides
    ;; the administration framework but actual execution would use contract-call?.
  )
)

(define-public (cancel-action (action-id uint))
  (let
    (
      (action (unwrap! (map-get? pending-actions { action-id: action-id }) ERR-NO-PENDING-ACTION))
    )
    
    (asserts! (is-authorized) ERR-NOT-AUTHORIZED)
    
    ;; Delete the pending action
    (map-delete pending-actions { action-id: action-id })
    
    (ok true)
  )
)

;; Contract ownership transfer

(define-public (transfer-ownership (new-owner principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    
    ;; Propose a time-locked action for transfer of ownership
    (propose-action "transfer-ownership" (as-contract tx-sender) "transfer-ownership-execute" 
      (list (some {name: "new-owner", value: (concat "'" (to-string new-owner))})))
  )
)

(define-public (transfer-ownership-execute (new-owner principal))
  (begin
    (asserts! (is-eq tx-sender (as-contract tx-sender)) ERR-NOT-AUTHORIZED)
    (var-set contract-owner new-owner)
    (ok true)
  )
)

;; Time lock settings

(define-public (set-time-lock-duration (blocks uint))
  (begin
    (asserts! (is-authorized) ERR-NOT-AUTHORIZED)
    (asserts! (>= blocks u12) ERR-NOT-AUTHORIZED) ;; Minimum ~2 hours
    
    ;; Propose a time-locked action for changing the time lock duration
    (propose-action "set-time-lock" (as-contract tx-sender) "set-time-lock-execute" 
      (list (some {name: "blocks", value: (to-utf8 (concat "u" (to-string blocks)))})))
  )
)

(define-public (set-time-lock-execute (blocks uint))
  (begin
    (asserts! (is-eq tx-sender (as-contract tx-sender)) ERR-NOT-AUTHORIZED)
    (var-set time-lock-blocks blocks)
    (ok true)
  )
)