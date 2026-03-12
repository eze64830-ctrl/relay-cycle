;; RelayCycle - Dynamic DeFi Lending Protocol
;; Clarity Version: 2
;; Epoch: 2.1

;; ===================================
;; Constants
;; ===================================

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-authorized (err u101))
(define-constant err-invalid-amount (err u102))
(define-constant err-insufficient-collateral (err u103))
(define-constant err-insufficient-balance (err u104))
(define-constant err-position-not-found (err u105))
(define-constant err-rate-locked (err u106))
(define-constant err-invalid-cycle (err u107))
(define-constant err-liquidation-threshold (err u108))

;; Protocol parameters
(define-constant basis-points u10000)
(define-constant min-collateral-ratio u15000) ;; 150%
(define-constant liquidation-ratio u12000) ;; 120%
(define-constant cycle-duration u21600) ;; 6 hours in seconds
(define-constant base-interest-rate u500) ;; 5% in basis points

;; ===================================
;; Data Variables
;; ===================================

(define-data-var current-cycle uint u0)
(define-data-var cycle-start-time uint u0)
(define-data-var total-supply-relay uint u0)
(define-data-var total-supply-cycle uint u0)
(define-data-var total-deposits uint u0)
(define-data-var total-borrows uint u0)
(define-data-var current-interest-rate uint base-interest-rate)
(define-data-var protocol-paused bool false)

;; ===================================
;; Data Maps
;; ===================================

;; Token balances
(define-map relay-balances principal uint)
(define-map cycle-balances principal uint)

;; Deposit positions
(define-map deposits 
  principal 
  {
    amount: uint,
    timestamp: uint,
    accumulated-yield: uint
  }
)

;; Borrow positions
(define-map borrows 
  principal 
  {
    amount: uint,
    collateral: uint,
    interest-rate: uint,
    timestamp: uint,
    rate-locked: bool,
    locked-until: uint
  }
)

;; Cycle rate history
(define-map cycle-rates 
  uint 
  {
    rate: uint,
    utilization: uint,
    timestamp: uint
  }
)

;; User allowances for RELAY token
(define-map relay-allowances 
  {owner: principal, spender: principal} 
  uint
)

;; User allowances for CYCLE token
(define-map cycle-allowances 
  {owner: principal, spender: principal} 
  uint
)

;; ===================================
;; Private Functions
;; ===================================

(define-private (calculate-utilization-rate)
  (let
    (
      (total-deposited (var-get total-deposits))
      (total-borrowed (var-get total-borrows))
    )
    (if (is-eq total-deposited u0)
      u0
      (/ (* total-borrowed basis-points) total-deposited)
    )
  )
)

(define-private (calculate-dynamic-rate (utilization uint))
  (let
    (
      (base-rate (var-get current-interest-rate))
      (utilization-multiplier (/ utilization u100))
    )
    (+ base-rate (* utilization-multiplier u10))
  )
)

(define-private (get-balance-relay (account principal))
  (default-to u0 (map-get? relay-balances account))
)

(define-private (get-balance-cycle (account principal))
  (default-to u0 (map-get? cycle-balances account))
)

(define-private (calculate-interest (principal-amount uint) (rate uint) (time-elapsed uint))
  (let
    (
      (annual-interest (/ (* principal-amount rate) basis-points))
      (seconds-per-year u31536000)
    )
    (/ (* annual-interest time-elapsed) seconds-per-year)
  )
)

(define-private (check-liquidation-threshold (collateral uint) (debt uint))
  (let
    (
      (required-collateral (/ (* debt liquidation-ratio) basis-points))
    )
    (>= collateral required-collateral)
  )
)

;; ===================================
;; Public Functions - Token Operations
;; ===================================

(define-public (transfer-relay (amount uint) (sender principal) (recipient principal))
  (let
    (
      (sender-balance (get-balance-relay sender))
    )
    (asserts! (is-eq tx-sender sender) err-not-authorized)
    (asserts! (>= sender-balance amount) err-insufficient-balance)
    (asserts! (> amount u0) err-invalid-amount)
    
    (map-set relay-balances sender (- sender-balance amount))
    (map-set relay-balances recipient (+ (get-balance-relay recipient) amount))
    
    (print {event: "relay-transfer", sender: sender, recipient: recipient, amount: amount})
    (ok true)
  )
)

(define-public (transfer-cycle (amount uint) (sender principal) (recipient principal))
  (let
    (
      (sender-balance (get-balance-cycle sender))
    )
    (asserts! (is-eq tx-sender sender) err-not-authorized)
    (asserts! (>= sender-balance amount) err-insufficient-balance)
    (asserts! (> amount u0) err-invalid-amount)
    
    (map-set cycle-balances sender (- sender-balance amount))
    (map-set cycle-balances recipient (+ (get-balance-cycle recipient) amount))
    
    (print {event: "cycle-transfer", sender: sender, recipient: recipient, amount: amount})
    (ok true)
  )
)

;; ===================================
;; Public Functions - Deposit Operations
;; ===================================

(define-public (deposit (amount uint))
  (let
    (
      (current-deposit (default-to 
        {amount: u0, timestamp: u0, accumulated-yield: u0}
        (map-get? deposits tx-sender)
      ))
    )
    (asserts! (not (var-get protocol-paused)) err-not-authorized)
    (asserts! (> amount u0) err-invalid-amount)
    
    ;; Update deposit position
    (map-set deposits tx-sender {
      amount: (+ (get amount current-deposit) amount),
      timestamp: block-height,
      accumulated-yield: (get accumulated-yield current-deposit)
    })
    
    ;; Update total deposits
    (var-set total-deposits (+ (var-get total-deposits) amount))
    
    ;; Mint CYCLE tokens as reward (1:1 ratio for simplicity)
    (map-set cycle-balances tx-sender (+ (get-balance-cycle tx-sender) (/ amount u10)))
    (var-set total-supply-cycle (+ (var-get total-supply-cycle) (/ amount u10)))
    
    (print {event: "deposit", user: tx-sender, amount: amount, total-deposits: (var-get total-deposits)})
    (ok true)
  )
)

(define-public (withdraw (amount uint))
  (let
    (
      (current-deposit (unwrap! (map-get? deposits tx-sender) err-position-not-found))
      (deposited-amount (get amount current-deposit))
    )
    (asserts! (not (var-get protocol-paused)) err-not-authorized)
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (>= deposited-amount amount) err-insufficient-balance)
    
    ;; Update deposit position
    (if (is-eq deposited-amount amount)
      (map-delete deposits tx-sender)
      (map-set deposits tx-sender {
        amount: (- deposited-amount amount),
        timestamp: block-height,
        accumulated-yield: (get accumulated-yield current-deposit)
      })
    )
    
    ;; Update total deposits
    (var-set total-deposits (- (var-get total-deposits) amount))
    
    (print {event: "withdraw", user: tx-sender, amount: amount, total-deposits: (var-get total-deposits)})
    (ok true)
  )
)

;; ===================================
;; Public Functions - Borrow Operations
;; ===================================

(define-public (borrow (amount uint) (collateral-amount uint))
  (let
    (
      (required-collateral (/ (* amount min-collateral-ratio) basis-points))
      (current-rate (var-get current-interest-rate))
    )
    (asserts! (not (var-get protocol-paused)) err-not-authorized)
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (>= collateral-amount required-collateral) err-insufficient-collateral)
    (asserts! (<= amount (var-get total-deposits)) err-insufficient-balance)
    
    ;; Create borrow position
    (map-set borrows tx-sender {
      amount: amount,
      collateral: collateral-amount,
      interest-rate: current-rate,
      timestamp: block-height,
      rate-locked: false,
      locked-until: u0
    })
    
    ;; Update total borrows
    (var-set total-borrows (+ (var-get total-borrows) amount))
    
    (print {event: "borrow", user: tx-sender, amount: amount, collateral: collateral-amount, rate: current-rate})
    (ok true)
  )
)

(define-public (repay (amount uint))
  (let
    (
      (borrow-position (unwrap! (map-get? borrows tx-sender) err-position-not-found))
      (borrowed-amount (get amount borrow-position))
      (time-elapsed (- block-height (get timestamp borrow-position)))
      (interest (calculate-interest borrowed-amount (get interest-rate borrow-position) time-elapsed))
      (total-debt (+ borrowed-amount interest))
    )
    (asserts! (not (var-get protocol-paused)) err-not-authorized)
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (<= amount total-debt) err-invalid-amount)
    
    ;; Update or delete borrow position
    (if (is-eq amount total-debt)
      (begin
        (map-delete borrows tx-sender)
        (var-set total-borrows (- (var-get total-borrows) borrowed-amount))
      )
      (begin
        (map-set borrows tx-sender (merge borrow-position {
          amount: (- total-debt amount),
          timestamp: block-height
        }))
        (var-set total-borrows (- (var-get total-borrows) 
          (if (<= amount borrowed-amount) amount borrowed-amount)
        ))
      )
    )
    
    (print {event: "repay", user: tx-sender, amount: amount, interest: interest})
    (ok true)
  )
)

;; ===================================
;; Public Functions - Rate Management
;; ===================================

(define-public (lock-interest-rate (duration uint))
  (let
    (
      (borrow-position (unwrap! (map-get? borrows tx-sender) err-position-not-found))
      (current-rate (var-get current-interest-rate))
    )
    (asserts! (not (get rate-locked borrow-position)) err-rate-locked)
    (asserts! (> duration u0) err-invalid-amount)
    
    (map-set borrows tx-sender (merge borrow-position {
      rate-locked: true,
      locked-until: (+ block-height duration),
      interest-rate: current-rate
    }))
    
    (print {event: "rate-locked", user: tx-sender, rate: current-rate, duration: duration})
    (ok true)
  )
)

(define-public (advance-cycle)
  (let
    (
      (current (var-get current-cycle))
      (utilization (calculate-utilization-rate))
      (new-rate (calculate-dynamic-rate utilization))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    ;; Record cycle data
    (map-set cycle-rates current {
      rate: (var-get current-interest-rate),
      utilization: utilization,
      timestamp: block-height
    })
    
    ;; Advance to next cycle
    (var-set current-cycle (+ current u1))
    (var-set cycle-start-time block-height)
    (var-set current-interest-rate new-rate)
    
    (print {event: "cycle-advanced", cycle: (var-get current-cycle), new-rate: new-rate, utilization: utilization})
    (ok true)
  )
)

;; ===================================
;; Public Functions - Liquidation
;; ===================================

(define-public (liquidate (borrower principal))
  (let
    (
      (borrow-position (unwrap! (map-get? borrows borrower) err-position-not-found))
      (debt (get amount borrow-position))
      (collateral (get collateral borrow-position))
    )
    (asserts! (not (check-liquidation-threshold collateral debt)) err-liquidation-threshold)
    
    ;; Remove borrow position
    (map-delete borrows borrower)
    (var-set total-borrows (- (var-get total-borrows) debt))
    
    ;; Transfer collateral to liquidator (simplified)
    (print {event: "liquidation", borrower: borrower, liquidator: tx-sender, debt: debt, collateral: collateral})
    (ok true)
  )
)

;; ===================================
;; Public Functions - Admin
;; ===================================

(define-public (mint-relay (recipient principal) (amount uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (> amount u0) err-invalid-amount)
    
    (map-set relay-balances recipient (+ (get-balance-relay recipient) amount))
    (var-set total-supply-relay (+ (var-get total-supply-relay) amount))
    
    (print {event: "relay-minted", recipient: recipient, amount: amount})
    (ok true)
  )
)

(define-public (toggle-pause)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set protocol-paused (not (var-get protocol-paused)))
    (ok (var-get protocol-paused))
  )
)

(define-public (update-interest-rate (new-rate uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-rate u5000) err-invalid-amount) ;; Max 50%
    
    (var-set current-interest-rate new-rate)
    (print {event: "rate-updated", new-rate: new-rate})
    (ok true)
  )
)

;; ===================================
;; Read-Only Functions
;; ===================================

(define-read-only (get-relay-balance (account principal))
  (ok (get-balance-relay account))
)

(define-read-only (get-cycle-balance (account principal))
  (ok (get-balance-cycle account))
)

(define-read-only (get-deposit-info (account principal))
  (ok (map-get? deposits account))
)

(define-read-only (get-borrow-info (account principal))
  (ok (map-get? borrows account))
)

(define-read-only (get-current-cycle)
  (ok (var-get current-cycle))
)

(define-read-only (get-current-rate)
  (ok (var-get current-interest-rate))
)

(define-read-only (get-utilization-rate)
  (ok (calculate-utilization-rate))
)

(define-read-only (get-protocol-stats)
  (ok {
    total-deposits: (var-get total-deposits),
    total-borrows: (var-get total-borrows),
    utilization: (calculate-utilization-rate),
    current-rate: (var-get current-interest-rate),
    current-cycle: (var-get current-cycle),
    relay-supply: (var-get total-supply-relay),
    cycle-supply: (var-get total-supply-cycle),
    paused: (var-get protocol-paused)
  })
)

(define-read-only (get-cycle-history (cycle-id uint))
  (ok (map-get? cycle-rates cycle-id))
)

(define-read-only (check-liquidation-status (borrower principal))
  (match (map-get? borrows borrower)
    position 
      (ok {
        can-liquidate: (not (check-liquidation-threshold 
          (get collateral position) 
          (get amount position)
        )),
        collateral: (get collateral position),
        debt: (get amount position)
      })
    (ok {can-liquidate: false, collateral: u0, debt: u0})
  )
)
