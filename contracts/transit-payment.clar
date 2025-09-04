;; Public Transit Fare Integration System
;; A multi-modal payment system with route planning and usage analytics

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u401))
(define-constant ERR_INSUFFICIENT_BALANCE (err u402))
(define-constant ERR_INVALID_ROUTE (err u403))
(define-constant ERR_RIDE_NOT_FOUND (err u404))
(define-constant ERR_ALREADY_COMPLETED (err u405))
(define-constant ERR_INVALID_FARE (err u406))

;; Data Variables
(define-data-var next-ride-id uint u1)
(define-data-var system-fee-rate uint u2) ;; 2% system fee

;; Data Maps
(define-map user-balances principal uint)
(define-map transit-routes
  { route-id: uint }
  {
    operator: principal,
    base-fare: uint,
    distance: uint,
    active: bool
  })

(define-map ride-records
  { ride-id: uint }
  {
    user: principal,
    route-id: uint,
    start-station: (string-ascii 50),
    end-station: (string-ascii 50),
    fare-paid: uint,
    timestamp: uint,
    completed: bool,
    transfer-discount: uint
  })

(define-map user-stats
  { user: principal }
  {
    total-rides: uint,
    total-spent: uint,
    monthly-rides: uint,
    loyalty-points: uint
  })

(define-map operator-analytics
  { operator: principal }
  {
    total-revenue: uint,
    total-rides: uint,
    average-fare: uint,
    active-routes: uint
  })

;; Public Functions

;; Add funds to user account
(define-public (add-balance (amount uint))
  (let ((current-balance (default-to u0 (map-get? user-balances tx-sender))))
    (map-set user-balances tx-sender (+ current-balance amount))
    (ok true)
  )
)

;; Register a new transit route
(define-public (register-route (route-id uint) (base-fare uint) (distance uint))
  (begin
    (asserts! (> base-fare u0) ERR_INVALID_FARE)
    (asserts! (> distance u0) ERR_INVALID_ROUTE)
    (map-set transit-routes 
      { route-id: route-id }
      {
        operator: tx-sender,
        base-fare: base-fare,
        distance: distance,
        active: true
      })
    (update-operator-routes tx-sender)
    (ok route-id)
  )
)

;; Purchase a ride ticket
(define-public (purchase-ride (route-id uint) (start-station (string-ascii 50)) (end-station (string-ascii 50)))
  (let (
    (route-info (unwrap! (map-get? transit-routes { route-id: route-id }) ERR_INVALID_ROUTE))
    (user-balance (default-to u0 (map-get? user-balances tx-sender)))
    (base-fare (get base-fare route-info))
    (transfer-discount (calculate-transfer-discount tx-sender))
    (final-fare (- base-fare transfer-discount))
    (system-fee (/ (* final-fare (var-get system-fee-rate)) u100))
    (operator-payment (- final-fare system-fee))
    (ride-id (var-get next-ride-id))
  )
    (asserts! (get active route-info) ERR_INVALID_ROUTE)
    (asserts! (>= user-balance final-fare) ERR_INSUFFICIENT_BALANCE)
    
    ;; Deduct fare from user balance
    (map-set user-balances tx-sender (- user-balance final-fare))
    
    ;; Create ride record
    (map-set ride-records
      { ride-id: ride-id }
      {
        user: tx-sender,
        route-id: route-id,
        start-station: start-station,
        end-station: end-station,
        fare-paid: final-fare,
        timestamp: stacks-block-height,
        completed: false,
        transfer-discount: transfer-discount
      })
    
    ;; Update statistics
    (update-user-stats tx-sender final-fare)
    (update-operator-analytics (get operator route-info) operator-payment)
    
    ;; Increment ride ID
    (var-set next-ride-id (+ ride-id u1))
    (ok ride-id)
  )
)

;; Complete a ride
(define-public (complete-ride (ride-id uint))
  (let ((ride-info (unwrap! (map-get? ride-records { ride-id: ride-id }) ERR_RIDE_NOT_FOUND)))
    (asserts! (is-eq (get user ride-info) tx-sender) ERR_NOT_AUTHORIZED)
    (asserts! (not (get completed ride-info)) ERR_ALREADY_COMPLETED)
    
    ;; Mark ride as completed
    (map-set ride-records 
      { ride-id: ride-id }
      (merge ride-info { completed: true }))
    
    ;; Award loyalty points
    (award-loyalty-points tx-sender (get fare-paid ride-info))
    (ok true)
  )
)

;; Update route status (operator only)
(define-public (update-route-status (route-id uint) (active bool))
  (let ((route-info (unwrap! (map-get? transit-routes { route-id: route-id }) ERR_INVALID_ROUTE)))
    (asserts! (is-eq (get operator route-info) tx-sender) ERR_NOT_AUTHORIZED)
    
    (map-set transit-routes 
      { route-id: route-id }
      (merge route-info { active: active }))
    (ok true)
  )
)

;; Read-Only Functions

;; Get user balance
(define-read-only (get-balance (user principal))
  (default-to u0 (map-get? user-balances user))
)

;; Get route information
(define-read-only (get-route-info (route-id uint))
  (map-get? transit-routes { route-id: route-id })
)

;; Get ride details
(define-read-only (get-ride-details (ride-id uint))
  (map-get? ride-records { ride-id: ride-id })
)

;; Get user statistics
(define-read-only (get-user-stats (user principal))
  (map-get? user-stats { user: user })
)

;; Get operator analytics
(define-read-only (get-operator-analytics (operator principal))
  (map-get? operator-analytics { operator: operator })
)

;; Calculate fare with potential transfer discount
(define-read-only (calculate-fare (route-id uint) (user principal))
  (let (
    (route-info (unwrap! (map-get? transit-routes { route-id: route-id }) ERR_INVALID_ROUTE))
    (base-fare (get base-fare route-info))
    (transfer-discount (calculate-transfer-discount user))
  )
    (ok (- base-fare transfer-discount))
  )
)

;; Private Functions

;; Calculate transfer discount based on recent rides
(define-private (calculate-transfer-discount (user principal))
  (let ((user-stats-data (map-get? user-stats { user: user })))
    (if (is-some user-stats-data)
      (let ((monthly-rides (get monthly-rides (unwrap-panic user-stats-data))))
        (if (>= monthly-rides u10) u5 u0)) ;; 5 unit discount for frequent riders
      u0)
  )
)

;; Update user statistics
(define-private (update-user-stats (user principal) (fare uint))
  (let (
    (current-stats (default-to 
      { total-rides: u0, total-spent: u0, monthly-rides: u0, loyalty-points: u0 }
      (map-get? user-stats { user: user })))
  )
    (map-set user-stats 
      { user: user }
      {
        total-rides: (+ (get total-rides current-stats) u1),
        total-spent: (+ (get total-spent current-stats) fare),
        monthly-rides: (+ (get monthly-rides current-stats) u1),
        loyalty-points: (get loyalty-points current-stats)
      })
  )
)

;; Update operator analytics
(define-private (update-operator-analytics (operator principal) (revenue uint))
  (let (
    (current-analytics (default-to
      { total-revenue: u0, total-rides: u0, average-fare: u0, active-routes: u0 }
      (map-get? operator-analytics { operator: operator })))
    (new-total-rides (+ (get total-rides current-analytics) u1))
    (new-total-revenue (+ (get total-revenue current-analytics) revenue))
  )
    (map-set operator-analytics
      { operator: operator }
      {
        total-revenue: new-total-revenue,
        total-rides: new-total-rides,
        average-fare: (/ new-total-revenue new-total-rides),
        active-routes: (get active-routes current-analytics)
      })
  )
)

;; Update operator route count
(define-private (update-operator-routes (operator principal))
  (let (
    (current-analytics (default-to
      { total-revenue: u0, total-rides: u0, average-fare: u0, active-routes: u0 }
      (map-get? operator-analytics { operator: operator })))
  )
    (map-set operator-analytics
      { operator: operator }
      (merge current-analytics { active-routes: (+ (get active-routes current-analytics) u1) }))
  )
)

;; Award loyalty points
(define-private (award-loyalty-points (user principal) (fare uint))
  (let (
    (current-stats (default-to 
      { total-rides: u0, total-spent: u0, monthly-rides: u0, loyalty-points: u0 }
      (map-get? user-stats { user: user })))
    (points-earned (/ fare u10)) ;; 1 point per 10 units spent
  )
    (map-set user-stats 
      { user: user }
      (merge current-stats { loyalty-points: (+ (get loyalty-points current-stats) points-earned) }))
  )
)
