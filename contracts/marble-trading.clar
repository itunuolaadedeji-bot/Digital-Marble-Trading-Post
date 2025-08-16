;; marble-trading-post.clar
;; Digital Marble Trading Post - Collectible exchange with rarity tracking

;; Constants
(define-constant ERR_NOT_AUTHORIZED (err u1001))
(define-constant ERR_MARBLE_NOT_FOUND (err u1002))
(define-constant ERR_LISTING_NOT_FOUND (err u1003))
(define-constant ERR_INSUFFICIENT_FUNDS (err u1004))
(define-constant ERR_INVALID_RARITY (err u1005))

;; Data Variables
(define-data-var next-marble-id uint u1)
(define-data-var next-listing-id uint u1)

;; Data Maps
(define-map marbles uint {
    owner: principal,
    name: (string-ascii 64),
    rarity: uint,
    color: (string-ascii 32),
    pattern: (string-ascii 32),
    created-at: uint
})

(define-map listings uint {
    marble-id: uint,
    seller: principal,
    price: uint,
    created-at: uint,
    active: bool
})

(define-map user-collections principal (list 100 uint))
(define-map rarity-counts uint uint)

;; Private Functions
(define-private (calculate-base-value (rarity uint))
    (if (<= rarity u1)
        u10000 ;; Common: 100 STX
        (if (<= rarity u2)
            u25000 ;; Uncommon: 250 STX
            (if (<= rarity u3)
                u50000 ;; Rare: 500 STX
                u100000)))) ;; Legendary: 1000 STX

(define-private (update-user-collections (seller principal) (marble-id uint) (buyer principal))
    (let ((buyer-collection (default-to (list) (map-get? user-collections buyer))))
        (map-set user-collections seller (list))
        (map-set user-collections buyer (unwrap-panic (as-max-len? (append buyer-collection marble-id) u100)))
        true))

(define-private (remove-from-collection (collection (list 100 uint)) (marble-id uint))
    (list))

;; Public Functions
(define-public (mint-marble (name (string-ascii 64)) (rarity uint) (color (string-ascii 32)) (pattern (string-ascii 32)))
    (let ((marble-id (var-get next-marble-id))
          (current-count (default-to u0 (map-get? rarity-counts rarity))))
        (asserts! (<= rarity u4) ERR_INVALID_RARITY)
        (map-set marbles marble-id {
            owner: tx-sender,
            name: name,
            rarity: rarity,
            color: color,
            pattern: pattern,
            created-at: stacks-block-height
        })
        (map-set rarity-counts rarity (+ current-count u1))
        (let ((current-collection (default-to (list) (map-get? user-collections tx-sender))))
            (map-set user-collections tx-sender (unwrap-panic (as-max-len? (append current-collection marble-id) u100))))
        (var-set next-marble-id (+ marble-id u1))
        (ok marble-id)))

(define-public (create-listing (marble-id uint) (price uint))
    (let ((marble (unwrap! (map-get? marbles marble-id) ERR_MARBLE_NOT_FOUND))
          (listing-id (var-get next-listing-id)))
        (asserts! (is-eq (get owner marble) tx-sender) ERR_NOT_AUTHORIZED)
        (map-set listings listing-id {
            marble-id: marble-id,
            seller: tx-sender,
            price: price,
            created-at: stacks-block-height,
            active: true
        })
        (var-set next-listing-id (+ listing-id u1))
        (ok listing-id)))

(define-public (buy-marble (listing-id uint))
    (let ((listing (unwrap! (map-get? listings listing-id) ERR_LISTING_NOT_FOUND))
          (marble (unwrap! (map-get? marbles (get marble-id listing)) ERR_MARBLE_NOT_FOUND)))
        (asserts! (get active listing) ERR_LISTING_NOT_FOUND)
        (try! (stx-transfer? (get price listing) tx-sender (get seller listing)))
        (map-set marbles (get marble-id listing) (merge marble { owner: tx-sender }))
        (map-set listings listing-id (merge listing { active: false }))
        (update-user-collections (get seller listing) (get marble-id listing) tx-sender)
        (ok (get marble-id listing))))

(define-public (cancel-listing (listing-id uint))
    (let ((listing (unwrap! (map-get? listings listing-id) ERR_LISTING_NOT_FOUND)))
        (asserts! (is-eq (get seller listing) tx-sender) ERR_NOT_AUTHORIZED)
        (asserts! (get active listing) ERR_LISTING_NOT_FOUND)
        (map-set listings listing-id (merge listing { active: false }))
        (ok true)))

;; Read-only Functions
(define-read-only (get-marble (marble-id uint))
    (map-get? marbles marble-id))

(define-read-only (get-listing (listing-id uint))
    (map-get? listings listing-id))

(define-read-only (get-user-collection (user principal))
    (default-to (list) (map-get? user-collections user)))

(define-read-only (get-marble-value (marble-id uint))
    (let ((marble (unwrap! (map-get? marbles marble-id) ERR_MARBLE_NOT_FOUND))
          (base-value (calculate-base-value (get rarity marble)))
          (rarity-scarcity (/ u1000000 (default-to u1 (map-get? rarity-counts (get rarity marble))))))
        (ok (+ base-value rarity-scarcity))))

(define-read-only (get-collection-stats (user principal))
    (let ((collection (get-user-collection user)))
        (ok {
            total-marbles: (len collection),
            collection-value: (fold + (map get-single-marble-value collection) u0)
        })))

(define-private (get-single-marble-value (marble-id uint))
    (match (get-marble-value marble-id)
        ok-value ok-value
        err-value u0))
