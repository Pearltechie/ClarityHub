(define-map storage-providers {provider: principal} {capacity: uint, price-per-month: uint, verified: bool})

(define-map leased-storage {user: principal} {provider: principal, size: uint, expiry: uint})

(define-data-var min-storage-fee uint u50) ;; Minimum STX fee per month

;; Register as a storage provider
(define-public (register-storage-provider (capacity uint) (price uint))
  (begin
    (asserts! (> capacity u0) "Capacity must be greater than zero")
    (asserts! (>= price (var-get min-storage-fee)) "Price too low")
    (map-insert storage-providers {provider: tx-sender} {capacity: capacity, price-per-month: price, verified: false})
    (ok "Storage provider registered, awaiting verification")))

;; Lease storage
(define-public (lease-storage (provider principal) (size uint) (months uint))
  (begin
    (asserts! (is-some (map-get? storage-providers {provider: provider})) "Invalid storage provider")
    (let ((provider-data (unwrap-panic (map-get? storage-providers {provider: provider})))
          (total-cost (* months (get price-per-month (unwrap-panic (map-get? storage-providers {provider: provider}))))))  
      (asserts! (<= size (get capacity provider-data)) "Requested size exceeds provider capacity")
      (stx-transfer? total-cost tx-sender provider)
      (map-insert leased-storage {user: tx-sender} {provider: provider, size: size, expiry: (+ (block-height) (* months 43200))})
      (ok "Storage leased successfully")))))

;; Check lease status
(define-read-only (check-lease (user principal))
  (match (map-get? leased-storage {user: user})
    lease-data
    (ok {provider: (get provider lease-data), size: (get size lease-data), expiry: (get expiry lease-data)})
    (err "No active lease found")))

;; Renew storage lease
(define-public (renew-storage (months uint))
  (begin
    (asserts! (is-some (map-get? leased-storage {user: tx-sender})) "No active lease to renew")
    (let ((lease-data (unwrap-panic (map-get? leased-storage {user: tx-sender})))
          (provider (get provider lease-data))
          (price (get price-per-month (unwrap-panic (map-get? storage-providers {provider: provider}))))
          (total-cost (* months price)))
      (stx-transfer? total-cost tx-sender provider)
      (map-set leased-storage {user: tx-sender} {provider: provider, size: (get size lease-data), expiry: (+ (block-height) (* months 43200))})
      (ok "Lease renewed successfully")))))

;; Storage provider reclaims expired storage
(define-public (reclaim-storage (user principal))
  (begin
    (asserts! (is-some (map-get? leased-storage {user: user})) "No storage to reclaim")
    (let ((lease-data (unwrap-panic (map-get? leased-storage {user: user}))))
      (asserts! (< (get expiry lease-data) (block-height)) "Lease has not expired yet")
      (map-delete leased-storage {user: user})
      (ok "Storage reclaimed by provider"))))
