(define-map bandwidth-listings 
  {id: uint} 
  {provider: principal, price-per-gb: uint, total-gb: uint, available: bool}
)

(define-map bandwidth-leases 
  {lease-id: uint} 
  {buyer: principal, provider: principal, leased-gb: uint, total-cost: uint, status: (string-ascii 10)}
)

(define-map reputation 
  {user: principal} 
  {score: uint, successful-leases: uint}
)

(define-public (list-bandwidth (id uint) (price-per-gb uint) (total-gb uint))
  (begin
    (asserts! (is-none (map-get? bandwidth-listings {id: id})) "Listing already exists")
    (map-insert bandwidth-listings {id: id} {provider: tx-sender, price-per-gb: price-per-gb, total-gb: total-gb, available: true})
    (ok "Bandwidth listed successfully")
  )
)

(define-public (lease-bandwidth (id uint) (lease-id uint) (requested-gb uint))
  (begin
    (let ((listing (map-get? bandwidth-listings {id: id})))
      (asserts! (is-some listing) "Bandwidth listing does not exist")
      (asserts! (is-eq (get available (unwrap-panic listing)) true) "Bandwidth not available")
      (asserts! (<= requested-gb (get total-gb (unwrap-panic listing))) "Not enough bandwidth available")
      
      (let ((cost (* requested-gb (get price-per-gb (unwrap-panic listing)))))
        (stx-transfer? cost tx-sender (get provider (unwrap-panic listing)))
        (map-insert bandwidth-leases {lease-id: lease-id} {buyer: tx-sender, provider: (get provider (unwrap-panic listing)), leased-gb: requested-gb, total-cost: cost, status: "active"})
        (map-set bandwidth-listings {id: id} {provider: (get provider (unwrap-panic listing)), price-per-gb: (get price-per-gb (unwrap-panic listing)), total-gb: (- (get total-gb (unwrap-panic listing)) requested-gb), available: (if (<= requested-gb (get total-gb (unwrap-panic listing))) true false)})
        (ok "Bandwidth leased successfully")
      )
    )
  )
)

(define-public (complete-lease (lease-id uint))
  (begin
    (let ((lease (map-get? bandwidth-leases {lease-id: lease-id})))
      (asserts! (is-some lease) "Lease does not exist")
      (asserts! (is-eq tx-sender (get provider (unwrap-panic lease))) "Not the provider")
      (asserts! (is-eq (get status (unwrap-panic lease)) "active") "Lease is not active")

      (map-set bandwidth-leases {lease-id: lease-id} {buyer: (get buyer (unwrap-panic lease)), provider: (get provider (unwrap-panic lease)), leased-gb: (get leased-gb (unwrap-panic lease)), total-cost: (get total-cost (unwrap-panic lease)), status: "completed"})
      
      (let ((old-rep (map-get? reputation {user: (get provider (unwrap-panic lease))})))
        (if (is-some old-rep)
          (map-set reputation {user: (get provider (unwrap-panic lease))} 
                   {score: (+ (get score (unwrap-panic old-rep)) 10), successful-leases: (+ (get successful-leases (unwrap-panic old-rep)) 1)})
          (map-insert reputation {user: (get provider (unwrap-panic lease))} {score: 10, successful-leases: 1})
        )
      )
      (ok "Lease completed successfully")
    )
  )
)

(define-public (cancel-lease (lease-id uint))
  (begin
    (let ((lease (map-get? bandwidth-leases {lease-id: lease-id})))
      (asserts! (is-some lease) "Lease does not exist")
      (asserts! (is-eq tx-sender (get buyer (unwrap-panic lease))) "Not the buyer")
      (asserts! (is-eq (get status (unwrap-panic lease)) "active") "Lease is not active")

      (stx-transfer? (get total-cost (unwrap-panic lease)) (get provider (unwrap-panic lease)) (as-contract tx-sender))
      (map-set bandwidth-leases {lease-id: lease-id} {buyer: (get buyer (unwrap-panic lease)), provider: (get provider (unwrap-panic lease)), leased-gb: (get leased-gb (unwrap-panic lease)), total-cost: (get total-cost (unwrap-panic lease)), status: "canceled"})

      (ok "Lease canceled and funds refunded")
    )
  )
)
