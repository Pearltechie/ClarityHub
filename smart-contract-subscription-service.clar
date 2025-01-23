(define-data-var subscription-fee uint u100)
(define-map subscribers {user: principal} {expiry: uint})

(define-public (subscribe (months uint))
  (begin
    (let ((cost (* months (var-get subscription-fee))))
      (stx-transfer? cost tx-sender (as-contract tx-sender))
      (let ((current-expiry (default-to 0 (get expiry (map-get? subscribers {user: tx-sender})))))
        (map-set subscribers {user: tx-sender} 
          {expiry: (+ (block-height) (* months 43200))})) ;; 1 month = ~43,200 blocks
      (ok "Subscription successful")))))

(define-read-only (check-subscription (user principal))
  (match (map-get? subscribers {user: user})
    subscription
    (ok (>= (get expiry subscription) (block-height)))
    (err "No subscription found")))
