(define-map investments { investor: principal } { amount: uint, unlock-time: uint })

(define-public (invest (amount uint) (duration uint))
  (begin
    (asserts! (> amount u0) "Amount must be greater than zero")
    (let ((unlock-time (+ block-height duration)))
      (map-insert investments { investor: tx-sender } { amount: amount, unlock-time: unlock-time })
      (stx-transfer? amount tx-sender (as-contract tx-sender))
      (ok "Investment locked")))))

(define-public (withdraw)
  (let ((investment (map-get? investments { investor: tx-sender })))
    (match investment
      invest-data
      (if (>= block-height (get unlock-time invest-data))
          (begin
            (stx-transfer? (get amount invest-data) (as-contract tx-sender) tx-sender)
            (map-delete investments { investor: tx-sender })
            (ok "Funds withdrawn"))
          (err "Funds still locked")))
      (err "No investment found"))))
