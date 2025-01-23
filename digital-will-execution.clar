(define-data-var owner principal tx-sender)
(define-data-var beneficiary principal none)
(define-data-var inheritance uint u0)
(define-data-var executed bool false)

(define-public (set-beneficiary (beneficiary principal) (amount uint))
  (begin
    (asserts! (is-eq tx-sender (var-get owner)) "Only the owner can set the beneficiary")
    (var-set beneficiary beneficiary)
    (var-set inheritance amount)
    (ok "Beneficiary set successfully")))

(define-public (execute-will (death-certificate-hash (buff 32)))
  (begin
    (asserts! (not (var-get executed)) "Will has already been executed")
    (asserts! (is-some (var-get beneficiary)) "Beneficiary not set")
    (stx-transfer? (var-get inheritance) (as-contract tx-sender) (unwrap-panic (var-get beneficiary)))
    (var-set executed true)
    (ok "Will executed successfully")))
