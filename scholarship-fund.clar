(define-map scholarships { student: principal } { amount: uint, milestone: uint, disbursed: uint })
(define-data-var total-fund uint u0)

(define-public (donate (amount uint))
  (begin
    (var-set total-fund (+ (var-get total-fund) amount))
    (stx-transfer? amount tx-sender (as-contract tx-sender))
    (ok "Donation received"))))

(define-public (apply-for-scholarship (student principal) (amount uint) (milestone uint))
  (begin
    (asserts! (> (var-get total-fund) amount) "Insufficient funds")
    (map-insert scholarships { student: student } { amount: amount, milestone: milestone, disbursed: 0 })
    (ok "Scholarship granted"))))

(define-public (release-funds (student principal) (score uint))
  (let ((scholarship (map-get? scholarships { student: student })))
    (match scholarship
      scholarship-data
      (if (>= score (get milestone scholarship-data))
          (let ((remaining (- (get amount scholarship-data) (get disbursed scholarship-data))))
            (stx-transfer? remaining (as-contract tx-sender) student)
            (map-set scholarships { student: student } { amount: (get amount scholarship-data), milestone: (get milestone scholarship-data), disbursed: (get amount scholarship-data) })
            (ok "Funds disbursed")))
          (err "Milestone not met")))
      (err "Scholarship not found"))))
