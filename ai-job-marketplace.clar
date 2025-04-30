(define-map job-listings 
  {id: uint} 
  {employer: principal, worker: (optional principal), amount: uint, status: (string-ascii 10)}
)

(define-map reputation 
  {user: principal} 
  {score: uint, completed-jobs: uint}
)

(define-public (post-job (id uint) (amount uint))
  (begin
    (asserts! (is-none (map-get? job-listings {id: id})) "Job already exists")
    (stx-transfer? amount tx-sender (as-contract tx-sender))
    (map-insert job-listings {id: id} {employer: tx-sender, worker: none, amount: amount, status: "open"})
    (ok "Job posted successfully")
  )
)

(define-public (apply-for-job (id uint) (worker principal))
  (begin
    (let ((job (map-get? job-listings {id: id})))
      (asserts! (is-some job) "Job does not exist")
      (asserts! (is-none (get worker (unwrap-panic job))) "Job already assigned")
      (map-set job-listings {id: id} {employer: (get employer (unwrap-panic job)), worker: (some worker), amount: (get amount (unwrap-panic job)), status: "assigned"})
      (ok "Job assigned")
    )
  )
)

(define-public (submit-work (id uint) (worker principal))
  (begin
    (let ((job (map-get? job-listings {id: id})))
      (asserts! (is-some job) "Job does not exist")
      (asserts! (is-eq (get worker (unwrap-panic job)) (some worker)) "Not assigned to you")
      (map-set job-listings {id: id} {employer: (get employer (unwrap-panic job)), worker: (get worker (unwrap-panic job)), amount: (get amount (unwrap-panic job)), status: "submitted"})
      (ok "Work submitted")
    )
  )
)

(define-public (approve-work (id uint))
  (begin
    (let ((job (map-get? job-listings {id: id})))
      (asserts! (is-some job) "Job does not exist")
      (asserts! (is-eq tx-sender (get employer (unwrap-panic job))) "Not employer")
      (asserts! (is-eq (get status (unwrap-panic job)) "submitted") "Work not submitted yet")
      
      (let ((worker (get worker (unwrap-panic job))) (amount (get amount (unwrap-panic job))))
        (stx-transfer? amount (as-contract tx-sender) (unwrap-panic worker))
        (map-set job-listings {id: id} {employer: (get employer (unwrap-panic job)), worker: (get worker (unwrap-panic job)), amount: amount, status: "completed"})
        
        (let ((old-rep (map-get? reputation {user: (unwrap-panic worker)})))
          (if (is-some old-rep)
            (map-set reputation {user: (unwrap-panic worker)} 
                     {score: (+ (get score (unwrap-panic old-rep)) 10), completed-jobs: (+ (get completed-jobs (unwrap-panic old-rep)) 1)})
            (map-insert reputation {user: (unwrap-panic worker)} {score: 10, completed-jobs: 1})
          )
        )
        (ok "Work approved and payment released")
      )
    )
  )
)

(define-public (dispute-job (id uint))
  (begin
    (let ((job (map-get? job-listings {id: id})))
      (asserts! (is-some job) "Job does not exist")
      (asserts! (is-eq (get status (unwrap-panic job)) "submitted") "Job is not in dispute stage")
      (map-set job-listings {id: id} {employer: (get employer (unwrap-panic job)), worker: (get worker (unwrap-panic job)), amount: (get amount (unwrap-panic job)), status: "dispute"})
      (ok "Job dispute initiated")
    )
  )
)
