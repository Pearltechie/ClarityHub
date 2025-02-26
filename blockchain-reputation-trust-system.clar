(define-map reputation-scores 
  { user: principal } 
  { score: int, last-updated: uint })

(define-map feedbacks 
  { reviewer: principal, reviewee: principal } 
  { rating: int, timestamp: uint })

(define-constant max-score 100)
(define-constant min-score -100)

(define-public (give-feedback (reviewee principal) (rating int))
  (begin
    (asserts! (>= rating -10) "Invalid rating: Minimum is -10")
    (asserts! (<= rating 10) "Invalid rating: Maximum is 10")
    (asserts! (not (is-eq tx-sender reviewee)) "Cannot rate yourself")

    (let ((existing-feedback (map-get? feedbacks { reviewer: tx-sender, reviewee: reviewee })))
      (match existing-feedback
        feedback
        (err "Feedback already provided")
        
        (begin
          (map-insert feedbacks { reviewer: tx-sender, reviewee: reviewee } 
            { rating: rating, timestamp: block-height })
          
          (let ((current-score (default-to 0 (get score (map-get? reputation-scores { user: reviewee })))))
            (let ((new-score (+ current-score rating)))
              (map-set reputation-scores { user: reviewee } 
                { score: (max (min-score) (min new-score max-score)), last-updated: block-height })
              (ok "Feedback submitted successfully"))))))))

(define-public (decay-reputation (user principal))
  (let ((rep (map-get? reputation-scores { user: user })))
    (match rep
      data
      (let ((time-diff (- block-height (get last-updated data))))
        (if (> time-diff 10000)
            (begin
              (map-set reputation-scores { user: user } 
                { score: (max min-score (- (get score data) 5)), last-updated: block-height })
              (ok "Reputation score decayed"))
            (err "Decay period not reached")))
      (err "User has no reputation score"))))

(define-read-only (get-reputation (user principal))
  (map-get? reputation-scores { user: user }))
