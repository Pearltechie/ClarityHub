(define-map proposals {proposal-id: uint} {creator: principal, description: (string-utf8 200), votes: uint, ai-score: uint, executed: bool})

(define-map members {member: principal} {is-active: bool})

(define-data-var min-ai-score uint u70) ;; Minimum AI score for execution
(define-data-var min-votes uint u10) ;; Minimum votes for execution

(define-public (submit-proposal (proposal-id uint) (description (string-utf8 200)) (ai-score uint))
  (begin
    (asserts! (is-some (map-get? members {member: tx-sender})) "Only DAO members can submit proposals")
    (map-insert proposals {proposal-id: proposal-id} {creator: tx-sender, description: description, votes: 0, ai-score: ai-score, executed: false})
    (ok "Proposal submitted")))

(define-public (vote-on-proposal (proposal-id uint))
  (begin
    (asserts! (is-some (map-get? proposals {proposal-id: proposal-id})) "Proposal not found")
    (map-set proposals {proposal-id: proposal-id} 
      {creator: (get creator (unwrap-panic (map-get? proposals {proposal-id: proposal-id}))), 
       description: (get description (unwrap-panic (map-get? proposals {proposal-id: proposal-id}))), 
       votes: (+ 1 (get votes (unwrap-panic (map-get? proposals {proposal-id: proposal-id})))), 
       ai-score: (get ai-score (unwrap-panic (map-get? proposals {proposal-id: proposal-id}))), 
       executed: false})
    (ok "Vote registered")))

(define-public (execute-proposal (proposal-id uint))
  (begin
    (asserts! (is-some (map-get? proposals {proposal-id: proposal-id})) "Proposal not found")
    (asserts! (>= (get ai-score (unwrap-panic (map-get? proposals {proposal-id: proposal-id}))) (var-get min-ai-score)) "AI score too low")
    (asserts! (>= (get votes (unwrap-panic (map-get? proposals {proposal-id: proposal-id}))) (var-get min-votes)) "Not enough votes")
    (map-set proposals {proposal-id: proposal-id} 
      {creator: (get creator (unwrap-panic (map-get? proposals {proposal-id: proposal-id}))), 
       description: (get description (unwrap-panic (map-get? proposals {proposal-id: proposal-id}))), 
       votes: (get votes (unwrap-panic (map-get? proposals {proposal-id: proposal-id}))), 
       ai-score: (get ai-score (unwrap-panic (map-get? proposals {proposal-id: proposal-id}))), 
       executed: true})
    (ok "Proposal executed successfully")))
