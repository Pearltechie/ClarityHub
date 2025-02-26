(define-map bounties { id: uint } { creator: principal, description: (string-utf8 100), reward: uint, status: (string-ascii 10) })
(define-map applications { id: uint, applicant: principal } { approved: bool })
(define-map escrow { id: uint } { amount: uint })

(define-public (post-bounty (id uint) (description (string-utf8 100)) (reward uint))
  (begin
    (asserts! (is-none (map-get? bounties { id: id })) "Bounty ID already exists")
    (map-insert bounties { id: id } { creator: tx-sender, description: description, reward: reward, status: "open" })
    (map-insert escrow { id: id } { amount: reward })
    (ok "Bounty posted successfully"))))

(define-public (apply-bounty (id uint))
  (asserts! (is-some (map-get? bounties { id: id })) "Bounty not found")
  (map-set applications { id: id, applicant: tx-sender } { approved: false })
  (ok "Application submitted"))

(define-public (approve-bounty (id uint) (applicant principal))
  (let ((bounty (map-get? bounties { id: id })))
    (match bounty
      bounty-data
      (begin
        (asserts! (is-eq (get creator bounty-data) tx-sender) "Only the creator can approve")
        (asserts! (is-eq (get status bounty-data) "open") "Bounty is not open")
        (map-set bounties { id: id } { creator: (get creator bounty-data), description: (get description bounty-data), reward: (get reward bounty-data), status: "completed" })
        (stx-transfer? (get reward bounty-data) tx-sender applicant)
        (ok "Bounty approved and payment sent"))
      (err "Bounty not found"))))
