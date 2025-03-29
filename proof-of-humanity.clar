(define-map registered-humans {user: principal} {verified: bool, attestations: uint})
(define-map attesters {user: principal} {verified: bool, stake: uint})

(define-data-var min-attestations uint u3) ;; Minimum attestations needed
(define-data-var security-deposit uint u100) ;; STX deposit for attestations

;; User submits a Proof of Humanity request
(define-public (register-human ())
  (begin
    (asserts! (is-none (map-get? registered-humans {user: tx-sender})) "User already registered")
    (stx-transfer? (var-get security-deposit) tx-sender (as-contract tx-sender)) ;; Security deposit
    (map-insert registered-humans {user: tx-sender} {verified: false, attestations: u0})
    (ok "Registration request submitted")))

;; Verified humans attest to a new user
(define-public (attest-human (human principal))
  (begin
    (asserts! (is-some (map-get? registered-humans {user: human})) "User not registered")
    (asserts! (is-some (map-get? attesters {user: tx-sender})) "Attester not verified")
    (asserts! (not (get verified (unwrap-panic (map-get? registered-humans {user: human})))) "User already verified")

    ;; Increase attestations count
    (let ((attestation-count (+ 1 (get attestations (unwrap-panic (map-get? registered-humans {user: human}))))))
      (map-set registered-humans {user: human} {verified: (>= attestation-count (var-get min-attestations)), attestations: attestation-count}))
    (ok "Attestation submitted")))

;; Check if a user is a verified human
(define-read-only (is-human (user principal))
  (match (map-get? registered-humans {user: user})
    human-data
    (ok (get verified human-data))
    (err "User not registered")))

;; Penalize false attestations
(define-public (penalize-false-attestation (attester principal))
  (begin
    (asserts! (is-some (map-get? attesters {user: attester})) "Attester not found")
    (let ((stake (get stake (unwrap-panic (map-get? attesters {user: attester})))))
      (asserts! (> stake u0) "No stake to penalize")
      (stx-transfer? stake (as-contract tx-sender) (as-contract tx-sender))
      (map-delete attesters {user: attester})
      (ok "Attester penalized and removed"))))
