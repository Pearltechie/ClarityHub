(define-data-var oracle principal 'SP3XG3Z0XQX0QX0QX0QX0QX0QX0QX0QX0QX0Q)

(define-public (set-oracle (new-oracle principal))
  (begin
    (asserts! (is-eq tx-sender 'SZ2JCWQ0X0KX0X0X0X0X0X0X0X0X0X0X0X0X0X) "Unauthorized")
    (var-set oracle new-oracle)
    (ok "Oracle updated")))

(define-public (oracle-trigger-event)
  (begin
    (asserts! (is-eq tx-sender (var-get oracle)) "Unauthorized oracle")
    (var-set event-occurred true)
    (ok "Event triggered by oracle")))
