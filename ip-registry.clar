(define-map ip-registry { ip-hash: (buff 32) } { owner: principal, timestamp: uint })

(define-public (register-ip (ip-hash (buff 32)))
  (begin
    (asserts! (is-none (map-get? ip-registry { ip-hash: ip-hash })) "IP already registered")
    (map-insert ip-registry { ip-hash: ip-hash } { owner: tx-sender, timestamp: block-height })
    (ok "IP registered successfully"))))

(define-public (transfer-ip (ip-hash (buff 32)) (new-owner principal))
  (let ((ip (map-get? ip-registry { ip-hash: ip-hash })))
    (match ip
      ip-data
      (if (is-eq (get owner ip-data) tx-sender)
          (begin
            (map-set ip-registry { ip-hash: ip-hash } { owner: new-owner, timestamp: (get timestamp ip-data) })
            (ok "IP ownership transferred"))
          (err "Only the owner can transfer")))
      (err "IP not found"))))
