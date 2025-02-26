(define-map nft-leases { nft-id: uint } { owner: principal, renter: principal, total-price: uint, paid-amount: uint, payment-deadline: uint })

(define-public (lease-nft (nft-id uint) (renter principal) (total-price uint))
  (begin
    (asserts! (is-none (map-get? nft-leases { nft-id: nft-id })) "NFT already leased")
    (map-insert nft-leases { nft-id: nft-id } { owner: tx-sender, renter: renter, total-price: total-price, paid-amount: 0, payment-deadline: (+ block-height 1000) })
    (ok "NFT leased successfully"))))

(define-public (make-payment (nft-id uint) (amount uint))
  (let ((lease (map-get? nft-leases { nft-id: nft-id })))
    (match lease
      lease-data
      (let ((new-paid-amount (+ (get paid-amount lease-data) amount)))
        (asserts! (<= new-paid-amount (get total-price lease-data)) "Overpayment not allowed")
        (stx-transfer? amount tx-sender (get owner lease-data))
        (if (= new-paid-amount (get total-price lease-data))
            (begin
              (map-delete nft-leases { nft-id: nft-id })
              (ok "Ownership transferred!"))
            (begin
              (map-set nft-leases { nft-id: nft-id } { owner: (get owner lease-data), renter: (get renter lease-data), total-price: (get total-price lease-data), paid-amount: new-paid-amount, payment-deadline: (get payment-deadline lease-data) })
              (ok "Partial payment made")))))
      (err "NFT lease not found"))))

(define-public (reclaim-nft (nft-id uint))
  (let ((lease (map-get? nft-leases { nft-id: nft-id })))
    (match lease
      lease-data
      (if (> block-height (get payment-deadline lease-data))
          (begin
            (map-delete nft-leases { nft-id: nft-id })
            (ok "NFT reclaimed due to default"))
          (err "Payment deadline not reached")))
      (err "NFT lease not found"))))
