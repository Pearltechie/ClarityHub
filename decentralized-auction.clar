(define-map auctions {auction-id: uint} {creator: principal, item: (string-utf8 50), reserve-price: uint, end-time: uint, highest-bid: uint, highest-bidder: principal})

(define-map bids {auction-id: uint, bidder: principal} {amount: uint})

(define-public (create-auction (auction-id uint) (item (string-utf8 50)) (reserve-price uint) (duration uint))
  (let (
        (end-time (+ (block-height) duration))
        (creator tx-sender)
      )
    (if (map-insert auctions {auction-id: auction-id} {creator: creator, item: item, reserve-price: reserve-price, end-time: end-time, highest-bid: 0, highest-bidder: creator})
        (ok "Auction created successfully")
        (err "Auction ID already exists"))))

(define-public (place-bid (auction-id uint) (bid-amount uint))
  (let (
        (auction (map-get? auctions {auction-id: auction-id}))
        (bidder tx-sender)
      )
    (match auction
      auction-data
      (let (
            (current-highest-bid (get highest-bid auction-data))
            (end-time (get end-time auction-data))
          )
        (if (and (> bid-amount current-highest-bid) (<= (block-height) end-time))
            (begin
              (map-set auctions {auction-id: auction-id} {creator: (get creator auction-data), item: (get item auction-data), reserve-price: (get reserve-price auction-data), end-time: end-time, highest-bid: bid-amount, highest-bidder: bidder})
              (ok "Bid placed successfully"))
            (err "Bid is too low or auction has ended")))
      (err "Auction not found"))))

(define-public (finalize-auction (auction-id uint))
  (let (
        (auction (map-get? auctions {auction-id: auction-id}))
        (caller tx-sender)
      )
    (match auction
      auction-data
      (let (
            (end-time (get end-time auction-data))
            (highest-bid (get highest-bid auction-data))
            (highest-bidder (get highest-bidder auction-data))
          )
        (if (> (block-height) end-time)
            (begin
              (if (> highest-bid 0)
                  (stx-transfer? highest-bid (get creator auction-data) highest-bidder))
              (ok "Auction finalized successfully"))
            (err "Auction has not ended yet")))
      (err "Auction not found"))))
