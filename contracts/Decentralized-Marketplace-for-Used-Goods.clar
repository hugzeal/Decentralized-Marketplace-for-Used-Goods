(define-constant contract-owner tx-sender)
(define-constant listing-fee u1000)
(define-constant escrow-fee u500)

(define-data-var next-listing-id uint u1)
(define-data-var next-nft-id uint u1)

(define-map Listings
    { id: uint }
    {
        seller: principal,
        price: uint,
        title: (string-ascii 50),
        description: (string-ascii 256),
        status: (string-ascii 20),
        nft-id: uint,
    }
)

(define-map Escrows
    { id: uint }
    {
        buyer: principal,
        seller: principal,
        amount: uint,
        status: (string-ascii 20),
    }
)

(define-map NFTOwnership
    { id: uint }
    { owner: principal }
)

(define-non-fungible-token used-goods-nft uint)

(define-public (create-listing
        (title (string-ascii 50))
        (description (string-ascii 256))
        (price uint)
    )
    (let (
            (listing-id (var-get next-listing-id))
            (nft-id (var-get next-nft-id))
        )
        (asserts! (> price u0) (err u1))
        (try! (stx-transfer? listing-fee tx-sender contract-owner))
        (try! (nft-mint? used-goods-nft nft-id tx-sender))
        (map-set Listings { id: listing-id } {
            seller: tx-sender,
            price: price,
            title: title,
            description: description,
            status: "active",
            nft-id: nft-id,
        })
        (map-set NFTOwnership { id: nft-id } { owner: tx-sender })
        (var-set next-listing-id (+ listing-id u1))
        (var-set next-nft-id (+ nft-id u1))
        (ok listing-id)
    )
)

(define-public (buy-item (listing-id uint))
    (let (
            (listing (unwrap! (map-get? Listings { id: listing-id }) (err u2)))
            (current-price (get price listing))
            (current-seller (get seller listing))
            (nft-id (get nft-id listing))
        )
        (asserts! (is-eq (get status listing) "active") (err u3))
        (asserts! (not (is-eq tx-sender current-seller)) (err u4))
        (asserts! (>= (stx-get-balance tx-sender) current-price) (err u5))
        (try! (stx-transfer? current-price tx-sender contract-owner))
        (map-set Escrows { id: listing-id } {
            buyer: tx-sender,
            seller: current-seller,
            amount: current-price,
            status: "pending",
        })
        (map-set Listings { id: listing-id }
            (merge listing { status: "in-escrow" })
        )
        (ok listing-id)
    )
)

(define-private (create-escrow
        (listing-id uint)
        (buyer principal)
        (seller principal)
        (amount uint)
    )
    (begin
        (map-set Escrows { id: listing-id } {
            buyer: buyer,
            seller: seller,
            amount: amount,
            status: "pending",
        })
        (ok true)
    )
)

(define-public (confirm-delivery (listing-id uint))
    (let (
            (listing (unwrap! (map-get? Listings { id: listing-id }) (err u5)))
            (escrow (unwrap! (map-get? Escrows { id: listing-id }) (err u6)))
            (nft-id (get nft-id listing))
        )
        (asserts! (is-eq (get status escrow) "pending") (err u7))
        (asserts! (is-eq tx-sender (get buyer escrow)) (err u8))
        (try! (stx-transfer? (- (get amount escrow) escrow-fee) contract-owner
            (get seller escrow)
        ))
        (try! (stx-transfer? escrow-fee contract-owner contract-owner))
        (try! (nft-transfer? used-goods-nft nft-id (get seller listing)
            (get buyer escrow)
        ))
        (map-set Listings { id: listing-id }
            (merge listing { status: "completed" })
        )
        (map-set Escrows { id: listing-id }
            (merge escrow { status: "completed" })
        )
        (map-set NFTOwnership { id: nft-id } { owner: (get buyer escrow) })
        (ok true)
    )
)

(define-public (cancel-listing (listing-id uint))
    (let ((listing (unwrap! (map-get? Listings { id: listing-id }) (err u9))))
        (asserts! (is-eq tx-sender (get seller listing)) (err u10))
        (asserts! (is-eq (get status listing) "active") (err u11))
        (try! (nft-burn? used-goods-nft (get nft-id listing) tx-sender))
        (map-set Listings { id: listing-id }
            (merge listing { status: "cancelled" })
        )
        (ok true)
    )
)

(define-read-only (get-listing (listing-id uint))
    (ok (map-get? Listings { id: listing-id }))
)

(define-read-only (get-escrow (listing-id uint))
    (ok (map-get? Escrows { id: listing-id }))
)

(define-read-only (get-nft-owner (nft-id uint))
    (ok (map-get? NFTOwnership { id: nft-id }))
)

(define-public (update-listing-price
        (listing-id uint)
        (new-price uint)
    )
    (let ((listing (unwrap! (map-get? Listings { id: listing-id }) (err u12))))
        (asserts! (is-eq tx-sender (get seller listing)) (err u13))
        (asserts! (is-eq (get status listing) "active") (err u14))
        (asserts! (> new-price u0) (err u15))
        (map-set Listings { id: listing-id } (merge listing { price: new-price }))
        (ok true)
    )
)
