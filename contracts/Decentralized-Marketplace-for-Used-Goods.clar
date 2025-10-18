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
        (let ((fee (get-listing-fee-for-seller tx-sender)))
            (try! (stx-transfer? fee tx-sender contract-owner))
        )
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
(define-constant dispute-fee u200)
(define-constant arbitrator-reward u100)
(define-constant verification-fee u2000)
(define-constant verified-listing-fee u500)
(define-constant auction-fee u800)
(define-constant min-bid-increment u100)
(define-constant auction-extension-blocks u10)
(define-constant offer-duration-blocks u144)

(define-data-var next-dispute-id uint u1)
(define-data-var next-verification-id uint u1)
(define-data-var next-auction-id uint u1)
(define-data-var next-offer-id uint u1)

(define-map Arbitrators
    { arbitrator: principal }
    {
        active: bool,
        reputation-score: uint,
        disputes-resolved: uint,
    }
)

(define-map Disputes
    { id: uint }
    {
        listing-id: uint,
        buyer: principal,
        seller: principal,
        arbitrator: principal,
        reason: (string-ascii 256),
        status: (string-ascii 20),
        resolution: (string-ascii 20),
        created-at: uint,
    }
)

(define-map SellerVerifications
    { seller: principal }
    {
        verification-level: (string-ascii 20),
        verified-at: uint,
        verification-id: uint,
        documents-hash: (string-ascii 64),
        status: (string-ascii 20),
    }
)

(define-map VerificationRequests
    { id: uint }
    {
        seller: principal,
        requested-level: (string-ascii 20),
        documents-hash: (string-ascii 64),
        status: (string-ascii 20),
        submitted-at: uint,
        reviewed-at: uint,
    }
)

(define-map Auctions
    { id: uint }
    {
        seller: principal,
        title: (string-ascii 50),
        description: (string-ascii 256),
        starting-price: uint,
        current-bid: uint,
        highest-bidder: principal,
        end-block: uint,
        status: (string-ascii 20),
        nft-id: uint,
    }
)

(define-map AuctionBids
    {
        auction-id: uint,
        bidder: principal,
    }
    {
        bid-amount: uint,
        bid-block: uint,
        refunded: bool,
    }
)

(define-map PriceOffers
    { id: uint }
    {
        listing-id: uint,
        buyer: principal,
        seller: principal,
        offer-amount: uint,
        status: (string-ascii 20),
        created-at: uint,
        expires-at: uint,
    }
)

(define-map ListingOffers
    {
        listing-id: uint,
        buyer: principal,
    }
    { offer-id: uint }
)

(define-public (register-arbitrator)
    (begin
        (map-set Arbitrators { arbitrator: tx-sender } {
            active: true,
            reputation-score: u100,
            disputes-resolved: u0,
        })
        (ok true)
    )
)

(define-public (create-dispute
        (listing-id uint)
        (reason (string-ascii 256))
    )
    (let (
            (dispute-id (var-get next-dispute-id))
            (escrow (unwrap! (map-get? Escrows { id: listing-id }) (err u20)))
            (listing (unwrap! (map-get? Listings { id: listing-id }) (err u21)))
        )
        (asserts! (is-eq (get status escrow) "pending") (err u22))
        (asserts! (is-eq tx-sender (get buyer escrow)) (err u23))
        (try! (stx-transfer? dispute-fee tx-sender contract-owner))
        (map-set Disputes { id: dispute-id } {
            listing-id: listing-id,
            buyer: (get buyer escrow),
            seller: (get seller escrow),
            arbitrator: contract-owner,
            reason: reason,
            status: "open",
            resolution: "pending",
            created-at: burn-block-height,
        })
        (map-set Escrows { id: listing-id } (merge escrow { status: "disputed" }))
        (var-set next-dispute-id (+ dispute-id u1))
        (ok dispute-id)
    )
)

(define-public (assign-arbitrator
        (dispute-id uint)
        (arbitrator principal)
    )
    (let ((dispute (unwrap! (map-get? Disputes { id: dispute-id }) (err u24))))
        (asserts! (is-eq tx-sender contract-owner) (err u25))
        (asserts! (is-eq (get status dispute) "open") (err u26))
        (asserts! (is-some (map-get? Arbitrators { arbitrator: arbitrator }))
            (err u27)
        )
        (map-set Disputes { id: dispute-id }
            (merge dispute {
                arbitrator: arbitrator,
                status: "assigned",
            })
        )
        (ok true)
    )
)

(define-public (resolve-dispute
        (dispute-id uint)
        (resolution (string-ascii 20))
    )
    (let (
            (dispute (unwrap! (map-get? Disputes { id: dispute-id }) (err u28)))
            (listing-id (get listing-id dispute))
            (escrow (unwrap! (map-get? Escrows { id: listing-id }) (err u29)))
            (listing (unwrap! (map-get? Listings { id: listing-id }) (err u30)))
            (arbitrator-data (unwrap! (map-get? Arbitrators { arbitrator: tx-sender }) (err u31)))
        )
        (asserts! (is-eq tx-sender (get arbitrator dispute)) (err u32))
        (asserts! (is-eq (get status dispute) "assigned") (err u33))
        (asserts!
            (or (is-eq resolution "buyer-wins") (is-eq resolution "seller-wins"))
            (err u34)
        )
        (if (is-eq resolution "buyer-wins")
            (try! (stx-transfer? (get amount escrow) contract-owner (get buyer escrow)))
            (begin
                (try! (stx-transfer? (- (get amount escrow) arbitrator-reward)
                    contract-owner (get seller escrow)
                ))
                (try! (nft-transfer? used-goods-nft (get nft-id listing)
                    (get seller listing) (get buyer escrow)
                ))
            )
        )
        (try! (stx-transfer? arbitrator-reward contract-owner tx-sender))
        (map-set Disputes { id: dispute-id }
            (merge dispute {
                status: "resolved",
                resolution: resolution,
            })
        )
        (map-set Escrows { id: listing-id } (merge escrow { status: "resolved" }))
        (map-set Listings { id: listing-id }
            (merge listing { status: "completed" })
        )
        (map-set Arbitrators { arbitrator: tx-sender }
            (merge arbitrator-data {
                disputes-resolved: (+ (get disputes-resolved arbitrator-data) u1),
                reputation-score: (+ (get reputation-score arbitrator-data) u10),
            })
        )
        (ok true)
    )
)

(define-read-only (get-dispute (dispute-id uint))
    (ok (map-get? Disputes { id: dispute-id }))
)

(define-read-only (get-arbitrator (arbitrator principal))
    (ok (map-get? Arbitrators { arbitrator: arbitrator }))
)
(define-map SellerProfiles
    { seller: principal }
    {
        total-sales: uint,
        total-rating-points: uint,
        rating-count: uint,
        average-rating: uint,
        joined-at: uint,
    }
)

(define-map TransactionRatings
    { listing-id: uint }
    {
        buyer: principal,
        seller: principal,
        rating: uint,
        review: (string-ascii 256),
        rated-at: uint,
    }
)

(define-map BuyerRatingHistory
    {
        buyer: principal,
        listing-id: uint,
    }
    { has-rated: bool }
)

(define-public (initialize-seller-profile)
    (let ((existing-profile (map-get? SellerProfiles { seller: tx-sender })))
        (asserts! (is-none existing-profile) (err u40))
        (map-set SellerProfiles { seller: tx-sender } {
            total-sales: u0,
            total-rating-points: u0,
            rating-count: u0,
            average-rating: u0,
            joined-at: burn-block-height,
        })
        (ok true)
    )
)

(define-public (rate-seller
        (listing-id uint)
        (rating uint)
        (review (string-ascii 256))
    )
    (let (
            (listing (unwrap! (map-get? Listings { id: listing-id }) (err u41)))
            (escrow (unwrap! (map-get? Escrows { id: listing-id }) (err u42)))
            (seller (get seller listing))
            (seller-profile (unwrap! (map-get? SellerProfiles { seller: seller }) (err u43)))
            (rating-history (map-get? BuyerRatingHistory {
                buyer: tx-sender,
                listing-id: listing-id,
            }))
        )
        (asserts! (is-eq tx-sender (get buyer escrow)) (err u44))
        (asserts! (is-eq (get status escrow) "completed") (err u45))
        (asserts! (and (>= rating u1) (<= rating u5)) (err u46))
        (asserts! (is-none rating-history) (err u47))
        (let (
                (new-rating-count (+ (get rating-count seller-profile) u1))
                (new-total-points (+ (get total-rating-points seller-profile) rating))
                (new-average (/ new-total-points new-rating-count))
            )
            (map-set TransactionRatings { listing-id: listing-id } {
                buyer: tx-sender,
                seller: seller,
                rating: rating,
                review: review,
                rated-at: burn-block-height,
            })
            (map-set BuyerRatingHistory {
                buyer: tx-sender,
                listing-id: listing-id,
            } { has-rated: true }
            )
            (map-set SellerProfiles { seller: seller } {
                total-sales: (+ (get total-sales seller-profile) u1),
                total-rating-points: new-total-points,
                rating-count: new-rating-count,
                average-rating: new-average,
                joined-at: (get joined-at seller-profile),
            })
            (ok true)
        )
    )
)

(define-public (get-seller-listings (seller principal))
    (ok (filter get-active-listings-for-seller
        (list
            u1             u2             u3             u4             u5
            u6             u7             u8             u9             u10
            u11             u12             u13             u14             u15
            u16             u17             u18             u19             u20
            u21             u22             u23             u24             u25
            u26             u27             u28             u29             u30
            u31             u32             u33             u34             u35
            u36             u37             u38             u39             u40
            u41             u42             u43             u44             u45
            u46             u47             u48             u49             u50
            u51             u52             u53             u54             u55
            u56             u57             u58             u59             u60
            u61             u62             u63             u64             u65
            u66             u67             u68             u69             u70
            u71             u72             u73             u74             u75
            u76             u77             u78             u79             u80
            u81             u82             u83             u84             u85
            u86             u87             u88             u89             u90
            u91             u92             u93             u94             u95
            u96             u97             u98             u99             u100
        )))
)

(define-private (get-active-listings-for-seller (listing-id uint))
    (match (map-get? Listings { id: listing-id })
        listing (is-eq (get status listing) "active")
        false
    )
)

(define-read-only (get-seller-profile (seller principal))
    (ok (map-get? SellerProfiles { seller: seller }))
)

(define-read-only (get-transaction-rating (listing-id uint))
    (ok (map-get? TransactionRatings { listing-id: listing-id }))
)

(define-read-only (has-buyer-rated
        (buyer principal)
        (listing-id uint)
    )
    (ok (map-get? BuyerRatingHistory {
        buyer: buyer,
        listing-id: listing-id,
    }))
)

(define-read-only (get-seller-reputation-tier (seller principal))
    (let ((profile (map-get? SellerProfiles { seller: seller })))
        (match profile
            seller-data (let (
                    (avg-rating (get average-rating seller-data))
                    (total-sales (get total-sales seller-data))
                )
                (if (and (>= avg-rating u4) (>= total-sales u50))
                    (ok "platinum")
                    (if (and (>= avg-rating u4) (>= total-sales u20))
                        (ok "gold")
                        (if (and (>= avg-rating u3) (>= total-sales u10))
                            (ok "silver")
                            (ok "bronze")
                        )
                    )
                )
            )
            (ok "unrated")
        )
    )
)

(define-public (request-verification
        (requested-level (string-ascii 20))
        (documents-hash (string-ascii 64))
    )
    (let ((verification-id (var-get next-verification-id)))
        (asserts!
            (or
                (is-eq requested-level "basic")
                (is-eq requested-level "premium")
            )
            (err u50)
        )
        (asserts! (is-none (map-get? SellerVerifications { seller: tx-sender }))
            (err u51)
        )
        (try! (stx-transfer? verification-fee tx-sender contract-owner))
        (map-set VerificationRequests { id: verification-id } {
            seller: tx-sender,
            requested-level: requested-level,
            documents-hash: documents-hash,
            status: "pending",
            submitted-at: burn-block-height,
            reviewed-at: u0,
        })
        (var-set next-verification-id (+ verification-id u1))
        (ok verification-id)
    )
)

(define-public (approve-verification (request-id uint))
    (let (
            (request (unwrap! (map-get? VerificationRequests { id: request-id }) (err u52)))
            (seller (get seller request))
        )
        (asserts! (is-eq tx-sender contract-owner) (err u53))
        (asserts! (is-eq (get status request) "pending") (err u54))
        (map-set SellerVerifications { seller: seller } {
            verification-level: (get requested-level request),
            verified-at: burn-block-height,
            verification-id: request-id,
            documents-hash: (get documents-hash request),
            status: "verified",
        })
        (map-set VerificationRequests { id: request-id }
            (merge request {
                status: "approved",
                reviewed-at: burn-block-height,
            })
        )
        (ok true)
    )
)

(define-public (reject-verification (request-id uint))
    (let ((request (unwrap! (map-get? VerificationRequests { id: request-id }) (err u55))))
        (asserts! (is-eq tx-sender contract-owner) (err u56))
        (asserts! (is-eq (get status request) "pending") (err u57))
        (map-set VerificationRequests { id: request-id }
            (merge request {
                status: "rejected",
                reviewed-at: burn-block-height,
            })
        )
        (ok true)
    )
)

(define-read-only (get-listing-fee-for-seller (seller principal))
    (let ((verification (map-get? SellerVerifications { seller: seller })))
        (match verification
            verified-data (if (is-eq (get status verified-data) "verified")
                verified-listing-fee
                listing-fee
            )
            listing-fee
        )
    )
)

(define-read-only (is-seller-verified (seller principal))
    (let ((verification (map-get? SellerVerifications { seller: seller })))
        (match verification
            verified-data (is-eq (get status verified-data) "verified")
            false
        )
    )
)

(define-read-only (get-seller-verification-level (seller principal))
    (let ((verification (map-get? SellerVerifications { seller: seller })))
        (match verification
            verified-data (if (is-eq (get status verified-data) "verified")
                (ok (some (get verification-level verified-data)))
                (ok none)
            )
            (ok none)
        )
    )
)

(define-read-only (get-verification-request (request-id uint))
    (ok (map-get? VerificationRequests { id: request-id }))
)

(define-read-only (get-seller-verification (seller principal))
    (ok (map-get? SellerVerifications { seller: seller }))
)

(define-public (create-auction
        (title (string-ascii 50))
        (description (string-ascii 256))
        (starting-price uint)
        (duration-blocks uint)
    )
    (let (
            (auction-id (var-get next-auction-id))
            (nft-id (var-get next-nft-id))
            (end-block (+ burn-block-height duration-blocks))
        )
        (asserts! (> starting-price u0) (err u60))
        (asserts! (> duration-blocks u0) (err u61))
        (try! (stx-transfer? auction-fee tx-sender contract-owner))
        (try! (nft-mint? used-goods-nft nft-id tx-sender))
        (map-set Auctions { id: auction-id } {
            seller: tx-sender,
            title: title,
            description: description,
            starting-price: starting-price,
            current-bid: u0,
            highest-bidder: tx-sender,
            end-block: end-block,
            status: "active",
            nft-id: nft-id,
        })
        (map-set NFTOwnership { id: nft-id } { owner: tx-sender })
        (var-set next-auction-id (+ auction-id u1))
        (var-set next-nft-id (+ nft-id u1))
        (ok auction-id)
    )
)

(define-public (place-bid
        (auction-id uint)
        (bid-amount uint)
    )
    (let (
            (auction (unwrap! (map-get? Auctions { id: auction-id }) (err u62)))
            (current-bid (get current-bid auction))
            (highest-bidder (get highest-bidder auction))
            (min-bid (if (is-eq current-bid u0)
                (get starting-price auction)
                (+ current-bid min-bid-increment)
            ))
        )
        (asserts! (is-eq (get status auction) "active") (err u63))
        (asserts! (< burn-block-height (get end-block auction)) (err u64))
        (asserts! (not (is-eq tx-sender (get seller auction))) (err u65))
        (asserts! (>= bid-amount min-bid) (err u66))
        (try! (stx-transfer? bid-amount tx-sender contract-owner))
        (if (> current-bid u0)
            (try! (stx-transfer? current-bid contract-owner highest-bidder))
            true
        )
        (let ((new-end-block (if (< (- (get end-block auction) burn-block-height)
                    auction-extension-blocks
                )
                (+ burn-block-height auction-extension-blocks)
                (get end-block auction)
            )))
            (map-set Auctions { id: auction-id }
                (merge auction {
                    current-bid: bid-amount,
                    highest-bidder: tx-sender,
                    end-block: new-end-block,
                })
            )
        )
        (map-set AuctionBids {
            auction-id: auction-id,
            bidder: tx-sender,
        } {
            bid-amount: bid-amount,
            bid-block: burn-block-height,
            refunded: false,
        })
        (ok true)
    )
)

(define-public (finalize-auction (auction-id uint))
    (let (
            (auction (unwrap! (map-get? Auctions { id: auction-id }) (err u67)))
            (current-bid (get current-bid auction))
            (highest-bidder (get highest-bidder auction))
            (seller (get seller auction))
            (nft-id (get nft-id auction))
        )
        (asserts! (is-eq (get status auction) "active") (err u68))
        (asserts! (>= burn-block-height (get end-block auction)) (err u69))
        (if (> current-bid u0)
            (begin
                (try! (stx-transfer? (- current-bid escrow-fee) contract-owner seller))
                (try! (stx-transfer? escrow-fee contract-owner contract-owner))
                (try! (nft-transfer? used-goods-nft nft-id seller highest-bidder))
                (map-set NFTOwnership { id: nft-id } { owner: highest-bidder })
                (map-set Auctions { id: auction-id }
                    (merge auction { status: "sold" })
                )
            )
            (begin
                (try! (nft-burn? used-goods-nft nft-id seller))
                (map-set Auctions { id: auction-id }
                    (merge auction { status: "unsold" })
                )
            )
        )
        (ok true)
    )
)

(define-public (cancel-auction (auction-id uint))
    (let ((auction (unwrap! (map-get? Auctions { id: auction-id }) (err u70))))
        (asserts! (is-eq tx-sender (get seller auction)) (err u71))
        (asserts! (is-eq (get status auction) "active") (err u72))
        (asserts! (is-eq (get current-bid auction) u0) (err u73))
        (try! (nft-burn? used-goods-nft (get nft-id auction) tx-sender))
        (map-set Auctions { id: auction-id }
            (merge auction { status: "cancelled" })
        )
        (ok true)
    )
)

(define-read-only (get-auction (auction-id uint))
    (ok (map-get? Auctions { id: auction-id }))
)

(define-read-only (get-auction-bid
        (auction-id uint)
        (bidder principal)
    )
    (ok (map-get? AuctionBids {
        auction-id: auction-id,
        bidder: bidder,
    }))
)

(define-read-only (is-auction-active (auction-id uint))
    (let ((auction (map-get? Auctions { id: auction-id })))
        (match auction
            auction-data (and
                (is-eq (get status auction-data) "active")
                (< burn-block-height (get end-block auction-data))
            )
            false
        )
    )
)

(define-read-only (get-auction-time-remaining (auction-id uint))
    (let ((auction (map-get? Auctions { id: auction-id })))
        (match auction
            auction-data (if (>= burn-block-height (get end-block auction-data))
                (ok u0)
                (ok (- (get end-block auction-data) burn-block-height))
            )
            (err u74)
        )
    )
)

(define-public (make-offer
        (listing-id uint)
        (offer-amount uint)
    )
    (let (
            (offer-id (var-get next-offer-id))
            (listing (unwrap! (map-get? Listings { id: listing-id }) (err u80)))
            (seller (get seller listing))
            (expires-at (+ burn-block-height offer-duration-blocks))
        )
        (asserts! (is-eq (get status listing) "active") (err u81))
        (asserts! (not (is-eq tx-sender seller)) (err u82))
        (asserts! (> offer-amount u0) (err u83))
        (asserts! (< offer-amount (get price listing)) (err u84))
        (asserts!
            (is-none (map-get? ListingOffers {
                listing-id: listing-id,
                buyer: tx-sender,
            }))
            (err u85)
        )
        (map-set PriceOffers { id: offer-id } {
            listing-id: listing-id,
            buyer: tx-sender,
            seller: seller,
            offer-amount: offer-amount,
            status: "pending",
            created-at: burn-block-height,
            expires-at: expires-at,
        })
        (map-set ListingOffers {
            listing-id: listing-id,
            buyer: tx-sender,
        } { offer-id: offer-id }
        )
        (var-set next-offer-id (+ offer-id u1))
        (ok offer-id)
    )
)

(define-public (accept-offer (offer-id uint))
    (let (
            (offer (unwrap! (map-get? PriceOffers { id: offer-id }) (err u86)))
            (listing-id (get listing-id offer))
            (listing (unwrap! (map-get? Listings { id: listing-id }) (err u87)))
            (buyer (get buyer offer))
            (offer-amount (get offer-amount offer))
        )
        (asserts! (is-eq tx-sender (get seller offer)) (err u88))
        (asserts! (is-eq (get status offer) "pending") (err u89))
        (asserts! (< burn-block-height (get expires-at offer)) (err u90))
        (asserts! (is-eq (get status listing) "active") (err u91))
        (try! (stx-transfer? offer-amount buyer contract-owner))
        (map-set Escrows { id: listing-id } {
            buyer: buyer,
            seller: tx-sender,
            amount: offer-amount,
            status: "pending",
        })
        (map-set Listings { id: listing-id }
            (merge listing {
                status: "in-escrow",
                price: offer-amount,
            })
        )
        (map-set PriceOffers { id: offer-id }
            (merge offer { status: "accepted" })
        )
        (ok listing-id)
    )
)

(define-public (reject-offer (offer-id uint))
    (let ((offer (unwrap! (map-get? PriceOffers { id: offer-id }) (err u92))))
        (asserts! (is-eq tx-sender (get seller offer)) (err u93))
        (asserts! (is-eq (get status offer) "pending") (err u94))
        (map-set PriceOffers { id: offer-id }
            (merge offer { status: "rejected" })
        )
        (ok true)
    )
)

(define-public (cancel-offer (offer-id uint))
    (let ((offer (unwrap! (map-get? PriceOffers { id: offer-id }) (err u95))))
        (asserts! (is-eq tx-sender (get buyer offer)) (err u96))
        (asserts! (is-eq (get status offer) "pending") (err u97))
        (map-set PriceOffers { id: offer-id }
            (merge offer { status: "cancelled" })
        )
        (ok true)
    )
)

(define-read-only (get-offer (offer-id uint))
    (ok (map-get? PriceOffers { id: offer-id }))
)

(define-read-only (get-listing-offer
        (listing-id uint)
        (buyer principal)
    )
    (ok (map-get? ListingOffers {
        listing-id: listing-id,
        buyer: buyer,
    }))
)

(define-read-only (is-offer-valid (offer-id uint))
    (let ((offer (map-get? PriceOffers { id: offer-id })))
        (match offer
            offer-data (and
                (is-eq (get status offer-data) "pending")
                (< burn-block-height (get expires-at offer-data))
            )
            false
        )
    )
)

(define-read-only (get-offer-expiry (offer-id uint))
    (let ((offer (map-get? PriceOffers { id: offer-id })))
        (match offer
            offer-data (if (>= burn-block-height (get expires-at offer-data))
                (ok u0)
                (ok (- (get expires-at offer-data) burn-block-height))
            )
            (err u98)
        )
    )
)
