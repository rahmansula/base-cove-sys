;; BaseCove - Decentralized Storage Infrastructure
;; Clarity Version 3, Epoch 3.0
;;
;; Features:
;;   - Proof-of-Storage-Quality (PoSQ) node registry
;;   - Provider staking with SLA enforcement and penalties
;;   - Storage deal creation and lifecycle management
;;   - Storage futures market (pre-purchase capacity)
;;   - Cross-chain attestation stubs
;;   - Data deduplication registry

;; ============================================================
;; CONSTANTS
;; ============================================================

(define-constant CONTRACT-OWNER tx-sender)

(define-constant ERR-NOT-AUTHORIZED       (err u100))
(define-constant ERR-NODE-NOT-FOUND       (err u101))
(define-constant ERR-DEAL-NOT-FOUND       (err u102))
(define-constant ERR-FUTURES-NOT-FOUND    (err u103))
(define-constant ERR-INSUFFICIENT-STAKE   (err u104))
(define-constant ERR-ALREADY-REGISTERED   (err u105))
(define-constant ERR-INVALID-PARAMS       (err u106))
(define-constant ERR-DEAL-EXPIRED         (err u107))
(define-constant ERR-NOT-ACTIVE           (err u108))
(define-constant ERR-CONTENT-EXISTS       (err u109))

;; Minimum stake required per promised GB of storage (in microSTX)
(define-constant MIN-STAKE-PER-GB u1000000)

;; Penalty rate in basis points (500 = 5%)
(define-constant PENALTY-RATE-BPS u500)

;; Maximum node quality score
(define-constant MAX-QUALITY-SCORE u1000)

;; ============================================================
;; DATA MAPS AND VARS
;; ============================================================

;; Global counters
(define-data-var next-deal-id uint u1)
(define-data-var next-future-id uint u1)

;; Storage node registry
;; Nodes are keyed by their principal
(define-map storage-nodes
  principal
  {
    ;; PoSQ metrics
    retrieval-speed-score: uint,   ;; 0-1000, higher is better
    uptime-score: uint,            ;; 0-1000, higher is better
    geo-diversity-score: uint,     ;; 0-1000, higher is better
    composite-score: uint,         ;; weighted average of above three
    ;; Capacity and staking
    promised-gb: uint,
    used-gb: uint,
    staked-amount: uint,
    ;; Status flags
    is-active: bool,
    registered-at: uint            ;; block height
  }
)

;; Storage deals between clients and providers
(define-map storage-deals
  uint
  {
    client: principal,
    provider: principal,
    content-hash: (buff 32),       ;; SHA-256 of the content
    size-gb: uint,
    tier: (string-ascii 8),        ;; "premium" | "standard" | "archive"
    price-per-block: uint,         ;; microSTX per Stacks block
    start-block: uint,
    end-block: uint,
    is-active: bool,
    sla-penalty-balance: uint,     ;; accumulated penalties owed to client
    last-verified-block: uint
  }
)

;; Storage futures: clients pre-purchase capacity at a fixed price
(define-map storage-futures
  uint
  {
    buyer: principal,
    provider: principal,
    reserved-gb: uint,
    price-per-gb: uint,            ;; locked-in price in microSTX
    escrowed-amount: uint,
    delivery-deadline: uint,       ;; block height by which provider must fulfil
    is-fulfilled: bool,
    is-cancelled: bool
  }
)

;; Content-hash deduplication registry
;; Maps content hash to the first deal ID that stored it
(define-map content-registry
  (buff 32)
  {
    first-deal-id: uint,
    reference-count: uint
  }
)

;; Cross-chain attestation stubs
;; Maps (chain-id, content-hash) to a Clarity-side attestation record
(define-map cross-chain-attestations
  { chain-id: (string-ascii 32), content-hash: (buff 32) }
  {
    attested-by: principal,
    attest-block: uint,
    proof: (buff 64)               ;; raw cryptographic proof bytes
  }
)

;; ============================================================
;; PRIVATE HELPERS
;; ============================================================

;; Compute a composite PoSQ score as a weighted average:
;;   40% retrieval speed + 40% uptime + 20% geo diversity
(define-private (compute-composite-score
    (speed uint) (uptime uint) (geo uint))
  (/ (+ (+ (* speed u40) (* uptime u40)) (* geo u20)) u100)
)

;; Compute the penalty amount from a staked balance
(define-private (compute-penalty (stake uint))
  (/ (* stake PENALTY-RATE-BPS) u10000)
)

;; ============================================================
;; NODE MANAGEMENT
;; ============================================================

;; Register a new storage node.
;; The caller must attach enough STX to cover MIN-STAKE-PER-GB * promised-gb.
(define-public (register-node
    (promised-gb uint)
    (retrieval-speed-score uint)
    (uptime-score uint)
    (geo-diversity-score uint))
  (let (
    (caller tx-sender)
    (required-stake (* MIN-STAKE-PER-GB promised-gb))
    (composite (compute-composite-score
                  retrieval-speed-score uptime-score geo-diversity-score))
  )
    (asserts! (is-none (map-get? storage-nodes caller)) ERR-ALREADY-REGISTERED)
    (asserts! (>= (stx-get-balance caller) required-stake) ERR-INSUFFICIENT-STAKE)
    (asserts! (<= retrieval-speed-score MAX-QUALITY-SCORE) ERR-INVALID-PARAMS)
    (asserts! (<= uptime-score MAX-QUALITY-SCORE) ERR-INVALID-PARAMS)
    (asserts! (<= geo-diversity-score MAX-QUALITY-SCORE) ERR-INVALID-PARAMS)
    (asserts! (> promised-gb u0) ERR-INVALID-PARAMS)

    ;; Transfer stake into the contract
    (try! (stx-transfer? required-stake caller (as-contract tx-sender)))

    (map-set storage-nodes caller {
      retrieval-speed-score: retrieval-speed-score,
      uptime-score: uptime-score,
      geo-diversity-score: geo-diversity-score,
      composite-score: composite,
      promised-gb: promised-gb,
      used-gb: u0,
      staked-amount: required-stake,
      is-active: true,
      registered-at: stacks-block-height
    })
    (ok true)
  )
)

;; Update PoSQ scores for a node (only the node itself may update).
(define-public (update-node-scores
    (retrieval-speed-score uint)
    (uptime-score uint)
    (geo-diversity-score uint))
  (let (
    (caller tx-sender)
    (node (unwrap! (map-get? storage-nodes caller) ERR-NODE-NOT-FOUND))
    (composite (compute-composite-score
                  retrieval-speed-score uptime-score geo-diversity-score))
  )
    (asserts! (<= retrieval-speed-score MAX-QUALITY-SCORE) ERR-INVALID-PARAMS)
    (asserts! (<= uptime-score MAX-QUALITY-SCORE) ERR-INVALID-PARAMS)
    (asserts! (<= geo-diversity-score MAX-QUALITY-SCORE) ERR-INVALID-PARAMS)

    (map-set storage-nodes caller (merge node {
      retrieval-speed-score: retrieval-speed-score,
      uptime-score: uptime-score,
      geo-diversity-score: geo-diversity-score,
      composite-score: composite
    }))
    (ok composite)
  )
)

;; Deactivate a node and withdraw stake (only when used-gb is 0).
(define-public (deregister-node)
  (let (
    (caller tx-sender)
    (node (unwrap! (map-get? storage-nodes caller) ERR-NODE-NOT-FOUND))
  )
    (asserts! (get is-active node) ERR-NOT-ACTIVE)
    (asserts! (is-eq (get used-gb node) u0) ERR-INVALID-PARAMS)

    (map-set storage-nodes caller (merge node { is-active: false }))

    ;; Return stake
    (as-contract
      (stx-transfer? (get staked-amount node) tx-sender caller)
    )
  )
)

;; ============================================================
;; STORAGE DEALS
;; ============================================================

;; Create a storage deal.
;; Client pays the full price for the duration upfront into escrow.
(define-public (create-deal
    (provider principal)
    (content-hash (buff 32))
    (size-gb uint)
    (tier (string-ascii 8))
    (price-per-block uint)
    (duration-blocks uint))
  (let (
    (caller tx-sender)
    (deal-id (var-get next-deal-id))
    (total-cost (* price-per-block duration-blocks))
    (end-blk (+ stacks-block-height duration-blocks))
    (pnode (unwrap! (map-get? storage-nodes provider) ERR-NODE-NOT-FOUND))
  )
    (asserts! (get is-active pnode) ERR-NOT-ACTIVE)
    (asserts! (> size-gb u0) ERR-INVALID-PARAMS)
    (asserts! (> duration-blocks u0) ERR-INVALID-PARAMS)

    ;; Escrow payment
    (try! (stx-transfer? total-cost caller (as-contract tx-sender)))

    ;; Register content hash (deduplication)
    (match (map-get? content-registry content-hash)
      existing
        (map-set content-registry content-hash
          (merge existing { reference-count: (+ (get reference-count existing) u1) }))
      (map-set content-registry content-hash
        { first-deal-id: deal-id, reference-count: u1 })
    )

    ;; Update provider used capacity
    (map-set storage-nodes provider
      (merge pnode { used-gb: (+ (get used-gb pnode) size-gb) }))

    (map-set storage-deals deal-id {
      client: caller,
      provider: provider,
      content-hash: content-hash,
      size-gb: size-gb,
      tier: tier,
      price-per-block: price-per-block,
      start-block: stacks-block-height,
      end-block: end-blk,
      is-active: true,
      sla-penalty-balance: u0,
      last-verified-block: stacks-block-height
    })
    (var-set next-deal-id (+ deal-id u1))
    (ok deal-id)
  )
)

;; Report an SLA violation (underperformance).
;; CONTRACT-OWNER acts as the oracle for PoSQ verification results.
;; A penalty is slashed from the provider's stake and credited to the client.
(define-public (report-sla-violation (deal-id uint))
  (let (
    (deal (unwrap! (map-get? storage-deals deal-id) ERR-DEAL-NOT-FOUND))
    (provider (get provider deal))
    (pnode (unwrap! (map-get? storage-nodes provider) ERR-NODE-NOT-FOUND))
    (penalty (compute-penalty (get staked-amount pnode)))
  )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (get is-active deal) ERR-NOT-ACTIVE)
    (asserts! (<= stacks-block-height (get end-block deal)) ERR-DEAL-EXPIRED)

    ;; Deduct penalty from provider stake
    (map-set storage-nodes provider
      (merge pnode {
        staked-amount: (- (get staked-amount pnode) penalty)
      }))

    ;; Accumulate penalty for the client to claim
    (map-set storage-deals deal-id
      (merge deal {
        sla-penalty-balance: (+ (get sla-penalty-balance deal) penalty),
        last-verified-block: stacks-block-height
      }))
    (ok penalty)
  )
)

;; Client claims accumulated SLA penalties.
(define-public (claim-sla-penalty (deal-id uint))
  (let (
    (deal (unwrap! (map-get? storage-deals deal-id) ERR-DEAL-NOT-FOUND))
    (payout (get sla-penalty-balance deal))
  )
    (asserts! (is-eq tx-sender (get client deal)) ERR-NOT-AUTHORIZED)
    (asserts! (> payout u0) ERR-INVALID-PARAMS)

    (map-set storage-deals deal-id (merge deal { sla-penalty-balance: u0 }))
    (as-contract
      (stx-transfer? payout tx-sender (get client deal))
    )
  )
)

;; Close an expired deal and release escrowed funds to the provider.
(define-public (settle-deal (deal-id uint))
  (let (
    (deal (unwrap! (map-get? storage-deals deal-id) ERR-DEAL-NOT-FOUND))
    (provider (get provider deal))
    (pnode (unwrap! (map-get? storage-nodes provider) ERR-NODE-NOT-FOUND))
    (total-earned (* (get price-per-block deal)
                     (- (get end-block deal) (get start-block deal))))
    (net-payout (- total-earned (get sla-penalty-balance deal)))
  )
    (asserts! (get is-active deal) ERR-NOT-ACTIVE)
    (asserts! (>= stacks-block-height (get end-block deal)) ERR-INVALID-PARAMS)

    ;; Mark deal closed
    (map-set storage-deals deal-id (merge deal { is-active: false }))

    ;; Free up provider capacity
    (map-set storage-nodes provider
      (merge pnode {
        used-gb: (- (get used-gb pnode) (get size-gb deal))
      }))

    ;; Decrement dedup reference count
    (match (map-get? content-registry (get content-hash deal))
      reg
        (map-set content-registry (get content-hash deal)
          (merge reg { reference-count: (- (get reference-count reg) u1) }))
      true
    )

    ;; Pay provider net earnings
    (as-contract
      (stx-transfer? net-payout tx-sender provider)
    )
  )
)

;; ============================================================
;; STORAGE FUTURES MARKET
;; ============================================================

;; Buy a storage future: lock in a price for future capacity.
(define-public (buy-storage-future
    (provider principal)
    (reserved-gb uint)
    (price-per-gb uint)
    (delivery-deadline uint))
  (let (
    (caller tx-sender)
    (future-id (var-get next-future-id))
    (escrow (* reserved-gb price-per-gb))
    (pnode (unwrap! (map-get? storage-nodes provider) ERR-NODE-NOT-FOUND))
  )
    (asserts! (get is-active pnode) ERR-NOT-ACTIVE)
    (asserts! (> reserved-gb u0) ERR-INVALID-PARAMS)
    (asserts! (> delivery-deadline stacks-block-height) ERR-INVALID-PARAMS)

    (try! (stx-transfer? escrow caller (as-contract tx-sender)))

    (map-set storage-futures future-id {
      buyer: caller,
      provider: provider,
      reserved-gb: reserved-gb,
      price-per-gb: price-per-gb,
      escrowed-amount: escrow,
      delivery-deadline: delivery-deadline,
      is-fulfilled: false,
      is-cancelled: false
    })
    (var-set next-future-id (+ future-id u1))
    (ok future-id)
  )
)

;; Provider fulfils a storage future (acknowledges delivery).
(define-public (fulfil-storage-future (future-id uint))
  (let (
    (future (unwrap! (map-get? storage-futures future-id) ERR-FUTURES-NOT-FOUND))
    (provider (get provider future))
  )
    (asserts! (is-eq tx-sender provider) ERR-NOT-AUTHORIZED)
    (asserts! (not (get is-fulfilled future)) ERR-NOT-ACTIVE)
    (asserts! (not (get is-cancelled future)) ERR-NOT-ACTIVE)
    (asserts! (<= stacks-block-height (get delivery-deadline future)) ERR-DEAL-EXPIRED)

    (map-set storage-futures future-id
      (merge future { is-fulfilled: true }))

    ;; Release escrow to provider
    (as-contract
      (stx-transfer? (get escrowed-amount future) tx-sender provider)
    )
  )
)

;; Buyer cancels and reclaims escrow if deadline has passed without fulfilment.
(define-public (cancel-storage-future (future-id uint))
  (let (
    (future (unwrap! (map-get? storage-futures future-id) ERR-FUTURES-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender (get buyer future)) ERR-NOT-AUTHORIZED)
    (asserts! (not (get is-fulfilled future)) ERR-NOT-ACTIVE)
    (asserts! (not (get is-cancelled future)) ERR-NOT-ACTIVE)
    (asserts! (> stacks-block-height (get delivery-deadline future)) ERR-INVALID-PARAMS)

    (map-set storage-futures future-id
      (merge future { is-cancelled: true }))

    (as-contract
      (stx-transfer? (get escrowed-amount future) tx-sender (get buyer future))
    )
  )
)

;; ============================================================
;; CROSS-CHAIN ATTESTATION
;; ============================================================

;; Record a cross-chain storage attestation with a cryptographic proof.
;; Only the CONTRACT-OWNER (acting as bridge oracle) may submit attestations.
(define-public (submit-attestation
    (input-chain-id (string-ascii 32))
    (input-content-hash (buff 32))
    (input-proof (buff 64)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set cross-chain-attestations
      { chain-id: input-chain-id, content-hash: input-content-hash }
      { attested-by: tx-sender, attest-block: stacks-block-height, proof: input-proof })
    (ok true)
  )
)

;; ============================================================
;; READ-ONLY QUERIES
;; ============================================================

(define-read-only (get-node (node principal))
  (map-get? storage-nodes node)
)

(define-read-only (get-deal (deal-id uint))
  (map-get? storage-deals deal-id)
)

(define-read-only (get-future (future-id uint))
  (map-get? storage-futures future-id)
)

(define-read-only (get-content-record (content-hash (buff 32)))
  (map-get? content-registry content-hash)
)

(define-read-only (get-attestation
    (input-chain-id (string-ascii 32))
    (input-content-hash (buff 32)))
  (map-get? cross-chain-attestations
    { chain-id: input-chain-id, content-hash: input-content-hash })
)

(define-read-only (get-next-deal-id)
  (var-get next-deal-id)
)

(define-read-only (get-next-future-id)
  (var-get next-future-id)
)
