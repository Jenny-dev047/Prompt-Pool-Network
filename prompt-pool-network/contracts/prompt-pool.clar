;; PromptPool Network
;; A platform for proposing, voting on, and funding AI prompt ideas

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-voted (err u102))
(define-constant err-insufficient-funds (err u103))
(define-constant err-prompt-not-active (err u104))
(define-constant err-goal-not-reached (err u105))
(define-constant err-already-withdrawn (err u106))
(define-constant err-no-contribution (err u107))
(define-constant err-prompt-still-active (err u108))
(define-constant err-deadline-passed (err u109))
(define-constant err-invalid-deadline (err u110))
(define-constant err-cannot-downvote (err u111))

;; Data Variables
(define-data-var prompt-id-nonce uint u0)
(define-data-var total-prompts-funded uint u0)
(define-data-var total-funding-amount uint u0)

;; Data Maps
(define-map prompts
    uint
    {
        creator: principal,
        title: (string-ascii 100),
        description: (string-ascii 500),
        funding-goal: uint,
        current-funding: uint,
        votes: uint,
        active: bool,
        deadline: uint,
        created-at: uint
    }
)

(define-map votes
    {prompt-id: uint, voter: principal}
    bool
)

(define-map contributions
    {prompt-id: uint, contributor: principal}
    uint
)

(define-map withdrawn-funds
    uint
    bool
)

(define-map prompt-tags
    {prompt-id: uint, tag: (string-ascii 50)}
    bool
)

(define-map contributor-count
    uint
    uint
)

;; Read-only functions
(define-read-only (get-prompt (prompt-id uint))
    (map-get? prompts prompt-id)
)

(define-read-only (has-voted (prompt-id uint) (voter principal))
    (default-to false (map-get? votes {prompt-id: prompt-id, voter: voter}))
)

(define-read-only (get-contribution (prompt-id uint) (contributor principal))
    (default-to u0 (map-get? contributions {prompt-id: prompt-id, contributor: contributor}))
)

(define-read-only (get-next-prompt-id)
    (var-get prompt-id-nonce)
)

(define-read-only (get-funding-progress (prompt-id uint))
    (match (map-get? prompts prompt-id)
        prompt (ok {
            current: (get current-funding prompt),
            goal: (get funding-goal prompt),
            percentage: (if (> (get funding-goal prompt) u0)
                (/ (* (get current-funding prompt) u100) (get funding-goal prompt))
                u0
            ),
            reached: (>= (get current-funding prompt) (get funding-goal prompt))
        })
        err-not-found
    )
)

(define-read-only (get-platform-stats)
    (ok {
        total-prompts: (var-get prompt-id-nonce),
        total-funded: (var-get total-prompts-funded),
        total-amount: (var-get total-funding-amount)
    })
)

(define-read-only (has-withdrawn (prompt-id uint))
    (default-to false (map-get? withdrawn-funds prompt-id))
)

(define-read-only (get-contributor-count (prompt-id uint))
    (default-to u0 (map-get? contributor-count prompt-id))
)

(define-read-only (has-tag (prompt-id uint) (tag (string-ascii 50)))
    (default-to false (map-get? prompt-tags {prompt-id: prompt-id, tag: tag}))
)

(define-read-only (is-deadline-passed (prompt-id uint))
    (match (map-get? prompts prompt-id)
        prompt (ok (>= stacks-block-height (get deadline prompt)))
        err-not-found
    )
)

;; Public functions
;; #[allow(unchecked_data)]
(define-public (create-prompt (title (string-ascii 100)) (description (string-ascii 500)) (funding-goal uint))
    (let
        ((new-id (var-get prompt-id-nonce)))
        (map-set prompts new-id
            {
                creator: tx-sender,
                title: title,
                description: description,
                funding-goal: funding-goal,
                current-funding: u0,
                votes: u0,
                active: true,
                deadline: (+ stacks-block-height u4320),
                created-at: stacks-block-height
            }
        )
        (map-set contributor-count new-id u0)
        (var-set prompt-id-nonce (+ new-id u1))
        (ok new-id)
    )
)

(define-public (vote-prompt (prompt-id uint))
    (let
        ((prompt (unwrap! (map-get? prompts prompt-id) err-not-found)))
        (asserts! (get active prompt) err-prompt-not-active)
        (asserts! (not (has-voted prompt-id tx-sender)) err-already-voted)
        (map-set votes {prompt-id: prompt-id, voter: tx-sender} true)
        (map-set prompts prompt-id (merge prompt {votes: (+ (get votes prompt) u1)}))
        (ok true)
    )
)

;; #[allow(unchecked_data)]
(define-public (fund-prompt (prompt-id uint) (amount uint))
    (let
        (
            (prompt (unwrap! (map-get? prompts prompt-id) err-not-found))
            (previous-contribution (get-contribution prompt-id tx-sender))
            (current-contributors (get-contributor-count prompt-id))
        )
        (asserts! (get active prompt) err-prompt-not-active)
        (asserts! (< stacks-block-height (get deadline prompt)) err-deadline-passed)
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (map-set contributions 
            {prompt-id: prompt-id, contributor: tx-sender}
            (+ previous-contribution amount)
        )
        (map-set prompts prompt-id 
            (merge prompt {current-funding: (+ (get current-funding prompt) amount)})
        )
        ;; Increment contributor count if first-time contributor
        (if (is-eq previous-contribution u0)
            (map-set contributor-count prompt-id (+ current-contributors u1))
            true
        )
        (ok true)
    )
)

(define-public (close-prompt (prompt-id uint))
    (let
        ((prompt (unwrap! (map-get? prompts prompt-id) err-not-found)))
        (asserts! (is-eq tx-sender (get creator prompt)) err-owner-only)
        (map-set prompts prompt-id (merge prompt {active: false}))
        (ok true)
    )
)

;; Function 1: Withdraw funds once goal is reached
(define-public (withdraw-funds (prompt-id uint))
    (let
        (
            (prompt (unwrap! (map-get? prompts prompt-id) err-not-found))
            (amount (get current-funding prompt))
        )
        (asserts! (is-eq tx-sender (get creator prompt)) err-owner-only)
        (asserts! (>= (get current-funding prompt) (get funding-goal prompt)) err-goal-not-reached)
        (asserts! (not (has-withdrawn prompt-id)) err-already-withdrawn)
        
        (try! (as-contract (stx-transfer? amount tx-sender (get creator prompt))))
        (map-set withdrawn-funds prompt-id true)
        (var-set total-prompts-funded (+ (var-get total-prompts-funded) u1))
        (var-set total-funding-amount (+ (var-get total-funding-amount) amount))
        (ok amount)
    )
)

;; Function 2: Reopen a closed prompt
(define-public (reopen-prompt (prompt-id uint))
    (let
        ((prompt (unwrap! (map-get? prompts prompt-id) err-not-found)))
        (asserts! (is-eq tx-sender (get creator prompt)) err-owner-only)
        (asserts! (not (get active prompt)) err-prompt-still-active)
        (map-set prompts prompt-id (merge prompt {active: true}))
        (ok true)
    )
)

;; Function 3: Refund contributor if goal not reached and prompt closed
(define-public (request-refund (prompt-id uint))
    (let
        (
            (prompt (unwrap! (map-get? prompts prompt-id) err-not-found))
            (contribution (get-contribution prompt-id tx-sender))
        )
        (asserts! (not (get active prompt)) err-prompt-still-active)
        (asserts! (< (get current-funding prompt) (get funding-goal prompt)) err-goal-not-reached)
        (asserts! (> contribution u0) err-no-contribution)
        
        (try! (as-contract (stx-transfer? contribution tx-sender tx-sender)))
        (map-delete contributions {prompt-id: prompt-id, contributor: tx-sender})
        (map-set prompts prompt-id 
            (merge prompt {current-funding: (- (get current-funding prompt) contribution)})
        )
        (ok contribution)
    )
)

;; Function 4: Update prompt details
(define-public (update-prompt (prompt-id uint) (new-title (string-ascii 100)) (new-description (string-ascii 500)))
    (let
        ((prompt (unwrap! (map-get? prompts prompt-id) err-not-found)))
        (asserts! (is-eq tx-sender (get creator prompt)) err-owner-only)
        (asserts! (get active prompt) err-prompt-not-active)
        (map-set prompts prompt-id 
            (merge prompt {
                title: new-title,
                description: new-description
            })
        )
        (ok true)
    )
)

;; Function 5: Transfer prompt ownership
(define-public (transfer-prompt-ownership (prompt-id uint) (new-owner principal))
    (let
        ((prompt (unwrap! (map-get? prompts prompt-id) err-not-found)))
        (asserts! (is-eq tx-sender (get creator prompt)) err-owner-only)
        (map-set prompts prompt-id (merge prompt {creator: new-owner}))
        (ok true)
    )
)

;; Function 6: Increase funding goal
(define-public (increase-funding-goal (prompt-id uint) (additional-amount uint))
    (let
        ((prompt (unwrap! (map-get? prompts prompt-id) err-not-found)))
        (asserts! (is-eq tx-sender (get creator prompt)) err-owner-only)
        (asserts! (get active prompt) err-prompt-not-active)
        (asserts! (> additional-amount u0) err-insufficient-funds)
        (map-set prompts prompt-id 
            (merge prompt {
                funding-goal: (+ (get funding-goal prompt) additional-amount)
            })
        )
        (ok (+ (get funding-goal prompt) additional-amount))
    )
)

;; Function 7: Add tags to prompts for categorization
(define-public (add-tag (prompt-id uint) (tag (string-ascii 50)))
    (let
        ((prompt (unwrap! (map-get? prompts prompt-id) err-not-found)))
        (asserts! (is-eq tx-sender (get creator prompt)) err-owner-only)
        (map-set prompt-tags {prompt-id: prompt-id, tag: tag} true)
        (ok true)
    )
)

;; Function 8: Extend deadline for active prompt
(define-public (extend-deadline (prompt-id uint) (additional-blocks uint))
    (let
        ((prompt (unwrap! (map-get? prompts prompt-id) err-not-found)))
        (asserts! (is-eq tx-sender (get creator prompt)) err-owner-only)
        (asserts! (get active prompt) err-prompt-not-active)
        (asserts! (> additional-blocks u0) err-invalid-deadline)
        (map-set prompts prompt-id 
            (merge prompt {
                deadline: (+ (get deadline prompt) additional-blocks)
            })
        )
        (ok (+ (get deadline prompt) additional-blocks))
    )
)

;; Function 9: Batch fund multiple prompts at once
(define-public (batch-fund-prompts (prompt-ids (list 10 uint)) (amounts (list 10 uint)))
    (let
        (
            (total-amount (fold + amounts u0))
        )
        (asserts! (> total-amount u0) err-insufficient-funds)
        (try! (stx-transfer? total-amount tx-sender (as-contract tx-sender)))
        (ok (map fund-prompt-internal prompt-ids amounts))
    )
)

;; Internal helper for batch funding
(define-private (fund-prompt-internal (prompt-id uint) (amount uint))
    (match (map-get? prompts prompt-id)
        prompt 
            (begin
                (map-set contributions 
                    {prompt-id: prompt-id, contributor: tx-sender}
                    (+ (get-contribution prompt-id tx-sender) amount)
                )
                (map-set prompts prompt-id 
                    (merge prompt {current-funding: (+ (get current-funding prompt) amount)})
                )
                true
            )
        false
    )
)

;; Function 10: Auto-close expired prompts
(define-public (close-expired-prompt (prompt-id uint))
    (let
        ((prompt (unwrap! (map-get? prompts prompt-id) err-not-found)))
        (asserts! (get active prompt) err-prompt-not-active)
        (asserts! (>= stacks-block-height (get deadline prompt)) err-invalid-deadline)
        (map-set prompts prompt-id (merge prompt {active: false}))
        (ok true)
    )
)

;; Function 11: Get detailed prompt analytics
(define-public (get-prompt-analytics (prompt-id uint))
    (let
        ((prompt (unwrap! (map-get? prompts prompt-id) err-not-found)))
        (ok {
            prompt-id: prompt-id,
            creator: (get creator prompt),
            funding-percentage: (if (> (get funding-goal prompt) u0)
                (/ (* (get current-funding prompt) u100) (get funding-goal prompt))
                u0
            ),
            votes: (get votes prompt),
            contributors: (get-contributor-count prompt-id),
            days-remaining: (if (> (get deadline prompt) stacks-block-height)
                (/ (- (get deadline prompt) stacks-block-height) u144)
                u0
            ),
            is-funded: (>= (get current-funding prompt) (get funding-goal prompt)),
            is-expired: (>= stacks-block-height (get deadline prompt)),
            age-in-days: (/ (- stacks-block-height (get created-at prompt)) u144)
        })
    )
)