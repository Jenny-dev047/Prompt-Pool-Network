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