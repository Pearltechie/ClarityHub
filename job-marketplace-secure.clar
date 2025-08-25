;; Decentralized Job Marketplace Smart Contract with Security Features
;; This contract includes role-based access control, input validation, rate limiting,
;; audit logging, and emergency controls for enhanced security.

;; ===============================================================================
;; DATA STRUCTURES
;; ===============================================================================

(define-map job-seekers
    principal
    {
        name: (string-ascii 100),
        skills: (list 10 (string-ascii 50)),
        location: (string-ascii 100),
        resume: (string-ascii 500),
        created-at: uint,
        last-updated: uint,
        is-active: bool,
        reputation-score: uint
    }
)

(define-map employers
    principal
    {
        company-name: (string-ascii 100),
        industry: (string-ascii 50),
        location: (string-ascii 100),
        created-at: uint,
        last-updated: uint,
        is-verified: bool,
        is-active: bool,
        reputation-score: uint
    }
)

(define-map job-listings
    uint  ;; Changed to uint for unique job IDs
    {
        title: (string-ascii 100),
        description: (string-ascii 500),
        employer: principal,
        location: (string-ascii 100),
        requirements: (list 10 (string-ascii 50)),
        created-at: uint,
        expires-at: uint,
        is-active: bool,
        salary-range: (string-ascii 50)
    }
)

;; Security and Access Control Maps
(define-map admins principal bool)
(define-map moderators principal bool)
(define-map blacklisted-users principal bool)
(define-map user-actions principal {last-action: uint, action-count: uint})

;; Rate limiting map (tracks user actions per block)
(define-map rate-limits 
    {user: principal, action: (string-ascii 20)} 
    {count: uint, reset-block: uint}
)

;; Audit log for important actions
(define-map audit-log
    uint
    {
        user: principal,
        action: (string-ascii 50),
        target: (optional principal),
        block-height: uint,
        details: (string-ascii 200)
    }
)

;; ===============================================================================
;; CONSTANTS AND VARIABLES
;; ===============================================================================

;; Error constants
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-EXISTS (err u409))
(define-constant ERR-INVALID-INPUT (err u400))
(define-constant ERR-UNAUTHORIZED (err u401))
(define-constant ERR-BLACKLISTED (err u403))
(define-constant ERR-RATE-LIMITED (err u429))
(define-constant ERR-EXPIRED (err u410))
(define-constant ERR-INACTIVE (err u423))

;; Rate limiting constants
(define-constant MAX-ACTIONS-PER-BLOCK u5)
(define-constant RATE-LIMIT-WINDOW u10) ;; 10 blocks

;; Contract state variables
(define-data-var contract-owner principal tx-sender)
(define-data-var contract-paused bool false)
(define-data-var next-job-id uint u1)
(define-data-var next-audit-id uint u1)
(define-data-var min-reputation uint u0)

;; ===============================================================================
;; SECURITY HELPER FUNCTIONS
;; ===============================================================================

;; Check if contract is paused
(define-private (is-contract-active)
    (not (var-get contract-paused))
)

;; Check if user is contract owner
(define-private (is-owner (user principal))
    (is-eq user (var-get contract-owner))
)

;; Check if user is admin
(define-private (is-admin (user principal))
    (default-to false (map-get? admins user))
)

;; Check if user is moderator or admin
(define-private (is-moderator-or-admin (user principal))
    (or (is-admin user) (default-to false (map-get? moderators user)))
)

;; Check if user is blacklisted
(define-private (is-blacklisted (user principal))
    (default-to false (map-get? blacklisted-users user))
)

;; Rate limiting check
(define-private (check-rate-limit (user principal) (action (string-ascii 20)))
    (let
        (
            (current-block block-height)
            (limit-key {user: user, action: action})
            (existing-limit (map-get? rate-limits limit-key))
        )
        (match existing-limit
            limit
            (if (>= current-block (+ (get reset-block limit) RATE-LIMIT-WINDOW))
                ;; Reset the counter
                (begin
                    (map-set rate-limits limit-key {count: u1, reset-block: current-block})
                    (ok true)
                )
                ;; Check if under limit
                (if (< (get count limit) MAX-ACTIONS-PER-BLOCK)
                    (begin
                        (map-set rate-limits limit-key {count: (+ (get count limit) u1), reset-block: (get reset-block limit)})
                        (ok true)
                    )
                    (err ERR-RATE-LIMITED)
                )
            )
            ;; First action
            (begin
                (map-set rate-limits limit-key {count: u1, reset-block: current-block})
                (ok true)
            )
        )
    )
)

;; Input validation helpers
(define-private (is-valid-string (input (string-ascii 500)))
    (and (> (len input) u0) (<= (len input) u500))
)

(define-private (is-valid-skills (skills (list 10 (string-ascii 50))))
    (and (> (len skills) u0) (<= (len skills) u10))
)

;; Log audit entry
(define-private (log-audit-event 
    (user principal) 
    (action (string-ascii 50)) 
    (target (optional principal))
    (details (string-ascii 200)))
    (let ((audit-id (var-get next-audit-id)))
        (map-set audit-log audit-id
            {
                user: user,
                action: action,
                target: target,
                block-height: block-height,
                details: details
            }
        )
        (var-set next-audit-id (+ audit-id u1))
        (ok audit-id)
    )
)

;; ===============================================================================
;; ADMIN AND MODERATION FUNCTIONS
;; ===============================================================================

;; Add admin (only owner)
(define-public (add-admin (new-admin principal))
    (begin
        (asserts! (is-owner tx-sender) (err ERR-UNAUTHORIZED))
        (asserts! (is-contract-active) (err ERR-INACTIVE))
        (map-set admins new-admin true)
        (unwrap-panic (log-audit-event tx-sender "ADD_ADMIN" (some new-admin) "Admin added"))
        (ok "Admin added successfully")
    )
)

;; Add moderator (admin only)
(define-public (add-moderator (new-moderator principal))
    (begin
        (asserts! (is-admin tx-sender) (err ERR-UNAUTHORIZED))
        (asserts! (is-contract-active) (err ERR-INACTIVE))
        (map-set moderators new-moderator true)
        (unwrap-panic (log-audit-event tx-sender "ADD_MODERATOR" (some new-moderator) "Moderator added"))
        (ok "Moderator added successfully")
    )
)

;; Blacklist user (moderator or admin)
(define-public (blacklist-user (user principal) (reason (string-ascii 200)))
    (begin
        (asserts! (is-moderator-or-admin tx-sender) (err ERR-UNAUTHORIZED))
        (asserts! (is-contract-active) (err ERR-INACTIVE))
        (map-set blacklisted-users user true)
        (unwrap-panic (log-audit-event tx-sender "BLACKLIST_USER" (some user) reason))
        (ok "User blacklisted successfully")
    )
)

;; Remove from blacklist (admin only)
(define-public (unblacklist-user (user principal))
    (begin
        (asserts! (is-admin tx-sender) (err ERR-UNAUTHORIZED))
        (asserts! (is-contract-active) (err ERR-INACTIVE))
        (map-delete blacklisted-users user)
        (unwrap-panic (log-audit-event tx-sender "UNBLACKLIST_USER" (some user) "User removed from blacklist"))
        (ok "User removed from blacklist")
    )
)

;; Emergency pause contract (admin only)
(define-public (pause-contract)
    (begin
        (asserts! (is-admin tx-sender) (err ERR-UNAUTHORIZED))
        (var-set contract-paused true)
        (unwrap-panic (log-audit-event tx-sender "PAUSE_CONTRACT" none "Contract paused"))
        (ok "Contract paused")
    )
)

;; Resume contract (admin only)
(define-public (resume-contract)
    (begin
        (asserts! (is-admin tx-sender) (err ERR-UNAUTHORIZED))
        (var-set contract-paused false)
        (unwrap-panic (log-audit-event tx-sender "RESUME_CONTRACT" none "Contract resumed"))
        (ok "Contract resumed")
    )
)

;; ===============================================================================
;; CORE FUNCTIONALITY WITH SECURITY
;; ===============================================================================

;; Enhanced job seeker registration with security checks
(define-public (register-job-seeker 
    (name (string-ascii 100))
    (skills (list 10 (string-ascii 50)))
    (location (string-ascii 100))
    (resume (string-ascii 500)))
    (let
        (
            (caller tx-sender)
            (existing-profile (map-get? job-seekers caller))
            (current-time block-height)
        )
        ;; Security checks
        (asserts! (is-contract-active) (err ERR-INACTIVE))
        (asserts! (not (is-blacklisted caller)) (err ERR-BLACKLISTED))
        (try! (check-rate-limit caller "REGISTER"))
        
        ;; Business logic checks
        (asserts! (is-none existing-profile) (err ERR-ALREADY-EXISTS))
        (asserts! (is-valid-string name) (err ERR-INVALID-INPUT))
        (asserts! (is-valid-string location) (err ERR-INVALID-INPUT))
        (asserts! (is-valid-string resume) (err ERR-INVALID-INPUT))
        (asserts! (is-valid-skills skills) (err ERR-INVALID-INPUT))
        
        ;; Store the new job seeker profile
        (map-set job-seekers caller
            {
                name: name,
                skills: skills,
                location: location,
                resume: resume,
                created-at: current-time,
                last-updated: current-time,
                is-active: true,
                reputation-score: u50  ;; Starting reputation
            }
        )
        
        ;; Log the action
        (unwrap-panic (log-audit-event caller "REGISTER_JOB_SEEKER" none "Job seeker registered"))
        
        (ok "Job seeker profile registered successfully")
    )
)

;; Enhanced employer registration with security checks
(define-public (register-employer 
    (company-name (string-ascii 100))
    (industry (string-ascii 50))
    (location (string-ascii 100)))
    (let
        (
            (caller tx-sender)
            (existing-profile (map-get? employers caller))
            (current-time block-height)
        )
        ;; Security checks
        (asserts! (is-contract-active) (err ERR-INACTIVE))
        (asserts! (not (is-blacklisted caller)) (err ERR-BLACKLISTED))
        (try! (check-rate-limit caller "REGISTER"))
        
        ;; Business logic checks
        (asserts! (is-none existing-profile) (err ERR-ALREADY-EXISTS))
        (asserts! (is-valid-string company-name) (err ERR-INVALID-INPUT))
        (asserts! (is-valid-string industry) (err ERR-INVALID-INPUT))
        (asserts! (is-valid-string location) (err ERR-INVALID-INPUT))
        
        ;; Store the new employer profile
        (map-set employers caller
            {
                company-name: company-name,
                industry: industry,
                location: location,
                created-at: current-time,
                last-updated: current-time,
                is-verified: false,
                is-active: true,
                reputation-score: u50  ;; Starting reputation
            }
        )
        
        ;; Log the action
        (unwrap-panic (log-audit-event caller "REGISTER_EMPLOYER" none "Employer registered"))
        
        (ok "Employer profile registered successfully")
    )
)

;; Enhanced job listing creation with security and expiration
(define-public (create-job-listing 
    (title (string-ascii 100))
    (description (string-ascii 500))
    (location (string-ascii 100))
    (requirements (list 10 (string-ascii 50)))
    (salary-range (string-ascii 50))
    (duration-blocks uint))
    (let
        (
            (caller tx-sender)
            (job-id (var-get next-job-id))
            (current-time block-height)
            (expiry-time (+ current-time duration-blocks))
        )
        ;; Security checks
        (asserts! (is-contract-active) (err ERR-INACTIVE))
        (asserts! (not (is-blacklisted caller)) (err ERR-BLACKLISTED))
        (try! (check-rate-limit caller "CREATE_JOB"))
        
        ;; Business logic checks
        (asserts! (is-some (map-get? employers caller)) (err ERR-UNAUTHORIZED))
        (asserts! (is-valid-string title) (err ERR-INVALID-INPUT))
        (asserts! (is-valid-string description) (err ERR-INVALID-INPUT))
        (asserts! (is-valid-string location) (err ERR-INVALID-INPUT))
        (asserts! (is-valid-skills requirements) (err ERR-INVALID-INPUT))
        (asserts! (> duration-blocks u0) (err ERR-INVALID-INPUT))
        
        ;; Store the new job listing
        (map-set job-listings job-id
            {
                title: title,
                description: description,
                employer: caller,
                location: location,
                requirements: requirements,
                created-at: current-time,
                expires-at: expiry-time,
                is-active: true,
                salary-range: salary-range
            }
        )
        
        ;; Update next job ID
        (var-set next-job-id (+ job-id u1))
        
        ;; Log the action
        (unwrap-panic (log-audit-event caller "CREATE_JOB_LISTING" none "Job listing created"))
        
        (ok job-id)
    )
)

;; Deactivate job listing (employer or moderator)
(define-public (deactivate-job-listing (job-id uint))
    (let
        (
            (caller tx-sender)
            (job-listing (unwrap! (map-get? job-listings job-id) (err ERR-NOT-FOUND)))
        )
        ;; Security checks
        (asserts! (is-contract-active) (err ERR-INACTIVE))
        (asserts! (not (is-blacklisted caller)) (err ERR-BLACKLISTED))
        
        ;; Authorization check
        (asserts! (or (is-eq caller (get employer job-listing)) 
                     (is-moderator-or-admin caller)) (err ERR-UNAUTHORIZED))
        
        ;; Update job listing
        (map-set job-listings job-id
            (merge job-listing {is-active: false})
        )
        
        ;; Log the action
        (unwrap-panic (log-audit-event caller "DEACTIVATE_JOB" none "Job listing deactivated"))
        
        (ok "Job listing deactivated")
    )
)

;; ===============================================================================
;; READ-ONLY FUNCTIONS WITH SECURITY
;; ===============================================================================

;; Get job listing with expiration check
(define-read-only (get-job-listing (job-id uint))
    (match (map-get? job-listings job-id)
        job 
        (if (and (get is-active job) 
                 (< block-height (get expires-at job)))
            (ok job)
            (err ERR-EXPIRED)
        )
        (err ERR-NOT-FOUND)
    )
)

;; Get job seeker profile (only active profiles)
(define-read-only (get-job-seeker-profile (user principal))
    (match (map-get? job-seekers user)
        profile 
        (if (get is-active profile)
            (ok profile)
            (err ERR-INACTIVE)
        )
        (err ERR-NOT-FOUND)
    )
)

;; Get employer profile (only active profiles)
(define-read-only (get-employer-profile (user principal))
    (match (map-get? employers user)
        profile 
        (if (get is-active profile)
            (ok profile)
            (err ERR-INACTIVE)
        )
        (err ERR-NOT-FOUND)
    )
)

;; Get contract status
(define-read-only (get-contract-status)
    (ok {
        is-paused: (var-get contract-paused),
        next-job-id: (var-get next-job-id),
        owner: (var-get contract-owner)
    })
)

;; Get audit log entry (admin only)
(define-read-only (get-audit-entry (audit-id uint))
    (if (is-admin tx-sender)
        (match (map-get? audit-log audit-id)
            entry (ok entry)
            (err ERR-NOT-FOUND)
        )
        (err ERR-UNAUTHORIZED)
    )
)

;; Check if user is blacklisted (public for transparency)
(define-read-only (is-user-blacklisted (user principal))
    (ok (is-blacklisted user))
)

;; Initialize contract with first admin
(begin
    (map-set admins tx-sender true)
    (print "Job Marketplace Contract Initialized")
)
