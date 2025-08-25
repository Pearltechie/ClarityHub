;; Enhanced User Profile Smart Contract with Security Features
;; This contract includes input validation, rate limiting, access control,
;; audit logging, and reputation management with security controls.

;; ===============================================================================
;; DATA STRUCTURES
;; ===============================================================================

(define-map user-profiles 
    {user: principal} 
    {
        name: (string-utf8 50), 
        email: (string-utf8 50), 
        reputation-score: uint,
        created-at: uint,
        last-updated: uint,
        is-verified: bool,
        is-active: bool,
        profile-version: uint,
        update-count: uint
    }
)

;; Security and Access Control Maps
(define-map admins principal bool)
(define-map moderators principal bool)
(define-map blacklisted-users principal bool)
(define-map suspended-users principal {suspended-until: uint, reason: (string-utf8 100)})

;; Rate limiting for profile operations
(define-map user-rate-limits 
    {user: principal, action: (string-utf8 20)} 
    {count: uint, reset-block: uint}
)

;; Profile change history for audit trail
(define-map profile-history
    {user: principal, version: uint}
    {
        old-name: (string-utf8 50),
        old-email: (string-utf8 50),
        changed-at: uint,
        change-type: (string-utf8 30)
    }
)

;; Reputation change log
(define-map reputation-log
    uint
    {
        user: principal,
        old-score: uint,
        new-score: uint,
        changed-by: principal,
        reason: (string-utf8 100),
        block-height: uint
    }
)

;; Email verification system
(define-map email-verification
    principal
    {
        verification-code: uint,
        expires-at: uint,
        attempts: uint,
        is-verified: bool
    }
)

;; Profile privacy settings
(define-map privacy-settings
    principal
    {
        profile-visibility: (string-utf8 20), ;; "public", "private", "friends"
        email-visibility: (string-utf8 20),   ;; "public", "private", "verified-only"
        reputation-visibility: (string-utf8 20) ;; "public", "private", "partial"
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
(define-constant ERR-SUSPENDED (err u423))
(define-constant ERR-RATE-LIMITED (err u429))
(define-constant ERR-NOT-VERIFIED (err u422))
(define-constant ERR-VERIFICATION-FAILED (err u420))

;; Rate limiting constants
(define-constant MAX-UPDATES-PER-HOUR u5)
(define-constant MAX-VERIFICATION-ATTEMPTS u3)
(define-constant RATE-LIMIT-WINDOW u60) ;; 60 blocks ≈ 1 hour
(define-constant VERIFICATION-EXPIRY u144) ;; 144 blocks ≈ 24 hours

;; Reputation constants
(define-constant MIN-REPUTATION u0)
(define-constant MAX-REPUTATION u1000)
(define-constant DEFAULT-REPUTATION u100)

;; Contract state variables
(define-data-var contract-owner principal tx-sender)
(define-data-var contract-paused bool false)
(define-data-var next-reputation-log-id uint u1)
(define-data-var require-email-verification bool true)
(define-data-var min-name-length uint u2)
(define-data-var max-profile-updates-per-day uint u3)

;; ===============================================================================
;; SECURITY HELPER FUNCTIONS
;; ===============================================================================

;; Check if contract is active
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

;; Check if user is suspended
(define-private (is-suspended (user principal))
    (match (map-get? suspended-users user)
        suspension
        (> (get suspended-until suspension) block-height)
        false
    )
)

;; Rate limiting check for profile operations
(define-private (check-rate-limit (user principal) (action (string-utf8 20)))
    (let
        (
            (current-block block-height)
            (limit-key {user: user, action: action})
            (existing-limit (map-get? user-rate-limits limit-key))
        )
        (match existing-limit
            limit
            (if (>= current-block (+ (get reset-block limit) RATE-LIMIT-WINDOW))
                ;; Reset the counter
                (begin
                    (map-set user-rate-limits limit-key {count: u1, reset-block: current-block})
                    (ok true)
                )
                ;; Check if under limit
                (if (< (get count limit) MAX-UPDATES-PER-HOUR)
                    (begin
                        (map-set user-rate-limits limit-key {count: (+ (get count limit) u1), reset-block: (get reset-block limit)})
                        (ok true)
                    )
                    (err ERR-RATE-LIMITED)
                )
            )
            ;; First action
            (begin
                (map-set user-rate-limits limit-key {count: u1, reset-block: current-block})
                (ok true)
            )
        )
    )
)

;; Input validation functions
(define-private (is-valid-name (name (string-utf8 50)))
    (and 
        (>= (len name) (var-get min-name-length))
        (<= (len name) u50)
        (not (is-eq name u""))
    )
)

(define-private (is-valid-email (email (string-utf8 50)))
    (and
        (> (len email) u5) ;; Minimum email length
        (<= (len email) u50)
        ;; Basic email format check (contains @ and .)
        (and (is-some (index-of email "@"))
             (is-some (index-of email ".")))
    )
)

;; Log reputation change
(define-private (log-reputation-change 
    (user principal) 
    (old-score uint) 
    (new-score uint) 
    (changed-by principal)
    (reason (string-utf8 100)))
    (let ((log-id (var-get next-reputation-log-id)))
        (map-set reputation-log log-id
            {
                user: user,
                old-score: old-score,
                new-score: new-score,
                changed-by: changed-by,
                reason: reason,
                block-height: block-height
            }
        )
        (var-set next-reputation-log-id (+ log-id u1))
        (ok log-id)
    )
)

;; Generate verification code (simplified)
(define-private (generate-verification-code)
    (+ (mod (unwrap-panic (get-block-info? id-header-hash (- block-height u1))) u900000) u100000)
)

;; ===============================================================================
;; ADMIN AND MODERATION FUNCTIONS
;; ===============================================================================

;; Add admin (only owner)
(define-public (add-admin (new-admin principal))
    (begin
        (asserts! (is-owner tx-sender) (err ERR-UNAUTHORIZED))
        (asserts! (is-contract-active) (err ERR-SUSPENDED))
        (map-set admins new-admin true)
        (ok "Admin added successfully")
    )
)

;; Add moderator (admin only)
(define-public (add-moderator (new-moderator principal))
    (begin
        (asserts! (is-admin tx-sender) (err ERR-UNAUTHORIZED))
        (asserts! (is-contract-active) (err ERR-SUSPENDED))
        (map-set moderators new-moderator true)
        (ok "Moderator added successfully")
    )
)

;; Blacklist user (moderator or admin)
(define-public (blacklist-user (user principal) (reason (string-utf8 100)))
    (begin
        (asserts! (is-moderator-or-admin tx-sender) (err ERR-UNAUTHORIZED))
        (asserts! (is-contract-active) (err ERR-SUSPENDED))
        (asserts! (not (is-admin user)) (err ERR-UNAUTHORIZED)) ;; Can't blacklist admins
        
        ;; Deactivate user profile if exists
        (match (map-get? user-profiles {user: user})
            profile
            (map-set user-profiles {user: user}
                (merge profile {is-active: false})
            )
            true ;; Profile doesn't exist, continue
        )
        
        (map-set blacklisted-users user true)
        (ok "User blacklisted successfully")
    )
)

;; Suspend user temporarily (moderator or admin)
(define-public (suspend-user (user principal) (duration-blocks uint) (reason (string-utf8 100)))
    (begin
        (asserts! (is-moderator-or-admin tx-sender) (err ERR-UNAUTHORIZED))
        (asserts! (is-contract-active) (err ERR-SUSPENDED))
        (asserts! (not (is-admin user)) (err ERR-UNAUTHORIZED)) ;; Can't suspend admins
        (asserts! (> duration-blocks u0) (err ERR-INVALID-INPUT))
        
        (map-set suspended-users user
            {
                suspended-until: (+ block-height duration-blocks),
                reason: reason
            }
        )
        (ok "User suspended successfully")
    )
)

;; Adjust user reputation (admin only)
(define-public (adjust-reputation (user principal) (new-score uint) (reason (string-utf8 100)))
    (begin
        (asserts! (is-admin tx-sender) (err ERR-UNAUTHORIZED))
        (asserts! (is-contract-active) (err ERR-SUSPENDED))
        (asserts! (<= new-score MAX-REPUTATION) (err ERR-INVALID-INPUT))
        
        (match (map-get? user-profiles {user: user})
            profile
            (let ((old-score (get reputation-score profile)))
                (map-set user-profiles {user: user}
                    (merge profile {reputation-score: new-score})
                )
                (unwrap-panic (log-reputation-change user old-score new-score tx-sender reason))
                (ok "Reputation adjusted successfully")
            )
            (err ERR-NOT-FOUND)
        )
    )
)

;; Pause contract (admin only)
(define-public (pause-contract)
    (begin
        (asserts! (is-admin tx-sender) (err ERR-UNAUTHORIZED))
        (var-set contract-paused true)
        (ok "Contract paused")
    )
)

;; Resume contract (admin only)
(define-public (resume-contract)
    (begin
        (asserts! (is-admin tx-sender) (err ERR-UNAUTHORIZED))
        (var-set contract-paused false)
        (ok "Contract resumed")
    )
)

;; ===============================================================================
;; EMAIL VERIFICATION FUNCTIONS
;; ===============================================================================

;; Request email verification
(define-public (request-email-verification)
    (let
        (
            (user tx-sender)
            (verification-code (generate-verification-code))
            (expires-at (+ block-height VERIFICATION-EXPIRY))
        )
        (asserts! (is-contract-active) (err ERR-SUSPENDED))
        (asserts! (not (is-blacklisted user)) (err ERR-BLACKLISTED))
        (asserts! (not (is-suspended user)) (err ERR-SUSPENDED))
        
        ;; Check if user has a profile
        (asserts! (is-some (map-get? user-profiles {user: user})) (err ERR-NOT-FOUND))
        
        (map-set email-verification user
            {
                verification-code: verification-code,
                expires-at: expires-at,
                attempts: u0,
                is-verified: false
            }
        )
        
        (ok verification-code) ;; In real implementation, this would trigger off-chain email
    )
)

;; Verify email with code
(define-public (verify-email (code uint))
    (let
        (
            (user tx-sender)
            (verification-data (unwrap! (map-get? email-verification user) (err ERR-NOT-FOUND)))
        )
        (asserts! (is-contract-active) (err ERR-SUSPENDED))
        (asserts! (not (is-blacklisted user)) (err ERR-BLACKLISTED))
        (asserts! (< block-height (get expires-at verification-data)) (err ERR-VERIFICATION-FAILED))
        (asserts! (< (get attempts verification-data) MAX-VERIFICATION-ATTEMPTS) (err ERR-VERIFICATION-FAILED))
        
        (if (is-eq code (get verification-code verification-data))
            ;; Verification successful
            (begin
                (map-set email-verification user
                    (merge verification-data {is-verified: true})
                )
                ;; Update user profile verification status
                (match (map-get? user-profiles {user: user})
                    profile
                    (map-set user-profiles {user: user}
                        (merge profile {is-verified: true})
                    )
                    false
                )
                (ok "Email verified successfully")
            )
            ;; Verification failed
            (begin
                (map-set email-verification user
                    (merge verification-data {attempts: (+ (get attempts verification-data) u1)})
                )
                (err ERR-VERIFICATION-FAILED)
            )
        )
    )
)

;; ===============================================================================
;; CORE PROFILE FUNCTIONS WITH SECURITY
;; ===============================================================================

;; Enhanced profile creation with security checks
(define-public (create-profile (name (string-utf8 50)) (email (string-utf8 50)))
    (let
        (
            (user tx-sender)
            (current-time block-height)
        )
        ;; Security checks
        (asserts! (is-contract-active) (err ERR-SUSPENDED))
        (asserts! (not (is-blacklisted user)) (err ERR-BLACKLISTED))
        (asserts! (not (is-suspended user)) (err ERR-SUSPENDED))
        (try! (check-rate-limit user "create"))
        
        ;; Input validation
        (asserts! (is-valid-name name) (err ERR-INVALID-INPUT))
        (asserts! (is-valid-email email) (err ERR-INVALID-INPUT))
        
        ;; Check if profile already exists
        (asserts! (is-none (map-get? user-profiles {user: user})) (err ERR-ALREADY-EXISTS))
        
        ;; Create profile
        (map-set user-profiles {user: user} 
            {
                name: name, 
                email: email, 
                reputation-score: DEFAULT-REPUTATION,
                created-at: current-time,
                last-updated: current-time,
                is-verified: false,
                is-active: true,
                profile-version: u1,
                update-count: u0
            }
        )
        
        ;; Set default privacy settings
        (map-set privacy-settings user
            {
                profile-visibility: u"public",
                email-visibility: u"private",
                reputation-visibility: u"public"
            }
        )
        
        (ok "Profile created successfully")
    )
)

;; Enhanced profile update with security and history tracking
(define-public (update-profile (name (optional (string-utf8 50))) (email (optional (string-utf8 50))))
    (let
        (
            (user tx-sender)
            (current-time block-height)
        )
        ;; Security checks
        (asserts! (is-contract-active) (err ERR-SUSPENDED))
        (asserts! (not (is-blacklisted user)) (err ERR-BLACKLISTED))
        (asserts! (not (is-suspended user)) (err ERR-SUSPENDED))
        (try! (check-rate-limit user "update"))
        
        (match (map-get? user-profiles {user: user})
            profile-data
            (let
                (
                    (new-name (match name some-name some-name (get name profile-data)))
                    (new-email (match email some-email some-email (get email profile-data)))
                    (new-version (+ (get profile-version profile-data) u1))
                    (new-update-count (+ (get update-count profile-data) u1))
                )
                ;; Input validation for new values
                (asserts! (is-valid-name new-name) (err ERR-INVALID-INPUT))
                (asserts! (is-valid-email new-email) (err ERR-INVALID-INPUT))
                
                ;; Check if email verification is required and email changed
                (if (and (var-get require-email-verification) 
                         (is-some email)
                         (not (is-eq new-email (get email profile-data))))
                    (begin
                        ;; Reset verification status if email changed
                        (match (map-get? email-verification user)
                            verification-data
                            (map-set email-verification user
                                (merge verification-data {is-verified: false})
                            )
                            true
                        )
                        ;; Update profile verification status
                        (map-set user-profiles {user: user}
                            (merge profile-data 
                                {
                                    name: new-name,
                                    email: new-email,
                                    last-updated: current-time,
                                    is-verified: false,
                                    profile-version: new-version,
                                    update-count: new-update-count
                                }
                            )
                        )
                    )
                    ;; Normal update without email change
                    (map-set user-profiles {user: user}
                        (merge profile-data 
                            {
                                name: new-name,
                                email: new-email,
                                last-updated: current-time,
                                profile-version: new-version,
                                update-count: new-update-count
                            }
                        )
                    )
                )
                
                ;; Store profile history
                (map-set profile-history {user: user, version: (get profile-version profile-data)}
                    {
                        old-name: (get name profile-data),
                        old-email: (get email profile-data),
                        changed-at: current-time,
                        change-type: (if (and (is-some name) (is-some email)) 
                                       u"name-email" 
                                       (if (is-some name) u"name-only" u"email-only"))
                    }
                )
                
                (ok "Profile updated successfully")
            )
            (err ERR-NOT-FOUND)
        )
    )
)

;; Update privacy settings
(define-public (update-privacy-settings 
    (profile-visibility (optional (string-utf8 20)))
    (email-visibility (optional (string-utf8 20)))
    (reputation-visibility (optional (string-utf8 20))))
    (let ((user tx-sender))
        (asserts! (is-contract-active) (err ERR-SUSPENDED))
        (asserts! (not (is-blacklisted user)) (err ERR-BLACKLISTED))
        (asserts! (not (is-suspended user)) (err ERR-SUSPENDED))
        
        ;; Check if user has a profile
        (asserts! (is-some (map-get? user-profiles {user: user})) (err ERR-NOT-FOUND))
        
        (match (map-get? privacy-settings user)
            current-settings
            (map-set privacy-settings user
                {
                    profile-visibility: (default-to (get profile-visibility current-settings) profile-visibility),
                    email-visibility: (default-to (get email-visibility current-settings) email-visibility),
                    reputation-visibility: (default-to (get reputation-visibility current-settings) reputation-visibility)
                }
            )
            ;; Create default settings if none exist
            (map-set privacy-settings user
                {
                    profile-visibility: (default-to u"public" profile-visibility),
                    email-visibility: (default-to u"private" email-visibility),
                    reputation-visibility: (default-to u"public" reputation-visibility)
                }
            )
        )
        (ok "Privacy settings updated successfully")
    )
)

;; ===============================================================================
;; READ-ONLY FUNCTIONS WITH PRIVACY CONTROLS
;; ===============================================================================

;; Get profile with privacy controls
(define-read-only (get-profile (user principal))
    (match (map-get? user-profiles {user: user})
        profile
        (if (get is-active profile)
            (match (map-get? privacy-settings user)
                privacy
                (let
                    (
                        (is-owner (is-eq tx-sender user))
                        (is-moderator (is-moderator-or-admin tx-sender))
                        (profile-visible (or is-owner 
                                           is-moderator
                                           (is-eq (get profile-visibility privacy) u"public")))
                    )
                    (if profile-visible
                        (ok {
                            name: (get name profile),
                            email: (if (or is-owner 
                                          is-moderator
                                          (is-eq (get email-visibility privacy) u"public"))
                                      (some (get email profile))
                                      none),
                            reputation-score: (if (or is-owner
                                                     is-moderator  
                                                     (is-eq (get reputation-visibility privacy) u"public"))
                                                 (some (get reputation-score profile))
                                                 none),
                            is-verified: (get is-verified profile),
                            created-at: (get created-at profile),
                            last-updated: (get last-updated profile)
                        })
                        (err ERR-UNAUTHORIZED)
                    )
                )
                ;; Default privacy if none set
                (ok {
                    name: (get name profile),
                    email: (if (is-eq tx-sender user) (some (get email profile)) none),
                    reputation-score: (some (get reputation-score profile)),
                    is-verified: (get is-verified profile),
                    created-at: (get created-at profile),
                    last-updated: (get last-updated profile)
                })
            )
            (err ERR-NOT-FOUND)
        )
        (err ERR-NOT-FOUND)
    )
)

;; Get profile history (user or moderator only)
(define-read-only (get-profile-history (user principal) (version uint))
    (if (or (is-eq tx-sender user) (is-moderator-or-admin tx-sender))
        (match (map-get? profile-history {user: user, version: version})
            history (ok history)
            (err ERR-NOT-FOUND)
        )
        (err ERR-UNAUTHORIZED)
    )
)

;; Get reputation change log (admin only)
(define-read-only (get-reputation-log (log-id uint))
    (if (is-admin tx-sender)
        (match (map-get? reputation-log log-id)
            log-entry (ok log-entry)
            (err ERR-NOT-FOUND)
        )
        (err ERR-UNAUTHORIZED)
    )
)

;; Get contract statistics (admin only)
(define-read-only (get-contract-stats)
    (if (is-admin tx-sender)
        (ok {
            is-paused: (var-get contract-paused),
            require-verification: (var-get require-email-verification),
            next-reputation-log-id: (var-get next-reputation-log-id),
            min-name-length: (var-get min-name-length)
        })
        (err ERR-UNAUTHORIZED)
    )
)

;; Check if user is blacklisted (public for transparency)
(define-read-only (is-user-blacklisted (user principal))
    (ok (is-blacklisted user))
)

;; Check user suspension status (public for transparency)
(define-read-only (get-user-suspension-status (user principal))
    (match (map-get? suspended-users user)
        suspension
        (ok {
            is-suspended: (> (get suspended-until suspension) block-height),
            suspended-until: (get suspended-until suspension),
            reason: (get reason suspension)
        })
        (ok {
            is-suspended: false,
            suspended-until: u0,
            reason: u""
        })
    )
)

;; Initialize contract with first admin
(begin
    (map-set admins tx-sender true)
    (print "User Profile Contract Initialized with Security Features")
)
