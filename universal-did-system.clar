(define-map user-profiles {user: principal} {name: (string-utf8 50), email: (string-utf8 50), reputation-score: uint})

(define-public (create-profile (name (string-utf8 50)) (email (string-utf8 50)))
  (let ((user tx-sender))
    (if (is-none (map-get? user-profiles {user: user}))
        (begin
          (map-insert user-profiles {user: user} {name: name, email: email, reputation-score: 0})
          (ok "Profile created successfully"))
        (err "Profile already exists"))))

(define-public (update-profile (name (optional (string-utf8 50))) (email (optional (string-utf8 50))))
  (let ((user tx-sender))
    (match (map-get? user-profiles {user: user})
      profile-data
      (begin
        (map-set user-profiles {user: user} 
          {name: (default-to (get name profile-data) name), 
           email: (default-to (get email profile-data) email), 
           reputation-score: (get reputation-score profile-data)})
        (ok "Profile updated successfully"))
      (err "Profile does not exist"))))

(define-read-only (get-profile (user principal))
  (map-get? user-profiles {user: user}))
