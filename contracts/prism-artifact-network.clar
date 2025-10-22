;; prism-artifact-network - Light refraction metaphor for data distribution and Chronological asset management protocol

;; ========================================
;; PROTOCOL ADMINISTRATOR
;; ========================================
(define-constant PROTOCOL_MASTER tx-sender)

;; ========================================
;; GLOBAL STATE MANAGEMENT
;; ========================================
(define-data-var total-artifact-count uint u0)

;; ========================================
;; ERROR CODE DEFINITIONS
;; ========================================
(define-constant ERR_ASSET_MISSING (err u401))
(define-constant ERR_DUPLICATE_ENTRY (err u402)) 
(define-constant ERR_INVALID_NAME (err u403))
(define-constant ERR_WEIGHT_VIOLATION (err u404))
(define-constant ERR_NO_PERMISSION (err u405))
(define-constant ERR_LOCATION_INVALID (err u406))
(define-constant ERR_ACTION_BLOCKED (err u407))
(define-constant ERR_FORBIDDEN (err u408))
(define-constant ERR_AUTH_FAILED (err u409))
(define-constant ERR_PRINCIPAL_REJECTED (err u410))
(define-constant ERR_DUPLICATE_WATCH (err u411))
(define-constant ERR_NOT_WATCHING (err u412))

;; ========================================
;; CORE DATA REGISTRIES
;; ========================================


;; Watchlist management map
(define-map custodian-watchlist
  { custodian: principal, asset-id: uint }
  {
    watch-started: uint,
    watch-updated: uint
  }
)

;; Permission control map
(define-map permission-ledger
  { asset-id: uint, requester: principal }
  { 
    is-permitted: bool,
    granted-by: principal,
    grant-height: uint
  }
)
;; Artifact storage map
(define-map artifact-ledger
  { asset-id: uint }
  {
    label: (string-ascii 64),
    custodian: principal,
    mass-value: uint,
    creation-height: uint,
    zone-code: (string-ascii 32),
    spec-data: (string-ascii 128),
    tag-array: (list 10 (string-ascii 32))
  }
)


;; ========================================
;; TAG VALIDATION UTILITIES
;; ========================================

;; Validate single tag format
(define-private (is-tag-valid (tag (string-ascii 32)))
  (and 
    (> (len tag) u0)
    (< (len tag) u33)
  )
)

;; Validate tag collection
(define-private (are-tags-valid (tags (list 10 (string-ascii 32))))
  (and
    (> (len tags) u0)
    (<= (len tags) u10)
    (is-eq (len (filter is-tag-valid tags)) (len tags))
  )
)

;; ========================================
;; VALIDATION HELPER FUNCTIONS
;; ========================================

;; Verify watchlist status
(define-private (is-on-watchlist (asset-id uint) (custodian principal))
  (is-some (map-get? custodian-watchlist { custodian: custodian, asset-id: asset-id }))
)


;; Check asset existence
(define-private (does-asset-exist (asset-id uint))
  (is-some (map-get? artifact-ledger { asset-id: asset-id }))
)

;; Verify custodian ownership
(define-private (is-valid-custodian (asset-id uint) (check-principal principal))
  (match (map-get? artifact-ledger { asset-id: asset-id })
    asset-data (is-eq (get custodian asset-data) check-principal)
    false
  )
)

;; Validate principal address format
(define-private (is-principal-valid (check-principal principal))
  (not (is-eq check-principal 'ST000000000000000000002AMW42H))
)

;; Check permission status
(define-private (has-valid-permission (asset-id uint) (requester principal))
  (match (map-get? permission-ledger { asset-id: asset-id, requester: requester })
    perm-data (get is-permitted perm-data)
    false
  )
)

;; ========================================
;; WATCHLIST OPERATIONS
;; ========================================

;; Add asset to personal watchlist
(define-public (watchlist-add (asset-id uint))
  (let
    (
      (asset-data (unwrap! (map-get? artifact-ledger { asset-id: asset-id }) ERR_ASSET_MISSING))
    )
    (asserts! (does-asset-exist asset-id) ERR_ASSET_MISSING)
    (asserts! (has-valid-permission asset-id tx-sender) ERR_FORBIDDEN)
    (asserts! (not (is-on-watchlist asset-id tx-sender)) ERR_DUPLICATE_WATCH)

    (map-insert custodian-watchlist
      { custodian: tx-sender, asset-id: asset-id }
      {
        watch-started: block-height,
        watch-updated: block-height
      }
    )
    (ok true)
  )
)

;; Remove asset from personal watchlist
(define-public (watchlist-remove (asset-id uint))
  (let
    (
      (asset-data (unwrap! (map-get? artifact-ledger { asset-id: asset-id }) ERR_ASSET_MISSING))
    )
    (asserts! (does-asset-exist asset-id) ERR_ASSET_MISSING)
    (asserts! (is-on-watchlist asset-id tx-sender) ERR_NOT_WATCHING)

    (map-delete custodian-watchlist { custodian: tx-sender, asset-id: asset-id })
    (ok true)
  )
)

;; ========================================
;; PERMISSION MANAGEMENT
;; ========================================

;; Grant permission to another principal
(define-public (grant-access (asset-id uint) (requester principal))
  (let
    (
      (asset-data (unwrap! (map-get? artifact-ledger { asset-id: asset-id }) ERR_ASSET_MISSING))
    )
    (asserts! (does-asset-exist asset-id) ERR_ASSET_MISSING)
    (asserts! (is-valid-custodian asset-id tx-sender) ERR_NO_PERMISSION)
    (asserts! (not (is-eq requester tx-sender)) ERR_AUTH_FAILED)

    (map-set permission-ledger
      { asset-id: asset-id, requester: requester }
      { 
        is-permitted: true,
        granted-by: tx-sender,
        grant-height: block-height
      }
    )
    (ok true)
  )
)

;; Revoke permission from principal
(define-public (revoke-access (asset-id uint) (requester principal))
  (let
    (
      (asset-data (unwrap! (map-get? artifact-ledger { asset-id: asset-id }) ERR_ASSET_MISSING))
      (perm-data (unwrap! (map-get? permission-ledger { asset-id: asset-id, requester: requester }) ERR_NO_PERMISSION))
    )
    (asserts! (does-asset-exist asset-id) ERR_ASSET_MISSING)
    (asserts! (is-valid-custodian asset-id tx-sender) ERR_NO_PERMISSION)
    (asserts! (not (is-eq requester tx-sender)) ERR_AUTH_FAILED)

    (map-delete permission-ledger { asset-id: asset-id, requester: requester })
    (if (is-on-watchlist asset-id requester)
      (map-delete custodian-watchlist { custodian: requester, asset-id: asset-id })
      true
    )
    (ok true)
  )
)

;; ========================================
;; ASSET LIFECYCLE MANAGEMENT
;; ========================================

;; Create new artifact entry
(define-public (create-artifact (label (string-ascii 64)) (mass uint) (zone-code (string-ascii 32)) 
                                      (spec-data (string-ascii 128)) (tag-array (list 10 (string-ascii 32))))
  (let
    (
      (next-id (+ (var-get total-artifact-count) u1))
    )
    (asserts! (> (len label) u0) ERR_INVALID_NAME)
    (asserts! (< (len label) u65) ERR_INVALID_NAME)
    (asserts! (> mass u0) ERR_WEIGHT_VIOLATION)
    (asserts! (< mass u1000000000) ERR_WEIGHT_VIOLATION)
    (asserts! (> (len zone-code) u0) ERR_LOCATION_INVALID)
    (asserts! (< (len zone-code) u33) ERR_LOCATION_INVALID)
    (asserts! (> (len spec-data) u0) ERR_INVALID_NAME)
    (asserts! (< (len spec-data) u129) ERR_INVALID_NAME)
    (asserts! (are-tags-valid tag-array) ERR_INVALID_NAME)

    (map-insert artifact-ledger
      { asset-id: next-id }
      {
        label: label,
        custodian: tx-sender,
        mass-value: mass,
        creation-height: block-height,
        zone-code: zone-code,
        spec-data: spec-data,
        tag-array: tag-array
      }
    )

    (map-insert permission-ledger
      { asset-id: next-id, requester: tx-sender }
      { 
        is-permitted: true,
        granted-by: tx-sender,
        grant-height: block-height
      }
    )
    (var-set total-artifact-count next-id)
    (ok next-id)
  )
)

;; Update artifact metadata
(define-public (modify-artifact (asset-id uint) (new-label (string-ascii 64)) (new-mass uint) 
                                           (new-zone-code (string-ascii 32)) (new-spec-data (string-ascii 128)) 
                                           (new-tag-array (list 10 (string-ascii 32))))
  (let
    (
      (asset-data (unwrap! (map-get? artifact-ledger { asset-id: asset-id }) ERR_ASSET_MISSING))
    )
    (asserts! (does-asset-exist asset-id) ERR_ASSET_MISSING)
    (asserts! (is-eq (get custodian asset-data) tx-sender) ERR_NO_PERMISSION)
    (asserts! (> (len new-label) u0) ERR_INVALID_NAME)
    (asserts! (< (len new-label) u65) ERR_INVALID_NAME)
    (asserts! (> new-mass u0) ERR_WEIGHT_VIOLATION)
    (asserts! (< new-mass u1000000000) ERR_WEIGHT_VIOLATION)
    (asserts! (> (len new-zone-code) u0) ERR_LOCATION_INVALID)
    (asserts! (< (len new-zone-code) u33) ERR_LOCATION_INVALID)
    (asserts! (> (len new-spec-data) u0) ERR_INVALID_NAME)
    (asserts! (< (len new-spec-data) u129) ERR_INVALID_NAME)
    (asserts! (are-tags-valid new-tag-array) ERR_INVALID_NAME)

    (map-set artifact-ledger
      { asset-id: asset-id }
      (merge asset-data { 
        label: new-label, 
        mass-value: new-mass, 
        zone-code: new-zone-code, 
        spec-data: new-spec-data, 
        tag-array: new-tag-array 
      })
    )
    (ok true)
  )
)

;; Transfer custodianship
(define-public (transfer-custodianship (asset-id uint) (new-custodian principal))
  (let
    (
      (asset-data (unwrap! (map-get? artifact-ledger { asset-id: asset-id }) ERR_ASSET_MISSING))
    )
    (asserts! (does-asset-exist asset-id) ERR_ASSET_MISSING)
    (asserts! (is-valid-custodian asset-id tx-sender) ERR_NO_PERMISSION)
    (asserts! (not (is-eq new-custodian tx-sender)) ERR_AUTH_FAILED)
    (asserts! (is-principal-valid new-custodian) ERR_PRINCIPAL_REJECTED)

    (map-set artifact-ledger
      { asset-id: asset-id }
      (merge asset-data { custodian: new-custodian })
    )

    (map-set permission-ledger
      { asset-id: asset-id, requester: new-custodian }
      {
        is-permitted: true,
        granted-by: tx-sender,
        grant-height: block-height
      }
    )
    (ok true)
  )
)

;; Delete artifact from registry
(define-public (remove-artifact (asset-id uint))
  (let
    (
      (asset-data (unwrap! (map-get? artifact-ledger { asset-id: asset-id }) ERR_ASSET_MISSING))
    )
    (asserts! (does-asset-exist asset-id) ERR_ASSET_MISSING)
    (asserts! (is-eq (get custodian asset-data) tx-sender) ERR_NO_PERMISSION)

    (map-delete artifact-ledger { asset-id: asset-id })
    (ok true)
  )
)

;; ========================================
;; ADVANCED SECURITY FEATURES
;; ========================================

;; Multilayer authentication mechanism
(define-public (authenticate-advanced (asset-id uint) (auth-code uint) (operation-type (string-ascii 32)))
  (let
    (
      (asset-data (unwrap! (map-get? artifact-ledger { asset-id: asset-id }) ERR_ASSET_MISSING))
      (perm-data (unwrap! (map-get? permission-ledger { asset-id: asset-id, requester: tx-sender }) ERR_FORBIDDEN))
      (hash-result (+ asset-id block-height (get mass-value asset-data)))
    )
    (asserts! (does-asset-exist asset-id) ERR_ASSET_MISSING)
    (asserts! (has-valid-permission asset-id tx-sender) ERR_FORBIDDEN)
    (asserts! (> (len operation-type) u0) ERR_INVALID_NAME)
    (asserts! (< (len operation-type) u33) ERR_INVALID_NAME)
    (asserts! (> auth-code u0) ERR_AUTH_FAILED)

    (asserts! (is-eq (mod hash-result u1000) (mod auth-code u1000)) ERR_AUTH_FAILED)

    (map-set permission-ledger
      { asset-id: asset-id, requester: tx-sender }
      (merge perm-data { grant-height: block-height })
    )

    (ok hash-result)
  )
)

;; Create audit record
(define-public (log-access-event (asset-id uint) (event-type (string-ascii 32)) (event-notes (string-ascii 96)))
  (let
    (
      (asset-data (unwrap! (map-get? artifact-ledger { asset-id: asset-id }) ERR_ASSET_MISSING))
    )
    (asserts! (does-asset-exist asset-id) ERR_ASSET_MISSING)
    (asserts! (has-valid-permission asset-id tx-sender) ERR_FORBIDDEN)
    (asserts! (> (len event-type) u0) ERR_INVALID_NAME)
    (asserts! (< (len event-type) u33) ERR_INVALID_NAME)
    (asserts! (> (len event-notes) u0) ERR_INVALID_NAME)
    (asserts! (< (len event-notes) u97) ERR_INVALID_NAME)

    (map-set permission-ledger
      { asset-id: asset-id, requester: tx-sender }
      { 
        is-permitted: true,
        granted-by: tx-sender,
        grant-height: block-height
      }
    )

    (ok block-height)
  )
)

;; Comprehensive integrity validation
(define-public (verify-integrity (asset-id uint))
  (let
    (
      (asset-data (unwrap! (map-get? artifact-ledger { asset-id: asset-id }) ERR_ASSET_MISSING))
      (custodian-perm (map-get? permission-ledger { asset-id: asset-id, requester: (get custodian asset-data) }))
    )
    (asserts! (does-asset-exist asset-id) ERR_ASSET_MISSING)
    (asserts! (is-valid-custodian asset-id tx-sender) ERR_NO_PERMISSION)

    (asserts! (is-some custodian-perm) ERR_FORBIDDEN)
    (asserts! (and (> (len (get label asset-data)) u0) (< (len (get label asset-data)) u65)) ERR_INVALID_NAME)
    (asserts! (and (> (get mass-value asset-data) u0) (< (get mass-value asset-data) u1000000000)) ERR_WEIGHT_VIOLATION)
    (asserts! (and (> (len (get zone-code asset-data)) u0) (< (len (get zone-code asset-data)) u33)) ERR_LOCATION_INVALID)
    (asserts! (and (> (len (get spec-data asset-data)) u0) (< (len (get spec-data asset-data)) u129)) ERR_INVALID_NAME)
    (asserts! (are-tags-valid (get tag-array asset-data)) ERR_INVALID_NAME)

    (ok true)
  )
)

;; Emergency override by administrator
(define-public (admin-override (asset-id uint) (new-custodian principal) (justification (string-ascii 64)))
  (let
    (
      (asset-data (unwrap! (map-get? artifact-ledger { asset-id: asset-id }) ERR_ASSET_MISSING))
    )
    (asserts! (is-eq tx-sender PROTOCOL_MASTER) ERR_NO_PERMISSION)
    (asserts! (does-asset-exist asset-id) ERR_ASSET_MISSING)
    (asserts! (is-principal-valid new-custodian) ERR_PRINCIPAL_REJECTED)
    (asserts! (> (len justification) u0) ERR_INVALID_NAME)
    (asserts! (< (len justification) u65) ERR_INVALID_NAME)
    (asserts! (not (is-eq new-custodian (get custodian asset-data))) ERR_AUTH_FAILED)

    (map-set artifact-ledger
      { asset-id: asset-id }
      (merge asset-data { custodian: new-custodian })
    )

    (map-set permission-ledger
      { asset-id: asset-id, requester: new-custodian }
      {
        is-permitted: true,
        granted-by: PROTOCOL_MASTER,
        grant-height: block-height
      }
    )
    (ok true)
  )
)


;; Clear all permissions except custodian
(define-public (clear-all-permissions (asset-id uint))
  (let
    (
      (asset-data (unwrap! (map-get? artifact-ledger { asset-id: asset-id }) ERR_ASSET_MISSING))
    )
    (asserts! (does-asset-exist asset-id) ERR_ASSET_MISSING)
    (asserts! (is-valid-custodian asset-id tx-sender) ERR_NO_PERMISSION)

    (map-set permission-ledger
      { asset-id: asset-id, requester: tx-sender }
      { 
        is-permitted: true,
        granted-by: tx-sender,
        grant-height: block-height
      }
    )
    (ok true)
  )
)


