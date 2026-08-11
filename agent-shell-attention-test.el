;;; agent-shell-attention-test.el --- Tests for agent-shell-attention  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(setq load-prefer-newer t)

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'agent-shell-attention)

;; Minimal stubs so we can create buffers that satisfy `(derived-mode-p 'agent-shell-mode)`.
(define-derived-mode agent-shell-mode fundamental-mode "agent-shell")
(defvar-local agent-shell--state nil)

(defun agent-shell-attention-test--permission-state (request-id tool-call-id)
  "Return test state with TOOL-CALL-ID awaiting permission REQUEST-ID."
  (let ((state (make-hash-table :test #'eq))
        (tool-calls (make-hash-table :test #'equal))
        (tool-call (make-hash-table :test #'eq)))
    (puthash :permission-request-id request-id tool-call)
    (puthash :status "pending" tool-call)
    (puthash tool-call-id tool-call tool-calls)
    (puthash :tool-calls tool-calls state)
    state))

(ert-deftest agent-shell-attention--apply-indicator-location-handles-nonlists ()
  (let ((orig-default-mlmi (default-value 'mode-line-misc-info))
        (orig-default-gms (default-value 'global-mode-string)))
    (unwind-protect
        (let ((mode-line-misc-info "misc")
              (global-mode-string "gms")
              (agent-shell-attention-indicator-location 'global-mode-string))
          (agent-shell-attention--apply-indicator-location)
          (should (listp global-mode-string))
          (should (member agent-shell-attention--mode-line global-mode-string))
          (should (listp mode-line-misc-info)))
      (setq-default mode-line-misc-info orig-default-mlmi)
      (setq-default global-mode-string orig-default-gms))))

(ert-deftest agent-shell-attention--message-skips-dead-buffers ()
  (let ((buffer (generate-new-buffer " *asa-dead*")))
    (kill-buffer buffer)
    (should-not (agent-shell-attention--message buffer "hi"))))

(ert-deftest agent-shell-attention--buffer-selected-p-counts-viewport ()
  "A session's viewport buffer in the selected window counts as selected."
  (let ((shell (generate-new-buffer "*asa-viewport-shell*"))
        (viewport (generate-new-buffer "*asa-viewport-shell* [viewport]"))
        (other (generate-new-buffer "*asa-other-shell* [viewport]"))
        (before (window-buffer (selected-window))))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell-viewport--shell-buffer)
                   (lambda (&optional buffer)
                     (let ((name (buffer-name (or buffer (current-buffer)))))
                       (when (string-suffix-p " [viewport]" name)
                         (get-buffer
                          (substring name 0 (- (length name)
                                               (length " [viewport]")))))))))
          (set-window-buffer (selected-window) shell)
          (should (agent-shell-attention--buffer-selected-p shell))
          (set-window-buffer (selected-window) viewport)
          (should (agent-shell-attention--buffer-selected-p shell))
          (set-window-buffer (selected-window) other)
          (should-not (agent-shell-attention--buffer-selected-p shell)))
      (set-window-buffer (selected-window) before)
      (kill-buffer shell)
      (kill-buffer viewport)
      (kill-buffer other))))

(ert-deftest agent-shell-attention--buffer-selected-p-without-viewport-library ()
  "Without agent-shell-viewport loaded, only the shell buffer counts."
  (should-not (fboundp 'agent-shell-viewport--shell-buffer))
  (let ((shell (generate-new-buffer "*asa-plain-shell*"))
        (viewport (generate-new-buffer "*asa-plain-shell* [viewport]"))
        (before (window-buffer (selected-window))))
    (unwind-protect
        (progn
          (set-window-buffer (selected-window) shell)
          (should (agent-shell-attention--buffer-selected-p shell))
          (set-window-buffer (selected-window) viewport)
          (should-not (agent-shell-attention--buffer-selected-p shell)))
      (set-window-buffer (selected-window) before)
      (kill-buffer shell)
      (kill-buffer viewport))))

(ert-deftest agent-shell-attention--permission-pending-p-robustness ()
  (let ((buffer (generate-new-buffer " *asa-perm*")))
    (unwind-protect
        (with-current-buffer buffer
          (agent-shell-mode)
          ;; Non-map tool calls should not error.
          (setq agent-shell--state (let ((state (make-hash-table :test #'eq)))
                                     (puthash :tool-calls 123 state)
                                     state))
          (should-not (agent-shell-attention--permission-pending-p buffer))
          ;; Pending permission tool call should be detected.
          (let* ((tool-calls (make-hash-table :test #'equal))
                 (tool-call (let ((m (make-hash-table :test #'eq)))
                              (puthash :permission-request-id "req-1" m)
                              (puthash :status "pending" m)
                              m))
                 (state (make-hash-table :test #'eq)))
            (puthash "tool-1" tool-call tool-calls)
            (puthash :tool-calls tool-calls state)
            (setq agent-shell--state state)
            (should (agent-shell-attention--permission-pending-p buffer))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest agent-shell-attention-display-buffer-across-tabs-ignores-tab-errors ()
  (skip-unless (fboundp 'tab-bar-select-tab))
  (let ((tab-bar-mode t)
        (seen 0))
    (cl-letf (((symbol-function 'tab-bar-tabs)
               (lambda (&optional _frame)
                 (list '((ws . bogus)) '((ws . bogus)))))
              ((symbol-function 'tab-bar--current-tab)
               (lambda () '((ws . bogus))))
              ((symbol-function 'agent-shell-attention--tab-displays-buffer-p)
               (lambda (_tab _buffer)
                 (setq seen (1+ seen))
                 (= seen 2)))
              ((symbol-function 'tab-bar-select-tab)
               (lambda (_idx) (error "boom"))))
      (should-not (agent-shell-attention-display-buffer-across-tabs (current-buffer) nil)))))

(ert-deftest agent-shell-attention--active-entry-records-pending-first ()
  (let ((agent-shell-attention--pending (make-hash-table :test #'eq))
        (agent-shell-attention--busy (make-hash-table :test #'eq))
        (pending-buf (generate-new-buffer " *asa-pending*"))
        (busy-buf (generate-new-buffer " *asa-busy*")))
    (unwind-protect
        (progn
          (with-current-buffer pending-buf (agent-shell-mode))
          (with-current-buffer busy-buf
            (agent-shell-mode)
            (setq agent-shell--state (list (cons :active-requests '(req)))))
          (puthash pending-buf (cons "Need reply" 1.0) agent-shell-attention--pending)
          (let ((records (agent-shell-attention--active-entry-records)))
            (should (= (length records) 2))
            (should (eq (nth 0 (nth 0 records)) pending-buf))
            (should (eq (nth 2 (nth 0 records)) 'pending))
            (should (eq (nth 0 (nth 1 records)) busy-buf))
            (should-not (nth 1 (nth 1 records)))
            (should (eq (nth 2 (nth 1 records)) 'busy))))
      (when (buffer-live-p pending-buf) (kill-buffer pending-buf))
      (when (buffer-live-p busy-buf) (kill-buffer busy-buf)))))

(ert-deftest agent-shell-attention--completion-table-metadata-and-ordering ()
  (let ((agent-shell-attention--pending (make-hash-table :test #'eq))
        (agent-shell-attention--busy (make-hash-table :test #'eq))
        (agent-shell-attention-jump-show-groups nil)
        (pending-buf (generate-new-buffer " *asa-pending*"))
        (busy-buf (generate-new-buffer " *asa-busy*")))
    (unwind-protect
        (progn
          (with-current-buffer pending-buf (agent-shell-mode))
          (with-current-buffer busy-buf
            (agent-shell-mode)
            (setq agent-shell--state (list (cons :active-requests '(req)))))
          ;; Mark the same buffer both pending and busy; it should only appear once.
          (puthash pending-buf (cons "Need reply" 1.0) agent-shell-attention--pending)
          (with-current-buffer pending-buf
            (setq agent-shell--state (list (cons :active-requests '(req)))))
          (let* ((records (agent-shell-attention--active-entry-records))
                 (candidates (agent-shell-attention--unique-candidates-with-status records))
                 (table (agent-shell-attention--completion-table candidates))
                 (meta (funcall table "" nil 'metadata))
                 (sort-fn (cdr (assq 'display-sort-function (cdr meta)))))
            (should (equal (car meta) 'metadata))
            (should (functionp sort-fn))
            (should (assq 'affixation-function (cdr meta)))
            (should (assq 'annotation-function (cdr meta)))
            (let ((all (funcall table "" nil t)))
              (should (= (length all) 2))
              ;; Pending should come before busy-only, even if the input list is reversed.
              (let ((sorted (funcall sort-fn (reverse all))))
                (should (string-match-p "Need reply" (car sorted)))))))
      (when (buffer-live-p pending-buf) (kill-buffer pending-buf))
      (when (buffer-live-p busy-buf) (kill-buffer busy-buf)))))

(ert-deftest agent-shell-attention--completion-table-group-function-protocol ()
  (let ((agent-shell-attention--pending (make-hash-table :test #'eq))
        (agent-shell-attention--busy (make-hash-table :test #'eq))
        (agent-shell-attention-jump-show-groups t)
        (pending-buf (generate-new-buffer " *asa-pending*")))
    (unwind-protect
        (progn
          (with-current-buffer pending-buf (agent-shell-mode))
          (puthash pending-buf (cons "Need reply" 1.0) agent-shell-attention--pending)
          (let* ((records (agent-shell-attention--active-entry-records))
                 (candidates (agent-shell-attention--unique-candidates-with-status records))
                 (table (agent-shell-attention--completion-table candidates))
                 (meta (funcall table "" nil 'metadata))
                 (group-fn (cdr (assq 'group-function (cdr meta))))
                 (display (caar candidates)))
            (should (functionp group-fn))
            (should (stringp (funcall group-fn display nil)))
            (should (equal (funcall group-fn display t) display))))
      (when (buffer-live-p pending-buf) (kill-buffer pending-buf)))))

(ert-deftest agent-shell-attention--completion-tags-match-dashboard-vocabulary ()
  (should (equal (substring-no-properties
                  (agent-shell-attention--completion-tag 'pending))
                 "Awaiting"))
  (should (equal (substring-no-properties
                  (agent-shell-attention--completion-tag 'permission))
                 "Permissions"))
  (should (equal (substring-no-properties
                  (agent-shell-attention--completion-tag 'busy))
                 "Running")))

(ert-deftest agent-shell-attention--active-entry-records-detects-permissions ()
  (let ((agent-shell-attention--pending (make-hash-table :test #'eq))
        (agent-shell-attention--busy (make-hash-table :test #'eq))
        (permission-buf (generate-new-buffer " *asa-permission*")))
    (unwind-protect
        (progn
          (with-current-buffer permission-buf (agent-shell-mode))
          (puthash permission-buf (cons "Permission: write file" 1.0)
                   agent-shell-attention--pending)
          (let ((records (agent-shell-attention--active-entry-records)))
            (should (= (length records) 1))
            (should (eq (nth 0 (car records)) permission-buf))
            (should (eq (nth 2 (car records)) 'permission))))
      (when (buffer-live-p permission-buf)
        (kill-buffer permission-buf)))))

(ert-deftest agent-shell-attention--busy-live-buffers-syncs-from-active-requests ()
  (let ((agent-shell-attention--pending (make-hash-table :test #'eq))
        (agent-shell-attention--busy (make-hash-table :test #'eq))
        (agent-shell-attention--busy-since (make-hash-table :test #'eq))
        (agent-shell-attention--last-event (make-hash-table :test #'eq))
        (buffer (generate-new-buffer " *asa-busy-sync*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (agent-shell-mode)
            (setq agent-shell--state (list (cons :active-requests '(req)))))
          (should (equal (agent-shell-attention--busy-live-buffers)
                         (list buffer)))
          (should (gethash buffer agent-shell-attention--busy))
          (with-current-buffer buffer
            (setf (map-elt agent-shell--state :active-requests) nil))
          (should-not (agent-shell-attention--busy-live-buffers))
          (should-not (gethash buffer agent-shell-attention--busy)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest agent-shell-attention--around-send-command-supports-shell-buffer ()
  (let ((agent-shell-attention--pending (make-hash-table :test #'eq))
        (agent-shell-attention--busy (make-hash-table :test #'eq))
        (buffer (generate-new-buffer " *asa-send*"))
        (seen-buffer nil)
        (send-called nil)
        (request-decorated nil))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (agent-shell-mode))
          (cl-letf (((symbol-function 'agent-shell-attention--clear-buffer)
                     (lambda (_buffer) nil))
                    ((symbol-function 'agent-shell-attention--mark-busy)
                     (lambda (buf)
                       (setq seen-buffer buf)))
                    ((symbol-function 'agent-shell-attention--clear-busy)
                     (lambda (_buffer) nil))
                    ((symbol-function 'agent-shell-attention--decorate-request)
                     (lambda (_buffer request-args)
                       (setq request-decorated t)
                       request-args))
                    ((symbol-function 'acp-send-request)
                     (lambda (&rest _request-args)
                       'acp-ok))
                    ((symbol-function 'fake-orig)
                     (lambda (&rest _args)
                       (setq send-called t)
                       (acp-send-request :request 'dummy)
                       'orig-ok)))
            (should
             (eq (agent-shell-attention--around-send-command
                  #'fake-orig :prompt "hello" :shell-buffer buffer)
                 'orig-ok))
            (should send-called)
            (should (eq seen-buffer buffer))
            (should request-decorated)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest agent-shell-attention--around-send-command-refreshes-dashboard-after-request-start ()
  (let ((agent-shell-attention--pending (make-hash-table :test #'eq))
        (agent-shell-attention--busy (make-hash-table :test #'eq))
        (buffer (generate-new-buffer " *asa-send-refresh*"))
        (refreshes 0))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (agent-shell-mode))
          (cl-letf (((symbol-function 'agent-shell-attention--clear-buffer)
                     (lambda (_buffer) nil))
                    ((symbol-function 'agent-shell-attention--mark-busy)
                     (lambda (_buffer)
                       (agent-shell-attention--maybe-refresh-dashboard)))
                    ((symbol-function 'agent-shell-attention--maybe-refresh-dashboard)
                     (lambda ()
                       (setq refreshes (1+ refreshes))))
                    ((symbol-function 'agent-shell-attention--decorate-request)
                     (lambda (_buffer request-args)
                       request-args))
                    ((symbol-function 'acp-send-request)
                     (lambda (&rest _request-args)
                       'acp-ok))
                    ((symbol-function 'fake-orig)
                     (lambda (&rest _args)
                       (acp-send-request :request 'dummy)
                       'orig-ok)))
            (should
             (eq (agent-shell-attention--around-send-command
                  #'fake-orig :prompt "hello" :shell-buffer buffer)
                 'orig-ok))
            (should (= refreshes 2))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest agent-shell-attention--decorate-request-wraps-success-and-failure ()
  (let ((buffer (generate-new-buffer " *asa-decorate*"))
        (success-cleared nil)
        (success-handled nil)
        (failure-called nil)
        (orig-success nil)
        (orig-failure nil))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (agent-shell-mode))
          (cl-letf (((symbol-function 'agent-shell-attention--clear-busy)
                     (lambda (_buffer)
                       (setq success-cleared t)
                       (setq failure-called (or failure-called 'cleared))))
                    ((symbol-function 'agent-shell-attention--handle-success)
                     (lambda (_buffer response)
                       (setq success-handled response)))
                    ((symbol-function 'agent-shell-attention--handle-failure)
                     (lambda (_buffer error raw-message)
                       (setq failure-called (list error raw-message)))))
            (let* ((wrapped (agent-shell-attention--decorate-request
                             buffer
                             (list :on-success (lambda (response)
                                                 (setq orig-success response))
                                   :on-failure (lambda (error raw-message)
                                                 (setq orig-failure
                                                       (list error raw-message))))))
                   (success-fn (plist-get wrapped :on-success))
                   (failure-fn (plist-get wrapped :on-failure))
                   (response '((stopReason . "end_turn")))
                   (error '((message . "boom")))
                   (raw '((message . "raw"))))
              (funcall success-fn response)
              (should success-cleared)
              (should-not success-handled)
              (should (equal orig-success response))
              (funcall failure-fn error raw)
              (should (equal failure-called (list error raw)))
              (should (equal orig-failure (list error raw))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest agent-shell-attention-jump-double-prefix-opens-dashboard ()
  (let ((opened nil))
    (cl-letf (((symbol-function 'agent-shell-attention-dashboard)
               (lambda ()
                 (setq opened t))))
      (agent-shell-attention-jump '(16))
      (should opened))))

(ert-deftest agent-shell-attention--dashboard-records-status-order ()
  (let* ((agent-shell-attention--pending (make-hash-table :test #'eq))
         (agent-shell-attention--busy (make-hash-table :test #'eq))
         (agent-shell-attention--busy-since (make-hash-table :test #'eq))
         (agent-shell-attention--last-event (make-hash-table :test #'eq))
         (pending-buf (generate-new-buffer " *asa-dashboard-pending*"))
         (busy-buf (generate-new-buffer " *asa-dashboard-busy*"))
         (failed-buf (generate-new-buffer " *asa-dashboard-failed*"))
         (done-buf (generate-new-buffer " *asa-dashboard-done*"))
         (idle-buf (generate-new-buffer " *asa-dashboard-idle*")))
    (unwind-protect
        (progn
          (dolist (buffer (list pending-buf busy-buf failed-buf done-buf idle-buf))
            (with-current-buffer buffer
              (agent-shell-mode)))
          (puthash pending-buf (cons "Permission: write file" 10.0)
                   agent-shell-attention--pending)
          (with-current-buffer busy-buf
            (setq agent-shell--state (list (cons :active-requests '(req)))))
          (puthash busy-buf 20.0 agent-shell-attention--busy-since)
          (puthash failed-buf (list :status 'failed
                                    :summary "Network timeout"
                                    :timestamp 40.0)
                   agent-shell-attention--last-event)
          (puthash done-buf (list :status 'done
                                  :summary "Finished"
                                  :timestamp 30.0)
                   agent-shell-attention--last-event)
          (let* ((records (agent-shell-attention--dashboard-records))
                 (statuses (mapcar (lambda (record)
                                     (plist-get record :status))
                                   records)))
            (should (equal statuses '(pending busy idle idle idle)))
            (should (equal (plist-get (nth 0 records) :status-text)
                           "Permissions: (write file)"))
            (should (equal (plist-get (nth 1 records) :status-text) "Running"))
            (should (equal (plist-get (nth 2 records) :status-text) "Idle"))
            (should (equal (plist-get (nth 3 records) :status-text) "Idle"))
            (should (equal (plist-get (nth 4 records) :status-text) "Idle"))))
      (dolist (buffer (list pending-buf busy-buf failed-buf done-buf idle-buf))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest agent-shell-attention--pending-status-text ()
  (should (equal (agent-shell-attention--pending-status-text
                  (cons "Finished" 1.0))
                 "Awaiting"))
  (should (equal (agent-shell-attention--pending-status-text
                  (cons "Permission: Run touch /tmp/probe (execute)" 1.0))
                 "Permissions: (Run touch /tmp/probe (execute))")))

(ert-deftest agent-shell-attention--pending-status-text-truncation-customizable ()
  (let ((agent-shell-attention-dashboard-permission-detail-length 10))
    (should (equal (agent-shell-attention--pending-status-text
                    (cons "Permission: long permission detail" 1.0))
                   "Permissions: (long pe...)")))
  (let ((agent-shell-attention-dashboard-permission-detail-length 0))
    (should (equal (agent-shell-attention--pending-status-text
                    (cons "Permission: long permission detail" 1.0))
                   "Permissions"))))

(ert-deftest agent-shell-attention--handle-event-permission-request-marks-buffer ()
  (let ((buffer (generate-new-buffer " *asa-event-permission*"))
        (message-text nil)
        (marked nil))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (agent-shell-mode))
          (cl-letf (((symbol-function 'agent-shell-attention--message)
                     (lambda (_buffer text)
                       (setq message-text text)))
                    ((symbol-function 'agent-shell-attention--mark-buffer)
                     (lambda (buf label &rest _args)
                       (setq marked (list buf label)))))
            (agent-shell-attention--handle-event
             buffer
             '((:event . permission-request)
               (:data . ((:tool-call . ((:title . "write file")
                                        (:kind . "execute")))))))
            (should (equal message-text "Permission: write file (execute)"))
            (should (equal marked
                           (list buffer "Permission: write file (execute)")))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest agent-shell-attention--handle-event-permission-request-stores-identity ()
  (let ((agent-shell-attention--pending (make-hash-table :test #'eq))
        (buffer (generate-new-buffer " *asa-event-permission-id*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (agent-shell-mode))
          (cl-letf (((symbol-function 'agent-shell-attention--message)
                     (lambda (&rest _) nil)))
            (agent-shell-attention--handle-event
             buffer
             '((:event . permission-request)
               (:data . ((:request-id . "req-1")
                         (:tool-call-id . "tool-1")
                         (:tool-call . ((:title . "write file")
                                        (:kind . "execute"))))))))
          (let ((entry (gethash buffer agent-shell-attention--pending)))
            (should (equal (agent-shell-attention--pending-entry-label entry)
                           "Permission: write file (execute)"))
            (should (equal (agent-shell-attention--pending-entry-request-id entry)
                           "req-1"))
            (should (equal (agent-shell-attention--pending-entry-tool-call-id entry)
                           "tool-1"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest agent-shell-attention--handle-event-permission-response-clears-matching-entry ()
  (let ((agent-shell-attention--pending (make-hash-table :test #'eq))
        (buffer (generate-new-buffer " *asa-event-permission-clear*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (agent-shell-mode)
            (setq agent-shell--state
                  (agent-shell-attention-test--permission-state
                   "req-2" "tool-2")))
          (puthash buffer
                   (list :label "Permission: old"
                         :timestamp 1.0
                         :request-id "req-1"
                         :tool-call-id "tool-1")
                   agent-shell-attention--pending)
          (agent-shell-attention--handle-event
           buffer
           '((:event . permission-response)
             (:data . ((:request-id . "req-1")
                       (:tool-call-id . "tool-1")))))
          (should-not (gethash buffer agent-shell-attention--pending)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest agent-shell-attention--handle-event-permission-response-keeps-newer-entry ()
  (let ((agent-shell-attention--pending (make-hash-table :test #'eq))
        (buffer (generate-new-buffer " *asa-event-permission-keep*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (agent-shell-mode)
            (setq agent-shell--state
                  (agent-shell-attention-test--permission-state
                   "req-2" "tool-2")))
          (puthash buffer
                   (list :label "Permission: new"
                         :timestamp 2.0
                         :request-id "req-2"
                         :tool-call-id "tool-2")
                   agent-shell-attention--pending)
          (agent-shell-attention--handle-event
           buffer
           '((:event . permission-response)
             (:data . ((:request-id . "req-1")
                       (:tool-call-id . "tool-1")))))
          (let ((entry (gethash buffer agent-shell-attention--pending)))
            (should entry)
            (should (equal (agent-shell-attention--pending-entry-label entry)
                           "Permission: new"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest agent-shell-attention--handle-event-permission-response-clears-legacy-entry ()
  (let ((agent-shell-attention--pending (make-hash-table :test #'eq))
        (buffer (generate-new-buffer " *asa-event-permission-legacy*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (agent-shell-mode)
            (setq agent-shell--state (make-hash-table :test #'eq)))
          (puthash buffer (cons "Permission: old" 1.0)
                   agent-shell-attention--pending)
          (agent-shell-attention--handle-event
           buffer
           '((:event . permission-response)
             (:data . ((:request-id . "req-1")
                       (:tool-call-id . "tool-1")))))
          (should-not (gethash buffer agent-shell-attention--pending)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest agent-shell-attention--handle-event-turn-complete-clears-busy ()
  (let ((buffer (generate-new-buffer " *asa-event-turn*"))
        (cleared nil)
        (handled nil))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (agent-shell-mode))
          (cl-letf (((symbol-function 'agent-shell-attention--clear-busy)
                     (lambda (buf)
                       (setq cleared buf)))
                    ((symbol-function 'agent-shell-attention--handle-success)
                     (lambda (buf response)
                       (setq handled (list buf response)))))
            (agent-shell-attention--handle-event
             buffer
             '((:event . turn-complete)
               (:data . ((:stop-reason . "end_turn"))))
            )
            (should (eq cleared buffer))
            (should (equal handled
                           (list buffer '((:stop-reason . "end_turn")))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest agent-shell-attention--subscribe-buffer-registers-once ()
  (let ((buffer (generate-new-buffer " *asa-subscribe*"))
        (agent-shell-attention--subscriptions (make-hash-table :test #'eq))
        (calls 0))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (agent-shell-mode))
          (cl-letf (((symbol-function 'agent-shell-subscribe-to)
                     (lambda (&rest args)
                       (setq calls (1+ calls))
                       (should (eq (plist-get args :shell-buffer) buffer))
                       'token-1)))
            (agent-shell-attention--subscribe-buffer buffer)
            (agent-shell-attention--subscribe-buffer buffer)
            (should (= calls 1))
            (should (eq (gethash buffer agent-shell-attention--subscriptions)
                        'token-1))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest agent-shell-attention--dashboard-buffer-name-right-truncates ()
  (let ((agent-shell-attention-dashboard-buffer-column-width 12))
    (should (equal (agent-shell-attention--dashboard-buffer-name "short")
                   "short"))
    (let ((name (agent-shell-attention--dashboard-buffer-name
                 "Codex Agent @ emacsd")))
      (should (string-prefix-p "…" name))
      (should (string-suffix-p "emacsd" name))
      (should (<= (length name) 12)))))

(ert-deftest agent-shell-attention--dashboard-timestamp-format-customizable ()
  (let* ((ts 1234567890.0)
         (agent-shell-attention-dashboard-time-format "%Y-%m-%d")
         (expected (format-time-string "%Y-%m-%d" (seconds-to-time ts))))
    (should (equal (agent-shell-attention--format-timestamp ts) expected))
    (should (equal (agent-shell-attention--format-timestamp nil) "-"))))

(ert-deftest agent-shell-attention--dashboard-entry-time-cell ()
  (let* ((record (list :buffer (current-buffer)
                       :name "buf"
                       :activity-time 10.0
                       :status-text "Idle"))
         (entries (agent-shell-attention--dashboard-entries (list record)))
         (cols (cadr (car entries)))
         (timestamp (aref cols 1)))
    (should (stringp timestamp))
    (should-not (get-text-property 0 'help-echo timestamp))))

(ert-deftest agent-shell-attention-dashboard-mode-buffer-column-customized ()
  (let ((agent-shell-attention-dashboard-buffer-column-width 24))
    (with-temp-buffer
      (agent-shell-attention-dashboard-mode)
      (let ((column (aref tabulated-list-format 0)))
        (should (equal (nth 1 column) 24))
        (should (plist-get (nthcdr 3 column) :right-align))))))

(ert-deftest agent-shell-attention-dashboard-mode-map-includes-session-actions ()
  (should (eq (lookup-key agent-shell-attention-dashboard-mode-map (kbd "D"))
              #'agent-shell-attention-dashboard-kill-session))
  (should (eq (lookup-key agent-shell-attention-dashboard-mode-map (kbd "o"))
              #'agent-shell-attention-dashboard-visit-other-window))
  (should (eq (lookup-key agent-shell-attention-dashboard-mode-map (kbd "j"))
              #'agent-shell-attention-dashboard-open-ambient-directory)))

(ert-deftest agent-shell-attention-dashboard-visit-other-window-clears-pending ()
  (let ((buffer (generate-new-buffer " *asa-dashboard-other-window*"))
        (cleared nil)
        (visited nil))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (agent-shell-mode))
          (cl-letf (((symbol-function 'agent-shell-attention-dashboard--selected-live-buffer)
                     (lambda () buffer))
                    ((symbol-function 'agent-shell-attention--permission-pending-p)
                     (lambda (_buffer) nil))
                    ((symbol-function 'agent-shell-attention--clear-buffer)
                     (lambda (buf)
                       (setq cleared buf)))
                    ((symbol-function 'switch-to-buffer-other-window)
                     (lambda (buf &optional _norecord)
                       (setq visited buf))))
            (agent-shell-attention-dashboard-visit-other-window)
            (should (eq visited buffer))
            (should (eq cleared buffer))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest agent-shell-attention-dashboard-visit-other-window-keeps-row ()
  (let* ((agent-shell-attention--pending (make-hash-table :test #'eq))
         (agent-shell-attention--busy (make-hash-table :test #'eq))
         (agent-shell-attention--busy-since (make-hash-table :test #'eq))
         (agent-shell-attention--last-event (make-hash-table :test #'eq))
         (agent-shell-attention-dashboard-buffer-name
          " *asa-dashboard-visit-other-window-dashboard*")
         (first (generate-new-buffer " *asa-dashboard-visit-a*"))
         (second (generate-new-buffer " *asa-dashboard-visit-b*"))
         (dashboard (get-buffer-create agent-shell-attention-dashboard-buffer-name))
         (visited nil))
    (unwind-protect
        (progn
          (dolist (buffer (list first second))
            (with-current-buffer buffer
              (agent-shell-mode)))
          (puthash first (cons "Finished" 20.0)
                   agent-shell-attention--pending)
          (puthash first (list :status 'pending
                               :summary "Finished"
                               :timestamp 20.0)
                   agent-shell-attention--last-event)
          (puthash second (cons "Finished" 10.0)
                   agent-shell-attention--pending)
          (puthash second (list :status 'pending
                                :summary "Finished"
                                :timestamp 10.0)
                   agent-shell-attention--last-event)
          (with-current-buffer dashboard
            (agent-shell-attention-dashboard-mode)
            (agent-shell-attention-dashboard-refresh)
            (goto-char (point-min))
            (should (eq (tabulated-list-get-id) first))
            (cl-letf (((symbol-function 'switch-to-buffer-other-window)
                       (lambda (buffer &optional _norecord)
                         (setq visited buffer))))
              (agent-shell-attention-dashboard-visit-other-window))
            (should (eq visited first))
            (should (eq (tabulated-list-get-id) second))))
      (dolist (buffer (list first second dashboard))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest agent-shell-attention-dashboard-open-ambient-directory-uses-buffer-directory ()
  (let ((buffer (generate-new-buffer " *asa-dashboard-ambient*"))
        (opened-dir nil))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (agent-shell-mode)
            (setq default-directory "/tmp/"))
          (cl-letf (((symbol-function 'agent-shell-attention-dashboard--selected-live-buffer)
                     (lambda () buffer))
                    ((symbol-function 'dired)
                     (lambda (directory &optional _switches)
                       (setq opened-dir directory))))
            (agent-shell-attention-dashboard-open-ambient-directory)
            (should (equal opened-dir "/tmp/"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest agent-shell-attention-dashboard-refresh-keeps-nearby-row-after-delete ()
  (let* ((agent-shell-attention--pending (make-hash-table :test #'eq))
         (agent-shell-attention--busy (make-hash-table :test #'eq))
         (agent-shell-attention--busy-since (make-hash-table :test #'eq))
         (agent-shell-attention--last-event (make-hash-table :test #'eq))
         (first (generate-new-buffer " *asa-dashboard-row-a*"))
         (second (generate-new-buffer " *asa-dashboard-row-b*"))
         (third (generate-new-buffer " *asa-dashboard-row-c*")))
    (unwind-protect
        (progn
          (dolist (buffer (list first second third))
            (with-current-buffer buffer
              (agent-shell-mode)))
          (puthash first (cons "Finished" 30.0)
                   agent-shell-attention--pending)
          (puthash second (cons "Finished" 20.0)
                   agent-shell-attention--pending)
          (puthash third (cons "Finished" 10.0)
                   agent-shell-attention--pending)
          (with-temp-buffer
            (agent-shell-attention-dashboard-mode)
            (agent-shell-attention-dashboard-refresh)
            (should (= (length tabulated-list-entries) 3))
            (goto-char (point-min))
            (forward-line 1)
            (should (eq (tabulated-list-get-id) second))
            (kill-buffer second)
            (agent-shell-attention-dashboard-refresh)
            (should (= (length tabulated-list-entries) 2))
            (should (eq (tabulated-list-get-id) third))))
      (dolist (buffer (list first second third))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest agent-shell-attention-dashboard-kill-session-kills-selected-buffer ()
  (let ((buffer (generate-new-buffer " *asa-dashboard-kill*"))
        (refreshed nil))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (agent-shell-mode))
          (cl-letf (((symbol-function 'agent-shell-attention-dashboard--selected-live-buffer)
                     (lambda () buffer))
                    ((symbol-function 'yes-or-no-p)
                     (lambda (_prompt) t))
                    ((symbol-function 'agent-shell-attention-dashboard-refresh)
                     (lambda ()
                       (setq refreshed t))))
            (agent-shell-attention-dashboard-kill-session)
            (should-not (buffer-live-p buffer))
            (should refreshed)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest agent-shell-attention--on-buffer-killed-refreshes-dashboard-after-kill ()
  (let* ((agent-shell-attention--pending (make-hash-table :test #'eq))
         (agent-shell-attention--busy (make-hash-table :test #'eq))
         (agent-shell-attention--busy-since (make-hash-table :test #'eq))
         (agent-shell-attention--last-event (make-hash-table :test #'eq))
         (agent-shell-attention--subscriptions (make-hash-table :test #'eq))
         (agent-shell-attention-dashboard-buffer-name
          " *asa-dashboard-after-kill-dashboard*")
         (killed (generate-new-buffer " *asa-dashboard-after-kill-a*"))
         (remaining (generate-new-buffer " *asa-dashboard-after-kill-b*"))
         (dashboard (get-buffer-create agent-shell-attention-dashboard-buffer-name)))
    (unwind-protect
        (progn
          (dolist (buffer (list killed remaining))
            (with-current-buffer buffer
              (agent-shell-mode)))
          (puthash killed (cons "Finished" 20.0)
                   agent-shell-attention--pending)
          (puthash killed (list :status 'pending
                                :summary "Finished"
                                :timestamp 20.0)
                   agent-shell-attention--last-event)
          (puthash remaining (cons "Finished" 10.0)
                   agent-shell-attention--pending)
          (puthash remaining (list :status 'pending
                                   :summary "Finished"
                                   :timestamp 10.0)
                   agent-shell-attention--last-event)
          (with-current-buffer killed
            (add-hook 'kill-buffer-hook
                      #'agent-shell-attention--on-buffer-killed nil t))
          (with-current-buffer dashboard
            (agent-shell-attention-dashboard-mode)
            (agent-shell-attention-dashboard-refresh)
            (goto-char (point-min))
            (should (eq (tabulated-list-get-id) killed)))
          (kill-buffer killed)
          (sit-for 0.01)
          (with-current-buffer dashboard
            (should-not (assoc killed tabulated-list-entries))
            (should (eq (tabulated-list-get-id) remaining))))
      (dolist (buffer (list killed remaining dashboard))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest agent-shell-attention--on-buffer-killed-preserves-dashboard-window-point ()
  (let* ((agent-shell-attention--pending (make-hash-table :test #'eq))
         (agent-shell-attention--busy (make-hash-table :test #'eq))
         (agent-shell-attention--busy-since (make-hash-table :test #'eq))
         (agent-shell-attention--last-event (make-hash-table :test #'eq))
         (agent-shell-attention--subscriptions (make-hash-table :test #'eq))
         (agent-shell-attention-dashboard-buffer-name
          " *asa-dashboard-window-point-dashboard*")
         (first (generate-new-buffer " *asa-dashboard-window-point-a*"))
         (killed (generate-new-buffer " *asa-dashboard-window-point-b*"))
         (third (generate-new-buffer " *asa-dashboard-window-point-c*"))
         (dashboard (get-buffer-create agent-shell-attention-dashboard-buffer-name)))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (dolist (buffer (list first killed third))
            (with-current-buffer buffer
              (agent-shell-mode)))
          (puthash first (cons "Finished" 30.0)
                   agent-shell-attention--pending)
          (puthash killed (cons "Finished" 20.0)
                   agent-shell-attention--pending)
          (puthash third (cons "Finished" 10.0)
                   agent-shell-attention--pending)
          (with-current-buffer killed
            (add-hook 'kill-buffer-hook
                      #'agent-shell-attention--on-buffer-killed nil t))
          (switch-to-buffer dashboard)
          (agent-shell-attention-dashboard-mode)
          (agent-shell-attention-dashboard-refresh)
          (let* ((dashboard-window (selected-window))
                 (agent-window (split-window-right)))
            (goto-char (point-min))
            (forward-line 1)
            (should (eq (tabulated-list-get-id) killed))
            (set-window-point dashboard-window (point))
            (select-window agent-window)
            (switch-to-buffer killed)
            (with-current-buffer dashboard
              (goto-char (point-min)))
            (kill-buffer killed)
            (sit-for 0.01)
            (with-current-buffer dashboard
              (should-not (assoc killed tabulated-list-entries))
              (should (= (length tabulated-list-entries) 2))
              (save-excursion
                (goto-char (window-point dashboard-window))
                (should (eq (tabulated-list-get-id) third))))))
      (dolist (buffer (list first killed third dashboard))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest agent-shell-attention--state-change-hooks-refresh-dashboard ()
  (let* ((agent-shell-attention--pending (make-hash-table :test #'eq))
         (agent-shell-attention--busy (make-hash-table :test #'eq))
         (agent-shell-attention--busy-since (make-hash-table :test #'eq))
         (agent-shell-attention--last-event (make-hash-table :test #'eq))
         (buffer (generate-new-buffer " *asa-refresh*"))
         (refreshes 0))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (agent-shell-mode))
          (cl-letf (((symbol-function 'agent-shell-attention--maybe-refresh-dashboard)
                     (lambda ()
                       (setq refreshes (1+ refreshes)))))
            (agent-shell-attention--mark-busy buffer)
            (agent-shell-attention--clear-busy buffer)
            (agent-shell-attention--mark-buffer buffer "Permission: foo" :force t)
            (agent-shell-attention--clear-buffer buffer))
          (should (= refreshes 4)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest agent-shell-attention--dashboard-running-overrides-stale-permission-summary ()
  (let* ((agent-shell-attention--pending (make-hash-table :test #'eq))
         (agent-shell-attention--busy (make-hash-table :test #'eq))
         (agent-shell-attention--busy-since (make-hash-table :test #'eq))
         (agent-shell-attention--last-event (make-hash-table :test #'eq))
         (busy-buf (generate-new-buffer " *asa-dashboard-running*")))
    (unwind-protect
        (progn
          (with-current-buffer busy-buf
            (agent-shell-mode)
            (setq agent-shell--state (list (cons :active-requests '(req)))))
          (puthash busy-buf 20.0 agent-shell-attention--busy-since)
          (puthash busy-buf
                   (list :status 'pending
                         :summary "Permission: Run touch /tmp/probe (execute)"
                         :timestamp 10.0)
                   agent-shell-attention--last-event)
          (let ((record (car (agent-shell-attention--dashboard-records))))
            (should (eq (plist-get record :status) 'busy))
            (should (equal (plist-get record :status-text) "Running"))))
      (when (buffer-live-p busy-buf)
        (kill-buffer busy-buf)))))

(provide 'agent-shell-attention-test)

;;; agent-shell-attention-test.el ends here
