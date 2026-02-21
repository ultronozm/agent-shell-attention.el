;;; agent-shell-attention-test.el --- Tests for agent-shell-attention  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(setq load-prefer-newer t)

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'agent-shell-attention)

;; Minimal stubs so we can create buffers that satisfy `(derived-mode-p 'agent-shell-mode)`.
(define-derived-mode agent-shell-mode fundamental-mode "agent-shell")
(defvar-local agent-shell--state nil)

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
          (with-current-buffer busy-buf (agent-shell-mode))
          (puthash pending-buf (cons "Need reply" 1.0) agent-shell-attention--pending)
          (puthash busy-buf 1 agent-shell-attention--busy)
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
          (with-current-buffer busy-buf (agent-shell-mode))
          ;; Mark the same buffer both pending and busy; it should only appear once.
          (puthash pending-buf (cons "Need reply" 1.0) agent-shell-attention--pending)
          (puthash pending-buf 1 agent-shell-attention--busy)
          (puthash busy-buf 1 agent-shell-attention--busy)
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
          (puthash busy-buf 1 agent-shell-attention--busy)
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

(ert-deftest agent-shell-attention--dashboard-entry-time-help-echo ()
  (let* ((record (list :buffer (current-buffer)
                       :name "buf"
                       :activity-time 10.0
                       :status-text "Idle"))
         (entries (agent-shell-attention--dashboard-entries (list record)))
         (cols (cadr (car entries)))
         (timestamp (aref cols 1)))
    (should (stringp timestamp))
    (should (stringp (get-text-property 0 'help-echo timestamp)))
    (should (string-match-p "Elapsed:" (get-text-property 0 'help-echo timestamp)))))

(ert-deftest agent-shell-attention-dashboard-mode-buffer-column-customized ()
  (let ((agent-shell-attention-dashboard-buffer-column-width 24))
    (with-temp-buffer
      (agent-shell-attention-dashboard-mode)
      (let ((column (aref tabulated-list-format 0)))
        (should (equal (nth 1 column) 24))
        (should (plist-get (nthcdr 3 column) :right-align))))))

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
            (agent-shell-mode))
          (puthash busy-buf 1 agent-shell-attention--busy)
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
