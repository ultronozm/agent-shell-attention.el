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

(provide 'agent-shell-attention-test)

;;; agent-shell-attention-test.el ends here
