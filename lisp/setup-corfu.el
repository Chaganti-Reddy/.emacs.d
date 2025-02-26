;;; setup-corfu.el --- Corfu Completion Setup -*- lexical-binding: t; -*-

;;; Commentary:
;; This file configures Corfu for better completion experience.

;;; Code:

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :custom
  (corfu-cycle t)                  ;; Enable cycling through candidates
  (corfu-auto t)                   ;; Auto-show completions
  (corfu-auto-prefix 3)            ;; Trigger completion after 3 characters
  (corfu-auto-delay 0.0)           ;; No delay in showing completions
  (corfu-quit-no-match 'separator) ;; Quit when no match
  (corfu-preview-current nil)      ;; Disable inline preview
  (corfu-scroll-margin 2)          ;; Start scrolling when 2 candidates remain
  (corfu-count 10)                 ;; Show up to 10 candidates
  (corfu-echo-documentation t)     ;; Show documentation in echo area
  :config
  (corfu-popupinfo-mode 1)         ;; Show documentation in a popup like VSCode
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ("S-TAB" . corfu-previous)
        ("C-h" . corfu-popupinfo-toggle) ;; Toggle popup info
        ("M-SPC" . corfu-insert-separator)))

;; Icons for Corfu completion
(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(provide 'setup-corfu)
;;; setup-corfu.el ends here
