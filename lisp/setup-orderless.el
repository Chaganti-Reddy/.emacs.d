;;; setup-orderless.el --- Smart completion matching with Orderless -*- lexical-binding: t; -*-

;;; Commentary:
;; This file configures Orderless for smarter, flexible, and fast completion matching.

;;; Code:

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(basic substring initials flex orderless))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles . (basic partial-completion orderless)))
     (bookmark (styles . (basic substring)))
     (library (styles . (basic substring)))
     (embark-keybinding (styles . (basic substring)))
     (imenu (styles . (basic substring orderless)))
     (consult-location (styles . (basic substring orderless)))
     (kill-ring (styles . (orderless)))
     (eglot (styles . (orderless flex)))))
  (completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (setq-default case-fold-search t)) ;; Case-insensitive searches

;; ----------------------------
;; CAPF AUTOSUGGEST: Inline Suggestions
;; ----------------------------
(use-package capf-autosuggest
  :disabled
  :ensure t
  :defer
  :hook (eshell-mode . capf-autosuggest-mode)
  :custom (capf-autosuggest-dwim-next-line nil))

(provide 'setup-orderless)
;;; setup-orderless.el ends here
