;;; packages/corfu.el --- Corfu Package -*- lexical-binding: t; -*-

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :custom
  (corfu-cycle t)                  ;; Cycle through candidates
  (corfu-auto t)                   ;; Auto-show completions
  (corfu-auto-prefix 1)            ;; Show completions after typing 1 character
  (corfu-auto-delay 0.0)           ;; Instant completions
  (corfu-quit-no-match 'separator) ;; Quit when no match
  (corfu-echo-documentation t)     ;; Show docs in echo area
  (corfu-preview-current nil)      ;; No inline preview
  :config
  (corfu-popupinfo-mode 1)         ;; Show docs like VSCode
  :hook
  ;; In eshell, disable auto-completion but keep the quit settings.
  (eshell-mode . (lambda ()
		   (setq-local corfu-quit-at-boundary t
			       corfu-quit-no-match t
			       corfu-auto nil)))
  ;; Customize completion styles for Corfu.
  (corfu-mode . (lambda ()
		  (setq-local completion-styles '(basic)
			      completion-category-overrides nil
			      completion-category-defaults nil)))
  :bind (:map corfu-map
	      ("TAB" . corfu-next)
	      ("S-TAB" . corfu-previous)
	      ("C-h" . corfu-popupinfo-toggle) ;; Show docs
	      ("M-SPC" . corfu-insert-separator)))

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  ;; Add the Nerd Icons Corfu formatter to Corfu's margin formatters.
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package eldoc
  :ensure nil
  :custom
  (eldoc-echo-area-use-multiline-p t) ;; Show full docs in echo area
  (eldoc-documentation-strategy 'eldoc-documentation-compose))

 (use-package eldoc-box
  :ensure t
  :hook (eglot-managed-mode . eldoc-box-hover-mode) ;; Enable it for Eglot
  :bind (:map eglot-mode-map
	      ("C-c d" . eldoc-box-help-at-point))) ;; Manually trigger it

(add-hook 'eldoc-box-buffer-setup-hook #'eldoc-box-prettify-ts-errors 0 t)


(provide 'packages/corfu)
;; packages/corfu.el ends here
