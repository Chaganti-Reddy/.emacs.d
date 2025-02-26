;;; editor.el --- Editor Settings -*- lexical-binding: t; -*-

;;; Commentary:
;; Editor settings such as formatting, indentations, undo managements etc..,

;;; Code:

;; ----------------------------------------------------------------------------
;; 📌 Code Formatting
;; ----------------------------------------------------------------------------
(use-package format-all
  :ensure t
  :defer t
  :preface
  (defun karna/format-code ()
    "Auto-format the current buffer intelligently.
Uses:
  - `prolog-indent-buffer' for `prolog-mode'
  - `eglot-format-buffer' if LSP supports document formatting
  - `format-all-buffer' otherwise"
    (interactive)
    (cond
     ((derived-mode-p 'prolog-mode) (prolog-indent-buffer))
     ((and (bound-and-true-p eglot-managed-mode)
           (eglot--server-capable :documentFormattingProvider))
      (eglot-format-buffer))
     ((fboundp 'format-all-buffer) (format-all-buffer))
     (t (message "No formatter available for %s" major-mode))))
  :hook (prog-mode . format-all-ensure-formatter))


;; ----------------------------------------------------------------------------
;; 🔄 Vim-like Undo Management (`undo-fu` + `undo-fu-session`)
;; ----------------------------------------------------------------------------

(use-package undo-fu
  :ensure t
  :defer t
  :config
  (setq undo-fu-allow-undo-in-region t  ;; Allow undo in active region
        undo-fu-ignore-keyboard-quit t) ;; Prevent undo reset on `C-g`
  
  ;; Define simple undo/redo functions
  (defun karna/undo ()
    "Perform an undo using `undo-fu`."
    (interactive)
    (undo-fu-only-undo))
  
  (defun karna/redo ()
    "Perform a redo using `undo-fu`."
    (interactive)
    (undo-fu-only-redo)))

(use-package undo-fu-session
  :ensure t
  :defer t
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")
        undo-fu-session-directory (karna/dir-concat user-cache-directory "undo-fu-session/")) ;; Store undo history in cache
  :hook ((prog-mode conf-mode text-mode tex-mode) . undo-fu-session-mode))

;; ----------------------------------------------------------------------------
;; 📌 BREADCRUMB NAVIGATION FOR EMACS
;; ----------------------------------------------------------------------------

(use-package breadcrumb
  :ensure t
  :defer t
  :init
  (breadcrumb-mode 1)  ;; Enable breadcrumb globally.
  :config
  (setq breadcrumb-imenu-max-length 30
	breadcrumb-project-max-length 30
	breadcrumb-imenu-crumb-separator " » "
	breadcrumb-project-crumb-separator " / "
	header-line-format
	'((:eval (concat (breadcrumb-project-crumbs) "  " (breadcrumb-imenu-crumbs))))))

;; ----------------------------------------------------------------------------
;; 📌 RAINBOW MODE (Color Highlighting)
;; ----------------------------------------------------------------------------

(use-package rainbow-mode
  :ensure t
  :defer t
  :hook ((org-mode prog-mode) . rainbow-mode))

;; ----------------------------------------------------------------------------
;; 📌 RAINBOW DELIMITERS (For Parenthesis Highlighting)
;; ----------------------------------------------------------------------------

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

;; ----------------------------------------------------------------------------
;; DRAG STUFF (Move Text with Ease)
;; ----------------------------------------------------------------------------

(use-package drag-stuff
  :ensure t
  :defer t
  :init
  (drag-stuff-global-mode 1)
  :config
  (drag-stuff-define-keys))

;; ----------------------------------------------------------------------------
;; 📌 PUNI (Smart Structural Editing)
;; ----------------------------------------------------------------------------

(use-package puni
  :ensure t
  :defer t
  :init (puni-global-mode))

;; ----------------------------------------------------------------------------
;; 📌 EXPAND REGION (Quick Selection Expansion)
;; ----------------------------------------------------------------------------

(use-package expand-region
  :ensure t
  :defer t
  :bind ("C-=" . er/expand-region))

;; ----------------------------------------------------------------------------
;; 📌 WS-BUTLER (Remove Trailing Whitespace)
;; ----------------------------------------------------------------------------

(use-package ws-butler
  :ensure t
  :defer t
  :init (ws-butler-global-mode))

;; ----------------------------------------------------------------------------
;; HIGHLIGHT INDENTATION GUIDES
;; ----------------------------------------------------------------------------

(use-package highlight-indent-guides
  :ensure t
  :defer t
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character
	highlight-indent-guides-character ?\|
	highlight-indent-guides-responsive 'top
	highlight-indent-guides-auto-enabled nil) ;; Disable auto coloring

  ;; Adjust colors based on theme background
  (defun karna/highlight-indent-guides-set-colors ()
    "Set colors for `highlight-indent-guides` based on the current theme."
    (let ((dark-mode (eq (frame-parameter nil 'background-mode) 'dark)))
      (set-face-foreground 'highlight-indent-guides-character-face (if dark-mode "gray40" "gray40"))
      (set-face-foreground 'highlight-indent-guides-top-character-face (if dark-mode "white" "black"))
      (set-face-foreground 'highlight-indent-guides-stack-character-face "gray60")))

  (add-hook 'after-load-theme-hook #'karna/highlight-indent-guides-set-colors)
  (karna/highlight-indent-guides-set-colors)) ;; Apply colors immediately

;; ----------------------------------------------------------------------------
;; FLYCHECK FOR EMACS
;; ----------------------------------------------------------------------------

(use-package flycheck
  :ensure t
  :defer t
  :init
  (global-flycheck-mode)
  :config
  ;; Adjust when Flycheck runs syntax checks.
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)
	;; Increase the error threshold to avoid disabling checkers on too many errors.
	flycheck-checker-error-threshold 1000))

;; ----------------------------------------------------------------------------
;; SIDELINE FLYMAKE
;; ----------------------------------------------------------------------------

(use-package sideline-flymake
  :ensure t
  :defer t
  :hook (flymake-mode . sideline-mode)
  :custom
  (sideline-flymake-display-mode 'line) ;; Show errors on the current line
  (sideline-backends-right '(sideline-flymake)))

;; ----------------------------------------------------------------------------
;; WAKATIME MODE
;; ----------------------------------------------------------------------------

(use-package wakatime-mode
  :ensure t
  :config
  (when (executable-find "wakatime-cli")
    (global-wakatime-mode)))


(provide 'editor)
;;; editor.el ends here
