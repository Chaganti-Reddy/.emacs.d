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
;; DRAG STUFF (Move Text with Ease)
;; ----------------------------------------------------------------------------

(use-package drag-stuff
  :ensure t
  :defer t
  :hook ((org-mode . drag-stuff-mode)
	 (LaTeX-mode . drag-stuff-mode)
	 (prog-mode . drag-stuff-mode)
	 (text-mode . drag-stuff-mode))
  :config
  (drag-stuff-define-keys))

;; ----------------------------------------------------------------------------
;; 📌 PUNI (Smart Structural Editing)
;; ----------------------------------------------------------------------------

(use-package puni
  :ensure t
  :defer t
  :hook ((org-mode . puni-mode)
	 (LaTeX-mode . puni-mode)
	 (prog-mode . puni-mode)
	 (text-mode . puni-mode)))

;; ----------------------------------------------------------------------------
;; 📌 EXPAND REGION (Quick Selection Expansion)
;; ----------------------------------------------------------------------------

(use-package expand-region
  :ensure t
  :defer t
  :bind ("C-+" . er/expand-region))

;; ----------------------------------------------------------------------------
;; 📌 WS-BUTLER (Remove Trailing Whitespace)
;; ----------------------------------------------------------------------------

(use-package ws-butler
  :ensure t
  :defer t
  :hook ((org-mode . ws-butler-mode)
	 (LaTeX-mode . ws-butler-mode)
	 (prog-mode . ws-butler-mode)
	 (text-mode . ws-butler-mode)))

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
  :hook ((prog-mode . flycheck-mode)
	 (LaTeX-mode . flycheck-mode))
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
  :hook ((prog-mode . sideline-mode)
	 (LaTeX-mode . sideline-mode))
  :custom
  (sideline-flymake-display-mode 'line) ;; Show errors on the current line
  (sideline-backends-right '(sideline-flymake)))

;; Jinx: Enchanted spell-checking
(use-package jinx
  :ensure t
  :hook (((text-mode org-mode LaTeX-mode prog-mode) . jinx-mode))
  :bind (("C-:" . jinx-correct)
	 ("C-M-$" . jinx-languages))
  :custom
  (jinx-camel-modes '(prog-mode))
  (jinx-delay 0.01))



;; Hl-TODO
(use-package hl-todo
  :ensure t
  :defer
  :hook ((prog-mode org-mode) . hl-todo-mode)
  :bind (:map prog-mode-map
	 ("M-g t" . hl-todo-next)
	 ("M-g T" . hl-todo-previous))
  :config
  (setq hl-todo-highlight-punctuation ":"
	hl-todo-keyword-faces
	'(("TODO" . warning)
	  ("FIXME" . error)
	  ("HACK" . font-lock-constant-face)
	  ("REVIEW" . font-lock-keyword-face)
	  ("NOTE" . success)
	  ("DEPRECATED" . font-lock-doc-face))
	hl-todo-wrap-movement t)

  (defvar-keymap hl-todo-repeat-map
    :repeat t
    "n" #'hl-todo-next
    "p" #'hl-todo-previous))

;; ----------------------------------------------------------------------------
;; BREADCRUMB NAVIGATION FOR EMACS
;; ----------------------------------------------------------------------------


(use-package breadcrumb
  :ensure t
  :config
  (setq breadcrumb-imenu-max-length 30
	breadcrumb-project-max-length 30
	breadcrumb-imenu-crumb-separator " » "
	breadcrumb-project-crumb-separator " / ")
  (breadcrumb-mode 1))  ;; Enable globally



;; Colorize color names and parens in buffers
(use-package rainbow-mode
  :defer t
  :commands rainbow-mode
  :ensure t)

(use-package rainbow-delimiters
  :defer t
  :commands rainbow-delimiters-mode
  :ensure t)

(provide 'editor)
;;; editor.el ends here
