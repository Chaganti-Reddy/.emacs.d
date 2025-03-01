;;; setup-eglot.el --- Minimal and Efficient Eglot Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This file sets up Eglot, the built-in LSP client in Emacs, with performance optimizations
;; and useful keybindings.

;;; Code:

(use-package eglot
  :ensure nil
  :defer
  :init
  (fset #'jsonrpc--log-event #'ignore)
  :custom
  (eglot-autoshutdown t)                      ;; Auto-shutdown LSP server when no managed buffers exist
  (eglot-sync-connect 1)                      ;; Make connection synchronous for immediate availability
  (eglot-events-buffer-size 0)                ;; Disable event logging (for performance)
  (eldoc-echo-area-use-multiline-p nil)       ;; Prevent multiline messages in minibuffer
  :bind (:map eglot-mode-map
	      ("C-c a" . eglot-code-actions)           ;; Show code actions
	      ("C-c f" . eglot-format-buffer)          ;; Format buffer
	      ("C-c r" . eglot-rename)                 ;; Rename symbol
	      ("C-c i" . consult-imenu)                ;; Jump to function/class (via consult-imenu)
	      ("C-c o" . eglot-code-action-organize-imports)) ;; Organize imports
  :hook ((python-ts-mode . eglot-ensure)
	 (go-ts-mode . eglot-ensure)
	 (yaml-mode . eglot-ensure)
	 (dockerfile-mode . eglot-ensure)
	 (web-mode . eglot-ensure)
	 (css-mode . eglot-ensure)
	 (html-mode . eglot-ensure)
	 (typescript-ts-mode . eglot-ensure)
	 (javascript-ts-mode . eglot-ensure)
	 (json-ts-mode . eglot-ensure)
	 (eglot-managed-mode . karna/eglot-setup)) ;; Custom setup function
  :config
  ;; Associate modes with their respective language servers
  (setq eglot-server-programs
	`((python-ts-mode   . ("pyright-langserver" "--stdio"))
	  (go-ts-mode       . ("gopls"))
	  (yaml-mode        . ("yaml-language-server" "--stdio"))
	  (dockerfile-mode  . ("docker-langserver" "--stdio"))
	  (web-mode         . ("vscode-html-language-server" "--stdio"))
	  (html-mode        . ("vscode-html-language-server" "--stdio"))
	  (css-mode         . ("vscode-css-language-server" "--stdio"))
	  (javascript-ts-mode . ("typescript-language-server" "--stdio"))
	  (typescript-ts-mode . ("typescript-language-server" "--stdio"))
	  (json-ts-mode     . ("vscode-json-language-server" "--stdio"))))

  ;; Custom setup for Eglot
  (defun karna/eglot-setup ()
    "Custom settings for `eglot-managed-mode`."
    (setq-local completion-category-defaults nil) ;; Avoid interference with custom completion
    (setq-local completion-category-overrides '((eglot (orderless styles)))) ;; Use `orderless` for better completion
    (when (featurep 'eldoc-box)
      (add-hook 'eldoc-mode-hook #'eldoc-box-hover-mode nil t)))) ;; Enable `eldoc-box-hover-mode` for in-place docs

;; ----------------------------
;; ELDOC: Documentation Display
;; ----------------------------
(use-package eldoc
  :defer
  :ensure nil
  :custom
  (eldoc-echo-area-use-multiline-p t) ;; Show full documentation in echo area
  (eldoc-documentation-strategy 'eldoc-documentation-compose))

(use-package eldoc-box
  :defer
  :ensure t
  :hook (eglot-managed-mode . eldoc-box-hover-mode)
  :bind (:map eglot-mode-map
	      ("C-c d" . eldoc-box-help-at-point)))

;; Prettify tree-sitter errors in Eldoc
(add-hook 'eldoc-box-buffer-setup-hook #'eldoc-box-prettify-ts-errors)

;; --------------------------------------------------------------
;; 🚀 Emmet for Fast HTML/CSS Writing
;; --------------------------------------------------------------
(use-package emmet-mode
  :hook ((web-mode . emmet-mode)
	 (css-mode . emmet-mode)
	 (html-mode . emmet-mode))
  :custom
  (emmet-expand-jsx-className? t))

;; --------------------------------------------------------------
;; 🔥 Web-Mode for JSX, HTML, Vue, etc.
;; --------------------------------------------------------------
(use-package web-mode
  :mode ("\\.html\\'" "\\.css\\'" "\\.js\\'" "\\.jsx\\'" "\\.ts\\'" "\\.tsx\\'" "\\.vue\\'")
  :custom
  (web-mode-enable-auto-quoting nil)
  (web-mode-enable-current-element-highlight t)
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-style-padding 2)
  (web-mode-script-padding 2)
  (web-mode-enable-auto-opening t)
  (web-mode-enable-auto-pairing t)
  (web-mode-enable-auto-indentation t)
  (web-mode-enable-auto-closing t))

;; --------------------------------------------------------------
;; 🐍 Python Development (Linter + Formatter)
;; --------------------------------------------------------------


(add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))

(use-package python-black
  :after python
  :hook (python-ts-mode . python-black-on-save-mode))

;; --------------------------------------------------------------
;; 🏗️ Docker & Kubernetes Support
;; --------------------------------------------------------------
(use-package dockerfile-mode
  :mode "Dockerfile\\'")

(use-package k8s-mode
  :hook (k8s-mode . eglot-ensure))

;; --------------------------------------------------------------
;; 🚀 Golang Setup
;; --------------------------------------------------------------
(use-package go-mode
  :mode "\\.go\\'"
  :hook ((go-mode . eglot-ensure)
	 (before-save . gofmt-before-save)))

;; --------------------------------------------------------------
;; 📜 JSON & YAML Formatting
;; --------------------------------------------------------------
(use-package json-mode
  :mode "\\.json\\'")

(use-package yaml-mode
  :mode ("\\.yml\\'" "\\.yaml\\'"))

;; --------------------------------------------------------------
;; 🎵 MATLAB/Octave Support
;; --------------------------------------------------------------

(setq auto-mode-alist
      (append '(("\\.m\\'" . octave-mode)) auto-mode-alist))

(setq inferior-octave-program "/usr/bin/octave"
      inferior-octave-startup-args '("-f"))

(add-hook 'octave-mode-hook
	  (lambda ()
	    (setq-local octave-comment-char ?%)
	    (abbrev-mode 1)
	    (auto-fill-mode 1)
	    (electric-indent-mode 1)
	    (setq-local indent-tabs-mode nil)
	    (setq-local octave-block-offset 4)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((octave . t)))

;; --------------------------------------------------------------
;; 🎛️ Toggle Octave REPL in a Right Split
;; --------------------------------------------------------------
(defun karna/open-octave-right-side ()
  "Toggle an Octave REPL in a vertical split on the right side."
  (interactive)
  (let ((octave-buffer (get-buffer "*Inferior Octave*"))
	(octave-window (get-buffer-window "*Inferior Octave*")))
    (if octave-buffer
	(if octave-window
	    (delete-window octave-window)
	  (progn
	    (split-window-right)
	    (other-window 1)
	    (switch-to-buffer octave-buffer)
	    (other-window 1)))
      (progn
	(split-window-right)
	(other-window 1)
	(run-octave)
	(switch-to-buffer "*Inferior Octave*")
	(other-window 1)))))

;; --------------------------------------------------------------
;; 🐍 Conda Virtual Environment Management
;; --------------------------------------------------------------
(use-package conda
  :ensure t
  :defer t
  :init
  (setq conda-anaconda-home (expand-file-name "~/miniconda"))
  (setq conda-env-home-directory (expand-file-name "~/miniconda"))
  :config
  (conda-env-initialize-interactive-shells)
  (conda-env-initialize-eshell)
  (setq conda-env-autoactivate-mode nil)  ;; Disable auto-activation globally.
  (conda-mode-line-setup)                 ;; Update modeline when Conda env changes.

  ;; Hook for restarting Python shell when activating a Conda env.
  (add-hook 'conda-postactivate-hook #'karna/restart-python-shell-with-conda))
(with-eval-after-load 'conda
  (conda-mode-line-setup))

;; Manually add conda to PATH if needed
(add-to-list 'exec-path (expand-file-name "~/miniconda/bin")) ; Update path
(setenv "PATH" (concat (expand-file-name "~/miniconda/bin:") (getenv "PATH")))

(defun mood-line-segment-python-env ()
  "Display the current Python virtual environment or Conda environment in the modeline."
  (when (derived-mode-p 'python-mode)
    (cond
     ((bound-and-true-p conda-env-current-name)
      (format "🐍[%s]" conda-env-current-name))
     ((bound-and-true-p pyvenv-virtual-env-name)
      (format "🐍[%s]" pyvenv-virtual-env-name))
     (t ""))))

(defun karna/restart-python-shell-with-conda ()
  "Restart Python shell using the currently activated Conda environment."
  (interactive)
  (when (bound-and-true-p conda-env-current-name)
    (let* ((conda-base-path (or (getenv "CONDA_PREFIX") "~/miniconda"))
	   (env-path (if (string= conda-env-current-name "base")
			 conda-base-path
		       (concat conda-base-path "/envs/" conda-env-current-name)))
	   (env-bin (concat env-path "/bin/python")))
      (when (get-process "Python") (delete-process "Python"))
      (when (get-buffer "*Python*") (kill-buffer "*Python*"))
      (if (file-executable-p env-bin)
	  (progn
	    (setq-local python-shell-interpreter env-bin)
	    (setq-local python-shell-interpreter-args "-i")
	    (setq-local python-interpreter env-bin)  ;; If using pythonic.el.
	    (run-python (concat env-bin " -i") nil nil)
	    (message "Switched Python shell to Conda environment: %s"
		     conda-env-current-name))
	(message "Error: Could not find Python executable at %s" env-bin)))))

;; --------------------------------------------------------------
;; 🐍 Python Virtual Environment (pyvenv)
;; --------------------------------------------------------------
(use-package pyvenv
  :ensure t
  :defer t
  :config
  (setq pyvenv-mode-line-indicator '(pyvenv-virtual-env-name (" venv:" pyvenv-virtual-env-name " ")))
  (pyvenv-mode t))  ;; Enable pyvenv globally.

(defun karna/pyvenv-autoload ()
  "Auto-activate Python virtual environment if 'env' exists in the project."
  (require 'pyvenv)
  (let* ((proj (project-current))
	 (venv-path (when proj (expand-file-name "env" (project-root proj)))))
    (when (and venv-path (file-exists-p venv-path))
      (pyvenv-activate venv-path))))

(add-hook 'python-ts-mode-hook #'karna/pyvenv-autoload)

;; --------------------------------------------------------------
;; 🐍 Open Python REPL in a Right Split
;; --------------------------------------------------------------
(defun karna/open-python-right-side ()
  "Toggle a Python REPL in a vertical split on the right side."
  (interactive)
  (let ((python-buffer (get-buffer "*Python*"))
	(python-window (get-buffer-window "*Python*")))
    (if python-buffer
	(if python-window
	    (delete-window python-window)
	  (progn
	    (split-window-right)
	    (other-window 1)
	    (switch-to-buffer python-buffer)
	    (other-window 1)))
      (progn
	(split-window-right)
	(other-window 1)
	(run-python)
	(when (get-buffer "*Python*")
	  (switch-to-buffer "*Python*"))
	(other-window 1)))))

(provide 'setup-eglot)
;;; setup-eglot.el ends here
