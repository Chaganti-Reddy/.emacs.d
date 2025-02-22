;;; dev.el --- Development Package -*- lexical-binding: t; -*-

(use-package eglot
  :ensure nil
  :custom
  (eglot-autoshutdown t)
  (eglot-sync-connect 1)
  (eglot-events-buffer-size 0)
  (eldoc-echo-area-use-multiline-p nil)
  :bind (:map eglot-mode-map
	      ("C-c a" . eglot-code-actions)
	      ("C-c f" . eglot-format-buffer)
	      ("C-c r" . eglot-rename)
	      ("C-c i" . consult-imenu)
	      ("C-c o" . eglot-code-action-organize-imports))
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
	 (eglot-managed-mode . my/eglot-setup))
  :config
  (dolist (server '((python-ts-mode   . ("pyright-langserver" "--stdio"))
		    (go-ts-mode       . ("gopls"))
		    (yaml-mode        . ("yaml-language-server" "--stdio"))
		    (dockerfile-mode  . ("docker-langserver" "--stdio"))
		    (web-mode         . ("vscode-html-language-server" "--stdio"))
		    (html-mode        . ("vscode-html-language-server" "--stdio"))
		    (css-mode         . ("vscode-css-language-server" "--stdio"))
		    (javascript-ts-mode . ("typescript-language-server" "--stdio"))
		    (typescript-ts-mode . ("typescript-language-server" "--stdio"))
		    (json-ts-mode     . ("vscode-json-language-server" "--stdio"))))
    (add-to-list 'eglot-server-programs server)))

;; --------------------------------------------------------------
;; 🛠️ Custom Eglot Setup (Auto-format & Keybindings)
;; --------------------------------------------------------------
(defun my/eglot-setup ()
  "Custom configuration for eglot-managed buffers."
  (electric-indent-local-mode t)
  (setq-local completion-category-defaults nil)

  (cond
   ;; Python Setup
   ((derived-mode-p 'python-ts-mode)
    (setq-local indent-tabs-mode nil
		python-indent-offset 4
		python-indent-guess-indent-offset nil)
    (local-set-key (kbd "<f6>") #'eglot-format-buffer))

   ;; Golang Setup
   ((derived-mode-p 'go-ts-mode)
    (setq-local tab-width 4
		indent-tabs-mode t)
    (local-set-key (kbd "<f6>") #'eglot-format-buffer))

   ;; YAML (Kubernetes, Helm)
   ((derived-mode-p 'yaml-mode)
    (setq-local yaml-indent-offset 2)
    (local-set-key (kbd "<f6>") #'eglot-format-buffer))

   ;; Web Development
   ((derived-mode-p 'web-mode)
    (setq-local web-mode-markup-indent-offset 2
		web-mode-css-indent-offset 2
		web-mode-code-indent-offset 2)
    (local-set-key (kbd "<f6>") #'eglot-format-buffer))

   ((derived-mode-p 'css-mode)
    (setq-local css-indent-offset 2)
    (local-set-key (kbd "<f6>") #'eglot-format-buffer))

   ((derived-mode-p 'javascript-ts-mode 'typescript-ts-mode)
    (setq-local js-indent-level 2)
    (local-set-key (kbd "<f6>") #'eglot-format-buffer))))

;; --------------------------------------------------------------
;; 🚀 Emmet for Fast HTML/CSS Writing
;; --------------------------------------------------------------
(use-package emmet-mode
  :hook ((web-mode . emmet-mode)
	 (css-mode . emmet-mode)
	 (html-mode . emmet-mode))
  :config
  (setq emmet-expand-jsx-className? t))

;; --------------------------------------------------------------
;; 🔥 Web-Mode for JSX, HTML, Vue, etc.
;; --------------------------------------------------------------
(use-package web-mode
  :mode ("\\.html\\'" "\\.css\\'" "\\.js\\'" "\\.jsx\\'" "\\.ts\\'" "\\.tsx\\'" "\\.vue\\'")
  :config
  (setq web-mode-enable-auto-quoting nil
	web-mode-enable-current-element-highlight t
	web-mode-markup-indent-offset 2
	web-mode-css-indent-offset 2
	web-mode-code-indent-offset 2
	web-mode-style-padding 2
	web-mode-script-padding 2
	web-mode-enable-auto-opening t
	web-mode-enable-auto-pairing t
	web-mode-enable-auto-indentation t
	web-mode-enable-auto-closing t))

;; --------------------------------------------------------------
;; 🐍 Python Development (Linter + Formatter)
;; --------------------------------------------------------------
(use-package python-black
  :demand t
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
  :mode "\\.yml\\'" "\\.yaml\\'")

;; --------------------------------------------------------------
;; MATLAB Support using Octave (for now 😅)
;; --------------------------------------------------------------

;; Use Octave mode for .m files
(setq auto-mode-alist
      (cons '("\\.m\\'" . octave-mode) auto-mode-alist))

;; Set Octave as the default interpreter in Emacs
(setq inferior-octave-startup-args '("-f"))
(setq inferior-octave-program "/usr/bin/octave")

;; Enable syntax highlighting and indentation
(add-hook 'octave-mode-hook
	  (lambda ()
	    (setq octave-comment-char ?%)
	    (abbrev-mode 1)
	    (auto-fill-mode 1)
	    (electric-indent-mode 1)
	    (setq indent-tabs-mode nil)
	    (setq octave-block-offset 4)
	    (if (eq window-system 'x)
		(font-lock-mode 1))))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((octave . t)))

(add-hook 'inferior-octave-mode-hook
	  (lambda ()
	    (turn-on-font-lock)
	    (define-key inferior-octave-mode-map [up]
	      'comint-previous-input)
	    (define-key inferior-octave-mode-map [down]
	      'comint-next-input)))

(with-eval-after-load 'octave
  (define-key octave-mode-map (kbd "C-c C-l") 'octave-send-line)
  (define-key octave-mode-map (kbd "C-c C-r") 'octave-send-region)
  (define-key octave-mode-map (kbd "C-c C-b") 'octave-send-buffer)
  (define-key octave-mode-map (kbd "C-c C-f") 'octave-send-defun)
  (define-key octave-mode-map (kbd "C-c C-c") 'octave-send-block))

;; (use-package matlab-mode
;;   :ensure t
;;   :mode ("\\.m\\'" . matlab-mode)
;;   :config
;;   (setq matlab-indent-function t)  ;; Smart indentation
;;   (setq matlab-show-mlint-warnings t)  ;; Show MLint warnings
;;   (setq matlab-shell-command "matlab")  ;; MATLAB command
;;   (setq matlab-shell-command-switches '("-nodesktop" "-nosplash"))) ;; No GUI

;; (add-hook 'matlab-mode-hook 'semantic-mode)
;; (require 'ob-matlab)
;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((matlab . t)))

;; Custom function to toggle an Octave REPL
(defun open-octave-right-side ()
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
	(when (get-buffer "*Inferior Octave*")
	  (switch-to-buffer "*Inferior Octave*"))
	(other-window 1)))))

;; --------------------------------------------------------------


(provide 'dev)
;; dev.el ends here
