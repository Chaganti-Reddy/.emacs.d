;;; my-coding.el --- IDE layer: tree-sitter, eglot, diagnostics -*- lexical-binding: t; -*-

;;; ---------------------------------------------------------------------------
;;; Tree-sitter
;;; ---------------------------------------------------------------------------
(setq treesit-font-lock-level 3)
;; Keep compiled grammars under var/ (not the default ~/.emacs.d/tree-sitter).
(defconst my/treesit-dir (my/var "tree-sitter/" t))
(add-to-list 'treesit-extra-load-path my/treesit-dir)

;; Grammar sources for `M-x treesit-install-language-grammar' (compiles from
;; git -- needs a C compiler; easy on Linux/mac, painful on Windows).
(setq treesit-language-source-alist
      '((c          . ("https://github.com/tree-sitter/tree-sitter-c"))
        (cpp        . ("https://github.com/tree-sitter/tree-sitter-cpp"))
        (python     . ("https://github.com/tree-sitter/tree-sitter-python"))
        (rust       . ("https://github.com/tree-sitter/tree-sitter-rust"))
        (java       . ("https://github.com/tree-sitter/tree-sitter-java"))
        (go         . ("https://github.com/tree-sitter/tree-sitter-go"))
        (gomod      . ("https://github.com/camdencheek/tree-sitter-go-mod"))
        (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
        (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
        (tsx        . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
        (json       . ("https://github.com/tree-sitter/tree-sitter-json"))
        (toml       . ("https://github.com/tree-sitter/tree-sitter-toml"))
        (yaml       . ("https://github.com/ikatyang/tree-sitter-yaml"))
        (bash       . ("https://github.com/tree-sitter/tree-sitter-bash"))
        (cmake      . ("https://github.com/uyha/tree-sitter-cmake"))))

;; On Windows, compiling grammars is hard. Pull casouri's pre-built DLLs into
;; the default grammar dir (~/.emacs.d/tree-sitter/) instead. Async; keep working.
(defun my/install-treesit-grammars-windows ()
  "Download pre-built tree-sitter grammar DLLs for Windows (async)."
  (interactive)
  (unless IS-WINDOWS (user-error "Only needed on Windows"))
  (let* ((url "https://github.com/casouri/tree-sitter-module/releases/latest/download/libs-windows-x64.zip")
         (zip (my/var "treesit-dlls.zip"))
         (dir my/treesit-dir)
         (dist (expand-file-name "dist/" dir))
         (ps (concat "$ProgressPreference='SilentlyContinue';"
                     (format "Invoke-WebRequest -Uri '%s' -OutFile '%s';" url zip)
                     (format "Expand-Archive -Path '%s' -DestinationPath '%s' -Force;" zip dir)
                     (format "Move-Item -Path '%s*' -Destination '%s' -Force;" dist dir)
                     (format "Remove-Item -Path '%s' -Recurse -Force;" dist)
                     (format "Remove-Item -Path '%s' -Force" zip))))
    (make-directory dir t)
    (message "Downloading tree-sitter DLLs in the background...")
    (make-process
     :name "treesit-install" :buffer "*treesit-install*"
     :command (list "powershell" "-NoProfile" "-NonInteractive" "-Command" ps)
     :sentinel (lambda (_p e)
                 (when (string-match-p "finished" e)
                   (message "Tree-sitter DLLs installed. Reopen files to use *-ts-mode."))))))

;; Use *-ts-mode only when the grammar is actually available (no errors if not).
(defun my/setup-treesit ()
  "Remap base modes + add auto-mode entries for languages with ready grammars."
  (when (require 'treesit nil t)
    (dolist (s '((c-mode      c-ts-mode      c)
                 (c++-mode    c++-ts-mode    cpp)
                 (python-mode python-ts-mode python)
                 (java-mode   java-ts-mode   java)
                 (js-mode     js-ts-mode     javascript)
                 (sh-mode     bash-ts-mode   bash)))
      (when (treesit-ready-p (nth 2 s) t)
        (add-to-list 'major-mode-remap-alist (cons (nth 0 s) (nth 1 s)))))
    (dolist (s '(("\\.rs\\'"    rust-ts-mode       rust)
                 ("\\.go\\'"    go-ts-mode         go)
                 ("\\.ts\\'"    typescript-ts-mode typescript)
                 ("\\.tsx\\'"   tsx-ts-mode        tsx)
                 ("\\.json\\'"  json-ts-mode       json)
                 ("\\.ya?ml\\'" yaml-ts-mode       yaml)
                 ("\\.toml\\'"  toml-ts-mode       toml)))
      (when (treesit-ready-p (nth 2 s) t)
        (add-to-list 'auto-mode-alist (cons (nth 0 s) (nth 1 s)))))))
(add-hook 'emacs-startup-hook #'my/setup-treesit)

;;; ---------------------------------------------------------------------------
;;; Eglot (built-in LSP client)
;;; ---------------------------------------------------------------------------
(use-package eglot
  :ensure nil                          ; ships with Emacs
  :init
  (setq eglot-autoshutdown t           ; kill server when last buffer closes
        eglot-events-buffer-size 0      ; don't log LSP traffic (memory)
        eglot-report-progress nil       ; no progress spam in echo area
        eglot-sync-connect nil          ; connect async; don't block
        eglot-extend-to-xref t)
  ;; Auto-start the server when opening these languages (if it's installed).
  :hook ((c-ts-mode c++-ts-mode python-ts-mode rust-ts-mode java-ts-mode
          go-ts-mode js-ts-mode typescript-ts-mode tsx-ts-mode) . eglot-ensure)
  :config
  (define-key eglot-mode-map (kbd "<f2>")    #'eglot-rename)
  (define-key eglot-mode-map (kbd "M-RET")   #'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-c e r") #'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c e a") #'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-c e d") #'xref-find-definitions)
  (define-key eglot-mode-map (kbd "C-c e R") #'xref-find-references)
  (define-key eglot-mode-map (kbd "C-c e f") #'eglot-format-buffer)
  (define-key eglot-mode-map (kbd "C-c e h") #'eldoc-doc-buffer)
  (add-hook 'eglot-managed-mode-hook
            (lambda () (when (eglot-managed-p) (eglot-inlay-hints-mode 1))))
  (setf (alist-get '(python-mode python-ts-mode) eglot-server-programs nil nil #'equal)
        (lambda (&rest _)
          (cond ((executable-find "basedpyright-langserver") '("basedpyright-langserver" "--stdio"))
                ((executable-find "pyright-langserver")       '("pyright-langserver" "--stdio"))
                (t '("pylsp"))))))

(setq xref-show-definitions-function #'xref-show-definitions-buffer)
(global-set-key (kbd "M-<f12>") #'xref-find-definitions-other-window)

;;; ---------------------------------------------------------------------------
;;; Eldoc + Flymake (diagnostics)
;;; ---------------------------------------------------------------------------
(setq eldoc-echo-area-use-multiline-p nil
      eldoc-echo-area-display-truncation-message nil
      ;; Show both the symbol's doc/signature AND any flymake diagnostic at point.
      eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly
      ;; fringe is 0, so render diagnostic markers in the margin (else invisible).
      flymake-indicator-type 'margins
      flymake-no-changes-timeout 0.5
      flymake-start-on-save-buffer t)

(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "M-n") #'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") #'flymake-goto-prev-error))
(global-set-key (kbd "C-c x") #'flymake-show-buffer-diagnostics)
(global-set-key (kbd "C-c X") #'flymake-show-project-diagnostics)

;;; ---------------------------------------------------------------------------
;;; Indentation per language (sane defaults; silence python's guess warning)
;;; ---------------------------------------------------------------------------
(setq python-indent-guess-indent-offset-verbose nil
      python-indent-offset 4)
(setq-default c-ts-mode-indent-offset 4
              c-basic-offset 4
              js-indent-level 2
              typescript-ts-mode-indent-offset 2       ; covers tsx-ts-mode too
              json-ts-mode-indent-offset 2
              rust-ts-mode-indent-offset 4
              java-ts-mode-indent-offset 4
              go-ts-mode-indent-offset 4
              css-indent-offset 2)
;; Go is tab-indented by convention (gofmt) -- use real tabs there only.
(add-hook 'go-ts-mode-hook (lambda () (setq indent-tabs-mode t)))

;;; ---------------------------------------------------------------------------
;;; Snippets (yasnippet) — provides `yasnippet-capf' used below
;;; ---------------------------------------------------------------------------
(use-package yasnippet
  :ensure t
  :defer t
  :config
  (let ((dir (expand-file-name "snippets" user-emacs-directory)))
    (make-directory dir t)
    (add-to-list 'yas-snippet-dirs dir)))

(use-package yasnippet-snippets
  :ensure t
  :defer t)

(add-hook 'emacs-startup-hook
          (lambda ()
            (run-with-idle-timer
             1.0 nil
             (lambda ()
               (require 'yasnippet)
               (require 'yasnippet-snippets)
               (yas-global-mode 1)))))

(defvar my/yas-extra-mode-alist
  '((c-ts-mode . c-mode) (c++-ts-mode . c++-mode) (python-ts-mode . python-mode)
    (rust-ts-mode . rust-mode) (java-ts-mode . java-mode) (go-ts-mode . go-mode)
    (js-ts-mode . js-mode) (typescript-ts-mode . typescript-mode)
    (tsx-ts-mode . typescript-mode) (bash-ts-mode . sh-mode)
    (latex-mode . LaTeX-mode) (LaTeX-mode . latex-mode))
  "Major mode -> extra mode whose snippets it should also load.")
(defun my/yas-activate-extra ()
  "Activate the bridged snippet mode for the current major mode."
  (when-let* ((extra (cdr (assq major-mode my/yas-extra-mode-alist))))
    (yas-activate-extra-mode extra)))
(add-hook 'yas-minor-mode-hook #'my/yas-activate-extra)

(global-set-key (kbd "C-c s s") #'yas-insert-snippet)

(use-package yasnippet-capf
  :ensure t
  :after (yasnippet cape))

;;; ---------------------------------------------------------------------------
;;; Cape + completion-at-point chains (one base chain, one LSP override)
;;; ---------------------------------------------------------------------------
(use-package cape :ensure t :defer t)

;; Code buffers WITHOUT an LSP: file paths, snippets, keywords, words.
(defun my/code-capfs ()
  (setq-local completion-at-point-functions
              (list #'cape-file #'yasnippet-capf #'cape-keyword #'cape-dabbrev)))
(add-hook 'prog-mode-hook #'my/code-capfs)

;; Eglot buffers: merge LSP + file + snippets into ONE popup (dabbrev fallback).
(defun my/eglot-capfs ()
  (require 'cape)
  (setq-local completion-at-point-functions
              (list (cape-capf-super #'eglot-completion-at-point
                                     #'cape-file #'yasnippet-capf)
                                     #'cape-dabbrev)))
(add-hook 'eglot-managed-mode-hook #'my/eglot-capfs)


;;; ---------------------------------------------------------------------------
;;; Apheleia — async format-on-save
;;; ---------------------------------------------------------------------------
;; Buffer-local (per code buffer) instead of global -> apheleia loads on the
;; first code file, not at startup.
(use-package apheleia
  :hook (prog-mode . apheleia-mode)
  :bind ("C-c f" . apheleia-format-buffer)
  :config
  (unless (assq 'ruff apheleia-formatters)
    (setf (alist-get 'ruff apheleia-formatters)
          '("ruff" "format" "--stdin-filename" filepath "-")))
  (setf (alist-get 'python-mode apheleia-mode-alist) 'ruff
        (alist-get 'python-ts-mode apheleia-mode-alist) 'ruff))

;;; ---------------------------------------------------------------------------
;;; Dape — Debug Adapter Protocol client
;;; ---------------------------------------------------------------------------
;; Needs a per-language debug adapter installed (codelldb for C/C++/Rust,
;; debugpy for Python, js-debug for node, ...). Keys avoid our compile f-keys.
(defconst my/dape-breakpoints-file (my/var "dape-breakpoints")
  "Where breakpoints persist (kept out of the config root).")
(use-package dape
  :init (setq dape-buffer-window-arrangement 'right 
              dape-inlay-hints t)
  :preface
  (defun my/dape-start-or-continue ()
    "F5: start a debug session, or continue if one is live."
    (interactive)
    (require 'dape)
    (if (ignore-errors (dape--live-connection 'last t))
        (dape-continue)
      (call-interactively #'dape)))
  :bind (("<f5>"     . my/dape-start-or-continue)
         ("S-<f5>"   . dape-quit)
         ("C-S-<f5>" . dape-restart)
         ("<f9>"     . dape-breakpoint-toggle)
         ("S-<f9>"   . dape-breakpoint-remove-all)
         ("<f10>"    . dape-next)
         ("<f11>"    . dape-step-in)
         ("S-<f11>"  . dape-step-out)
         ("<f12>"    . dape-info))
  :config
  (add-hook 'kill-emacs-hook
            (lambda () (dape-breakpoint-save my/dape-breakpoints-file)))
  (dape-breakpoint-load my/dape-breakpoints-file))

(provide 'my-coding)
;;; my-coding.el ends here
