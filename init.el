;;; init.el --- Main configuration -*- lexical-binding: t; -*-
;; Loaded after early-init.el. 

;;; ===========================================================================
;;; 1. Portable paths
;;; ===========================================================================
(defun my/emacs-path (relative)
  "Absolute path for RELATIVE inside `user-emacs-directory'."
  (expand-file-name relative user-emacs-directory))

(defconst my/cache-dir (my/emacs-path "var/")
  "Root for generated state (history, backups, caches).")

(defun my/var (path &optional dir-p)
  "Path under `my/cache-dir'. Ensure it (or its parent) exists.
With DIR-P, PATH itself is the directory."
  (let ((full (expand-file-name path my/cache-dir)))
    (make-directory (if dir-p full (file-name-directory full)) t)
    full))

;;; ===========================================================================
;;; 2. Keep state out of the config root (everything under var/)
;;; ===========================================================================
(setq backup-directory-alist `((".*" . ,(my/var "backup/" t)))
      backup-by-copying t version-control t delete-old-versions t
      kept-new-versions 6 kept-old-versions 2
      auto-save-list-file-prefix (my/var "auto-save/sessions/")
      auto-save-file-name-transforms `((".*" ,(my/var "auto-save/" t) t)))

(when (boundp 'lock-file-name-transforms)
  (setq lock-file-name-transforms `((".*" ,(my/var "lock/" t) t))))

(setq recentf-save-file       (my/var "recentf")
      savehist-file           (my/var "savehist")
      save-place-file         (my/var "saveplace")
      bookmark-default-file   (my/var "bookmark")
      project-list-file       (my/var "projects")
      eshell-directory-name   (my/var "eshell/" t)
      tramp-persistency-file-name (my/var "tramp")
      url-configuration-directory (my/var "url/" t)
      url-cache-directory     (my/var "url/cache/" t)
      nsm-settings-file       (my/var "network-security.data")
      server-auth-dir         (my/var "server/" t)
      transient-history-file  (my/var "transient/history.el")
      transient-levels-file   (my/var "transient/levels.el")
      transient-values-file   (my/var "transient/values.el"))

;;; ===========================================================================
;;; 3. Garbage collection: runtime values (early-init deferred it for boot)
;;; ===========================================================================
(defconst my/gc-runtime-threshold  (* 64 1024 1024) "Runtime GC threshold (bytes).")
(defconst my/gc-runtime-percentage 0.1)

(defun my/gc-runtime ()
  (setq gc-cons-threshold my/gc-runtime-threshold
        gc-cons-percentage my/gc-runtime-percentage))
(defun my/gc-max  () (setq gc-cons-threshold most-positive-fixnum))

(add-hook 'emacs-startup-hook    #'my/gc-runtime)
(add-hook 'minibuffer-setup-hook #'my/gc-max)        
(add-hook 'minibuffer-exit-hook  #'my/gc-runtime)

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %.2fs with %d GCs."
                     (float-time (time-subtract after-init-time before-init-time))
                     gcs-done)))

;;; ===========================================================================
;;; 4. OS-specific performance & subprocess reliability
;;; ===========================================================================
(defconst my/w32-pipe-buffer-size (* 64 1024) "Pipe buffer for LSP/git on Windows.")

(when IS-WINDOWS
  ;; Skip an expensive GC step that bites when many fonts/emoji are cached.
  (setq inhibit-compacting-font-caches t
        w32-pipe-buffer-size my/w32-pipe-buffer-size))

;; Engine performance (display + subprocess).
(defconst my/process-output-max (* 4 1024 1024)
  "Bytes read from a subprocess at once. Bigger = faster LSP.")
(setq read-process-output-max my/process-output-max
      redisplay-skip-fontification-on-input t   ; smoother scroll under keypresses
      bidi-inhibit-bpa t                         ; skip bidi paren algo -> faster redisplay
      ffap-machine-p-known 'reject               ; never DNS-ping a host when resolving paths
      large-file-warning-threshold (* 100 1024 1024)
      frame-resize-pixelwise t
      window-resize-pixelwise t)
(setq-default bidi-paragraph-direction 'left-to-right
              truncate-lines t)

;; Find git/clangd/pyright/... regardless of how Emacs was launched.
(add-to-list 'exec-path (my/emacs-path "bin"))

;;; ===========================================================================
;;; 5. Package system: package.el + use-package (built in)
;;; ===========================================================================
(require 'package)

(setq package-user-dir      (my/emacs-path "var/elpa/")
      package-gnupghome-dir (my/emacs-path "var/elpa/gnupg/")
      package-archives
      '(("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa"  . "https://melpa.org/packages/"))
      package-archive-priorities '(("gnu" . 10) ("nongnu" . 8) ("melpa" . 5))
      package-quickstart t
      package-quickstart-file (my/emacs-path "var/package-quickstart.el")
      package-native-compile t
      package-install-upgrade-built-in t)

;; Windows GPG frequently lacks the ELPA signing keys, causing "Failed to
;; verify signature / no public key" on install. Package downloads still go
;; over HTTPS, so the transport is authenticated; we skip the GPG check on
;; Windows for reliability. Proper alternative if you want the check back:
;; install the `gnu-elpa-keyring-update' package, which refreshes the keyring.
(when IS-WINDOWS
  (setq package-check-signature nil))

;; Load the prebuilt autoload bundle (installed packages are usable from this
;; alone). We deliberately do NOT read `package-archive-contents' here: parsing
;; MELPA's multi-MB archive list every startup is the single biggest avoidable
;; cost, and it's only needed to INSTALL packages, not to use installed ones.
(if (file-exists-p package-quickstart-file)
    (load package-quickstart-file nil 'nomessage)
  (package-initialize))

;; Populate the archive list lazily -- only the first time something actually
;; installs. Read the on-disk cache (no network); refresh only if absent.
(defun my/ensure-package-archives (&rest _)
  (unless package-archive-contents
    (condition-case nil
        (progn (package-read-all-archive-contents)
               (unless package-archive-contents (package-refresh-contents)))
      (error (package-refresh-contents)))))
(advice-add 'package-install :before #'my/ensure-package-archives)

(require 'use-package)
(setq use-package-always-ensure t       ; auto-install missing packages
      use-package-always-defer t        ; lazy by default; demand explicitly
      use-package-expand-minimally t
      use-package-compute-statistics t) ; M-x use-package-report

;;; ===========================================================================
;;; 6. lisp/ modules — only large feature collections live here
;;; ===========================================================================
(defconst my/lisp-dir (my/emacs-path "lisp/"))
(make-directory my/lisp-dir t)
(add-to-list 'load-path my/lisp-dir)
(setq load-prefer-newer t)

(defun my/load (feature)
  "Load FEATURE module from lisp/, logging failures instead of aborting init."
  (condition-case err
      (require feature)
    (error (message "Module %s failed: %s" feature (error-message-string err)))))

;; Keep modules byte-compiled: build once if needed, then recompile on save.
(unless (file-exists-p (expand-file-name "my-commands.elc" my/lisp-dir))
  (byte-recompile-directory my/lisp-dir 0))
(add-hook 'after-save-hook
          (lambda ()
            (when (and buffer-file-name
                       (file-in-directory-p buffer-file-name my/lisp-dir))
              (byte-compile-file buffer-file-name))))

(setq help-window-select t)

(my/load 'my-commands)            ; custom utility commands
(my/load 'setup-windows)          ; display-buffer placement rules
(my/load 'my-coding)              ; tree-sitter + eglot + diagnostics
(my/load 'my-writing)             ; org (research/latex) + markdown

;; Run a server so `emacsclient' (git EDITOR, "open in Emacs") reuses this
;; session. Start it once Emacs is idle -- keeps it off the startup path.
(add-hook 'emacs-startup-hook
          (lambda ()
            (run-with-idle-timer
             1.0 nil
             (lambda () (require 'server)
               (unless (server-running-p) (server-start))))))

;;; ===========================================================================
;;; 7. Editing mechanics & built-in UX
;;; ===========================================================================
(delete-selection-mode 1)          ; typing replaces the active region
(global-subword-mode 1)            ; CamelCase humps count as words
(setq sentence-end-double-space nil
      kill-whole-line t)           ; C-k at col 0 takes the newline too

(defconst my/tab-width 4)
(setq-default indent-tabs-mode nil
              tab-width my/tab-width)
(setq tab-always-indent 'complete) ; TAB indents, then completes

(setq save-interprogram-paste-before-kill t
      kill-do-not-save-duplicates t
      kill-ring-max 500
      mouse-drag-and-drop-region t
      mouse-drag-and-drop-region-cross-program t)

(defconst my/undo-limit        (* 64 1024 1024))
(defconst my/undo-strong-limit (* 96 1024 1024))
(defconst my/undo-outer-limit  (* 1024 1024 1024))
(setq undo-limit my/undo-limit
      undo-strong-limit my/undo-strong-limit
      undo-outer-limit my/undo-outer-limit)

(electric-pair-mode 1)
(setq electric-pair-preserve-balance t)
(add-hook 'prog-mode-hook          ; don't pair < > (templates, comparisons)
          (lambda () (modify-syntax-entry ?< ".") (modify-syntax-entry ?> ".")))

(add-hook 'prog-mode-hook #'hs-minor-mode)   ; code folding (hideshow)
(with-eval-after-load 'hideshow
  (define-key hs-minor-mode-map (kbd "C-{")     #'hs-hide-block)
  (define-key hs-minor-mode-map (kbd "C-}")     #'hs-show-block)
  (define-key hs-minor-mode-map (kbd "C-c C-{") #'hs-hide-all)
  (define-key hs-minor-mode-map (kbd "C-c C-}") #'hs-show-all))

(setq isearch-lazy-count t
      lazy-count-prefix-format nil
      lazy-count-suffix-format "  (%s/%s)"
      isearch-allow-scroll t
      isearch-wrap-pause 'no
      search-whitespace-regexp ".*?"
      search-invisible 'open)

(setq set-mark-command-repeat-pop t
      help-window-select t
      ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally
      compilation-scroll-output t)
(with-eval-after-load 'compile
  (require 'ansi-color)
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter))

(dolist (c '(narrow-to-region narrow-to-page narrow-to-defun
             upcase-region downcase-region))
  (put c 'disabled nil))

(when (fboundp 'which-key-mode)   
  (which-key-mode 1)
  (setq which-key-idle-delay 0.5))

(global-set-key (kbd "<escape>") #'keyboard-escape-quit)
(define-key minibuffer-local-map (kbd "<escape>") #'abort-recursive-edit)
(global-set-key (kbd "C-z")   #'undo-only)   ; C-z=suspend is useless in GUI
(global-set-key (kbd "C-S-z") #'undo-redo)
(global-set-key (kbd "C-/")   #'comment-line)

;; Visual undo tree (like neovim's undotree). Lazy: loads on first `C-x u'.
(use-package vundo
  :bind ("C-x u" . vundo)
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols
        vundo-compact-display t))

;; Persist undo/redo history across sessions (built-in undo, no undo-fu needed).
(use-package undo-fu-session
  :init
  (setq undo-fu-session-directory (my/var "undo-fu-session/" t)
        undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'" "\\.gpg\\'")
        undo-fu-session-compression 'gz)
  :hook (emacs-startup . undo-fu-session-global-mode))

(use-package expreg
  :ensure nil
  :bind (("C-=" . expreg-expand)
         ("C-+" . expreg-contract))
  :config
  (add-hook 'after-change-major-mode-hook
            (lambda ()
              (when (or (derived-mode-p 'text-mode) (eq major-mode 'fundamental-mode))
                (add-to-list 'expreg-functions #'expreg--sentence t)))))

(global-reveal-mode 1)
(setq browse-url-browser-function #'browse-url-default-browser)

;; `q' keeps its default (bury). `C-c q' fully kills the window's buffer when
;; you want a transient buffer (help/dired/compilation) gone for good.
(defun my/quit-window-kill (&optional window)
  "Like `quit-window' but kill the buffer instead of burying it."
  (interactive) (quit-window t window))
(global-set-key (kbd "C-c q") #'my/quit-window-kill)

;;; ===========================================================================
;;; 8. Session persistence (history, places, recent files)
;;; ===========================================================================
(setq history-length 1000
      history-delete-duplicates t
      savehist-autosave-interval 60
      savehist-save-minibuffer-history t
      savehist-additional-variables '(search-ring regexp-search-ring)
      global-auto-revert-non-file-buffers t
      auto-revert-verbose nil
      recentf-max-saved-items 500
      recentf-auto-cleanup 'never)

(defconst my/recentf-exclude
  '("/node_modules/" "/site-packages/" "/dist-packages/" "/\\.cargo/registry/"
    "/\\.rustup/" "/go/pkg/mod/" "/venv/" "/\\.venv/" "/\\.tox/" "/__pycache__/"
    "/\\.git/" "/build/" "/dist/" "/target/" "/\\.cache/" "/\\.next/"
    "\\.tmp\\'" "^/\\(?:ssh\\|su\\|sudo\\)?:")
  "Path patterns kept out of the recent-files list.")
(with-eval-after-load 'recentf
  (dolist (p my/recentf-exclude) (add-to-list 'recentf-exclude p))
  (add-to-list 'recentf-exclude
               (lambda (f)
                 (let ((f (expand-file-name f))
                       (cache (expand-file-name my/cache-dir))
                       (scratch (expand-file-name "scratch/" my/cache-dir)))
                   (and (string-prefix-p cache f)
                        (not (string-prefix-p scratch f)))))))

;; Enable after startup so reading history/places files doesn't slow boot.
(add-hook 'emacs-startup-hook
          (lambda ()
            (savehist-mode 1) (save-place-mode 1)
            (recentf-mode 1)  (global-auto-revert-mode 1)))

;;; ===========================================================================
;;; 9. Window & buffer navigation
;;; ===========================================================================
(setq switch-to-buffer-obey-display-actions t
      window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)

;; Vim-style window jumps. NOTE: shadows M-h mark-paragraph, M-k kill-sentence,
;; M-l downcase-word -- accepted trade for fast navigation.
(global-set-key (kbd "M-h") #'windmove-left)
(global-set-key (kbd "M-j") #'windmove-down)
(global-set-key (kbd "M-k") #'windmove-up)
(global-set-key (kbd "M-l") #'windmove-right)
(global-set-key (kbd "C-S-h") #'windmove-swap-states-left)
(global-set-key (kbd "C-S-j") #'windmove-swap-states-down)
(global-set-key (kbd "C-S-k") #'windmove-swap-states-up)
(global-set-key (kbd "C-S-l") #'windmove-swap-states-right)

(defun my/split-right-focus ()
  "Split right and focus the new window."
  (interactive) (select-window (split-window-right)))
(defun my/split-below-focus ()
  "Split below and focus the new window."
  (interactive) (select-window (split-window-below)))
(global-set-key (kbd "C-x 3") #'my/split-right-focus)
(global-set-key (kbd "C-x 2") #'my/split-below-focus)
;; Quick single-chord window ops (avoid M-digit = digit-argument):
(global-set-key (kbd "C-S-\\") #'my/split-right-focus) 
(global-set-key (kbd "C-|") #'my/split-below-focus)
(global-set-key (kbd "M-o")  #'other-window)           ; fast focus; repeat: o o o
(global-set-key (kbd "M-O")  #'delete-other-windows)   ; maximize current window
(global-set-key (kbd "C-M-o") #'delete-window)         ; close current window

(setq switch-to-prev-buffer-skip (lambda (_w buf _x) (my/hidden-buffer-p buf))
      switch-to-next-buffer-skip (lambda (_w buf _x) (my/hidden-buffer-p buf)))
(global-set-key (kbd "M-[") #'previous-buffer)
(global-set-key (kbd "M-]") #'next-buffer)

(add-hook 'emacs-startup-hook
          (lambda () (winner-mode 1) (window-divider-mode 1) (repeat-mode 1)))

;; popper: toggle/cycle "popup" buffers. `popper-display-control 'user' defers
;; WHERE they appear to setup-windows.el; popper just hides/recalls/cycles them.
(use-package popper
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type)
         ("C-~"   . popper-cycle-backwards))
  :init
  (add-hook 'emacs-startup-hook #'popper-mode)
  (setq popper-reference-buffers
        '("^\\*Messages\\*"
          ("[Oo]utput\\*$" . hide)
          (TeX-output-mode . hide)
          "\\*Preview-Ghostscript-Error\\*"
          ("\\*Async Shell Command\\*" . hide)
          ("^\\*Backtrace\\*" . hide)
          ("\\*Completions\\*")
          ("^\\*Compile-Log\\*" . hide)
          ("^\\*Warnings\\*" . hide)
          "\\*Shell Command Output\\*"
          ("\\*Org LaTeX Precompilation\\*" . hide)
          "^\\*Apropos" "^\\*Buffer List\\*" "^Calc:" "^\\*ielm\\*" "^\\*TeX Help\\*"
          help-mode Custom-mode pdf-view-mode occur-mode ibuffer-mode dired-mode
          bookmark-bmenu-mode xref--xref-buffer-mode calendar-mode
          compilation-mode flymake-diagnostics-buffer-mode
          magit-status-mode magit-process-mode magit-log-mode
          magit-revision-mode magit-diff-mode))
  :config
  ;; Group popups by project, but fall back to ungrouped (no error) for buffers
  ;; that aren't in a project -- raw `popper-group-by-project' signals there.
  (defun my/popper-group-by-project ()
    (condition-case nil (popper-group-by-project) (error nil)))
  (setq popper-group-function #'my/popper-group-by-project
        popper-display-control 'user
        popper-mode-line '(:eval (propertize " POP" 'face 'mode-line-emphasis)))
  (setq popper-reference-buffers
        (append popper-reference-buffers
                '("^\\*eshell.*\\*$" eshell-mode
                  "^\\*shell.*\\*$"  shell-mode
                  "^\\*term.*\\*$"   term-mode "^\\*vterm.*\\*$" vterm-mode
                  "^\\*dape.*\\*$"   dape-info-mode dape-repl-mode)))
  (popper-echo-mode 1))

;;; ===========================================================================
;;; 10. Dired (built-in file manager)
;;; ===========================================================================
(setq dired-kill-when-opening-new-dired-buffer t
      dired-dwim-target t
      delete-by-moving-to-trash t
      dired-recursive-copies 'always
      dired-recursive-deletes 'top
      dired-create-destination-dirs 'always
      dired-auto-revert-buffer t
      dired-mouse-drag-files t
      dired-isearch-filenames 'dwim
      dired-movement-style 'cycle
      dired-listing-switches "-agho --group-directories-first"
      wdired-allow-to-change-permissions t
      wdired-create-parent-directories t)

;; Windows/macOS lack GNU ls: use Emacs's own Lisp directory lister.
(when (memq system-type '(windows-nt darwin))
  (with-eval-after-load 'dired
    (require 'ls-lisp)
    (setq ls-lisp-use-insert-directory-program nil
          ls-lisp-dirs-first t)))

(defun my/dired-create-file (file)
  "Create empty FILE in the current Dired dir and jump to it."
  (interactive (list (read-file-name "Create file: " (dired-current-directory))))
  (write-region "" nil (expand-file-name file))
  (revert-buffer) (dired-goto-file (expand-file-name file)))

(with-eval-after-load 'dired
  (require 'dired-x)
  (setq dired-omit-files "\\`\\.\\.?\\'")   ; hide only . and ..
  (define-key dired-mode-map (kbd "H") #'dired-omit-mode)
  (define-key dired-mode-map (kbd "c") #'my/dired-create-file)
  (when (fboundp 'dired-do-open)
    (define-key dired-mode-map (kbd "C-<return>") #'dired-do-open)))
(add-hook 'dired-mode-hook #'dired-omit-mode)

;;; ===========================================================================
;;; 11. Completion & minibuffer (built-in)
;;; ===========================================================================
;; Vertical candidate list .
(fido-vertical-mode 1)
(setq completion-ignore-case t
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t
      completions-detailed t                     
      completions-sort 'historical                ; recently used candidates first
      completion-auto-help 'visible
      completion-cycle-threshold 3                
      enable-recursive-minibuffers t
      icomplete-compute-delay 0
      icomplete-show-matches-on-no-input t)
(minibuffer-depth-indicate-mode 1)

;; fido binds no TAB, so it falls back to `minibuffer-complete' (which pops the
;; *Completions* buffer). Make TAB complete to the highlighted candidate inline.
(with-eval-after-load 'icomplete
  (define-key icomplete-fido-mode-map (kbd "TAB")   #'icomplete-force-complete)
  (define-key icomplete-fido-mode-map (kbd "<tab>") #'icomplete-force-complete))

;; Orderless: space-separated, any-order matching. fido hardcodes the `flex'
;; style inside the minibuffer, so we re-assert orderless with a late (depth 95)
;; minibuffer-setup-hook that runs after fido's own setup.
(use-package orderless
  :demand t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)) (buffer (styles orderless basic)))))
(add-hook 'minibuffer-setup-hook
          (lambda () (setq-local completion-styles '(orderless basic))) 95)

;; Inline ghost-text completion preview (Emacs 30). Enabled per-buffer in
;; `my/setup-editing-buffer' (terminals/special excluded) and in the
;; eval-expression (M-:) minibuffer. RIGHT / TAB accept; M-n / M-p cycle.
(when (require 'completion-preview nil t)
  (setq completion-preview-exact-match-only nil)
  (define-key completion-preview-active-mode-map (kbd "<right>") #'completion-preview-insert)
  (define-key completion-preview-active-mode-map (kbd "TAB")     #'completion-preview-insert)
  (add-hook 'eval-expression-minibuffer-setup-hook #'completion-preview-mode))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer. 
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init
  (marginalia-mode))

;; Icons next to minibuffer candidates.
(use-package nerd-icons-completion
  :after marginalia
  :commands nerd-icons-completion-mode
  :config (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(defun my/enable-completion-icons ()
  "Load + enable minibuffer icons once, then unhook self."
  (remove-hook 'minibuffer-setup-hook #'my/enable-completion-icons)
  (nerd-icons-completion-mode 1)
  (nerd-icons-completion-marginalia-setup))
(add-hook 'minibuffer-setup-hook #'my/enable-completion-icons)
(add-hook 'emacs-startup-hook
          (lambda () (run-with-idle-timer 1 nil #'my/enable-completion-icons)))

;;; ===========================================================================
;;; 12. Appearance
;;; ===========================================================================
;; Main font + maximize + colors are set in early-init. Here: glyph fallback,
;; theme, and cheap UI niceties.
(defconst my/variable-pitch-candidates
  '("Merriweather" "Iosevka Aile" "Fira Sans" "Cambria" "Segoe UI" "Cantarell")
  "Proportional fonts for variable-pitch faces, tried best-first.")

(defun my/apply-fonts (&optional frame)
  "Weight, proportional face, and emoji/symbol glyph fallback for FRAME.
Family + size come from the frame created in early-init."
  (when (display-graphic-p frame)
    (set-face-attribute 'default frame :weight my/font-weight)
    (set-face-attribute 'fixed-pitch frame :family my/font-family :weight my/font-weight)
    (when-let* ((vp (seq-find (lambda (f) (find-font (font-spec :family f)))
                              my/variable-pitch-candidates)))
      (set-face-attribute 'variable-pitch frame :family vp :weight 'regular)
      (setf (alist-get vp face-font-rescale-alist nil nil #'equal) 0.9))
    (when IS-WINDOWS
      (set-fontset-font t 'emoji  (font-spec :family "Segoe UI Emoji") frame 'prepend)
      (set-fontset-font t 'symbol (font-spec :family "Segoe UI Symbol") frame 'append))))
(add-hook 'after-make-frame-functions #'my/apply-fonts)
(my/apply-fonts)

(setq frame-title-format '("%b — Emacs")
      ring-bell-function #'ignore
      use-dialog-box nil
      use-short-answers t)

(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs t
      modus-themes-mixed-fonts t
      modus-themes-variable-pitch-ui nil
      modus-themes-org-blocks 'gray-background
      modus-themes-prompts '(bold background)
      modus-themes-headings
      '((0 . (1.35)) (1 . (1.30)) (2 . (1.24))
        (3 . (semibold 1.17)) (4 . (1.14)) (t . (monochrome))))
(setq modus-themes-common-palette-overrides
      '((fg-line-number-active fg-main)
        (bg-line-number-inactive unspecified)
        (bg-line-number-active unspecified)
        (fringe unspecified)
        (border-mode-line-active unspecified)
        (border-mode-line-inactive unspecified)
        (date-common cyan) (date-deadline red-warmer) (date-event magenta-warmer)
        (date-now yellow-warmer) (date-scheduled magenta-cooler)
        (date-weekday cyan-cooler) (date-weekend blue-faint)))
(setq modus-vivendi-palette-overrides
      '((fg-main "#d6d6d4")
        (bg-main "#090909")
        (bg-region bg-lavender)
        (fg-heading-1 magenta-faint)
        (bg-mode-line-active bg-lavender)
        (fg-mode-line-active "#ffffff")))
(load-theme 'modus-vivendi :no-confirm)

(defconst my/line-number-width 3)
(setq display-line-numbers-type 'relative
      display-line-numbers-width my/line-number-width)

(defconst my/bare-buffer-exempt-modes
  '(term-mode eshell-mode shell-mode comint-mode pdf-view-mode image-mode
    dired-mode special-mode compilation-mode help-mode Info-mode
    completion-list-mode)
  "Modes (and descendants) that get neither line numbers nor whitespace marks.")
(defun my/setup-editing-buffer ()
  "Enable line numbers + whitespace visualization in real editing buffers.
Covers fundamental-mode/*scratch*; skips terminals, special, and the minibuffer."
  (unless (or (minibufferp)
              (apply #'derived-mode-p my/bare-buffer-exempt-modes))
    (display-line-numbers-mode 1)
    (whitespace-mode 1)
    (when (fboundp 'completion-preview-mode) (completion-preview-mode 1))))
(add-hook 'after-change-major-mode-hook #'my/setup-editing-buffer)
(add-hook 'text-mode-hook #'visual-line-mode)
(column-number-mode 1)

;; Visualize whitespace: spaces as dots, tabs as glyph, trailing highlighted.
;; Drop 'spaces/'space-mark from this list if the dots feel noisy.
(defconst my/whitespace-style
  '(face tabs tab-mark spaces space-mark trailing missing-newline-at-eof))
(setq whitespace-style my/whitespace-style
      whitespace-display-mappings '((tab-mark   ?\t [?▷ ?\t] [?\\ ?\t])
                                    (space-mark ?\s [?·]     [?.])))
;; Strip trailing whitespace on save, but only in code buffers.
(add-hook 'prog-mode-hook
          (lambda () (add-hook 'before-save-hook #'delete-trailing-whitespace nil t)))

;; Scrolling: keyboard scroll never recenters; keep a margin; smooth wheel.
(defconst my/scroll-margin 3)
(setq scroll-conservatively 101
      scroll-margin my/scroll-margin
      scroll-preserve-screen-position t
      mouse-wheel-progressive-speed nil
      mouse-wheel-scroll-amount '(2 ((shift) . 1) ((control) . text-scale))
      fast-but-imprecise-scrolling t)
(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))

;; Cursor & mouse pointer.
(setq-default cursor-type 'box)
(blink-cursor-mode -1)
(setq make-pointer-invisible t        ; hide mouse while typing
      x-stretch-cursor t)             ; cursor spans wide glyphs like tabs

;; Flush, fringeless look. NOTE: diff-hl/flymake normally draw in the fringe;
(defconst my/fringe-width 0 "Fringe width in pixels (0 = none).")
(fringe-mode my/fringe-width)

;; Tooltips in the echo area instead of popup windows.
(tooltip-mode -1)
(setq tooltip-use-echo-area t)

;; Matching parens; show the off-screen opener inline.
(show-paren-mode 1)
(setq show-paren-delay 0.1
      show-paren-when-point-inside-paren t
      show-paren-context-when-offscreen 'overlay)

;; Default mode line, fully non-interactive. The standard segments bake their
;; own keymap / mouse-face / help-echo as TEXT PROPERTIES (and inside
;; `:propertize' forms) -- so unbinding the global `mode-line' mouse keys or
;; nil-ing the segment keymap *variables* does nothing. We strip those
;; properties from the constructs themselves.
(defun my/ml-strip-plist (plist)
  "Drop mouse/keymap keys from a `:propertize' property list PLIST."
  (let (out)
    (while plist
      (unless (memq (car plist) '(local-map keymap mouse-face pointer help-echo))
        (setq out (nconc out (list (car plist) (cadr plist)))))
      (setq plist (cddr plist)))
    out))

(defun my/ml-strip (obj)
  "Recursively remove mouse interactivity from mode-line construct OBJ."
  (cond
   ((stringp obj)
    (let ((s (copy-sequence obj)))
      (remove-text-properties
       0 (length s)
       '(local-map nil keymap nil mouse-face nil pointer nil help-echo nil) s)
      s))
   ((not (consp obj)) obj)
   ((eq (car obj) :eval) obj)                       ; never rewrite code
   ((eq (car obj) :propertize)
    (cons :propertize (cons (my/ml-strip (cadr obj))
                            (my/ml-strip-plist (cddr obj)))))
   (t (cons (my/ml-strip (car obj)) (my/ml-strip (cdr obj))))))

(dolist (seg '(mode-line-front-space mode-line-mule-info mode-line-client
               mode-line-modified mode-line-remote mode-line-frame-identification
               mode-line-buffer-identification mode-line-position mode-line-modes
               mode-line-misc-info mode-line-end-spaces minor-mode-alist))
  (when (boundp seg)
    (set-default seg (my/ml-strip (default-value seg)))))

;; Belt-and-suspenders: neutralize the catch-all keys and the default tooltip.
(dolist (e '([mode-line down-mouse-1] [mode-line mouse-1] [mode-line drag-mouse-1]
             [mode-line down-mouse-2] [mode-line mouse-2]
             [mode-line down-mouse-3] [mode-line mouse-3]
             [header-line down-mouse-1] [header-line mouse-1]))
  (global-set-key e #'ignore))
(setq mode-line-default-help-echo nil)

;;; ===========================================================================
;;; 13. Icons (nerd-icons) — glyphs served by the installed Nerd Font
;;; ===========================================================================
(defconst my/icon-font "Symbols Nerd Font Mono" "Font family for nerd-icons.")
(use-package nerd-icons
  :defer t
  :init (setq nerd-icons-font-family my/icon-font))

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

;;; ===========================================================================
;;; 14. Version control (magit + diff-hl + wgrep)
;;; ===========================================================================
;; Windows: process spawning is magit's bottleneck -- point it straight at
;; git.exe, and expose Git's bundled Unix tools (diff/grep) for diff-hl/ediff.
(when IS-WINDOWS
  (let ((git-usr "C:/Program Files/Git/usr/bin")
        (git-exe "C:/Program Files/Git/bin/git.exe"))
    (when (file-directory-p git-usr)
      (add-to-list 'exec-path git-usr)
      (setenv "PATH" (concat git-usr ";" (getenv "PATH"))))
    (when (file-exists-p git-exe)
      (setq magit-git-executable git-exe))))

(use-package magit                       ; loads on first C-x g
  :init (setq magit-define-global-key-bindings nil)
  :bind (("C-x g"   . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("C-c g g" . magit-status)
         ("C-c g b" . magit-blame-addition)
         ("C-c g l" . magit-log-buffer-file))
  :config
  (setq magit-diff-refine-hunk 'all      ; word-level diff highlighting
        magit-save-repository-buffers 'dontask))

(use-package diff-hl                      ; git gutter (margin, since fringe=0)
  :hook ((dired-mode        . diff-hl-dired-mode)
         (magit-pre-refresh  . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :bind (("C-c g [" . diff-hl-previous-hunk)
         ("C-c g ]" . diff-hl-next-hunk)
         ("C-c g v" . diff-hl-show-hunk)
         ("C-c g s" . diff-hl-stage-current-hunk)
         ("C-c g r" . diff-hl-revert-hunk))
  :init (add-hook 'emacs-startup-hook #'global-diff-hl-mode)
  :config
  (diff-hl-flydiff-mode 1)               ; update gutter live as you type
  (diff-hl-margin-mode 1))               ; REQUIRED: fringe=0 -> draw in margin

(use-package wgrep                        ; edit grep results in place, then save
  :init (setq wgrep-auto-save-buffer t
              wgrep-change-readonly-file t)
  :bind (:map grep-mode-map ("C-c C-p" . wgrep-change-to-wgrep-mode)))

;;; ===========================================================================
;;; 15. Navigation & multi-cursor (avy, multiple-cursors, imenu, project)
;;; ===========================================================================
;; avy: jump anywhere by typing a short label. (M-j is windmove-down here, so
;; the timer command lives on C-;.)
(use-package avy
  :bind (("C-;"   . avy-goto-char-timer)
         ("C-'"   . avy-goto-line)
         ("C-c j" . avy-goto-char-2)
         ("C-c w" . avy-goto-word-1)
         ("C-c S" . avy-isearch))
  :config
  (setq avy-timeout-seconds 0.8
        avy-background t                  
        avy-keys '(?a ?s ?d ?f ?j ?k ?l ?\; ?w ?e ?i ?o))
  (set-face-attribute 'avy-background-face nil :foreground "#6c6c6c" :background 'unspecified)
  (set-face-attribute 'avy-lead-face   nil :foreground "#ffffff" :background "#ff2d00" :weight 'bold)
  (set-face-attribute 'avy-lead-face-0 nil :foreground "#1a1a1a" :background "#ff7a00" :weight 'bold)
  (set-face-attribute 'avy-lead-face-1 nil :foreground "#1a1a1a" :background "#ffb000" :weight 'bold)
  (set-face-attribute 'avy-lead-face-2 nil :foreground "#ffffff" :background "#ff4500" :weight 'bold))

;; multiple-cursors: VS Code-style multi-caret editing.
(use-package multiple-cursors
  :init (setq mc/list-file (my/var "mc-lists.el")) 
  :preface
  (defun my/mc-mark-next-or-delete-char (arg)
    "With an active region, add a cursor at the next match (VS Code C-d);
otherwise delete the character ahead."
    (interactive "p")
    (if (use-region-p) (mc/mark-next-like-this arg) (delete-char arg)))
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C-d"         . my/mc-mark-next-or-delete-char)
         ("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-<"     . mc/mark-all-like-this)
         ("C-\""        . mc/skip-to-next-like-this)
         ("C-:"         . mc/skip-to-previous-like-this)))

;; imenu (built-in): jump to defs/headings in the buffer (reveals folded text).
(setq imenu-auto-rescan t
      imenu-use-popup-menu nil
      imenu-max-item-length 100)
(global-set-key (kbd "M-s i") #'imenu)
(add-hook 'imenu-after-jump-hook
          (lambda () (when (bound-and-true-p outline-minor-mode) (outline-show-entry))))

;; project.el (built-in): also detect projects by these markers, not just VCS.
(with-eval-after-load 'project
  (setq project-vc-extra-root-markers
        '("package.json" "Cargo.toml" "pyproject.toml" "requirements.txt"
          "setup.py" "go.mod" "pom.xml" ".project")))
(setq project-switch-use-entire-map t)
(global-set-key (kbd "C-c p") #'project-switch-project)

;; project-x: save/restore each project's buffers + window layout (C-x p w to
;; save).
;; C-x p w	project-x-window-state-save	Manually save your current project session
;; C-x p j	project-x-window-state-load	Load session from a project
;; C-x p J	project-x-windows	Reload session for the current project
;; C-x p a	project-x-add-local-project	Add project + root marker
;; C-x p r	project-x-rename-session	Rename the session (without changing path)
;; C-x p d	project-x-clear-session	Clear session data

(use-package project-x
  :ensure nil
  :after project
  :demand t
  :config
  (setq project-x-window-list-file (my/var "project-x-save.el"))
  (setq project-x-local-identifier '("package.json" "mix.exs" "Project.toml" ".project"))
  (setq project-x-auto-save-delay 10)
  (setq project-prompter #'project-x--project-prompt)
  (setq tab-bar-show nil)
  ;; Silence the "Wrote/Saved project state" echo spam from auto-save.
  (dolist (fn '(project-x--window-state-write project-x-window-state-save))
    (advice-add fn :around (lambda (orig &rest a)
                             (let ((inhibit-message t)) (apply orig a)))))
  (project-x-mode 1)
  (project-x-tabs-mode 1))

;;; ===========================================================================
;;; 16. Start in a sensible working directory
;;; ===========================================================================
;; Windows launches from the Start Menu with cwd = install bin/, so new buffers
;; inherit C:/Program Files/... Land in the work root instead (HOME on Unix).
(defconst my/start-dir (if IS-WINDOWS "D:/" (expand-file-name "~/"))
  "Default working directory for fresh buffers at startup.")
(when (file-directory-p my/start-dir)
  (setq-default default-directory my/start-dir)
  (dolist (buf '("*scratch*" "*Messages*"))
    (when (get-buffer buf)
      (with-current-buffer buf (setq default-directory my/start-dir)))))

;;; ===========================================================================
;;; 17. Customize writes go to their own file, not here
;;; ===========================================================================
(setq custom-file (my/var "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file nil 'nomessage))

;;; init.el ends here
