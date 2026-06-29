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
      package-native-compile t)

;; Windows GPG frequently lacks the ELPA signing keys, causing "Failed to
;; verify signature / no public key" on install. Package downloads still go
;; over HTTPS, so the transport is authenticated; we skip the GPG check on
;; Windows for reliability. Proper alternative if you want the check back:
;; install the `gnu-elpa-keyring-update' package, which refreshes the keyring.
(when IS-WINDOWS
  (setq package-check-signature nil))

;; Fast path: load the prebuilt autoload bundle if package.el generated it
;; (it does on every install/delete). The quickstart bundle does NOT populate
;; `package-archive-contents', so we read the on-disk archive cache ourselves
;; (no network) -- otherwise the refresh guard below would fire every startup.
(if (file-exists-p package-quickstart-file)
    (progn
      (load package-quickstart-file nil 'nomessage)
      (package-read-all-archive-contents))
  (package-initialize))

;; Only when the archive cache is genuinely empty (first run) do we hit the
;; network. Wrapped so a flaky archive logs instead of aborting init.
(unless package-archive-contents
  (condition-case err
      (package-refresh-contents)
    (error (message "Package archive refresh failed (continuing): %s"
                    (error-message-string err)))))

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

(my/load 'my-commands)            ; custom utility commands

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

(global-reveal-mode 1)
(setq browse-url-browser-function #'browse-url-default-browser)

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
               (lambda (f) (string-prefix-p (expand-file-name my/cache-dir)
                                       (expand-file-name f)))))

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

(setq switch-to-prev-buffer-skip (lambda (_w buf _x) (my/hidden-buffer-p buf))
      switch-to-next-buffer-skip (lambda (_w buf _x) (my/hidden-buffer-p buf)))
(global-set-key (kbd "M-[") #'previous-buffer)
(global-set-key (kbd "M-]") #'next-buffer)

(add-hook 'emacs-startup-hook
          (lambda () (winner-mode 1) (window-divider-mode 1) (repeat-mode 1)))

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
;; Vertical candidate list + flex fuzzy matching.
(fido-vertical-mode 1)
(setq completion-styles '(flex basic)            ; fuzzy, with a safe fallback
      completion-ignore-case t
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t
      completions-detailed t                      ; inline annotations 
      completions-sort 'historical                ; recently used candidates first
      completion-auto-help 'visible
      enable-recursive-minibuffers t
      icomplete-compute-delay 0
      icomplete-show-matches-on-no-input t)
(minibuffer-depth-indicate-mode 1)

;; In-buffer code completion: inline ghost-text preview as you type (Emacs 30).
;; completion-at-point (TAB) accepts; M-n / M-p cycle candidates.
(when (fboundp 'global-completion-preview-mode)
  (global-completion-preview-mode 1))

;;; ===========================================================================
;;; 12. Appearance
;;; ===========================================================================
;; Main font + maximize + colors are set in early-init. Here: glyph fallback,
;; theme, and cheap UI niceties.
(defconst my/variable-pitch-candidates
  '("Fira Sans" "Segoe UI" "Cantarell" "Iosevka Aile")
  "Proportional fonts for variable-pitch faces, tried best-first.")

(defun my/apply-fonts (&optional frame)
  "Weight, proportional face, and emoji/symbol glyph fallback for FRAME.
Family + size come from the frame created in early-init."
  (when (display-graphic-p frame)
    (set-face-attribute 'default frame :weight my/font-weight)
    (set-face-attribute 'fixed-pitch frame :family my/font-family :weight my/font-weight)
    (when-let* ((vp (seq-find (lambda (f) (find-font (font-spec :family f)))
                              my/variable-pitch-candidates)))
      (set-face-attribute 'variable-pitch frame :family vp :weight 'regular))
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
      modus-themes-org-blocks 'gray-background)
(load-theme 'modus-vivendi :no-confirm)

(defconst my/line-number-width 3)
(setq display-line-numbers-type 'relative
      display-line-numbers-width my/line-number-width)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'visual-line-mode)
(column-number-mode 1)

;; Visualize whitespace: spaces as dots, tabs as glyph, trailing highlighted.
;; Drop 'spaces/'space-mark from this list if the dots feel noisy.
(defconst my/whitespace-style
  '(face tabs tab-mark spaces space-mark trailing missing-newline-at-eof))
(setq whitespace-style my/whitespace-style
      whitespace-display-mappings '((tab-mark   ?\t [?▷ ?\t] [?\\ ?\t])
                                    (space-mark ?\s [?·]     [?.])))
(add-hook 'prog-mode-hook #'whitespace-mode)
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
(setq-default cursor-type 'bar)
(blink-cursor-mode -1)
(setq make-pointer-invisible t        ; hide mouse while typing
      x-stretch-cursor t)             ; cursor spans wide glyphs like tabs

;; Tooltips in the echo area instead of popup windows.
(tooltip-mode -1)
(setq tooltip-use-echo-area t)

;; Matching parens; show the off-screen opener inline.
(show-paren-mode 1)
(setq show-paren-delay 0.1
      show-paren-when-point-inside-paren t
      show-paren-context-when-offscreen 'overlay)

;;; ===========================================================================
;;; 13. Icons (nerd-icons) — glyphs served by the installed Nerd Font
;;; ===========================================================================
;; No extra font download: point nerd-icons at our Nerd Font. More integrations
;; (completion, modeline) hook in during later steps.
(use-package nerd-icons
  :defer t
  :init (setq nerd-icons-font-family my/font-family))

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

;;; ===========================================================================
;;; 14. Customize writes go to their own file, not here
;;; ===========================================================================
(setq custom-file (my/var "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file nil 'nomessage))

;;; init.el ends here
