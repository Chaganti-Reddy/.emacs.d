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
      package-quickstart-file (my/emacs-path "var/package-quickstart.el"))

;; Fast path: load the prebuilt autoload bundle if package.el has generated it
;; (it does so on every install/delete); otherwise do the normal scan.
(if (file-exists-p package-quickstart-file)
    (load package-quickstart-file nil 'nomessage)
  (package-initialize))

;; First run only: fetch archive contents so :ensure can install packages.
(unless package-archive-contents
  (package-refresh-contents))

(require 'use-package)
(setq use-package-always-ensure t       ; auto-install missing packages
      use-package-always-defer t        ; lazy by default; demand explicitly
      use-package-expand-minimally t
      use-package-compute-statistics t) ; M-x use-package-report

;;; ===========================================================================
;;; 6. Feature modules (lisp/) — large / deferrable config lives here
;;; ===========================================================================
(defconst my/lisp-dir (my/emacs-path "lisp/"))
(make-directory my/lisp-dir t)
(add-to-list 'load-path my/lisp-dir)

(defun my/load (feature)
  "Load FEATURE module from lisp/, logging failures instead of aborting init."
  (condition-case err
      (require feature)
    (error (message "Module %s failed: %s" feature (error-message-string err)))))

;; (my/load 'my-completion) ... added as modules are built.

;;; ===========================================================================
;;; 7. Appearance
;;; ===========================================================================
;; Main font + maximize + colors are set in early-init. Here: glyph fallback,
;; theme, and cheap UI niceties.
(defun my/apply-glyph-fonts (&optional frame)
  "Emoji/symbol fallback so glyphs render instead of boxes."
  (when (and IS-WINDOWS (display-graphic-p))
    (set-fontset-font t 'emoji  (font-spec :family "Segoe UI Emoji") frame 'prepend)
    (set-fontset-font t 'symbol (font-spec :family "Segoe UI Symbol") frame 'append)))
(add-hook 'after-make-frame-functions #'my/apply-glyph-fonts)
(my/apply-glyph-fonts)

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
(column-number-mode 1)

;; Visualize whitespace: spaces as dots, tabs as glyph, trailing highlighted.
;; Drop 'spaces/'space-mark from this list if the dots feel noisy.
(defconst my/whitespace-style
  '(face tabs tab-mark spaces space-mark trailing missing-newline-at-eof))
(setq whitespace-style my/whitespace-style)
(add-hook 'prog-mode-hook #'whitespace-mode)

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

;;; ===========================================================================
;;; 8. Icons (nerd-icons) — glyphs served by the installed Nerd Font
;;; ===========================================================================
;; No extra font download: point nerd-icons at our Nerd Font. More integrations
;; (completion, modeline) hook in during later steps.
(use-package nerd-icons
  :defer t
  :init (setq nerd-icons-font-family my/font-family))

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

;;; ===========================================================================
;;; 9. Customize writes go to their own file, not here
;;; ===========================================================================
(setq custom-file (my/emacs-path "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file nil 'nomessage))

;;; init.el ends here
