;;; early-init.el --- Pre-GUI, pre-package setup -*- lexical-binding: t; -*-
;; Runs before the frame is drawn and before package.el. Job: boot fast and
;; make the first frame already correct (no flicker, no reflow).

;;; --- OS predicates (used across the whole config) --------------------------
(defconst IS-WINDOWS (eq system-type 'windows-nt))
(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))

;;; --- Appearance constants ------------------------
(defconst my/font-family "Iosevka NFM"
  "Main monospace font. Must be installed on the system.")
(defconst my/font-size 13 "Font size in points.")
(defconst my/font-weight 'semibold "Default face weight.")
(defconst my/bg-fallback "#090909" "Frame bg (matches modus-vivendi bg-main override).")
(defconst my/fg-fallback "#d6d6d4" "Frame fg (matches modus-vivendi fg-main override).")
(defconst my/frame-alpha 95 "Frame opacity 0-100.")
(defconst my/alpha-param (if IS-WINDOWS 'alpha 'alpha-background)
  "Windows w32 lacks per-pixel bg alpha, so use whole-frame `alpha' there;
elsewhere `alpha-background' keeps text opaque.")

;;; --- Defer GC during startup (init.el restores a runtime value) ------------
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; We initialize packages ourselves in init.el.
(setq package-enable-at-startup nil)

;;; --- First frame: chrome off, font/size/maximize/colors baked in ----------
;; Setting these in `default-frame-alist' means the frame is created this way
;; once, with no post-creation resize or repaint. Font here costs nothing; if
;; the family is missing Emacs silently uses its default.
(setq default-frame-alist
      `((menu-bar-lines . 0)
        (tool-bar-lines . 0)
        (vertical-scroll-bars)
        (font . ,(format "%s-%d" my/font-family my/font-size))
        (background-color . ,my/bg-fallback)
        (foreground-color . ,my/fg-fallback)
        (,my/alpha-param . ,my/frame-alpha)))
(setq tool-bar-mode nil menu-bar-mode nil
      frame-inhibit-implied-resize t)

;; Maximize. On Windows, `(fullscreen . maximized)' in `default-frame-alist'
;; makes the frame repaint (white/garbage) WHILE it maximizes mid-load -- the
;; classic startup glitch (bug#64846). Instead, maximize natively via the window
;; manager AFTER init, once the buffer is themed and drawn: one clean snap, no
;; glitch. Elsewhere the alist entry is fine.
(if IS-WINDOWS
    (add-hook 'window-setup-hook
              (lambda ()
                (when (fboundp 'w32-send-sys-command)
                  (w32-send-sys-command #xf030))))   ; SC_MAXIMIZE
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))

;;; --- Quiet startup ---------------------------------------------------------
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-buffer-menu t
      inhibit-startup-echo-area-message (user-login-name)
      initial-scratch-message
      (concat
       ";;   Emacs ready.  Scratch buffer — type freely.\n"
       ";;\n"
       ";;   Jump  C-;      Window  M-o       Popups  C-`\n"
       ";;   Find  C-x C-f  Recent  C-x C-r   Buffer  C-x b   Project  C-x p\n"
       ";;   Code  M-RET actions · C-c e r rename · C-c f format · M-n/M-p errors\n"
       ";;   Math  C-S-e eval-in-place · C-x * e live embedded Calc\n"
       ";;   Help  C-h k <key>   C-h f <fn>      Full reference:  README.md\n"
       ";;\n")
      initial-major-mode 'fundamental-mode   ; lighter *scratch*, faster boot
      inhibit-x-resources t)

;;; --- Native compilation: cache under var/, silence warnings ----------------
;; Must run here, before any native compilation, per startup.el docstring.
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache "var/eln-cache/"))
(setq native-comp-async-report-warnings-errors 'silent)

(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(setq default-input-method nil)

;;; --- Theme: load HERE, before the first frame paints -----------------------
;; modus-themes are built in. Loading the theme in early-init (not init) means
;; the frame is created already themed -- no white "box" flashing before the
;; theme applies ~1s into init. Palette/heading settings must precede load-theme.
(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs t
      modus-themes-mixed-fonts t                 ; code/tables/verbatim stay mono
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
        (date-weekday cyan-cooler) (date-weekend blue-faint) (date-holiday blue)
        (mail-recipient fg-main)
        (bg-region bg-sage) (fg-region unspecified)))
(setq modus-vivendi-palette-overrides
      '((fg-main "#d6d6d4")
        (bg-main "#090909")
        (bg-region bg-lavender)
        (fg-heading-1 magenta-faint)
        (bg-mode-line-active bg-lavender)
        (fg-mode-line-active "#ffffff")))
(setq modus-operandi-palette-overrides
      '((bg-mode-line-active bg-blue-intense)
        (fg-mode-line-active fg-main)
        (fg-heading-1 "#a01f64")
        (fg-heading-2 "#2f5f9f")
        (fg-heading-3 "#1a8388")))
(load-theme 'modus-vivendi :no-confirm)

;;; early-init.el ends here
