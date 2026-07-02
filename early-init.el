;;; early-init.el --- Pre-GUI, pre-package setup -*- lexical-binding: t; -*-

;;; --- OS predicates --------------------------
(defconst IS-WINDOWS (eq system-type 'windows-nt))
(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))

;;; --- Appearance constants ------------------------
(defconst my/font-family "Iosevka NFM"
  "Main monospace font. Must be installed on the system.")
(defconst my/font-size 13 "Font size in points.")
(defconst my/font-weight 'semibold "Default face weight.")
(defconst my/bg-fallback "#172030" "Frame bg (matches doom-rouge `bg' -- the default theme).")
(defconst my/fg-fallback "#FAFFF6" "Frame fg (matches doom-rouge `fg').")
(defconst my/frame-alpha 95 "Frame opacity 0-100.")
(defconst my/alpha-param (if IS-WINDOWS 'alpha 'alpha-background)
  "Windows w32 lacks per-pixel bg alpha, so use whole-frame `alpha' there;
elsewhere `alpha-background' keeps text opaque.")

;;; --- Defer GC during startup ------------
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(setq package-enable-at-startup nil
      package-quickstart nil
      load-prefer-newer t)

;;; --- First frame ----------
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

(if (not IS-WINDOWS)
    (add-to-list 'default-frame-alist '(fullscreen . maximized))
  ;; Initial non-daemon frame: clean WM maximize after load (dodges bug#64846).
  (add-hook 'window-setup-hook
            (lambda ()
              (when (fboundp 'w32-send-sys-command)
                (w32-send-sys-command #xf030))))       ; SC_MAXIMIZE
  (add-hook 'after-make-frame-functions
            (lambda (frame) (set-frame-parameter frame 'fullscreen 'maximized))))

;;; --- Quiet startup ---------------------------------------------------------
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-buffer-menu t
      inhibit-startup-echo-area-message (user-login-name)
      initial-scratch-message (purecopy "\
;; This buffer is for rough work and skethcy ideas that should not be saved.
;; To create a file, visit it with \\[find-file] and enter text in its buffer.

")
      initial-major-mode 'fundamental-mode
      inhibit-x-resources t)

;;; --- Native compilation: cache under var/, silence warnings ----------------
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache "var/eln-cache/"))
(setq native-comp-async-report-warnings-errors 'silent)
(when IS-WINDOWS
  (setq native-comp-async-jobs-number 1))

(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(setq default-input-method nil)

;;; --- Theme palette settings ------------------
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

;;; early-init.el ends here
