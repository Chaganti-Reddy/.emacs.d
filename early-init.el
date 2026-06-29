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
(defconst my/bg-fallback "#000000" "Frame bg before theme loads (avoids flash).")
(defconst my/fg-fallback "#ffffff" "Frame fg before theme loads.")
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
        (fullscreen . maximized)
        (font . ,(format "%s-%d" my/font-family my/font-size))
        (background-color . ,my/bg-fallback)
        (foreground-color . ,my/fg-fallback)
        (,my/alpha-param . ,my/frame-alpha)))
(setq tool-bar-mode nil menu-bar-mode nil
      frame-inhibit-implied-resize t)

;;; --- Quiet startup ---------------------------------------------------------
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-buffer-menu t
      inhibit-startup-echo-area-message (user-login-name)
      initial-scratch-message nil
      inhibit-x-resources t)

;;; --- Native compilation: cache under var/, silence warnings ----------------
;; Must run here, before any native compilation, per startup.el docstring.
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache "var/eln-cache/"))
(setq native-comp-async-report-warnings-errors 'silent)

(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(setq default-input-method nil)

;;; early-init.el ends here
