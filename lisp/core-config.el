;;; core-config.el --- Core Configuration Settings -*- lexical-binding: t; -*-

(use-package emacs
  :ensure nil
  :demand t
  :custom
  ;; Basic UI & Behavior
  (inhibit-startup-screen t)
  (user-full-name "Karna")
  (frame-inhibit-implied-resize t)
  (frame-resize-pixelwise t)

  ;; Miscellaneous Settings
  (long-line-threshold 100000)
  (use-file-dialog nil)
  (use-dialog-box nil)
  (pop-up-windows nil)
  (auto-save-interval 2400)
  (auto-save-timeout 300)
  (history-length 1000)
  (history-delete-duplicates t)
  (savehist-save-minibuffer-history t)
  (use-short-answers t)

  :hook
  ;; Clean up whitespace before saving any buffer
  (before-save . whitespace-cleanup)
  ;; For comint buffers, disable the exit query for the associated process
  (comint-exec . (lambda ()
		   (set-process-query-on-exit-flag
		    (get-buffer-process (current-buffer)) nil)))

  :config
  ;;----------------------------------------------------------------------------
  ;; Basic UI & Behavior Adjustments
  ;;----------------------------------------------------------------------------
  (delete-selection-mode 1)           ;; Allow deletion of selected text
  (electric-indent-mode -1)           ;; Disable auto-indenting
  (electric-pair-mode 1)              ;; Auto-insert matching parentheses
  (global-display-line-numbers-mode 1) ;; Enable line numbers globally
  (global-visual-line-mode 1)         ;; Enable visual line wrapping
  (menu-bar-mode -1)                  ;; Disable the menu bar
  (scroll-bar-mode -1)                ;; Disable the scroll bar
  (column-number-mode t)              ;; Show column numbers
  (size-indication-mode t)            ;; Display file size in the mode line
  (fringe-mode -1)                    ;; Remove UI fringes
  (tool-bar-mode -1)                  ;; Disable the tool bar
  (global-hl-line-mode -1)            ;; global highlighting of the current line
  (setq init-start-time (current-time))  ;; Record startup time

  ;;----------------------------------------------------------------------------
  ;; Auto-Reverting & UTF-8 Encoding Settings
  ;;----------------------------------------------------------------------------
  (setq ring-bell-function 'ignore) ;; Disable the bell sound.
  (setq display-time-default-load-average nil) ;; Hide the system load average in the mode line.
  (global-auto-revert-mode t)         ;; Auto-refresh buffers when files change
  (setq global-auto-revert-non-file-buffers t)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)

  ;;----------------------------------------------------------------------------
  ;; Appearance Tweaks
  ;;----------------------------------------------------------------------------
  (add-to-list 'default-frame-alist '(alpha-background . 100)) ;; Set frame transparency
  (global-prettify-symbols-mode 1)      ;; Display certain keywords as symbols

  ;;----------------------------------------------------------------------------
  ;; Miscellaneous Tweaks
  ;;----------------------------------------------------------------------------
  (defalias 'yes-or-no-p 'y-or-n-p)      ;; Use y/n prompts instead of yes/no
  (pixel-scroll-precision-mode 1)       ;; Enable precise pixel scrolling

  ;; History & Minibuffer Enhancements
  (savehist-mode 1)
  (add-to-list 'savehist-additional-variables 'global-mark-ring)

  ;;----------------------------------------------------------------------------
  ;; Keybindings
  ;;----------------------------------------------------------------------------
  (global-set-key [escape] 'keyboard-escape-quit))  ;; Make ESC quit prompts

;;----------------------------------------------------------------------------
;; Speed Benchmarking (Startup Time Reporting)
;;----------------------------------------------------------------------------
(let ((init-time (float-time (time-subtract (current-time) init-start-time)))
      (total-time (string-to-number (emacs-init-time "%f"))))
  (message (concat
	    (propertize "Startup time: " 'face 'bold)
	    (format "%.2fs " init-time)
	    (propertize (format "(+ %.2fs system time)"
				(- total-time init-time)) 'face 'shadow))))

;;----------------------------------------------------------------------------
;; Garbage Collection
;;----------------------------------------------------------------------------

;; --- 1. Delay GC during startup to speed things up ---
(setq gc-cons-threshold most-positive-fixnum)

;; --- 2. Configure gcmh to dynamically manage GC after startup ---
(use-package gcmh
  :ensure t
  :diminish
  :init
  (setq gcmh-high-cons-threshold 402653184  ;; 384 MB: high threshold during busy periods
	gcmh-low-cons-threshold 16777216     ;; 16 MB: lower threshold when idle
	gcmh-idle-delay 5)                  ;; Wait 5 seconds of idle before lowering the threshold
  :config
  (gcmh-mode 1))

;; --- 3. After startup, restore GC settings and report load time ---
(add-hook 'emacs-startup-hook
	  (lambda ()
	    ;; Reset GC thresholds to reasonable values for normal operation.
	    (setq gc-cons-threshold 402653184
		  gc-cons-percentage 0.6)
	    (message "*** Emacs loaded in %.2f seconds with %d garbage collections."
		     (float-time (time-subtract after-init-time before-init-time))
		     gcs-done)))

;; Increase the amount of data which Emacs reads from the process
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;;----------------------------------------------------------------------------
;; Native Compile Warning Suppression
;;----------------------------------------------------------------------------

;; Option 1: Disable deferred native compilation entirely.
;; This prevents background compilation tasks from running during interactive sessions.
(if (boundp 'comp-deferred-compilation)
    (setq comp-deferred-compilation nil)
  (setq native-comp-deferred-compilation nil))

;; Option 2 (alternative):
;; If you prefer to retain asynchronous native compilation to gradually compile packages in the background,
;; you can leave deferred compilation enabled and instead silence its warnings:
;; (setq native-comp-async-report-warnings-errors nil)

;; In noninteractive sessions (like batch scripts), prefer newer source files over stale byte-compiled ones.
;; This skips unnecessary mtime checks on *.elc files, shaving off some IO time.
(setq load-prefer-newer noninteractive)

;;----------------------------------------------------------------------------
;; BYTE Compilation
;;----------------------------------------------------------------------------

;; Byte-compile elisp files after saving
(defun auto-byte-recompile ()
  "Auto-recompile `.el' files to `.elc' if they exist."
  (when (and buffer-file-name
	     (eq major-mode 'emacs-lisp-mode)
	     (not (string-match-p "init\\.el$" buffer-file-name))
	     (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-recompile-file buffer-file-name)))

(defun byte-recompile-init-file ()
  "Recompile the Emacs init file on exit if it exists."
  (when (and user-init-file
	     (file-exists-p user-init-file))
    (byte-recompile-file user-init-file)))

(add-hook 'after-save-hook #'auto-byte-recompile)
(add-hook 'kill-emacs-hook #'byte-recompile-init-file)
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

;;----------------------------------------------------------------------------
;; Asynchronous Process
;;----------------------------------------------------------------------------

(use-package async
  :ensure t
  :defer t
  :init (dired-async-mode 1))


(provide 'core-config)
;;; core-config.el ends here
