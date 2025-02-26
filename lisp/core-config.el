;;; core-config.el --- Core Configurations -*- lexical-binding: t; -*-

;;; Commentary:
;; Core configurations for Emacs, including basic UI and behavior settings.

;;; Code:

;; 🗂️ Custom Directories for Cache Files
;; -------------------------------------
(defun karna/dir-concat (dir file)
  "Concatenate DIR and FILE into a full path safely."
  (expand-file-name file dir))

(defvar user-cache-directory (expand-file-name "~/.cache/emacs/")
  "Directory for cache and temporary files created by Emacs.")

;; Define paths for various cache-related files
(setq save-place-file (karna/dir-concat user-cache-directory "places")  ;; Saves cursor position in files
      transient-history-file (karna/dir-concat user-cache-directory "transient-history.el") ;; Stores transient states
      bookmark-default-file (karna/dir-concat user-cache-directory "bookmarks") ;; Bookmarks file
      auto-save-list-file-prefix (karna/dir-concat user-cache-directory "auto-save-list/.saves-") ;; Auto-save sessions
      savehist-file (karna/dir-concat user-cache-directory "savehist") ;; Minibuffer history
      tramp-persistency-file-name (karna/dir-concat user-cache-directory "tramp") ;; TRAMP file caching
      lsp-session-file (karna/dir-concat user-cache-directory "lsp-session") ;; LSP session
      dap-breakpoints-file (karna/dir-concat user-cache-directory "dap-breakpoints") ;; Debug breakpoints
      eshell-directory-name (karna/dir-concat user-cache-directory "eshell") ;; Eshell home
      eshell-history-file-name (karna/dir-concat user-cache-directory "eshell-history") ;; Eshell history
      eshell-last-dir-ring-file-name (karna/dir-concat user-cache-directory "eshell-last-dir-ring") ;; Last visited dirs
      undo-fu-session-directory (karna/dir-concat user-cache-directory "undo-fu-session/") ;; Undo history
      persp-state-default-file (karna/dir-concat user-cache-directory "sessions") ;; Workspaces session state
      org-id-locations-file (karna/dir-concat user-cache-directory ".org-id-locations") ;; Org IDs
      org-roam-db-location "/mnt/Karna/Git/Project-K/Org/Roam/org-roam.db" ;; Org-Roam database
      preview-tailor-storage-file (karna/dir-concat user-cache-directory ".preview-tailor")) ;; Preview-Tailor cache

;; 🚀 Enable Core Modes After Initialization
;; ------------------------------------------
(add-hook 'after-init-hook #'global-auto-revert-mode)  ;; Auto-refresh buffers when files change
(add-hook 'after-init-hook #'recentf-mode)             ;; Track recently opened files
(add-hook 'after-init-hook #'savehist-mode)            ;; Preserve minibuffer history
(add-hook 'after-init-hook #'save-place-mode)          ;; Remember cursor positions in files

;; 🎨 Basic UI & Behavior Customization
;; ------------------------------------
(use-package emacs
  :demand t
  :custom
  (inhibit-startup-screen t)  ;; Disable startup screen
  (user-full-name "Karna")  ;; Set user name
  (frame-resize-pixelwise t)  ;; Allow pixel-wise resizing
  (long-line-threshold 100000)  ;; Handle long lines better
  (use-file-dialog nil)  ;; Disable GUI file dialogs
  (use-dialog-box nil)  ;; Disable pop-up dialog boxes
  (pop-up-windows nil)  ;; Avoid creating pop-up windows
  (auto-save-interval 2400)  ;; Auto-save after 2400 keystrokes
  (auto-save-timeout 300)  ;; Auto-save every 5 minutes
  (history-length 1000)  ;; Increase history length
  (history-delete-duplicates t)  ;; Remove duplicate history entries
  (savehist-save-minibuffer-history t)  ;; Save minibuffer history
  (use-short-answers t)  ;; Use 'y/n' instead of 'yes/no'
  (global-auto-revert-non-file-buffers t)  ;; Refresh non-file buffers automatically
  (cursor-type 'box)  ;; Use a box cursor
  (blink-cursor-mode t)  ;; Enable cursor blinking
  (size-indication-mode t)  ;; Show file size in the mode line
  (fringe-mode 0)  ;; Remove left and right margins
  (animate-cursor-mode t)  ;; Smooth cursor animation
  (ring-bell-function 'ignore)  ;; Disable annoying bell sound
  (display-time-default-load-average nil)  ;; Hide system load average
  :hook
  (before-save . whitespace-cleanup)  ;; Remove trailing whitespace on save
  (comint-exec . (lambda () (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil))) ;; Prevent exit prompts
  :config
  (delete-selection-mode 1)  ;; Replace selected text when typing
  (electric-indent-mode -1)  ;; Disable electric indentation
  (electric-pair-mode 1)  ;; Auto-close brackets and quotes
  (global-display-line-numbers-mode 1)  ;; Enable line numbers
  (global-visual-line-mode 1)  ;; Enable word-wrapping globally
  (add-hook 'text-mode-hook 'visual-line-mode)  ;; Enable word wrap in text-mode
  (menu-bar-mode -1)  ;; Hide menu bar
  (scroll-bar-mode -1)  ;; Hide scroll bar
  (global-hl-line-mode -1)  ;; Disable highlighting of the current line
  (tool-bar-mode -1)  ;; Hide tool bar
  (setq init-start-time (current-time))  ;; Track startup time
  (set-terminal-coding-system 'utf-8)  ;; Set terminal encoding to UTF-8
  (set-keyboard-coding-system 'utf-8)  ;; Set keyboard encoding to UTF-8
  (set-selection-coding-system 'utf-8)  ;; Set selection encoding to UTF-8
  (setq locale-coding-system 'utf-8)  ;; Ensure system-wide UTF-8
  (prefer-coding-system 'utf-8)  ;; Prefer UTF-8 for encoding
  (add-to-list 'default-frame-alist '(alpha-background . 100)) ;; Set frame transparency
  (global-prettify-symbols-mode 1)  ;; Display mathematical symbols prettily
  (defalias 'yes-or-no-p 'y-or-n-p)  ;; Use 'y' instead of 'yes' for prompts
  (pixel-scroll-precision-mode 1)  ;; Smooth scrolling
  (savehist-mode 1)  ;; Enable persistent minibuffer history
  (add-to-list 'savehist-additional-variables 'global-mark-ring)  ;; Save mark ring across sessions
  ;; 🔥 Custom Keybindings
  (global-set-key (kbd "C-c K") 'kill-some-buffers)  ;; Kill multiple buffers interactively
  (global-set-key [escape] 'keyboard-escape-quit))  ;; Use ESC to close popups/prompts

;; ----------------------------------------------------------------------------
;; 📌 Recent Files (recentf) Configuration
;; ----------------------------------------------------------------------------
(require 'recentf)

(setq recentf-save-file (expand-file-name "recentf" user-cache-directory)
      recentf-max-saved-items 200    ;; Store up to 200 recent files
      recentf-auto-cleanup 180       ;; Auto-cleanup old entries every 180 seconds
      recentf-exclude
      '("~/.cache/emacs/"
        "~/.emacs.d/snippets/"
        "/mnt/Karna/Git/Project-K/Org/Journal/"
        "/mnt/Karna/Git/Project-K/Org/Tasks.org"
        "_region_\\.tex$"
        "<none>\\.tex$"
        "^/tmp/"
        "\\.gz$"))  ;; Exclude all .gz files no matter where they are

(recentf-mode 1)

;; ----------------------------------------------------------------------------
;; 📌 Asynchronous Process Management
;; ----------------------------------------------------------------------------
(use-package async
  :ensure t
  :defer t
  :init (dired-async-mode 1))

;; ----------------------------------------------------------------------------
;; 📌 Browser Settings
;; ----------------------------------------------------------------------------
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "zen-browser"
      browse-url-default-program "zen-browser"
      org-html-htmlize-output-type 'inline-css
      org-html-htmlize-browser "zen-browser")

;; ----------------------------------------------------------------------------
;; 📌 Org Mode File Associations
;; ----------------------------------------------------------------------------
(setq org-file-apps
      '(("auto-mode" . emacs)               ;; Open files in Emacs by default
        ("\\.mm\\'" . default)              ;; Use system default for .mm files
        ("\\.x?html?\\'" . "zen-browser %s") ;; Open HTML files in zen-browser
        ("\\.pdf\\'" . "~/.local/bin/zathura %s"))) ;; Open PDFs in Zathura

;; Workaround for xdg-open issue when opening files in Org mode
(defun karna/org-open-file-wrapper (orig-fun &rest args)
  "Fix xdg-open issue in Org mode."
  (let ((process-connection-type nil))
    (apply orig-fun args)))

(advice-add 'org-open-file :around #'karna/org-open-file-wrapper)

;; ----------------------------------------------------------------------------
;; 📌 Backup Settings
;; ----------------------------------------------------------------------------
(setq backup-directory-alist `((".*" . "~/.local/share/Trash/files"))
      auto-save-default nil          ;; Disable auto-save
      delete-old-versions t           ;; Auto-delete old backups
      kept-new-versions 5             ;; Keep 5 new versions
      kept-old-versions 3             ;; Keep 3 old versions
      version-control t)              ;; Enable versioned backups


(provide 'core-config)
;;; core-config.el ends here
