;;; init.el --- Emacs Init file -*- lexical-binding: t; -*-

;;; Commentary:
;; This is my init.el file which loads at startup of emacs.

;;; Code:

;; ** ELPACA

(defvar elpaca-installer-version 0.10)
(defvar elpaca-directory (expand-file-name "elpaca/" "~/.local/share/git"))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(push 'transient elpaca-ignored-dependencies)
(push 'notmuch elpaca-ignored-dependencies)
(push 'elnode elpaca-ignored-dependencies)

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))

;; Block until current queue processed.
(elpaca-wait)

(use-package elpaca-ui
  :ensure nil
  :bind (:map elpaca-ui-mode-map
         ("p" . previous-line)
         ("F" . elpaca-ui-mark-pull))
  :after popper
  :init
  (add-to-list 'popper-reference-buffers
               'elpaca-log-mode)
  (setf (alist-get '(major-mode . elpaca-log-mode)
                   display-buffer-alist
                   nil nil #'equal)
        '((display-buffer-at-bottom
          display-buffer-in-side-window)
          (side . bottom)
          (slot . 49)
          (window-height . 0.4)
          (body-function . select-window))))

(eval-when-compile
  (eval-after-load 'advice
    `(setq ad-redefinition-action 'accept))
  (setq use-package-verbose nil
        use-package-compute-statistics nil
        ;use-package-ignore-unknown-keywords t
        use-package-minimum-reported-time 0.01
        use-package-expand-minimally t
        use-package-enable-imenu-support t)
  (when init-file-debug
    (setq use-package-expand-minimally nil
          use-package-verbose t
          use-package-compute-statistics t
          debug-on-error t))
  (require 'use-package))

;; Note: Mostly copied from Karthinks Emacs Config 

;; Avoid defining multiple paths and make it messy
(use-package emacs
  :ensure nil
  :config
  (defun dir-concat (dir file)
    "join path DIR with filename FILE correctly"
    (concat (file-name-as-directory dir) file))

  ;; Adds ~/.emacs.d to the load-path
  (push (dir-concat user-emacs-directory "plugins/") load-path)
  (push (dir-concat user-emacs-directory "lisp/") load-path)
  (defvar user-cache-directory "~/.cache/emacs/"
  "Location where files created by emacs are placed."))

;; Optimizations to make Emacs more responsive.
(require 'setup-core)

;; Trying out [[https://gitlab.com/koral/gcmh][gcmh]] on an experimental basis.
(condition-case-unless-debug nil 
    (use-package gcmh
      :defer 2.5
      :ensure t
      ;; :hook (after-init . gcmh-mode)
      :config
      (defun gcmh-register-idle-gc ()
        "Register a timer to run `gcmh-idle-garbage-collect'.
Cancel the previous one if present."
        (unless (eq this-command 'self-insert-command)
          (let ((idle-t (if (eq gcmh-idle-delay 'auto)
		            (* gcmh-auto-idle-delay-factor gcmh-last-gc-time)
		          gcmh-idle-delay)))
            (if (timerp gcmh-idle-timer)
                (timer-set-time gcmh-idle-timer idle-t)
              (setf gcmh-idle-timer
	            (run-with-timer idle-t nil #'gcmh-idle-garbage-collect))))))
      (setq gcmh-idle-delay 'auto  ; default is 15s
            gcmh-high-cons-threshold (* 256 1024 1024)
            gcmh-verbose nil
            gc-cons-percentage 0.2)
      (gcmh-mode 1))
  (error (setq gc-cons-threshold (* 32 1024 1024)
               gc-cons-percentage 0.2)))


;; * DAEMON
;;;################################################################

;; Hack: When starting a server, silently load all the "heavy" libraries and
;; goodies you use. 

(when (daemonp)
  (defvar pulse-flag t)
  (add-hook
   'after-init-hook
   (defun my/load-packages-eagerly ()
     (add-hook 'server-visit-hook
               (lambda () (when (and (equal default-directory
                                       temporary-file-directory)
                                (equal major-mode 'text-mode)
                                (fboundp 'markdown-mode))
                       (markdown-mode))))
     (run-at-time 1 nil
                  (lambda () 
                    (when (fboundp 'pdf-tools-install) (pdf-tools-install t))
                    (load-library "pulse")
                    (when (string-suffix-p "server" server-name)
                      (let ((after-init-time (current-time)))
                         (dolist (lib '("org" "ob" "ox" "ol" "org-roam"
                                       "org-capture" "org-agenda" "latex" "reftex" "cdlatex"
                                       "consult" "helpful" "elisp-mode"
                                       "expand-region" "embrace"
                                       "ace-window" "avy" "yasnippet"
                                       "magit" "modus-themes" "diff-hl"
                                       "dired" "ibuffer" "pdf-tools"
                                       "emacs-wm"))
                          (with-demoted-errors "Error: %S" (load-library lib)))
                         (with-temp-buffer (org-mode))
                        (let ((elapsed (float-time (time-subtract (current-time)
                                                                  after-init-time))))
                          (message "[Pre-loaded packages in %.3fs]" elapsed)))))))))


(use-package org
  :defer t
  :ensure `(org
            :remotes ("tecosaur"
                      :repo "https://git.tecosaur.net/tec/org-mode.git"
                      :branch "dev")
            :files (:defaults ("etc/styles/" "etc/styles/*" "doc/*.texi"))
            :build t
            :pre-build
            (progn
              (with-temp-file "org-version.el"
               (require 'lisp-mnt)
               (let ((version
                      (with-temp-buffer
                        (insert-file-contents "lisp/org.el")
                        (lm-header "version")))
                     (git-version
                      (string-trim
                       (with-temp-buffer
                         (call-process "git" nil t nil "rev-parse" "--short" "HEAD")
                         (buffer-string)))))
                (insert
                 (format "(defun org-release () \"The release version of Org.\" %S)\n" version)
                 (format "(defun org-git-version () \"The truncate git commit hash of Org mode.\" %S)\n" git-version)
                 "(provide 'org-version)\n")))
              (require 'elpaca-menu-org)
              (elpaca-menu-org--build))
            :pin nil))

;;;################################################################
;; * UI
;;;################################################################

;; Miscellaneous UI preferences.
(require 'setup-ui)

(elpaca-wait)

;;;################################################################
;; * SAVE AND BACKUP
;;;################################################################
;; Put backups elsewhere:
(setq auto-save-interval 2400)
(setq auto-save-timeout 300)
(setq auto-save-list-file-prefix
      (dir-concat user-cache-directory "auto-save-list/.saves-"))
(setq backup-directory-alist
      `(("." . ,(dir-concat user-cache-directory "backup")))
      backup-by-copying t ; Use copies
      version-control t ; Use version numbers on backups
      delete-old-versions t ; Automatically delete excess backups
      kept-new-versions 10 ; Newest versions to keep
      kept-old-versions 5 ; Old versions to keep
      )

;;;################################################################
;; * MISCELLANEOUS PREFERENCES
;;;################################################################
;; For lazy typists
(setq use-short-answers t)
;; Move the mouse away if the cursor gets close
;; (mouse-avoidance-mode 'animate)

;; highlight the current line, as in Matlab
;; (global-hl-line-mode)

;; Confirm when killing Emacs
;; (setq confirm-kill-emacs (lambda (prompt)
;;                            (y-or-n-p-with-timeout prompt 2 nil)))

(setq uniquify-buffer-name-style 'forward)

(setq show-paren-delay 0.1
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t)

;; Underline looks a bit better when drawn lower
(setq x-underline-at-descent-line t)

;; FULLSCREEN
(global-set-key [C-f11] 'toggle-frame-fullscreen)

;; Frame title
(setq frame-title-format
      '(""
        (:eval
         (if (and (boundp 'org-roam-directory)
              (string-match-p org-roam-directory (or buffer-file-name "")))
             (replace-regexp-in-string
              ".*/[0-9]*-?" "roam:"
              (subst-char-in-string ?_ ?  buffer-file-name))
           "%b"))))

;; Byte-compile elisp files immediately after saving them if .elc exists:
(defun auto-byte-recompile ()
  "If the current buffer is in `emacs-lisp-mode' and there
  already exists an `.elc' file corresponding to the current
  buffer file, then recompile the file."
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
             (not ;; (string= user-init-file (buffer-file-name))
              (string-match-p "init\\.el$" (buffer-file-name)))
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-recompile-file buffer-file-name)))
(add-hook 'after-save-hook 'auto-byte-recompile)
(add-hook 'kill-emacs-hook (lambda () (byte-recompile-file user-init-file)))
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "zen-browser"
      browse-url-default-program "zen-browser"
      org-html-htmlize-output-type 'inline-css
      org-html-htmlize-browser "zen-browser")

;;;######################################################################
;; * INTERFACING WITH THE OS
;;;######################################################################

(if IS-WINDOWS
    (setq shell-file-name "C:/cygwin/cygwin.bat"))


;;;######################################################################
;; * LINE NUMBERS
;;;######################################################################
(global-display-line-numbers-mode -1)

(line-number-mode 1)

(defvar my/addons-enabled-modes (list 'prog-mode-hook
                                    'conf-unix-mode-hook
                                    'conf-windows-mode-hook
                                    'conf-javaprop-mode-hook
                                    'tex-mode-hook
                                    'text-mode-hook
                                    'message-mode-hook)
  "List of modes where special features (like line numbers)
  should be enabled.")

(dolist (mode-hook my/addons-enabled-modes)
  (add-hook mode-hook (lambda () "Turn on line numbers for major-mode"
                        (interactive)
                        (display-line-numbers-mode))))

(setq display-line-numbers-width-start t
      display-line-numbers-type t)

;; * MANAGE STATE
;; ** RECENTF
;; Keep track of recently opened files. Also feeds into the list of recent
;; directories used by consult-dir.
(use-package recentf
  :ensure nil
  :config
  (setq recentf-save-file (expand-file-name "recentf" user-cache-directory)
        recentf-max-saved-items 200
        recentf-auto-cleanup 300
          recentf-exclude
        '("~/.cache/emacs/"
          "~/.emacs.d/snippets/"
          "^/usr/share/emacs/"
          "/mnt/Karna/Git/Project-K/Org/Journal/"
          "/mnt/Karna/Git/Project-K/Org/Templates/"
          "/mnt/Karna/Git/Project-K/Org/Tasks.org"
          "_region_\\.tex$"
          "<none>\\.tex$"
          "^/tmp/"
          "\\.gz$"))
  (define-advice recentf-cleanup (:around (fun) silently)
    (let ((inhibit-message t)
          (message-log-max nil))
      (funcall fun)))
  (recentf-mode 1))

;; ** SAVEHIST
;; Save history across various prompts
(use-package savehist
  :ensure nil
  :defer 3
  :hook (after-init . savehist-mode)
  :config
  (setq savehist-file (dir-concat user-cache-directory "savehist")
        history-length 1000
        history-delete-duplicates t
        savehist-save-minibuffer-history t)
  (add-to-list 'savehist-additional-variables 'global-mark-ring))


;;;################################################################
;; * WINDOW MANAGEMENT
;;;################################################################

(require'buffer-move)

;;;----------------------------------------------------------------
;; ** POPPER
;;;----------------------------------------------------------------
;; https://github.com/karthink/popper
;; https://youtu.be/E-xUNlZi3rI?si=56ebEfcqFmbi9oyi

(use-package popper
  :ensure t
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (if (boundp 'elpaca-after-init-hook)
      (add-hook 'elpaca-after-init-hook #'popper-mode)
    (add-hook 'emacs-startup-hook #'popper-mode))
  (setq popper-reference-buffers
        '("^\\*Messages\\*"
          "[Oo]utput\\*$"
          "\\*Async Shell Command\\*"
          "^\\*Backtrace\\*"
          "\\*Completions\\*"
          "^\\*Compile-Log\\*"
          "^\\*Warnings\\*"
          "\\*Shell Command Output\\*"
          "^\\*evil-registers\\*"
          "^\\*gptel-ask\\*"
          "^Calc:"
          "^\\*ielm\\*"
          "^\\*TeX Help\\*"
          help-mode
          Custom-mode 
          messages-mode 
          occur-mode
          emacs-news-mode
          compilation-mode))
  :config
  (setq popper-group-function #'popper-group-by-project)
  (setq popper-mode-line '(:eval (propertize " POP" 'face 'mode-line-emphasis)))
  (setq popper-reference-buffers
        (append popper-reference-buffers
                '("^\\*eshell.*\\*$" eshell-mode
                  "^\\*shell.*\\*$"  shell-mode
                  "^\\*term.*\\*$"   term-mode
                  "^\\*vterm.*\\*$"  vterm-mode)))
  (popper-mode +1)
  (popper-echo-mode +1)) ;; For echo area hints

;; EVIL MODE 
(require 'setup-evil)
(require 'setup-consult)

;;----------------------------------------------------------------
;; ** BOOKMARKS
;;----------------------------------------------------------------
(use-package bookmark
  :ensure nil
  :defer 3
  :config
  (setq bookmark-default-file (dir-concat user-cache-directory "bookmarks")))

;;----------------------------------------------------------------
;; ** PROJECTS
;;----------------------------------------------------------------
(require 'setup-git)
(require 'setup-gptel)
(require 'projects)
(require 'setup-dired)

;;----------------------------------------------------------------
;; ** MIXED-PITCH-MODE
;;;----------------------------------------------------------------
(use-package mixed-pitch
  :ensure t
  :defer t
  :config
  (dolist (face '(line-number org-property-value org-drawer
                  error org-cite corfu-current corfu-default
                  org-meta-line org-tag))
    (add-to-list 'mixed-pitch-fixed-pitch-faces face))
  (setq mixed-pitch-set-height nil
        mixed-pitch-variable-pitch-cursor nil)
  (defun my/mixed-pitch-spacing ()
    (if mixed-pitch-mode
        (setq line-spacing 0.12)
      (setq line-spacing 0.0))))


;;;################################################################I
;; * FONTS AND COLORS
;;;################################################################
(use-package cus-face
  :ensure nil
  :config
  (pcase-dolist (`(,font           . ,scale)
                 '(("Merriweather" . 0.88)
                   ("IM FELL"      . 1.19)
                   ;; ("Latin Modern$" . 1.05)
                   ("Latin Modern Math" . 1.25)))
    (setf (alist-get font face-font-rescale-alist nil nil #'equal)
          scale))
  
  (cond (IS-LINUX
         (set-fontset-font t 'unicode "Symbola" nil 'prepend)
         (pcase-let ((`(,vp ,fp)
                      (cond
                       ((string= (getenv "XDG_SESSION_TYPE") "wayland")
                        '(120 110))
                       (t '(95 110)))))
           (custom-set-faces
            `(variable-pitch ((t (:family "Merriweather" ;; :height ,vp
                                  :width semi-expanded))))
            `(default ((t (:family "JetBrainsMono Nerd Font" :foundry "PfEd"
                           :slant normal :weight normal
                           :height ,fp :width normal)))))))
        (IS-WINDOWS
         (custom-set-faces
          '(default ((t (:family "Consolas" :foundry "outline"
                                 :slant normal :weight normal
                                 :height 120 :width normal))))))))

;; Italics for comments & keywords
(set-face-attribute 'font-lock-comment-face nil :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil :slant 'italic)
(set-display-table-slot standard-display-table 'truncation (make-glyph-code ?…))
(set-display-table-slot standard-display-table 'wrap (make-glyph-code ?–))

;;----------------------------------------------------------------
;; ** MODUS THEMES
;;----------------------------------------------------------------

(use-package ef-themes
  :ensure t
  :init
  ;; Define available themes
  (defvar karna/ef-theme-list '(ef-cyprus ef-winter ef-light)
    "List of EF themes to cycle through.")

  ;; Ensure we have a default theme at startup
  (defvar karna/current-ef-theme 'ef-light
    "Current EF theme in use.")

  :config
  (setq ef-themes-headings
        '((0 . (1.50))
          (1 . (1.28))
          (2 . (1.22))
          (3 . (1.17))
          (4 . (1.14))
          (t . (1.1))))

  (defun my/ef-themes-extra-faces ()
    "Tweak the style of the mode lines."
    (ef-themes-with-colors
      (custom-set-faces
       `(aw-leading-char-face ((,c :foreground ,fg-mode-line
                                :height 1.5 :weight semi-bold))))))

  (defun karna/toggle-ef-theme ()
    "Cycle through `karna/ef-theme-list`."
    (interactive)
    (setq karna/current-ef-theme
          (or (cadr (member karna/current-ef-theme karna/ef-theme-list))
              (car karna/ef-theme-list)))
    (ef-themes-select karna/current-ef-theme)
    (message "Switched to %s" karna/current-ef-theme))

  ;; Explicitly load the default theme on startup
  (ef-themes-select karna/current-ef-theme)

  ;; Apply extra face settings after theme load
  (add-hook 'ef-themes-post-load-hook #'my/ef-themes-extra-faces))


;; Protesilaos Stavrou's excellent high contrast themes, perfect for working in
;; bright sunlight (especially on my laptop's dim screen).
(use-package modus-themes
  :ensure t
  :defer
  :init
  (setq modus-themes-common-palette-overrides
        `((date-common cyan)   ; default value (for timestamps and more)
          (date-deadline red-warmer)
          (date-event magenta-warmer)
          (date-holiday blue) ; for M-x calendar
          (date-now yellow-warmer)
          (date-scheduled magenta-cooler)
          (date-weekday cyan-cooler)
          (date-weekend blue-faint)
          (mail-recipient fg-main)
          ;; (fg-heading-1 blue-warmer)
          ;; (fg-heading-2 yellow-cooler)
          ;; (fg-heading-3 cyan-cooler)
          ;; (fg-line-number-inactive "gray50")
          (fg-line-number-active fg-main)
          (bg-line-number-inactive unspecified)
          (bg-line-number-active unspecified)
          (bg-region bg-sage)
          (fg-region unspecified)
          ;; (comment yellow-cooler)
          ;; (string green-cooler)
          (fringe unspecified) ;; bg-blue-nuanced
          (border-mode-line-active unspecified)
          (border-mode-line-inactive unspecified)))
  (setq modus-operandi-palette-overrides
        '((bg-mode-line-active bg-blue-intense) ;
          (fg-mode-line-active fg-main)
          (fg-heading-1 "#a01f64")
          (fg-heading-2 "#2f5f9f") ;;"#193668"
          (fg-heading-3 "#1a8388")))
  (setq modus-vivendi-palette-overrides
        `((fg-main "#d6d6d4")
          ;; (bg-main "#121212")
          (bg-region bg-lavender)
          (bg-main "#090909")
          (fg-heading-1 magenta-faint)
          ;; (bg-main "#181A1B")
          (bg-mode-line-active bg-lavender) ;; bg-graph-magenta-1
          (fg-mode-line-active "#ffffff")))
  (setq modus-themes-org-blocks 'gray-background
        modus-themes-bold-constructs t
        modus-themes-prompts '(bold background)
        modus-themes-variable-pitch-ui nil
        modus-themes-headings
        '((0 . (1.35))
          (1 . (1.30))       ;variable-pitch
          (2 . (1.24))       ;variable-pitch
          (3 . (semibold 1.17))
          (4 . (1.14))
          (t . (monochrome)))))

;;;################################################################
;; * KEYBIND SETUP
;;;################################################################
(require 'setup-keybinds)

;;;################################################################
;; * MODELINE
;;;################################################################
(require 'setup-modeline)

(use-package wakatime-mode
  :ensure t
  :config
  (when (executable-find "wakatime-cli")
    (global-wakatime-mode)))


;;;################################################################
;; * EDITING
;;;######################################################################
(use-package iedit
  :ensure t
  :defer
  :bind (("C-M-;" . iedit-mode)
         ("M-s n" . my/iedit-1-down)
         ("M-s p" . my/iedit-1-up))
  :config
  (defun my/iedit-1-down (arg)
    (interactive "p")
    (let ((current-prefix-arg '(1)))
      (call-interactively #'iedit-mode)
      (iedit-expand-down-to-occurrence)))
  (defun my/iedit-1-up (arg)
    (interactive "p")
    (let ((current-prefix-arg '(1)))
      (call-interactively #'iedit-mode)
      (iedit-expand-up-to-occurrence))))

;; IMENU

(use-package imenu
  :ensure nil
  :defer t
  :hook (imenu-after-jump . my/imenu-show-entry)
  :bind ("M-s i" . imenu)
  :config
  (setq imenu-use-markers t
        imenu-auto-rescan t
        imenu-max-item-length 100
        imenu-use-popup-menu nil
        imenu-eager-completion-buffer t
        imenu-space-replacement " "
        imenu-level-separator "/")

  (declare-function org-at-heading-p "org")
  (declare-function org-show-entry "org")
  (declare-function org-reveal "org")
  (declare-function outline-show-entry "outline")

  (defun my/imenu-show-entry ()
    "Reveal index at point after successful `imenu' execution.
To be used with `imenu-after-jump-hook' or equivalent."
    (cond
     ((and (eq major-mode 'org-mode)
           (org-at-heading-p))
      (org-show-entry)
      (org-reveal t))
     ((bound-and-true-p prot-outline-minor-mode)
      (outline-show-entry)))))

;; Scratch 
(use-package scratch
  :ensure t
  :defer t
  :config
  (defun my/scratch-buffer-setup ()
    "Add contents to `scratch' buffer and name it accordingly.
If region is active, add its contents to the new buffer."
    (let* ((mode major-mode))
      (rename-buffer (format "*Scratch for %s*" mode) t)))
  (setf (alist-get "\\*Scratch for" display-buffer-alist nil nil #'equal)
        '((display-buffer-same-window)))
  :hook (scratch-create-buffer . my/scratch-buffer-setup)
  :bind ("C-c s" . scratch))

(defun delete-window-if-not-single ()
  "Delete window if not the only one."
  (when (not (one-window-p))
    (delete-window)))

(require 'editor)

;;;################################################################
;; * UTILITY
;;;################################################################
;; Count words, print ASCII table, etc
(require 'utilities)

;;;###autoload
(defun describe-word (word &optional prefix)
  "Briefly describe WORD entered by user. With PREFIX argument,
  show verbose descriptions with hyperlinks."
  (interactive "sDescribe word: \nP")
  (shell-command (concat "dict " word (cond ((null prefix) nil)
                                            (t " -v")))))

;;;###autoload
(defun describe-word-at-point (&optional prefix)
  "Briefly describe word at point. With PREFIX argument, show
  verbose descriptions with hyperlinks."
  (interactive "P")
  (let ( (word
          (if (region-active-p)
              (buffer-substring (region-beginning)
                                (region-end))
            (thing-at-point 'word))) )
    (shell-command (concat "dict " (cond ((null prefix) nil)
                                         (t "-f "))
                           word))))

(use-package async
  :defer t
  :hook (package-menu-mode . my/async-bytecomp-ensure)
  :config
  (defun my/async-bytecomp-ensure ()
    (async-bytecomp-package-mode 1))
  :init
  (with-eval-after-load 'dired
    (dired-async-mode 1)))

;;;################################################################
;; * PROGRAMMING
;;;################################################################

;; ** CALC
(require 'setup-calc)

;;;################################################################
;; ** UNDO HISTORY

;; The =undo-fu-session= package saves and restores the undo states of buffers
;; across Emacs sessions.
(use-package undo-fu
  :ensure t
  :defer t
  :config
  (setq undo-fu-allow-undo-in-region t  ;; Allow undo in active region
        undo-fu-ignore-keyboard-quit t) ;; Prevent undo reset on `C-g`
  
  ;; Define simple undo/redo functions
  (defun karna/undo ()
    "Perform an undo using `undo-fu`."
    (interactive)
    (undo-fu-only-undo))
  
  (defun karna/redo ()
    "Perform a redo using `undo-fu`."
    (interactive)
    (undo-fu-only-redo)))

(use-package undo-fu-session
  :ensure t
  :defer t
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")
        undo-fu-session-directory (dir-concat user-cache-directory "undo-fu-session/")) ;; Store undo history in cache
  :hook ((prog-mode conf-mode text-mode tex-mode) . undo-fu-session-mode))

;;;----------------------------------------------------------------
;; ** EGLOT - LSP
;;;----------------------------------------------------------------
(require 'setup-eglot)
(require 'setup-treesit)
(require 'setup-minibuffer) 
(require 'setup-orderless)

;; LATEX 
(require 'setup-doc) ;; PDF TOOLS 
(require 'setup-latex)

(use-package quail
  :ensure nil
  :defer
  :commands my/cdlatex-input-tex
  :config
  (defun my/cdlatex-input-tex ()
  (interactive)
  (require 'cdlatex nil t)
  (let ((cim current-input-method))
    (unless (equal cim "TeX")
      (activate-input-method "TeX"))
    (cl-letf (((symbol-function 'texmathp)
               (lambda () t))
              ((symbol-function 'insert)
               (lambda (symbol)
                 (setq unread-input-method-events
                       (nconc (quail-input-string-to-events symbol)
                              (list 0))))))
      (cdlatex-math-symbol))
    (unless (equal cim "TeX")
      (run-at-time 0 nil (lambda () (activate-input-method cim)))))))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/plugins/Emacs-TeQ/"))

(register-input-method
 "TeQ-Math" "Emacs-Teq-Latex" 'quail-use-package
 "TeQ-" "TeQ-Math input"
 "Emacs-TeQ.el")

;;;----------------------------------------------------------------
;; ** CORFU + CAPE + WORKSPACE
;;;----------------------------------------------------------------
(require 'setup-yas)
(require 'setup-corfu)
(require 'setup-cape)
(require 'workspace)
;; (require 'setup-completions-default)

(setq tab-always-indent 'complete)

;;;----------------------------------------------------------------
;; ** OLIVETTI
;;----------------------------------------------------------------
(use-package olivetti
  :commands (my/olivetti-mode)
  :ensure t
  :defer
  :config
  (setq-default
   olivetti-body-width 180
   olivetti-minimum-body-width 76
   olivetti-recall-visual-line-mode-entry-state t)

  (define-minor-mode my/olivetti-mode
    "Toggle buffer-local `olivetti-mode' with additional parameters.

Fringes are disabled. The modeline is hidden, except for
`prog-mode' buffers (see `my/mode-line-hidden-mode'). The default
typeface is set to a proportionately spaced family, except for
programming modes (see `my/variable-pitch-mode'). The cursor
becomes a blinking bar. Evil-mode (if bound) is disabled."
    :init-value nil
    :global nil
    (if my/olivetti-mode
        (progn
          (olivetti-mode 1)
          (set-window-fringes (selected-window) 0 0)
          (unless (derived-mode-p 'prog-mode)
            ;; (my/mode-line-hidden-mode 1)
            (mixed-pitch-mode 1))
          (if (bound-and-true-p evil-mode)
              (evil-emacs-state))
          ;; (setq-local line-spacing 0.16)
          ;; (setq-local cursor-type '(bar . 2))
          )
      (olivetti-mode -1)
      (set-window-fringes (selected-window) nil) ; Use default width
      (mixed-pitch-mode -1)
      (kill-local-variable 'line-spacing)
      ;; (unless (derived-mode-p 'prog-mode)
      ;;   (my/mode-line-hidden-mode -1))
      (when (and (bound-and-true-p evil-mode)
                 (evil-emacs-state-p))
        (evil-exit-emacs-state))
      (kill-local-variable 'cursor-type)))

  (define-minor-mode my/reader-mode
    "Mode to read a buffer in style. Pop it out into a frame,
turn on `view-mode', and `my/olivetti-mode', which in turn hides
the mode-line and switches to `variable-pitch-mode'."
    :init-value
    :global-nil
    (if my/reader-mode
        (progn
          (make-frame '((name . "dropdown_reader")))
          (my/olivetti-mode 1)
          (view-mode 1)
          (if (equal major-mode 'org-mode)
              (org-show-all)))
      (view-mode -1)
      (my/olivetti-mode -1)
      (delete-frame)))

  :bind
  ("C-c O" . my/olivetti-mode)
  ("C-c R" . my/reader-mode))

;; ORG MODE 

(require 'setup-org)
(require 'setup-roam)
(require 'org-latex-check-health)
(require 'setup-calendar)
(require 'setup-md)

;;;----------------------------------------------------------------
;; ** SHELL AND ESHELL PREFERENCES
;;;----------------------------------------------------------------
;; Settings for shell, eshell and vterm
(require 'shells)
(require 'eshell-prompt)

;; compile!
(use-package compile
  :ensure nil
  :defer t
  :hook (compilation-filter . ansi-color-compilation-filter)
  :config
  (setq compilation-always-kill t
        compilation-ask-about-save nil
        compilation-scroll-output 'first-error)
  (global-set-key [(f9)] 'compile))

;;;################################################################
;; * CUSTOM FILE
;;;################################################################

;; Don't populate the init file with custom-set-variables, create and use a
;; separate file instead.
(use-package cus-edit
  :ensure nil
  :config
  ;; Get custom-set-variables out of init.el
  (defvar my/custom-file (dir-concat user-emacs-directory "custom.el"))
  (setq custom-file my/custom-file)

  (defun my/cus-edit ()
    (let ((file my/custom-file))
      (unless (file-exists-p file)
        (make-empty-file file))
      (load-file file)))
  :hook (after-init . my/cus-edit))

;; ----------------------------------------------------------------------------
;; DASHBOARD CONFIGURATION
;; ----------------------------------------------------------------------------

(use-package dashboard
  :disabled
  :ensure t
  :init
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*"))
	dashboard-set-heading-icons t
	dashboard-set-file-icons t
	dashboard-display-icons-p t
	dashboard-icon-type 'nerd-icons
	dashboard-show-shortcuts nil
	dashboard-projects-backend 'project-el
	dashboard-banner-logo-title "I'll Walk My Own Path!"
	dashboard-startup-banner "~/.emacs.d/assets/emacs.png"
	dashboard-center-content t
	dashboard-items '((vocabulary)
			  (recents . 5)
			  (agenda . 5)
			  (bookmarks . 10)
			  (projects . 5))
	dashboard-startupify-list '(dashboard-insert-banner
				    dashboard-insert-newline
				    dashboard-insert-banner-title
				    dashboard-insert-newline
				    dashboard-insert-init-info
				    dashboard-insert-items)
	dashboard-item-generators '((vocabulary . karna/dashboard-insert-vocabulary)
				    (recents . dashboard-insert-recents)
				    (bookmarks . dashboard-insert-bookmarks)
				    (agenda . dashboard-insert-agenda)
				    (projects . dashboard-insert-projects)))

  (defun karna/dashboard-insert-vocabulary (_list-size)
    "Insert a 'Word of the Day' section in the dashboard."
    (dashboard-insert-heading " Word of the Day:"
			      nil
			      (all-the-icons-faicon "newspaper-o"
						    :height 1.2
						    :v-adjust 0.0
						    :face 'dashboard-heading))
    (insert "\n")
    (when (file-exists-p (concat user-emacs-directory "assets/words"))
      (let* ((lines (with-temp-buffer
		      (insert-file-contents (concat user-emacs-directory "assets/words"))
		      (split-string (buffer-string) "\n" t)))
	     (random-line (when lines (nth (random (length lines)) lines))))
	(when random-line
	  (insert "    " (string-join (split-string random-line) " "))))))

  :config
  (dashboard-setup-startup-hook)
  (add-hook 'dashboard-mode-hook (lambda () (display-line-numbers-mode -1))))

;; Dashboard Agenda Customizations
(setq dashboard-agenda-tags-format 'ignore
      dashboard-agenda-prefix-format "%i %s  "
      dashboard-agenda-item-icon "󰸗") ;; Nerd Font calendar icon
