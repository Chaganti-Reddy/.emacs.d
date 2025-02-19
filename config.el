;;; $EMACSDIR/config.el -*- lexical-binding: t; -*-

(add-to-list 'load-path "~/.emacs.d/lisp/") ; add scripts dir to load path
(require 'elpaca-setup)  ;; The Elpaca Package Manager
(require 'buffer-move)   ;; Buffer-move for better window management
(require 'eshell-prompt) ;; A fancy prompt for eshell

;;; STARTUP HOOKS

(add-hook 'after-init-hook #'global-auto-revert-mode) ;; Auto-revert in Emacs is a feature that automatically updates the contents of a buffer to reflect changes made to the underlying file on disk.

(add-hook 'after-init-hook #'recentf-mode) ;; recentf is an Emacs package that maintains a list of recently  accessed files, making it easier to reopen files you have worked on recently.

(add-hook 'after-init-hook #'savehist-mode) ;; savehist is an Emacs feature that preserves the minibuffer history between sessions.

(add-hook 'after-init-hook #'save-place-mode) ;; save-place is an Emacs package that allows you to remember the position of the cursor when opening a file.

;;; UI TWEAKINGS

(use-package emacs
  :ensure nil
  :demand t
  :custom
  ;; Basic UI & Behavior
  (inhibit-startup-screen t)
  (user-full-name "Karna")
  (frame-inhibit-implied-resize t)
  (frame-resize-pixelwise t)

  ;; Auto-Reverting & Encoding
  (locale-coding-system 'utf-8)
  (ring-bell-function 'ignore)
  (display-time-default-load-average nil)

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
  ;; Enable whitespace-mode for programming buffers
  ;; (prog-mode . whitespace-mode)
  ;; Clean up whitespace before saving any buffer
  (before-save . whitespace-cleanup)
  ;; For comint buffers, disable query on exit for the associated process
  (comint-exec . (lambda ()
		   (set-process-query-on-exit-flag
		    (get-buffer-process (current-buffer)) nil)))

  :config
  ;;----------------------------------------------------------------------------
  ;; Basic UI & Behavior
  ;;----------------------------------------------------------------------------
  (delete-selection-mode 1)           ;; Allow deletion of selected text
  (electric-indent-mode -1)           ;; Disable auto-indenting
  (electric-pair-mode 1)              ;; Automatically insert matching parens
  (global-display-line-numbers-mode 1) ;; Enable line numbers globally
  (global-visual-line-mode 1)         ;; Enable visual line wrapping
  (menu-bar-mode -1)                  ;; Disable the menu bar
  (scroll-bar-mode -1)                ;; Disable the scroll bar
  (column-number-mode t)              ;; Show column number in mode line
  (size-indication-mode t)            ;; Show file size in mode line
  (fringe-mode -1)                    ;; Remove UI fringes
  (tool-bar-mode -1)                  ;; Disable the tool bar
  (global-hl-line-mode -1)
  (setq init-start-time (current-time))

  ;;----------------------------------------------------------------------------
  ;; Auto-Reverting & Encoding
  ;;----------------------------------------------------------------------------
  (global-auto-revert-mode t)         ;; Auto-refresh buffers when files change
  (setq global-auto-revert-non-file-buffers t)

  ;; Call these functions so that terminal/keyboard/selection use UTF-8
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)

  ;;----------------------------------------------------------------------------
  ;; Appearance Tweaks
  ;;----------------------------------------------------------------------------
  (add-to-list 'default-frame-alist '(alpha-background . 100)) ;; Set frame transparency
  (global-prettify-symbols-mode 1)      ;; Display certain keywords as symbols

  ;;----------------------------------------------------------------------------
  ;; Miscellaneous Settings
  ;;----------------------------------------------------------------------------
  (defalias 'yes-or-no-p 'y-or-n-p)      ;; Use y/n instead of yes/no prompts
  (pixel-scroll-precision-mode 1)       ;; Enable precise pixel scrolling

  ;; History & Minibuffer
  (savehist-mode 1)
  (add-to-list 'savehist-additional-variables 'global-mark-ring)

  ;;----------------------------------------------------------------------------
  ;; Whitespace & Escape Key Behavior
  ;;----------------------------------------------------------------------------
  (global-set-key [escape] 'keyboard-escape-quit))

;; --- Speed benchmarking -----------------------------------------------------
(let ((init-time (float-time (time-subtract (current-time) init-start-time)))
      (total-time (string-to-number (emacs-init-time "%f"))))
  (message (concat
    (propertize "Startup time: " 'face 'bold)
    (format "%.2fs " init-time)
    (propertize (format "(+ %.2fs system time)"
			(- total-time init-time)) 'face 'shadow))))

;;; CUSTOM DIRECTORIES FOR CACHE FILES

(defun dir-concat (dir file)
  "join path DIR with filename FILE correctly"
  (concat (file-name-as-directory dir) file))

(defvar user-cache-directory "~/.cache/emacs/"
  "Location where files created by emacs are placed.") ;; Creating an emacs directory in ~/.cache for all the files that are created by emacs

(setq save-place-file (dir-concat user-cache-directory "places")) ;; Saving the place history in custom directory.
(setq transient-history-file (dir-concat user-cache-directory "transient-history.el")) ;; Saving transient-history file in ~/.cache/emacs/

(setq bookmark-default-file (dir-concat user-emacs-directory ".cache/bookmarks")) ;; Saving bookmarks in custom directory which is helped to display in emacs dashboard.

(setq auto-save-list-file-prefix (dir-concat user-cache-directory "auto-save-list/.saves-"))

(setq savehist-file (dir-concat user-cache-directory "savehist"))

(setq tramp-persistency-file-name (dir-concat user-cache-directory "tramp"))

(setq lsp-session-file (dir-concat user-cache-directory "lsp-session"))

(setq dap-breakpoints-file (dir-concat user-cache-directory "dap-breakpoints"))

(setq projectile-known-projects-file (dir-concat user-emacs-directory ".cache/projectile-bookmarks.eld"))

(setq eshell-directory-name (dir-concat user-cache-directory "eshell"))
(setq eshell-history-file-name (dir-concat user-cache-directory "eshell-history"))
(setq eshell-last-dir-ring-file-name (dir-concat user-cache-directory "eshell-last-dir-ring"))

(setq undo-fu-session-directory
	(dir-concat user-cache-directory "undo-fu-session/"))

(setq persp-state-default-file (dir-concat user-emacs-directory ".cache/sessions"))

;;; BROWSER SETTINGS

;; Set the default browser function and program
(setq browse-url-browser-function 'browse-url-generic)
(setq browse-url-generic-program "zen-browser")
(setq browse-url-default-program "zen-browser")

;; (Optional) If you're using Org mode and want to specify a browser for HTML export:
(setq org-html-htmlize-browser "zen-browser")

;; Configure file associations for Org mode
(setq org-file-apps
      '(("auto-mode" . emacs)
	("\\.mm\\'" . default)
	("\\.x?html?\\'" . "zen-browser %s")
	("\\.pdf\\'" . "~/.local/bin/zathura %s")))

;; (setcdr (assq 'system org-file-apps-defaults-gnu) "xdg-open %s")
(setcdr (assq 'system org-file-apps-gnu) "xdg-open %s")

(advice-add 'org-open-file :around
	    (lambda (orig-fun &rest args)
	      ;; Work around a weird problem with xdg-open.
	      (let ((process-connection-type nil))
		(apply orig-fun args))))

;;; RECENTF SETTINGS

(require 'recentf)

;; Configure recentf settings
(setq recentf-save-file (dir-concat user-cache-directory "recentf")
      recentf-max-saved-items 200
      recentf-auto-cleanup 180)

;; Exclude specific files from recentf tracking
(setq recentf-exclude
      '("~/.cache/emacs/"   ;; Exclude everything inside ~/.cache/emacs/
	"/mnt/Karna/Git/Project-K/Org/Tasks.org"
	"_region_\\.tex$"    ;; Exclude any _region_.tex file
	"^/tmp/"))           ;; Exclude everything inside /tmp/

;; Enable recentf mode
(recentf-mode 1)

;;; ASYNCHRONOUS PROCESS

(use-package async
  :ensure t
  :defer t
  :init (dired-async-mode 1))

;;; BACKUP SETTINGS

(setq backup-directory-alist '((".*" . "~/.local/share/Trash/files"))) ;; Default to Trash folder

;; Else you can also create a custom backup directory in ~/.cache/emacs and save the files versions wise

;;(setq backup-directory-alist
;;      `(("." . ,(dir-concat user-cache-directory "backup")))
;;      backup-by-copying t ; Use copies
;;      version-control t ; Use version numbers on backups
;;      delete-old-versions t ; Automatically delete excess backups
;;      kept-new-versions 5 ; Newest versions to keep
;;      kept-old-versions 3 ; Old versions to keep
;;      )

;;; GARBAGE COLLECTION

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

;;; NATIVE COMPILE SUPPRESSION

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

;;; BYTE COMPILATION

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

;;; EVIL MODE IN EMACS

;; Expands to: (elpaca evil (use-package evil :demand t))
(use-package evil
  :diminish
  :ensure t
  :init      ;; tweak evil's configuration before loading it
  (setq evil-want-integration t  ;; This is optional since it's already set to t by default.
	evil-want-keybinding nil
	evil-vsplit-window-right t
	evil-split-window-below t
	evil-undo-system 'undo-redo)  ;; Adds vim-like C-r redo functionality
  (evil-mode 1))

(use-package evil-collection
  :diminish
  :after evil
  :config
  ;; Do not uncomment this unless you want to specify each and every mode
  ;; that evil-collection should works with.  The following line is here
  ;; for documentation purposes in case you need it.
  ;; (setq evil-collection-mode-list '(calendar dashboard dired ediff info magit ibuffer))
  (add-to-list 'evil-collection-mode-list 'help) ;; evilify help mode
  (evil-collection-init))

(use-package evil-tutor :diminish)

;; Using RETURN to follow links in Org/Evil
;; Unmap keys in 'evil-maps if not done, (setq org-return-follows-link t) will not work
(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "SPC") nil)
  (define-key evil-motion-state-map (kbd "RET") nil)
  (define-key evil-motion-state-map (kbd "TAB") nil))
;; Setting RETURN key in org-mode to follow links
  (setq org-return-follows-link  t)

;;; WHICH KEY

(use-package which-key
  :init
    (which-key-mode 1)
  :diminish
  :config
  (setq which-key-side-window-location 'bottom
    which-key-sort-order #'which-key-key-order-alpha
    which-key-allow-imprecise-window-fit nil
    which-key-sort-uppercase-first nil
    which-key-add-column-padding 1
    which-key-max-display-columns nil
    which-key-min-display-lines 6
    which-key-side-window-slot -10
    which-key-side-window-max-height 0.25
    which-key-idle-delay 0.8
    which-key-max-description-length 25
    which-key-allow-imprecise-window-fit nil
    which-key-separator " → " ))

;;; EF THEMES

(use-package ef-themes
  :ensure t
  :config
  (defvar my/current-ef-theme 'ef-cyprus
    "Stores the current theme to toggle between `ef-cyprus` and `ef-winter`.")

  (defun my/toggle-ef-theme ()
    "Toggle between `ef-cyprus` and `ef-winter` themes."
    (interactive)
    (setq my/current-ef-theme (if (eq my/current-ef-theme 'ef-cyprus)
				  'ef-winter
				'ef-cyprus))
    (ef-themes-select my/current-ef-theme)
    (message "Switched to %s" my/current-ef-theme))

  ;; Load default theme
  (ef-themes-select my/current-ef-theme))

;;; FONTS

(add-to-list 'default-frame-alist '(font . "Roboto Mono Nerd Font-13:bold"))

(set-face-attribute 'default nil
		    :font "Roboto Mono Nerd Font"
		    :height 120
		    :weight 'bold)

(set-face-attribute 'fixed-pitch nil
		    :font "Roboto Mono Nerd Font"
		    :height 130
		    :weight 'bold)

(set-face-attribute 'variable-pitch nil
		    :font "Roboto Mono Nerd Font"
		    :height 120
		    :weight 'bold)

;; Italics for comments & keywords
(set-face-attribute 'font-lock-comment-face nil :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil :slant 'italic)

(setq-default line-spacing 0)

(set-display-table-slot standard-display-table 'truncation (make-glyph-code ?…))
(set-display-table-slot standard-display-table 'wrap (make-glyph-code ?–))

;;; ALL THE ICONS

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :ensure t
  :defer t
  :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))

(use-package all-the-icons-completion
  :ensure t
  :defer t
  :hook (marginalia-mode . #'all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package nerd-icons :defer t
  :custom
  (nerd-icons-color-icons t))

;;; EMACS DASHBOARD

;; Load dashboard instead of scratchpad at startup
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

;;(use-package page-break-lines :ensure t) ;; enable if you want horizontal lines between sections in dashboard.

(use-package dashboard
  :ensure t
  :init
  (setq initial-buffer-choice 'dashboard-open)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-display-icons-p t)
  (setq dashboard-icon-type 'nerd-icons)
  (setq dashboard-show-shortcuts nil)
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-banner-logo-title "I'll Walk My Own Path!")
  (setq dashboard-startup-banner 'logo)
  ;; (setq dashboard-startup-banner "~/.config/emacs/assets/emacs.png")
  (setq dashboard-center-content t)
  (setq dashboard-items '((vocabulary)
			  (recents . 5)
			  (agenda . 5)
			  (bookmarks . 10)
			  (projects . 5)))
  (setq dashboard-startupify-list '(dashboard-insert-banner
				    dashboard-insert-newline
				    dashboard-insert-banner-title
				    dashboard-insert-newline
				    dashboard-insert-init-info
				    dashboard-insert-items))
  (setq dashboard-item-generators '(
				    (vocabulary . gopar/dashboard-insert-vocabulary)
				    (recents . dashboard-insert-recents)
				    (bookmarks . dashboard-insert-bookmarks)
				    (agenda . dashboard-insert-agenda)
				    (projects . dashboard-insert-projects)))
  (defun gopar/dashboard-insert-vocabulary (list-size)
    (dashboard-insert-heading " Word of the Day:"
			      nil
			      (all-the-icons-faicon "newspaper-o"
						    :height 1.2
						    :v-adjust 0.0
						    :face 'dashboard-heading))
    (insert "\n")
    (let ((random-line nil)
	  (lines nil))
      (with-temp-buffer
	(insert-file-contents (concat user-emacs-directory "assets/words"))
	(goto-char (point-min))
	(setq lines (split-string (buffer-string) "\n" t))
	(setq random-line (nth (random (length lines)) lines))
	(setq random-line (string-join (split-string random-line) " ")))
      (insert "    " random-line)))
  :config
  (dashboard-setup-startup-hook)
  (add-hook 'dashboard-mode-hook
	    (lambda ()
	      (display-line-numbers-mode -1))))

;;; DOOM MODELINE

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom
  (inhibit-compacting-font-caches t)
  (doom-modeline-buffer-file-name-style 'relative-from-project)
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-minor-modes nil)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-buffer-state-icon nil)
  (doom-modeline-lsp nil)
  :hook (after-init . doom-modeline-mode)
  :config
    (setq doom-modeline-height 25      ;; sets modeline height
	  doom-modeline-bar-width 5    ;; sets right bar width
	  doom-modeline-persp-name t   ;; adds perspective name to modeline
	  doom-modeline-persp-icon t))

;;; DIMINISH

(use-package diminish
  :ensure t
  :defer t
  :init
  (diminish 'visual-line-mode)
  (diminish 'subword-mode)
  (diminish 'beacon-mode)
  (diminish 'irony-mode)
  (diminish 'page-break-lines-mode)
  (diminish 'rainbow-delimiters-mode)
  (diminish 'auto-revert-mode)
  (diminish 'yas-minor-mode)
)

;;; RAINBOW MODE

(use-package rainbow-mode
  :ensure t
  :defer t
  :diminish
  :hook org-mode prog-mode)

;;; RAINBOW DELIMITERS
(use-package rainbow-delimiters
  :ensure t
  :defer t
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
	 (clojure-mode . rainbow-delimiters-mode)))

;;; DRAG STUFF

(use-package drag-stuff
  :ensure t
  :defer t
  :diminish
  :init
  (drag-stuff-global-mode 1)
  (drag-stuff-define-keys))

;;; EDIFF SETUP

(setq ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)

(defun karna-ediff-hook ()
  (ediff-setup-keymap)
  (define-key ediff-mode-map "j" 'ediff-next-difference)
  (define-key ediff-mode-map "k" 'ediff-previous-difference))

(add-hook 'ediff-mode-hook 'karna-ediff-hook)

;;; GIT TIMEMACHINE

(use-package git-timemachine
  :after git-timemachine
  :defer t
  :hook (evil-normalize-keymaps . git-timemachine-hook)
  :config
    (evil-define-key 'normal git-timemachine-mode-map (kbd "C-j") 'git-timemachine-show-previous-revision)
    (evil-define-key 'normal git-timemachine-mode-map (kbd "C-k") 'git-timemachine-show-next-revision)
)

;;; MAGIT
;; Transient is required by Magit for handling popups and keybindings
(use-package transient
  :defer t)

;; Magit - A Git porcelain inside Emacs
(use-package magit
  :after transient  ;; Ensure transient is loaded first
  :defer t          ;; Load Magit when needed
  :custom
  (magit-show-long-lines-warning nil))  ;; Disable long lines warning in Magit

;;; SOME EXTRA STUFF
(use-package tldr :ensure t :defer t)

;; Use puni-mode globally and disable it for term-mode.
(use-package puni
  :defer t
  :init
  ;; The autoloads of Puni are set up so you can enable `puni-mode` or
  ;; `puni-global-mode` before `puni` is actually loaded. Only after you press
  ;; any key that calls Puni commands, it's loaded.
  (puni-global-mode)
  (add-hook 'term-mode-hook #'puni-disable-puni-mode))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; Removes whitespace from the ends of lines.
(use-package ws-butler
:init (ws-butler-global-mode))

;;; CALC

(use-package calc
  :ensure nil  ;; tells Elpaca not to manage calc because it's built-in
  :bind (("C-x c" . calc)
	 ("C-S-e" . latex-math-from-calc)
	 ("C-c e" . calc-embedded))
  :config
  (use-package calc-embed
    :ensure nil
    :bind (:map calc-override-minor-modes-map
	   ("'" . calc-algebraic-entry)))
  (use-package calc-yank
    :ensure nil
    :defer t
    :config
    (define-advice calc-finish-stack-edit (:around (orig-fn &rest args) pop-to-buffer)
      (cl-letf (((symbol-function 'switch-to-buffer)
		 #'pop-to-buffer))
	(apply orig-fn args))))
  (setq calc-make-windows-dedicated t)
  (defun latex-math-from-calc ()
    "Evaluate `calc' on the contents of line at point."
    (interactive)
    (let ((lang (if (memq major-mode '(org-mode latex-mode LaTex-mode))
		    'latex 'normal)))
      (cond ((region-active-p)
	     (let* ((beg (region-beginning))
		    (end (region-end))
		    (string (buffer-substring-no-properties beg end)))
	       (kill-region beg end)
	       (insert (calc-eval `(,string calc-language ,lang
					    calc-prefer-frac t
					    calc-angle-mode rad)))))
	    (t (let ((l (thing-at-point 'line)))
		 (end-of-line 1)
		 (kill-line 0)
		 (insert (calc-eval `(,l
				      calc-language ,lang
				      calc-prefer-frac t
				      calc-angle-mode rad)))))))))

;; If you need additional settings for calc after org-latex-preview, combine them into the same declaration:
(use-package calc
  :ensure nil
  :after org-latex-preview
  :hook (calc-mode . my/olp-calc-settings)
  :config
  (defun my/olp-calc-settings ()
    (setq-local org-latex-preview-numbered nil
		org-latex-preview-auto-ignored-commands
		'(mwheel-scroll pixel-scroll-precision
		  scroll-up-command scroll-down-command
		  scroll-other-window scroll-other-window-down))))

;;; PROJECTILE

(use-package projectile
  :ensure t
  :init
  ;; Set the search path for Projectile if the directory exists.
  (when (file-directory-p "/mnt/Karna/Git/Project-K")
    (setq projectile-project-search-path '("/mnt/Karna/Git/Project-K/")))
  ;; Define the action to take when switching projects.
  (setq projectile-switch-project-action #'projectile-dired)
  ;; :custom
  ;; Use Ivy for completion within Projectile.
  ;; (projectile-completion-system 'ivy)
  :config
  ;; Now that Projectile is loaded, append the extra directories to ignore.
  (setq projectile-globally-ignored-directories
	(append projectile-globally-ignored-directories
		'("node_modules" "dist" "build" "vendor" ".venv" "tmp" "cache" "log" "bower_components")))
  (projectile-mode 1))

;; (use-package counsel-projectile
;;   :ensure t
;;   :after (projectile counsel)  ;; Ensure Projectile and Counsel are loaded first.
;;   :config
;;   (counsel-projectile-mode 1))

(use-package consult-projectile
  :ensure (consult-projectile :type git :host gitlab :repo "OlMon/consult-projectile" :branch "master"))

;;; PERSPECTIVE

(use-package perspective
  :ensure t
  :custom
  ;; NOTE! I have also set 'SCP =' to open the perspective menu.
  ;; I'm only setting the additional binding because setting it
  ;; helps suppress an annoying warning message.
  (persp-mode-prefix-key (kbd "C-c M-p"))
  :config
  (persp-mode 1)
  ;; Sets a file to write to when we save states
)

;; This will group buffers by persp-name in ibuffer.
(add-hook 'ibuffer-hook
	  (lambda ()
	    (persp-ibuffer-set-filter-groups)
	    (unless (eq ibuffer-sorting-mode 'alphabetic)
	      (ibuffer-do-sort-by-alphabetic))))

;; Automatically save perspective states to file when Emacs exits.
;; (add-hook 'kill-emacs-hook #'persp-state-save)

(global-set-key (kbd "C-S-s") #'persp-state-save)

;;; VERTICO

(use-package vertico
  :init
  (vertico-mode)
  :hook
  ;; Tidy paths automatically in the minibuffer.
  (rfn-eshadow-update-overlay-hook . vertico-directory-tidy)
  :config
  ;; Load the directory extension once Vertico is loaded.
  (require 'vertico-directory)
  ;; Show more candidates
  ;(setq vertico-count 20)
  ;; Grow/shrink the minibuffer dynamically.
  (setq vertico-resize nil)
  ;; Enable cycling through candidates.
  (setq vertico-cycle t))

;;; MARGINALIA

(use-package marginalia
  :ensure t
  :defer t
  :bind (:map minibuffer-local-map
	 ("M-A" . marginalia-cycle))
  :custom
    (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init (marginalia-mode))

;;; CONSULT

(use-package consult
  :ensure t
  :bind (;; A recursive grep
	 ("M-s M-g" . consult-grep)
	 ;; Search for files names recursively
	 ("M-s M-f" . consult-fd)
	 ;; Search through the outline (headings) of the file
	 ("M-s M-o" . consult-outline)
	 ;; Search the current buffer
	 ("M-s M-l" . consult-line)
	 ;; Switch to another buffer, or bookmarked file, or recently
	 ;; opened file.
	 ("M-s M-b" . consult-buffer)))

;;; CONSULT DIR

(use-package consult-dir
 :ensure t
 :defer t
 :bind (("C-x C-d" . consult-dir)
 :map vertico-map
 ("C-x C-d" . consult-dir)
 ("C-x C-j" . consult-dir-jump-file)))

;; The `wgrep' packages lets us edit the results of a grep search
;; while inside a `grep-mode' buffer.  All we need is to toggle the
;; editable mode, make the changes, and then type C-c C-c to confirm
;; or C-c C-k to abort.
;; Further reading: https://protesilaos.com/emacs/dotemacs#h:9a3581df-ab18-4266-815e-2edd7f7e4852

(use-package wgrep
  :ensure t
  :bind ( :map grep-mode-map
	  ("e" . wgrep-change-to-wgrep-mode)
	  ("C-x C-q" . wgrep-change-to-wgrep-mode)
	  ("C-c C-c" . wgrep-finish-edit)))


;; Display a counter showing the number of the current and the other
;; matches.  Place it before the prompt, though it can be after it.
(setq isearch-lazy-count t)
(setq lazy-count-prefix-format "(%s/%s) ")
(setq lazy-count-suffix-format nil)

;; Make regular Isearch interpret the empty space as a regular
;; expression that matches any character between the words you give
;; it.
(setq search-whitespace-regexp ".*?")

;; Install the `wgrep' package.  It makes the grep buffers editable.

;; Install the `consult' package.  It provides lots of useful commands that
;; enhance the minibuffer experience of Emacs (e.g. for searching
;; lines/heading).

;; Install the `embark' package.  It allows you to perform context-sensitive
;; actions, using a prompt and then a key/action selection interface.

;;; ORDERLESS

(use-package orderless
  :ensure t
  :defer t
  :custom
  ;; (orderless-style-dispatchers '(orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(basic substring initials flex orderless))
  (completion-category-defaults nil)

  (setq completion-category-overrides
	;; NOTE 2021-10-25: I am adding `basic' because it works better as a
	;; default for some contexts.  Read:
	;; <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=50387>.
	;;
	;; `partial-completion' is a killer app for files, because it
	;; can expand ~/.l/s/fo to ~/.local/share/fonts.
	;;
	;; If `basic' cannot match my current input, Emacs tries the
	;; next completion style in the given order.  In other words,
	;; `orderless' kicks in as soon as I input a space or one of its
	;; style dispatcher characters.
	'((file (styles . (basic partial-completion orderless)))
	  (bookmark (styles . (basic substring)))
	  (library (styles . (basic substring)))
	  (embark-keybinding (styles . (basic substring)))
	  (imenu (styles . (basic substring orderless)))
	  (consult-location (styles . (basic substring orderless)))
	  (kill-ring (styles . (orderless)))
	  (eglot (styles . (orderless flex))))))

(setq completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq-default case-fold-search t)   ; For general regexp
(setq read-file-name-completion-ignore-case t)

;;; EMBARK

(use-package embark
  :ensure t
  :defer t
  :bind (("M-h" . embark-act))
  :commands (embark-act
	       embark-dwim
	       embark-export
	       embark-collect
	       embark-bindings
	       embark-prefix-help-command)

    :init
    (setq prefix-help-command #'embark-prefix-help-command)

    :config
    ;; Hide the mode line of the Embark live/completions buffers
    (add-to-list 'display-buffer-alist
		 '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
		   nil
		   (window-parameters (mode-line-format . none)))))

  ;;; EMBARK CONSULT

  (use-package embark-consult
     :ensure t
     :after (embark consult)
     :defer t
     :hook
     (embark-collection-mode . consult-preview-at-point-mode))

;;; CORFU

(use-package corfu
  :ensure t
  :defer t
  :init
  (global-corfu-mode)
  :custom
  (corfu-cycle t)               ;; Enable cycling through candidates
  (corfu-auto t)                ;; Enable auto-completion
  (corfu-auto-prefix 2)         ;; Minimum prefix length for auto-completion
  (corfu-auto-delay 0.1)          ;; No delay before suggestions appear
  (corfu-quit-no-match t)
  (corfu-quit-at-boundary t)
  (setq completion-cycle-threshold 3)
  (corfu-echo-documentation nil)
  (corfu-preview-current nil) ;; 'insert
  (setq corfu-preselect 'prompt)
  (setq corfu-separator ?\s)
  (corfu-preselect-first nil)
  (corfu-popupinfo-mode nil)      ;; Enable documentation popups
  :bind (:map corfu-map
	 ("S-RET" . nil)
	 ("RET"   . corfu-insert)
	 ("TAB"   . corfu-next)
	 ("[tab]" . corfu-next)
	 ("S-TAB" . corfu-previous)
	 ("C-h"   . corfu-info-documentation)
	 ("M-SPC" . corfu-insert-separator))
  :hook
  ;; In eshell, disable auto-completion but keep the quit settings.
  (eshell-mode . (lambda ()
		   (setq-local corfu-quit-at-boundary t
			       corfu-quit-no-match t
			       corfu-auto nil)))
  ;; Customize completion styles for Corfu.
  (corfu-mode . (lambda ()
		  (setq-local completion-styles '(basic)
			      completion-category-overrides nil
			      completion-category-defaults nil)))
  :config
  (corfu-history-mode))

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  ;; Add the Nerd Icons Corfu formatter to Corfu's margin formatters.
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;;; CAPE

(use-package cape
  :ensure t
  :init
  ;; (dolist (fn '(cape-file
	;; 	cape-keyword
	;;	cape-dabbrev
  ;;	cape-abbrev
	;;	cape-dict
	;;	;; cape-emoji
	;;	cape-sgml))
  ;;  (add-hook 'completion-at-point-functions fn 'append))

  ;; General completion functions for all programming modes
  (add-hook 'prog-mode-hook
            (lambda ()
              (add-hook 'completion-at-point-functions #'cape-keyword 'append)
              (add-hook 'completion-at-point-functions #'cape-dabbrev 'append)
              (add-hook 'completion-at-point-functions #'cape-file 'append)))

  ;; Elisp-specific completions
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (add-hook 'completion-at-point-functions #'cape-elisp-symbol 'append)
              (add-hook 'completion-at-point-functions #'cape-elisp-block 'append)
              (add-hook 'completion-at-point-functions #'cape-file 'append)))

  ;; Org mode completions
  (add-hook 'org-mode-hook
            (lambda ()
              (add-hook 'completion-at-point-functions #'cape-dabbrev 'append)
              (add-hook 'completion-at-point-functions #'cape-keyword 'append)
              (add-hook 'completion-at-point-functions #'cape-abbrev 'append)
              (add-hook 'completion-at-point-functions #'cape-file 'append)))

  ;; LaTeX-specific completions
  (add-hook 'latex-mode-hook
            (lambda ()
              (add-hook 'completion-at-point-functions #'cape-tex 'append)
              (add-hook 'completion-at-point-functions #'cape-dabbrev 'append)
              (add-hook 'completion-at-point-functions #'cape-keyword 'append)
              (add-hook 'completion-at-point-functions #'cape-file 'append)))

  ;; SGML/HTML/XML modes
  (add-hook 'sgml-mode-hook
            (lambda ()
              (add-hook 'completion-at-point-functions #'cape-sgml 'append)
              (add-hook 'completion-at-point-functions #'cape-dabbrev 'append)
              (add-hook 'completion-at-point-functions #'cape-file 'append)))

  ;; Text mode completions
  (add-hook 'text-mode-hook
            (lambda ()
              (add-hook 'completion-at-point-functions #'cape-dabbrev 'append)
              (add-hook 'completion-at-point-functions #'cape-abbrev 'append)
              (add-hook 'completion-at-point-functions #'cape-file 'append))))


;;; CAPF AUTOSUGGEST

(use-package capf-autosuggest
  :ensure t
  :defer t
  :hook ((eshell-mode . capf-autosuggest-mode))
  :custom
  (capf-autosuggest-dwim-next-line nil))

;;; TABNINE AI Completion

(use-package tabnine
  :commands (tabnine-start-process tabnine-mode)
  :ensure t
  :diminish "⌬"
  :custom
  (tabnine-wait 1)
  (tabnine-minimum-prefix-length 2)
  ;; :hook
  ;; ((prog-mode . tabnine-mode)
  ;; (org-mode . tabnine-mode)
  ;; (LaTeX-mode . tabnine-mode)
  ;; (text-mode . tabnine-mode)
  ;; (kill-emacs . tabnine-kill-process))
  :config
  (add-to-list 'completion-at-point-functions #'tabnine-completion-at-point)
  (tabnine-start-process)
  :bind
  (:map tabnine-completion-map
    ("<tab>" . tabnine-accept-completion)
    ("M-f" . tabnine-accept-completion-by-word)
    ("M-<return>" . tabnine-accept-completion-by-line)
    ("C-g" . tabnine-clear-overlay)
    ("M-[" . tabnine-previous-completion)
    ("M-]" . tabnine-next-completion)))

;;; GPTEL

(use-package gptel
  :ensure t
  :defer t
  :config

  ;;; KAGI Backend
  ;(gptel-make-kagi "Kagi"
  ;  :key (lambda () (auth-source-pick-first-password :host "kagi.com" :login "apikey")))

  (setq gptel-default-mode 'org-mode)
  (setq gptel-expert-commands t)

  ;; Define and Set Ollama as Default Backend
  (setq gptel-backend
	(gptel-make-ollama "Ollama"
			   :host "localhost:11434"
			   :stream t
			   :models '("llama3:latest"
				     "deepseek-coder:6.7b"
				     "mistral"
				     "zephyr"
				     "llama3:8b-instruct-q6_K"
				     "deepseek-r1:8b"
				     "qwen2.5:3b"))))
  ;; Set up keybinding for sending messages
(define-key global-map (kbd "C-c RET") 'gptel-send)

;;; UNDO FU

;; The =undo-fu-session= package saves and restores the undo states of buffers
;; across Emacs sessions.
(use-package undo-fu-session
  :ensure t
  :defer t
  :config
    (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  :hook ((prog-mode conf-mode text-mode tex-mode) . undo-fu-session-mode))

;;; Wakatime -- Install wakatime cli

(use-package wakatime-mode
  :ensure t
  :diminish
  :config
  (global-wakatime-mode))

;;; DIRED OPEN

(use-package dired-open
  :ensure t
  :defer t
  :config
  ;; Define applications to open specific file types
  (setq dired-open-extensions '(("gif" . "sxiv")   ;; Open GIFs with sxiv
				("jpg" . "sxiv")   ;; Open JPEGs with sxiv
				("png" . "sxiv")   ;; Open PNGs with sxiv
				("mkv" . "mpv")    ;; Open MKVs with mpv
				("mp4" . "mpv")))) ;; Open MP4s with mpv

;;; PEEP DIRED

(use-package peep-dired
  :after dired
  :ensure t
  :hook (evil-normalize-keymaps . peep-dired-hook) ;; Ensure peep-dired works with Evil mode
  :config
  ;; Define keybindings for navigation in Dired mode
  (evil-define-key 'normal dired-mode-map (kbd "h") 'dired-up-directory)   ;; Go up one directory
  (evil-define-key 'normal dired-mode-map (kbd "l") 'dired-open-file)      ;; Open file (use `dired-find-file` if not using `dired-open`)

  ;; Keybindings for navigating files in peep-dired preview mode
  (evil-define-key 'normal peep-dired-mode-map (kbd "j") 'peep-dired-next-file)  ;; Next file preview
  (evil-define-key 'normal peep-dired-mode-map (kbd "k") 'peep-dired-prev-file)) ;; Previous file preview

;;; SUDO EDIT

(use-package sudo-edit :ensure t :defer t)

;;; BREADCRUMB

(use-package breadcrumb
  :ensure t
  :config
  (breadcrumb-mode 1) ; Enable breadcrumb globally.
  ;; Optional tweaks for breadcrumb appearance:
  (setq breadcrumb-imenu-max-length 30
	breadcrumb-project-max-length 30
	breadcrumb-imenu-crumb-separator " » "
	breadcrumb-project-crumb-separator " / ")
  ;; Set the header-line to display both project and imenu breadcrumbs.
  (setq header-line-format
	'((:eval (concat (breadcrumb-project-crumbs)
			 "  " (breadcrumb-imenu-crumbs))))))

;;; JARCHIVE

(use-package jarchive
  :ensure t
  :after eglot
  :config
  (jarchive-setup))

;;; FLYCHECK

(use-package flycheck
  :ensure t
  :defer t
  :diminish flycheck-mode
  :init
  (global-flycheck-mode)
  :config
  ;; Adjust when Flycheck runs syntax checks.
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)
	;; Increase the error threshold to avoid disabling checkers on too many errors.
	flycheck-checker-error-threshold 1000))

(use-package sideline-flymake
  :hook (flymake-mode . sideline-mode)
  :custom
  (sideline-flymake-display-mode 'line) ;; show errors on the current line
  (sideline-backends-right '(sideline-flymake)))

;;; TREESITTER SOURCES

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  (c-ts-mode-indent-offset 4)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(setq treesit-language-source-alist
	'((templ      "https://github.com/vrischmann/tree-sitter-templ")
	  (bash       "https://github.com/tree-sitter/tree-sitter-bash")
	  (cmake      "https://github.com/uyha/tree-sitter-cmake")
	  (css        "https://github.com/tree-sitter/tree-sitter-css")
	  (elisp      "https://github.com/Wilfred/tree-sitter-elisp")
	  (go         "https://github.com/tree-sitter/tree-sitter-go")
	  (gomod      "https://github.com/camdencheek/tree-sitter-go-mod")
	  (html       "https://github.com/tree-sitter/tree-sitter-html")
	  (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
	  (cpp        "https://github.com/tree-sitter/tree-sitter-cpp")
	  (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
	  (json       "https://github.com/tree-sitter/tree-sitter-json")
	  (make       "https://github.com/alemuller/tree-sitter-make")
	  (markdown   "https://github.com/ikatyang/tree-sitter-markdown")
	  (python     "https://github.com/tree-sitter/tree-sitter-python")
	  (toml       "https://github.com/tree-sitter/tree-sitter-toml")
	  (tsx        "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
	  (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
	  (yaml       "https://github.com/ikatyang/tree-sitter-yaml")
	  (haskell    "https://github.com/tree-sitter/tree-sitter-haskell")
	  (typst      "https://github.com/uben0/tree-sitter-typst")
	  (java       "https://github.com/tree-sitter/tree-sitter-java")
	  (ruby       "https://github.com/tree-sitter/tree-sitter-ruby")
	  (rust       "https://github.com/tree-sitter/tree-sitter-rust")))

;;; FORMAT ALL

(use-package format-all
  :ensure t
  :defer t
  :preface
  (defun ian/format-code ()
    "Auto-format the entire buffer.
If in `prolog-mode', call `prolog-indent-buffer';
if the buffer is managed by Eglot and the LSP server supports document
formatting, call `eglot-format-buffer';
otherwise, call `format-all-buffer'."
    (interactive)
    (cond
     ((derived-mode-p 'prolog-mode)
      (prolog-indent-buffer))
     ((and (eglot-managed-p)
	   (eglot--server-capable :documentFormattingProvider))
      (eglot-format-buffer))
     (t (format-all-buffer))))
  :hook (prog-mode . format-all-ensure-formatter))

;;; EGLOT

(use-package eglot
  :ensure nil
  :custom
  (eglot-autoshutdown t)  ;; Shut down LSP when the buffer is closed.
  (eglot-sync-connect 1)   ;; Asynchronous connection.
  :bind (:map eglot-mode-map
	 ("C-c a" . eglot-code-actions)
	 ("C-c f" . flymake-show-buffer-diagnostics)
	 ("C-c r" . eglot-rename)
	 ("C-c i" . consult-imenu)
	 ("C-c b" . imenu-list-smart-toggle))
  :hook ((python-ts-mode . eglot-ensure)
	 (c-ts-mode        . eglot-ensure)
	 (c++-ts-mode      . eglot-ensure)
	 (go-ts-mode       . eglot-ensure)
	 (yaml-mode        . eglot-ensure)
	 (eglot-managed-mode . my/eglot-setup))
  :config
  (dolist (server `((c-ts-mode        . ("clangd"))
	;; (python-ts-mode   . ("pyright-langserver" "--stdio"))
		    (c++-ts-mode      . ("clangd"))
		    (go-ts-mode       . ("gopls"))))
    (add-to-list 'eglot-server-programs server)))

;; Custom eglot setup for buffer-local settings and keybindings.

(defun my/eglot-setup ()
  "Custom configuration for eglot-managed buffers."
  (electric-indent-local-mode t)
  (cond
   ((derived-mode-p 'python-ts-mode)
    (setq-local indent-tabs-mode nil
		python-indent-offset 4
		python-indent-guess-indent-offset nil)
    (local-set-key (kbd "<f6>") #'ian/format-code))
   ((derived-mode-p 'c-ts-mode 'c++-ts-mode)
    (setq-local c-default-style "linux"
		c-basic-offset 4)
    (local-set-key (kbd "<f6>") #'ian/format-code))
   ((derived-mode-p 'go-ts-mode)
    (setq-local tab-width 4
		indent-tabs-mode t)  ;; Go conventionally uses tabs.
    (local-set-key (kbd "<f6>") #'ian/format-code))
   ((derived-mode-p 'yaml-mode)
    nil)))


;; Additional auto-mode association for C++ using tree-sitter.
(add-to-list 'auto-mode-alist
	     '("\\(\\.ii\\|\\.\\(CC?\\|HH?\\)\\|\\.[ch]\\(pp\\|xx\\|\\+\\+\\)\\|\\.\\(cc\\|hh\\)\\)\\'"
	       . c++-ts-mode))

(use-package eldoc-box
  :ensure t
  :hook (eglot-managed-mode . eldoc-box-hover-mode) ;; Enable it for Eglot
  :bind (:map eglot-mode-map
	      ("C-c d" . eldoc-box-help-at-point))) ;; Manually trigger it

(add-hook 'eldoc-box-buffer-setup-hook #'eldoc-box-prettify-ts-errors 0 t)

;;; HIGHLIGHT INDENTATION GUIDES

(use-package highlight-indent-guides
  :ensure t
  :defer t
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-character ?\|)
  (setq highlight-indent-guides-responsive 'top)
  (setq highlight-indent-guides-auto-enabled nil) ;; Disable auto coloring

  ;; Adjust colors based on theme background
  (defun my/highlight-indent-guides-set-colors ()
    (if (eq (frame-parameter nil 'background-mode) 'dark)
	(progn
	  (set-face-foreground 'highlight-indent-guides-character-face "gray40")
	  (set-face-foreground 'highlight-indent-guides-top-character-face "white")
	  (set-face-foreground 'highlight-indent-guides-stack-character-face "gray60"))
      (progn
	(set-face-foreground 'highlight-indent-guides-character-face "gray40")
	(set-face-foreground 'highlight-indent-guides-top-character-face "black")
	(set-face-foreground 'highlight-indent-guides-stack-character-face "gray60"))))

  (add-hook 'after-load-theme-hook #'my/highlight-indent-guides-set-colors)
  (my/highlight-indent-guides-set-colors)) ;; Apply colors immediately

;;; CONDA

(use-package conda
  :ensure t
  :defer t
  :init
  ;; Set base directories (this is lightweight and safe to run on startup)
  (setq conda-anaconda-home (expand-file-name "~/miniconda"))
  (setq conda-env-home-directory (expand-file-name "~/miniconda"))
  :config
  ;; Initialize shells and modeline updates only when Conda is loaded.
  (conda-env-initialize-interactive-shells)
  (conda-env-initialize-eshell)
  (conda-env-autoactivate-mode nil)   ;; Disable global autoactivation.
  (conda-mode-line-setup)              ;; Update modeline when Conda env changes.
  :hook ((python-mode . conda-env-autoactivate-mode)
	 (conda-postactivate-hook . restart-python-shell-with-conda)))

;;; CUSTOM FUNCTIONS

(defun restart-python-shell-with-conda ()
  "Restart Python shell using the currently activated Conda environment."
  (interactive)
  (when (bound-and-true-p conda-env-current-name)
    (let* ((conda-base-path (or (getenv "CONDA_PREFIX") "~/miniconda"))
	   (env-path (if (string= conda-env-current-name "base")
			 conda-base-path
		       (concat conda-base-path "/envs/" conda-env-current-name)))
	   (env-bin (concat env-path "/bin/python"))
	   (python-buffer (get-buffer "*Python*")))
      ;; Kill existing Python shell if running.
      (when (get-process "Python")
	(delete-process "Python"))
      (when python-buffer
	(kill-buffer python-buffer))
      (delete-other-windows)
      (if (file-executable-p env-bin)
	  (progn
	    (setq-local python-shell-interpreter env-bin)
	    (setq-local python-shell-interpreter-args "-i")
	    (setq-local pythonic-interpreter env-bin)  ;; If using pythonic.el.
	    (run-python (concat env-bin " -i") nil nil)
	    (message "Switched Python shell to Conda environment: %s"
		     conda-env-current-name))
	(message "Error: Could not find Python executable at %s" env-bin)))))


(defun open-python-right-side ()
  "Toggle a Python REPL in a vertical split on the right side."
  (interactive)
  (let ((python-buffer (get-buffer "*Python*"))
	(python-window (get-buffer-window "*Python*")))
    (if python-buffer
	(if python-window
	    (progn
	      (delete-window python-window)
	      (other-window 1))
	  (progn
	    (split-window-right)
	    (other-window 1)
	    (run-python)
	    (when (get-buffer "*Python*")
	      (switch-to-buffer "*Python*"))
	    (other-window 1)))
      (progn
	(split-window-right)
	(other-window 1)
	(run-python)
	(when (get-buffer "*Python*")
	  (switch-to-buffer "*Python*"))
	(other-window 1)))))

(setq display-buffer-alist
      '(("\\*compilation\\*"
	 (display-buffer-reuse-window display-buffer-at-bottom)
	 (window-height . 0.3))))

;;; WEB MODE 

(use-package web-mode
  :ensure t
  :defer t
  :config
  (setq
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2
   web-mode-style-padding 2
   web-mode-script-padding 2
   web-mode-enable-auto-closing t
   web-mode-enable-auto-opening t
   web-mode-enable-auto-pairing t
   web-mode-enable-auto-indentation t)
  :mode
  (".html$" "*.php$"))

;;; PDF TOOLS

(use-package pdf-tools
  :ensure t
  :defer t
  :commands (pdf-loader-install)
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :custom
  (pdf-view-display-size 'fit-width)
  (pdf-annot-activate-created-annotations t)
  :bind (:map pdf-view-mode-map
	      ("j" . pdf-view-next-line-or-next-page)
	      ("k" . pdf-view-previous-line-or-previous-page)
	      ("C-=" . pdf-view-enlarge)
	      ("C--" . pdf-view-shrink)
	      ("C-s" . isearch-forward)
	      ("C-r" . isearch-backward))
  :init
  (pdf-loader-install)
  :hook (pdf-view-mode . (lambda ()
			   (display-line-numbers-mode -1)
			   (blink-cursor-mode -1)
			   (doom-modeline-mode -1)))
  :config
  (add-to-list 'revert-without-query ".pdf"))

;;; EPUB

(use-package nov
  :ensure t
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (setq nov-variable-pitch nil) ;; Use fixed-width font
  (setq nov-text-width t)) ;; Adjust text width dynamically

;;; LATEX

(use-package tex
  :ensure auctex
  :defer t
  :mode ("\\.tex\\'" . LaTeX-mode)
  :hook
  ((LaTeX-mode . LaTeX-math-mode)
   (LaTeX-mode . TeX-fold-mode)
   (LaTeX-mode-hook . magic-latex-buffer))
  :config
  ;; Basic AUCTeX settings.
  (setq TeX-auto-save t
	TeX-parse-self t
	TeX-PDF-mode t
	TeX-save-query nil)
  ;; Integration with RefTeX.
  (setq reftex-plug-into-AUCTeX t)
  ;; Master file setup: default to main.tex if it exists; otherwise prompt.
  (setq-default TeX-master
		(lambda ()
		  (let ((default-master (concat (file-name-directory (or (buffer-file-name) default-directory))
						"main.tex")))
		    (if (file-exists-p default-master)
			"main.tex"
		      (progn
			(message "main.tex not found, please select a master file")
			(read-file-name "Choose master file: " nil nil t))))))
  ;; LaTeX indentation and electric settings.
  (setq LaTeX-indent-level 2
	LaTeX-item-indent 2
	TeX-electric-math '("$" . "$"))
  ;; LaTeX preview settings.
  (setq preview-auto-cache-preamble t
  ;; preview-default-option-list '("floats" "graphics")
  preview-default-option-list '("displaymath" "graphics" "textmath" "footnotes" "sections" "showlabels" "psfixbb" "floats" "tabular")
	TeX-show-compilation nil))

(add-hook 'LaTeX-mode-hook #'rainbow-delimiters-mode)

(defun clear-latex-build ()
  "Remove all LaTeX compilation files except .tex and .pdf."
  (interactive)
  (when (y-or-n-p "Delete all LaTeX build files except .tex and .pdf? (y/n) ")
    (call-process "/bin/sh" nil nil nil "-c"
                  "rm -rf auto *.prv *.aux *.bbl *.blg *.log *.out *.toc *.lof *.lot *.synctex.gz *.fls *.fdb_latexmk _region_.tex")
    (message "LaTeX build files deleted.")))

;;; REFTEX

(use-package reftex
  :ensure nil  ;; RefTeX is built-in
  :defer t
  :preface
  ;; Explicit autoloads if desired (usually not needed)
  (autoload 'reftex-mode "reftex" "RefTeX Minor Mode" t)
  (autoload 'turn-on-reftex "reftex" "RefTeX Minor Mode" nil)
  (autoload 'reftex-citation "reftex-cite" "Make citation" nil)
  (autoload 'reftex-index-phrase-mode "reftex-index" "Phrase mode" t)
  :hook (LaTeX-mode . turn-on-reftex)
  :config
  (setq reftex-plug-into-AUCTeX t
	reftex-enable-partial-scans t
	reftex-save-parse-info t
	reftex-use-multiple-selection-buffers t
	reftex-toc-split-windows-horizontally t
	reftex-toc-split-windows-fraction 0.2))

;;; PREVIEW

(with-eval-after-load 'tex
  (add-to-list 'TeX-view-program-list
	       `("Zathura"
		 (,(concat (expand-file-name "~/.local/bin/zathura") " "
			   (when (boundp 'mode-io-correlate)
			     " --synctex-forward %n:0:%b -x \"emacsclient +%{line} %{input}\" ")
			   " %o"))
		 "zathura"))
  (setq TeX-view-program-selection '((output-pdf "Zathura"))
	TeX-source-correlate-start-server t
	TeX-source-correlate-mode t
	TeX-source-correlate-method 'synctex))

;;; CITAR

(use-package citar
  :ensure t
  :defer t
  :init
  (setq org-cite-insert-processor 'citar
	org-cite-follow-processor 'citar
	org-cite-activate-processor 'citar
	citar-bibliography org-cite-global-bibliography)
	;; citar-notes-paths '("~/Path/To/NotesDir")
	)

(use-package citar-embark
  :after (citar embark)
  :ensure t
  :defer t
  :init
  (setq citar-at-point-function 'embark-act)
  :config
  (citar-embark-mode 1))

;;; AUCTEX

(use-package auctex-latexmk
  :ensure t
  :defer t
  :config
  (auctex-latexmk-setup)
  (setq auctex-latexmk-inherit-TeX-PDF-mode t))

(use-package cdlatex
  :ensure t
  :defer t
  :hook (LaTeX-mode . turn-on-cdlatex))

;;; EVIL TEX

(use-package evil-tex
  :ensure t
  :hook (LaTeX-mode . evil-tex-mode))

;;; PREVIEW AUTO
(use-package preview-auto
  :after latex
  :hook (LaTeX-mode . preview-auto-mode)
  :config
  (setq preview-protect-point t)
  (setq preview-locating-previews-message nil)
  (setq preview-leave-open-previews-visible t)
  :custom
  (preview-auto-interval 0.1)

  ;; Uncomment the following only if you have followed the above
  ;; instructions concerning, e.g., hyperref:

  (preview-LaTeX-command-replacements '(preview-LaTeX-disable-pdfoutput)))

(use-package preview-tailor
  :ensure t
  :after preview
  :config
  (preview-tailor-init)
  :hook
  (kill-emacs . preview-tailor-save))

;; Preview Latex in markdown buffer as well using a temporary TeX file with preview auto mode.

(defun my-markdown-preview-hook ()
  "Setup LaTeX preview for Markdown mode with a fresh temporary TeX file."
  (setq-local preview-tailor-local-multiplier 0.7)
  
  ;; Always create a new temporary LaTeX file
  (setq-local my-preview-master (make-temp-file "preview-master" nil ".tex"))
  (with-temp-file my-preview-master
    (insert "\\documentclass{article}\n"
            "\\usepackage{amsmath, amssymb}\n"
            "\\begin{document}\n"
            "% Your LaTeX preview content will be inserted here\n"
            "\\end{document}\n"))
  
  (setq-local TeX-master my-preview-master)
  (preview-auto-mode))

(add-hook 'markdown-mode-hook 'my-markdown-preview-hook)

;;; ============================================================
;;; MARKDOWN SETUP
;;; ============================================================

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode))

(eval-after-load "org"
  '(require 'ox-md nil t))

;; --------------------------------
;; OX-GFM FOR GITHUB-FLAVORED MARKDOWN EXPORT
;; --------------------------------
(use-package ox-gfm
  :ensure t
  :after org
  :defer t)

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist
	     '("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode))

(autoload 'gfm-mode "markdown-mode"
   "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;;; OX-HUGO

(use-package ox-hugo
  :ensure ( :host github
	      :repo "kaushalmodi/ox-hugo"
	      :branch "main")
  :after ox)

;; Function to insert the current date
(defun insert-current-date ()
  "Insert the current date in the format YYYY-MM-DD at the point."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

;; Bind the function to C-c d in Org mode
(add-hook 'org-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c d") 'insert-current-date)))

;;; MARKDOWN PREVIEW MODE

(use-package markdown-preview-mode
  :ensure t
  :commands (markdown-preview-mode
	     markdown-preview-open-browser
	     markdown-preview-cleanup)
  :init
  ;; Set your Markdown processor (by default it uses "markdown")
  ;; If you want to use multimarkdown, make sure it's installed and in your PATH.
  ;(setq markdown-command "multimarkdown")
  :config
  ;; Optional: add extra JavaScript (e.g., MathJax)
  (add-to-list 'markdown-preview-javascript
	       "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-MML-AM_CHTML"))

;; Additional CSS
;; (setq markdown-preview-stylesheets
;;       (list "http://thomasf.github.io/solarized-css/solarized-light.min.css"))

;;; TOC & INDENTATION

(use-package toc-org
    :ensure t
    :defer t
    :commands toc-org-enable
    :init (add-hook 'org-mode-hook 'toc-org-enable))

(setq org-src-preserve-indentation t)

;; Prevent '<>' from auto-pairing in Org mode (fix for org-tempo)
(add-hook 'org-mode-hook
	  (lambda ()
	    (setq-local electric-pair-inhibit-predicate
			`(lambda (c)
			   (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))

(eval-after-load 'org-indent '(diminish 'org-indent-mode)) ;; Removes "Ind" from showing in the modeline.

;;; ORG UI SETTINGS

;; ORG MODERN PACKAGE
;;;; Better Looking Bullets
(use-package org-modern
  :ensure t
  :hook ((org-mode                 . org-modern-mode)
	 (org-agenda-finalize-hook . org-modern-agenda))
  :custom ((org-modern-todo t)
	   (org-modern-table nil)
	   (org-modern-variable-pitch nil)
	   (org-modern-block-fringe nil))
  :commands (org-modern-mode org-modern-agenda)
  :init (global-org-modern-mode))

(use-package org-transclusion
  :after org
  :bind ("C-c M-t" . org-transclusion-add))

(setq org-id-locations-file (concat user-emacs-directory ".cache/.org-id-locations"))

;; ORG CUSTOM HEADER FACES
(custom-set-faces
'(org-level-1 ((t (:inherit outline-1 :height 1.3))))
'(org-level-2 ((t (:inherit outline-2 :height 1.25))))
'(org-level-3 ((t (:inherit outline-3 :height 1.2))))
'(org-level-4 ((t (:inherit outline-4 :height 1.15))))
'(org-level-5 ((t (:inherit outline-5 :height 1.1))))
'(org-level-6 ((t (:inherit outline-5 :height 1.05))))
'(org-level-7 ((t (:inherit outline-5 :height 1.00)))))

;; ORG FONTS
(defun karna/org-colors-doom-one ()
 "Enable Doom One colors for Org headers."
 (interactive)
 (dolist
     (face
      '((org-level-1 1.7 "#51afef" ultra-bold)
	(org-level-2 1.6 "#c678dd" extra-bold)
	(org-level-3 1.5 "#98be65" bold)
	(org-level-4 1.4 "#da8548" semi-bold)
	(org-level-5 1.3 "#5699af" normal)
	(org-level-6 1.2 "#a9a1e1" normal)
	(org-level-7 1.1 "#46d9ff" normal)
	(org-level-8 1.0 "#ff6c6b" normal)))
   (set-face-attribute (nth 0 face) nil :font "Iosevka Comfy Motion" :weight (nth 3 face) :height (nth 1 face) :foreground (nth 2 face)))
 (set-face-attribute 'org-table nil :font "Iosevka Comfy Motion" :weight 'bold :height 1.0 :foreground "#bfafdf"))

;; (karna/org-colors-doom-one)


;;; ORG BULLETS -
;; Org-bullets gives us attractive bullets rather than asterisks.
;; (add-hook 'org-mode-hook 'org-indent-mode)
;; (use-package org-bullets :ensure t :defer t)
;; (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;;; ORG SUPERSTAR (ALTERNATIVE TO ORG BULLETS)
(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode))

;;; ORG REMARKS

(use-package org-remark
  :ensure t
  :bind (("C-c n m" . org-remark-mark)
	 ("C-c n l" . org-remark-mark-line)
	 :map org-remark-mode-map
	 ("C-c n o" . org-remark-open)
	 ("C-c n ]" . org-remark-view-next)
	 ("C-c n [" . org-remark-view-prev)
	 ("C-c n r" . org-remark-remove)
	 ("C-c n d" . org-remark-delete))
  :config
  (org-remark-global-tracking-mode +1)  ;; Moved inside :config to ensure `org-remark` is loaded
  ;; Optional modes
  (with-eval-after-load 'nov
    (org-remark-nov-mode +1))
  (with-eval-after-load 'info
    (org-remark-info-mode +1)))

;;; HIGHLIGHT TODO

(use-package hl-todo
  :ensure t
  :defer t
  :hook ((org-mode . hl-todo-mode)
	 (prog-mode . hl-todo-mode))
  :config
  (setq hl-todo-highlight-punctuation ":"
	hl-todo-keyword-faces
	`(("TODO"       warning bold)
	  ("FIXME"      error bold)
	  ("HACK"       font-lock-constant-face bold)
	  ("REVIEW"     font-lock-keyword-face bold)
	  ("NOTE"       success bold)
	  ("DEPRECATED" font-lock-doc-face bold))))

;;; ORG TEMPO

(require 'org-tempo)

;;; CENTER ORG MODE

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 180
	visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

;;; ORG AUTO TANGLE

(use-package org-auto-tangle
  :defer t
  :diminish
  :hook (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-default t))

(defun karna/insert-auto-tangle-tag ()
  "Insert auto-tangle tag in a literate config."
  (interactive)
  (org-end-of-line)
  (newline)
  (insert "#+auto_tangle: t")
  (evil-force-normal-state))

;;; ORG MERMAID

(use-package mermaid-mode
  :ensure t)

;; Install Mermaid CLI using - sudo npm install -g @mermaid-js/mermaid-cli
(use-package ob-mermaid
  :ensure t
  :config
  (setq ob-mermaid-cli-path "/usr/bin/mmdc") ;; Adjust this path to your mermaid-cli
  (org-babel-do-load-languages 'org-babel-load-languages
			       '((mermaid . t))))

;;; ORG EVAL

(setq org-confirm-babel-evaluate nil
      org-babel-clojure-backend 'cider
      org-babel-lisp-eval-fn #'sly-eval
      org-edit-src-content-indentation 0)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((sqlite . t) (haskell . t) (emacs-lisp . t) (shell . t) (python . t)
   (C . t) (lua . t) (dot . t) (java . t)
   (lisp . t) (clojure . t) (scheme . t)
   (forth . t) (R . t)))

;;; ORG MODE CORE SETTINGS

(setq org-directory "/mnt/Karna/Git/Project-K/Org/"
      org-default-notes-file (expand-file-name "notes.org" org-directory)
      org-ellipsis " ▼ "
      org-superstar-headline-bullets-list '("◉" "●" "○" "◆" "●" "○" "◆")
      org-superstar-itembullet-alist '((?+ . ?➤) (?- . ?✦)) ; changes +/- symbols in item lists
      org-log-done 'time
      org-hide-emphasis-markers t
      ;; ex. of org-link-abbrev-alist in action
      ;; [[arch-wiki:Name_of_Page][Description]]
      org-link-abbrev-alist
	'(("google" . "http://www.google.com/search?q=")
	  ("arch-wiki" . "https://wiki.archlinux.org/index.php/")
	  ("ddg" . "https://duckduckgo.com/?q=")
	  ("wiki" . "https://en.wikipedia.org/wiki/"))
      org-table-convert-region-max-lines 20000)


(setq org-todo-keywords
      '((sequence "IDEA(i)"      ; Generate research ideas
		  "LIT(l)"       ; Conduct literature review
		  "CODE(c)"      ; Develop code/algorithms
		  "TEST(t)"      ; Test implementations or experiments
		  "WRITE(w)"     ; Document findings or draft manuscripts
		  "REVIEW(r)"    ; Revise based on feedback
		  "|"
		  "SUBMITTED(s)" ; Work submitted for review/publication
		  "PUBLISHED(p)" ; Work published (or defended)
		  "ABANDONED(x)") ; Project discontinued
	(sequence "TODO(T)"       ; Basic task: not yet started
		  "NEXT(n)"       ; Basic task: immediate next action
		  "|"
		  "DONE(d!)"))) ; Basic task: completed

(setq org-export-backends '(md org ascii html icalendar latex odt rss)
      org-export-with-toc nil)

;; Load export backends
(require 'ox-md)
(require 'ox-org)

;;; ORG REFILE SETTINGS

(setq org-bookmark-names-plist nil) ;; Stop bookmarking on org captures and refiling

(setq org-refile-targets
      '(("Archive.org" :maxlevel . 1)
	("Tasks.org" :maxlevel . 1)))

(setq org-hide-drawers '("PROPERTIES"))

;; Save Org buffers after refiling!
;; (advice-add 'org-refile :after 'org-save-all-org-buffers)

;;; ORG TAG CONFIGURATION

(setq org-tag-alist
      '((:startgroup)
	("@errand"   . ?E)
	("@home"     . ?H)
	("@lab"      . ?L)
	("@office"   . ?O)
	(:endgroup)
	("agenda"    . ?a)
	("planning"  . ?p)
	("note"      . ?n)
	("idea"      . ?i)
	("lit"       . ?l)   ; literature review
	("code"      . ?c)
	("test"      . ?t)
	("write"     . ?w)
	("review"    . ?r)
	("submitted" . ?s)
	("published" . ?P)   ; uppercase P differentiates from planning
	("abandoned" . ?x)
	("meeting"   . ?m)
	("reading"   . ?R)))

;;; AGENDA BASICS

(setq org-agenda-files (directory-files-recursively org-directory "\\.org$") )

(setq org-agenda-start-with-log-mode t
      org-log-done 'time
      org-log-into-drawer t)

;;; CUSTOM AGENDA SETTINGS

;; Fancy Priorities Settings
(setq org-fancy-priorities-list '("🟥" "🟧" "🟨")
      org-priority-faces
      '((?A :foreground "#ff6c6b" :weight bold)  ; High priority (🟥)
	(?B :foreground "#98be65" :weight bold)  ; Medium priority (🟧)
	(?C :foreground "#c678dd" :weight bold)) ; Low priority (🟨)
      org-agenda-block-separator 8411)

;; Org Agenda Custom Commands
(setq org-agenda-custom-commands
      '(
	;; Dashboard: Agenda view + Next Tasks + Active Projects
	("d" "Dashboard"
	 ((agenda "" ((org-deadline-warning-days 7)))
	  (todo "NEXT" ((org-agenda-overriding-header "Next Tasks")))
	  (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

	;; Next Tasks: Focused view on tasks marked as NEXT
	("n" "Next Tasks"
	 ((todo "NEXT" ((org-agenda-overriding-header "Next Tasks")))))

	;; Work & Location-Based Tasks: Filter tasks by location tags
	("w" "Work & Location Tasks"
	 ((tags-todo "+@lab")
	  (tags-todo "+@office")
	  (tags-todo "+@errand")
	  (tags-todo "+@home")))

	;; Low-Effort Tasks: Show NEXT tasks with low estimated effort
	("e" "Low-Effort Tasks"
	 ((tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
		     ((org-agenda-overriding-header "Low Effort Tasks")
		      (org-agenda-max-todos 20)
		      (org-agenda-files org-agenda-files)))))

	;; Unified Research Workflow
	("r" "Unified Research Workflow"
	 ((todo "IDEA"      ((org-agenda-overriding-header "Research Ideas")))
	  (todo "LIT"       ((org-agenda-overriding-header "Literature Review")))
	  (todo "CODE"      ((org-agenda-overriding-header "Development / Coding")))
	  (todo "TEST"      ((org-agenda-overriding-header "Testing / Experiments")))
	  (todo "WRITE"     ((org-agenda-overriding-header "Writing / Documentation")))
	  (todo "REVIEW"    ((org-agenda-overriding-header "Revision / Feedback")))
	  (todo "SUBMITTED" ((org-agenda-overriding-header "Submitted Work")))
	  (todo "PUBLISHED" ((org-agenda-overriding-header "Published Work")))
	  (todo "ABANDONED" ((org-agenda-overriding-header "Discontinued Projects")))))

	;; Priority-Based View: Unfinished tasks by custom priority tags
	("v" "Priority View"
	 ((tags "PRIORITY=\"A\""
		((org-agenda-skip-function
		  '(org-agenda-skip-entry-if 'todo 'done))
		 (org-agenda-overriding-header "High-Priority Tasks")))
	  (tags "PRIORITY=\"B\""
		((org-agenda-skip-function
		  '(org-agenda-skip-entry-if 'todo 'done))
		 (org-agenda-overriding-header "Medium-Priority Tasks")))
	  (tags "PRIORITY=\"C\""
		((org-agenda-skip-function
		  '(org-agenda-skip-entry-if 'todo 'done))
		 (org-agenda-overriding-header "Low-Priority Tasks")))
	  (agenda "")
	  (alltodo "")))))

;;; ORG ROAM SETUP

(use-package org-roam
  :ensure t
  :defer t
  :init
  (setq org-roam-v2-ack t)   ; Acknowledge the v2 upgrade message
  :custom
  (org-roam-db-autosync-mode)           ; Automatically sync the Org Roam database
  (org-roam-completion-everywhere t)     ; Enable completion everywhere
  ;; (org-roam-dailies-capture-templates
  ;;     '(("d" "default" entry "* %<%I:%M %p>: %?"
  ;;        :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
			  "#+title: ${title}\n#+date: %U\n")
      :unnarrowed t)
     ("l" "programming language" plain
      "* Characteristics\n\n- Family: %?\n- Inspired by: \n\n* Reference:\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
			  "#+title: ${title}\n")
      :unnarrowed t)
     ("b" "book notes" plain
      (file "/mnt/Karna/Git/Project-K/Org/Templates/BooknoteTemplate.org")
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
			  "#+title: ${title}\n")
      :unnarrowed t)
     ("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
			  "#+title: ${title}\n#+filetags: Project")
      :unnarrowed t)))
  :config
  (org-roam-setup))

;;; ORG ROAM DIRECTORIES & GRAPH VIEWER

(with-eval-after-load 'org
  (setq org-roam-directory "/mnt/Karna/Git/Project-K/Org/Roam/"
	org-roam-graph-viewer "/usr/bin/zen-browser"))

(setq org-roam-dailies-directory "/mnt/Karna/Git/Project-K/Org/Journal/")

;;; ORG JOURNAL SETUP

(setq org-journal-dir "/mnt/Karna/Git/Project-K/Org/Journal/"
      org-journal-date-prefix "* "
      org-journal-time-prefix "** "
      org-journal-date-format "%B %d, %Y (%A) "
      org-journal-file-format "%Y-%m-%d.org")

;;; ORG ROAM DATABASE LOCATION

(setq org-roam-db-location "/mnt/Karna/Git/Project-K/Org/Roam/org-roam.db")

;;; LATEX COMPILER & BASIC PREVIEW SETTINGS

(setq org-latex-compiler "xelatex")
(setq org-latex-pdf-process '("xelatex %f"))
(setq org-latex-listings t)
(setq org-preview-latex-image-directory "~/.cache/emacs/lxtimg/")
(setq org-latex-preview-lxtpng-directory "~/.cache/emacs/lxtimg/")

(define-key org-mode-map (kbd "M-p") 'org-latex-export-to-pdf)

;;; CUSTOM LATEX CLASSES FOR ORG EXPORT

(with-eval-after-load 'ox-latex
  (dolist (class
           '(("IEEEtran" "\\documentclass[conference]{IEEEtran}"
              ("\\section{%s}" . "\\section*{%s}")
              ("\\subsection{%s}" . "\\subsection*{%s}")
              ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
              ("\\paragraph{%s}" . "\\paragraph*{%s}")
              ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
             ("article" "\\documentclass[11pt]{article}"
              ("\\section{%s}" . "\\section*{%s}")
              ("\\subsection{%s}" . "\\subsection*{%s}")
              ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
              ("\\paragraph{%s}" . "\\paragraph*{%s}")
              ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
             ("report" "\\documentclass[11pt]{report}"
              ("\\part{%s}" . "\\part*{%s}")
              ("\\chapter{%s}" . "\\chapter*{%s}")
              ("\\section{%s}" . "\\section*{%s}")
              ("\\subsection{%s}" . "\\subsection*{%s}")
              ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
             ("book" "\\documentclass[11pt]{book}"
              ("\\part{%s}" . "\\part*{%s}")
              ("\\chapter{%s}" . "\\chapter*{%s}")
              ("\\section{%s}" . "\\section*{%s}")
              ("\\subsection{%s}" . "\\subsection*{%s}")
              ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
             ("org-plain-latex" "\\documentclass{article}
               [NO-DEFAULT-PACKAGES]
               [PACKAGES]
               [EXTRA]"
              ("\\section{%s}" . "\\section*{%s}")
              ("\\subsection{%s}" . "\\subsection*{%s}")
              ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
              ("\\paragraph{%s}" . "\\paragraph*{%s}")
              ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
    (add-to-list 'org-latex-classes class)))

;;; ORG LATEX PREVIEW

;; Increase LaTeX preview size
(setq org-format-latex-options
      (plist-put org-format-latex-options :scale 2.0)) ;; Adjust as needed

;; Use dvisvgm for SVG-based previews (default)
(setq org-latex-create-formula-image-program 'dvisvgm) ;; Use dvisvgm for better compatibility
(setq org-preview-latex-default-process 'dvisvgm) ;; Default to dvisvgm

(setq org-startup-with-latex-preview t)


;; Prevent navigation commands from triggering LaTeX previews
(setq org-latex-preview-auto-ignored-commands
      '(next-line previous-line mwheel-scroll
	scroll-up-command scroll-down-command))

;; Enable consistent equation numbering
(setq org-latex-preview-numbered t)

;; Enable live previews for real-time LaTeX updates
(setq org-latex-preview-live t)

;; Reduce delay for faster live previews
(setq org-latex-preview-live-debounce 0.25)


;; Stolen fsrom the package ov
(defun ov-at (&optional point)
  "Get an overlay at POINT.
POINT defaults to the current `point'."
  (or point (setq point (point)))
  (car (overlays-at point)))
;; https://www.reddit.com/r/emacs/comments/169keg7/comment/jzierha/?utm_source=share&utm_medium=web2x&context=3
(defun org-justify-fragment-overlay (beg end image &optional imagetype)
  "Only equations at the beginning and also end of a line are justified."
  (if
   (and (= beg (line-beginning-position)) (= end (line-end-position)))
   (let* ((ov (ov-at))
  (disp (overlay-get ov 'display)))
     (overlay-put ov 'line-prefix `(space :align-to (- center (0.5 . ,disp)))))))
(advice-add 'org--make-preview-overlay :after 'org-justify-fragment-overlay)


;; Automatically refresh LaTeX previews on save or edits
(add-hook 'org-mode-hook
	  (lambda ()
	    (add-hook 'after-save-hook 'org-latex-preview nil 'local)
	    (add-hook 'after-change-functions
		      (lambda (&rest _) (org-latex-preview)) nil 'local)))

;;; LATEX FRAGMENT SCALE

(setq preview-scale-function 1)

;;; CITATION

(use-package citeproc
  :ensure t
  :defer t)


(with-eval-after-load 'org
  (require 'oc-csl)
(require 'oc-biblatex)
(require 'oc-natbib))
;; (setq org-cite-global-bibliography '("~/Path/To/bibliographyFile"))

;;; ESHELL

(setopt eshell-prompt-function 'fancy-shell)
(setopt eshell-prompt-regexp "^[^#$\n]* [$#] ")
(setopt eshell-highlight-prompt nil)

;; Disabling company mode in eshell, because it's annoying.
(setq company-global-modes '(not eshell-mode))

;; Adding a keybinding for 'pcomplete-list' on F9 key.
(add-hook 'eshell-mode-hook
	  (lambda ()
	    (define-key eshell-mode-map (kbd "<f9>") #'pcomplete-list)))


;; A function for easily creating multiple buffers of 'eshell'.
;; NOTE: `C-u M-x eshell` would also create new 'eshell' buffers.
(defun eshell-new (name)
  "Create new eshell buffer named NAME."
  (interactive "sName: ")
  (setq name (concat "$" name))
  (eshell)
  (rename-buffer name))

(use-package eshell-toggle
  :ensure t
  :defer t
  :custom
  (eshell-toggle-size-fraction 3)
  (eshell-toggle-use-projectile-root t)
  (eshell-toggle-run-command nil)
  (eshell-toggle-init-function #'eshell-toggle-init-ansi-term))

  (use-package eshell-syntax-highlighting
    :after esh-mode
    :config
    (eshell-syntax-highlighting-global-mode +1))

  ;; eshell-syntax-highlighting -- adds fish/zsh-like syntax highlighting.
  ;; eshell-rc-script -- your profile for eshell; like a bashrc for eshell.
  ;; eshell-aliases-file -- sets an aliases file for the eshell.

  (setq eshell-rc-script (concat user-emacs-directory "eshell/profile")
	eshell-aliases-file (concat user-emacs-directory "eshell/aliases")
	eshell-history-size 5000
	eshell-buffer-maximum-lines 5000
	eshell-hist-ignoredups t
	eshell-scroll-to-bottom-on-input t
	eshell-destroy-buffer-when-process-dies t
	eshell-visual-commands'("bash" "zsh" "htop" "ssh" "top" "fish"))

;;; VTERM

(use-package vterm
:ensure t
:defer t
:config
(setq shell-file-name "/bin/sh"
      vterm-max-scrollback 5000))

;;; VTERM TOGGLE

(use-package vterm-toggle
  :after vterm
  :ensure t
  :defer t
  :config
  ;; When running programs in Vterm and in 'normal' mode, make sure that ESC
  ;; kills the program as it would in most standard terminal programs.
  (evil-define-key 'normal vterm-mode-map (kbd "<escape>") 'vterm--self-insert)
  (setq vterm-toggle-fullscreen-p nil)
  (setq vterm-toggle-scope 'project)
  (add-to-list 'display-buffer-alist
	       '((lambda (buffer-or-name _)
		     (let ((buffer (get-buffer buffer-or-name)))
		       (with-current-buffer buffer
			 (or (equal major-mode 'vterm-mode)
			     (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
		  (display-buffer-reuse-window display-buffer-at-bottom)
		  ;;(display-buffer-reuse-window display-buffer-in-direction)
		  ;;display-buffer-in-direction/direction/dedicated is added in emacs27
		  ;;(direction . bottom)
		  ;;(dedicated . t) ;dedicated is supported in emacs27
		  (reusable-frames . visible)
		  (window-height . 0.4))))

;;; YASNIPPET

(use-package yasnippet
  :ensure t
  :diminish
  :hook
  ((prog-mode . yas-minor-mode)
   (text-mode . yas-minor-mode))
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets/")) ;; Ensure your custom snippet directory is included
  (yas-reload-all))

(add-hook 'latex-mode-hook #'yas-minor-mode)

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet
  :config
  (yas-reload-all)
  (yasnippet-snippets-initialize))

;; Keybinding to manually insert snippets
(global-set-key (kbd "C-c y") #'yas-insert-snippet)

;;; CALENDAR

;; https://stackoverflow.com/questions/9547912/emacs-calendar-show-more-than-3-months

(use-package calfw :ensure t :defer t)
(use-package calfw-org :ensure t :defer t)
;;(use-package calendar)

(defun karna/year-calendar (&optional year)
  (interactive)
  (require 'calendar)
  (let* (
      (current-year (number-to-string (nth 5 (decode-time (current-time)))))
      (month 0)
      (year (if year year (string-to-number (format-time-string "%Y" (current-time))))))
    (switch-to-buffer (get-buffer-create calendar-buffer))
    (when (not (eq major-mode 'calendar-mode))
      (calendar-mode))
    (setq displayed-month month)
    (setq displayed-year year)
    (setq buffer-read-only nil)
    (erase-buffer)
    ;; horizontal rows
    (dotimes (j 4)
      ;; vertical columns
      (dotimes (i 3)
	(calendar-generate-month
	  (setq month (+ month 1))
	  year
	  ;; indentation / spacing between months
	  (+ 5 (* 25 i))))
      (goto-char (point-max))
      (insert (make-string (- 10 (count-lines (point-min) (point-max))) ?\n))
      (widen)
      (goto-char (point-max))
      (narrow-to-region (point-max) (point-max)))
    (widen)
    (goto-char (point-min))
    (setq buffer-read-only t)))

(defun karna/scroll-year-calendar-forward (&optional arg event)
  "Scroll the yearly calendar by year in a forward direction."
  (interactive (list (prefix-numeric-value current-prefix-arg)
		     last-nonmenu-event))
  (unless arg (setq arg 0))
  (save-selected-window
    (if (setq event (event-start event)) (select-window (posn-window event)))
    (unless (zerop arg)
      (let* (
	      (year (+ displayed-year arg)))
	(karna/year-calendar year)))
    (goto-char (point-min))
    (run-hooks 'calendar-move-hook)))

(defun karna/scroll-year-calendar-backward (&optional arg event)
  "Scroll the yearly calendar by year in a backward direction."
  (interactive (list (prefix-numeric-value current-prefix-arg)
		     last-nonmenu-event))
  (karna/scroll-year-calendar-forward (- (or arg 1)) event))

(defalias 'year-calendar 'karna/year-calendar)

;;; KEYBINDINGS

(use-package evil-nerd-commenter
  :ensure t
  :defer t)

(use-package general
  :ensure t
  :config
  (general-evil-setup)

  ;; set up 'SPC' as the global leader key
  (general-create-definer karna/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC" ;; set leader
    :global-prefix "M-SPC") ;; access leader in insert mode

  (karna/leader-keys
    "SPC" '(counsel-M-x :wk "Counsel M-x")
    "." '(find-file :wk "Find file")
    "=" '(perspective-map :wk "Perspective") ;; Lists all the perspective keybindings
    "/" '(evilnc-comment-or-uncomment-lines :wk "Toggle Comments")
    "TAB TAB" '(comment-line :wk "Comment lines")
    "u" '(universal-argument :wk "Universal argument"))

   (karna/leader-keys
    "a" '(:ignore t :wk "A.I.")
    "a a" '(ellama-ask-about :wk "Ask ellama about region")
    "a c" '(:prefix "c" :wk "Code")
    "a c a" '(ellama-code-add :wk "Ellama code add")
    "a c c" '(ellama-code-complete :wk "Ellama code complete")
    "a c e" '(ellama-code-edit :wk "Ellama code edit")
    "a c i" '(ellama-code-improve :wk "Ellama code improve")
    "a c r" '(ellama-code-review :wk "Ellama code review")
    "a e" '(:ignore t :wk "Ellama enhance")
    "a e g" '(ellama-improve-grammar :wk "Ellama enhance wording")
    "a e w" '(ellama-improve-wording :wk "Ellama enhance grammar")
    "a i" '(ellama-chat :wk "Ask ellama")
    "a p" '(ellama-provider-select :wk "Ellama provider select")
    "a s" '(ellama-summarize :wk "Ellama summarize region")
    "a t" '(ellama-translate :wk "Ellama translate region"))

  (karna/leader-keys
    "b" '(:ignore t :wk "Bookmarks/Buffers")
    "b b" '(switch-to-buffer :wk "Switch to buffer")
    "b c" '(clone-indirect-buffer :wk "Create indirect buffer copy in a split")
    "b C" '(clone-indirect-buffer-other-window :wk "Clone indirect buffer in new window")
    "b d" '(bookmark-delete :wk "Delete bookmark")
    "b I" '(consult-buffer :wk "Preview buffers")
    "b i" '(persp-switch-to-buffer* :wk "Persp Ibuffer")
    "b k" '(kill-current-buffer :wk "Kill current buffer")
    "b K" '(kill-some-buffers :wk "Kill multiple buffers")
    "b l" '(list-bookmarks :wk "List bookmarks")
    "b m" '(bookmark-set :wk "Set bookmark")
    "b n" '(next-buffer :wk "Next buffer")
    "b p" '(previous-buffer :wk "Previous buffer")
    "b r" '(revert-buffer :wk "Reload buffer")
    "b R" '(rename-buffer :wk "Rename buffer")
    "b s" '(basic-save-buffer :wk "Save buffer")
    "b S" '(save-some-buffers :wk "Save multiple buffers")
    "b w" '(bookmark-save :wk "Save current bookmarks to bookmark file"))

  ;; (karna/leader-keys
  ;;   "c" '(:ignore t :wk "Centaur Tabs")
  ;;   "c n" '(centaur-tabs-forward-tab :wk "Next Tab")
  ;;   "c p" '(centaur-tabs-backward-tab :wk "Previous Tab")
  ;;   "c c" '(centaur-tabs-close-tab :wk "Close Tab")
  ;;   "c r" '(centaur-tabs-rename-tab :wk "Rename Tab")
  ;;   "c l" '(centaur-tabs-list-tabs :wk "List Tabs")
  ;;   "c m" '(centaur-tabs-move-current-tab-to-left :wk "Move Tab Left")
  ;;   "c <left>" '(karna/scroll-year-calendar-backward :wk "Scroll year calendar backward")
  ;;   "c <right>" '(karna/scroll-year-calendar-forward :wk "Scroll year calendar forward")
  ;;   "c y" '(karna/year-calendar :wk "Show year calendar")
  ;;   "c t" '(centaur-tabs-move-current-tab-to-right :wk "Move Tab Right"))


  (karna/leader-keys
    "c"  '(:ignore t :wk "Consult")
    ;; Buffer-related commands
    "c b" '(consult-buffer         :wk "Switch Buffer")

    ;; File-related commands
    "c d" '(:prefix "d"             :wk "Consult Directory")
    "c d f" '(consult-dir           :wk "Find Directory")
    "c d j" '(consult-dir-jump-file :wk "Jump to a directory")
    "c f" '(consult-fd           :wk "Find File")
    "c r" '(consult-recent-file    :wk "Recent Files")

    ;; Search commands
    "c l" '(consult-line           :wk "Search Lines")
    "c g" '(consult-grep           :wk "Grep Search")
    "c p" '(consult-ripgrep        :wk "Ripgrep Search")

    ;; Navigation commands
    "c i" '(consult-imenu          :wk "Imenu")
    "c o" '(consult-outline        :wk "Outline")

    ;; Other commands
    "c m" '(consult-man            :wk "Man Pages")
    "c k" '(consult-bookmark       :wk "Bookmarks")
    "c y" '(karna/year-calendar    :wk "Show year calendar"))

    (karna/leader-keys
    "d" '(:ignore t :wk "Dired")
    "d d" '(dired :wk "Open dired")
    "d f" '(wdired-finish-edit :wk "Writable dired finish edit")
    "d j" '(dired-jump :wk "Dired jump to current")
    "d n" '(treemacs-find-file :wk "Open file in Treemacs")
    ;; "d n" '(neotree-dir :wk "Open directory in neotree")
    "d p" '(peep-dired :wk "Peep-dired")
    "d w" '(wdired-change-to-wdired-mode :wk "Writable dired")

    ;; New prefix for favorite directories
    "d o" '(:ignore t :wk "Favorite Directories")
    "d o p" `((lambda () (interactive) (dired "/mnt/Karna/Git/portfolio/")) :wk "Open Portfolio")
    "d o P" `((lambda () (interactive) (dired "/mnt/Karna/Git/Project-K/")) :wk "Open Project-K")
    "d o h" `((lambda () (interactive) (dired "~")) :wk "Open Home")
    )

  (karna/leader-keys
    "e" '(:ignore t :wk "Ediff/Eshell/Eval/EWW")
    "e b" '(eval-buffer :wk "Evaluate elisp in buffer")
    "e d" '(eval-defun :wk "Evaluate defun containing or after point")
    "e e" '(eval-expression :wk "Evaluate and elisp expression")
    "e f" '(ediff-files :wk "Run ediff on a pair of files")
    "e F" '(ediff-files3 :wk "Run ediff on three files")
    "e h" '(counsel-esh-history :which-key "Eshell history")
    "e l" '(eval-last-sexp :wk "Evaluate elisp expression before point")
    "e n" '(eshell-new :wk "Create new eshell buffer")
    "e r" '(eval-region :wk "Evaluate elisp in region")
    "e R" '(eww-reload :which-key "Reload current page in EWW")
    "e s" '(eshell :which-key "Eshell")
    "e w" '(eww :which-key "EWW emacs web wowser"))

  (karna/leader-keys
    "f" '(:ignore t :wk "Files")
    "f c" '((lambda () (interactive)
	      (find-file "~/.emacs.d/config.org"))
	    :wk "Open emacs config.org")
    "f e" '((lambda () (interactive)
	      (dired "~/.emacs.d/"))
	    :wk "Open user-emacs-directory in dired")
    "f d" '(find-grep-dired :wk "Search for string in files in DIR")
    "f m" '(ian/format-code :wk "Format Buffer")
    "f g" '(consult-ripgrep :wk "Search for string current file")
    "f i" '((lambda () (interactive)
	      (find-file "~/dotfiles/install.sh"))
	    :wk "Open dotfiles install.sh")
    "f j" '(consult-dir-jump-file :wk "Jump to a file below current directory")
    "f l" '(consult-locate :wk "Locate a file")
    "f r" '(consult-recent-file :wk "Find recent files")
    "f u" '(sudo-edit-find-file :wk "Sudo find file")
    "f U" '(sudo-edit :wk "Sudo edit file"))

  (karna/leader-keys
    "g" '(:ignore t :wk "Git")
    "g /" '(magit-displatch :wk "Magit dispatch")
    "g ." '(magit-file-displatch :wk "Magit file dispatch")
    "g b" '(magit-branch-checkout :wk "Switch branch")
    "g c" '(:ignore t :wk "Create")
    "g c b" '(magit-branch-and-checkout :wk "Create branch and checkout")
    "g c c" '(magit-commit-create :wk "Create commit")
    "g c f" '(magit-commit-fixup :wk "Create fixup commit")
    "g C" '(magit-clone :wk "Clone repo")
    "g f" '(:ignore t :wk "Find")
    "g f c" '(magit-show-commit :wk "Show commit")
    "g f f" '(magit-find-file :wk "Magit find file")
    "g f g" '(magit-find-git-config-file :wk "Find gitconfig file")
    "g F" '(magit-fetch :wk "Git fetch")
    "g g" '(magit-status :wk "Magit status")
    "g i" '(magit-init :wk "Initialize git repo")
    "g l" '(magit-log-buffer-file :wk "Magit buffer log")
    "g r" '(vc-revert :wk "Git revert file")
    "g s" '(magit-stage-file :wk "Git stage file")
    "g t" '(git-timemachine :wk "Git time machine")
    "g u" '(magit-stage-file :wk "Git unstage file"))

 (karna/leader-keys
    "h" '(:ignore t :wk "Help")
    "h a" '(counsel-apropos :wk "Apropos")
    "h b" '(describe-bindings :wk "Describe bindings")
    "h c" '(describe-char :wk "Describe character under cursor")
    "h d" '(:ignore t :wk "Emacs documentation")
    "h d a" '(about-emacs :wk "About Emacs")
    "h d d" '(view-emacs-debugging :wk "View Emacs debugging")
    "h d f" '(view-emacs-FAQ :wk "View Emacs FAQ")
    "h d m" '(info-emacs-manual :wk "The Emacs manual")
    "h d n" '(view-emacs-news :wk "View Emacs news")
    "h d o" '(describe-distribution :wk "How to obtain Emacs")
    "h d p" '(view-emacs-problems :wk "View Emacs problems")
    "h d t" '(view-emacs-todo :wk "View Emacs todo")
    "h d w" '(describe-no-warranty :wk "Describe no warranty")
    "h e" '(view-echo-area-messages :wk "View echo area messages")
    "h f" '(describe-function :wk "Describe function")
    "h F" '(describe-face :wk "Describe face")
    "h g" '(describe-gnu-project :wk "Describe GNU Project")
    "h i" '(info :wk "Info")
    "h I" '(describe-input-method :wk "Describe input method")
    "h k" '(describe-key :wk "Describe key")
    "h l" '(view-lossage :wk "Display recent keystrokes and the commands run")
    "h L" '(describe-language-environment :wk "Describe language environment")
    "h m" '(describe-mode :wk "Describe mode")
    "h r" '(:ignore t :wk "Reload")
    "h r r" '((lambda () (interactive)
		(load-file "~/.emacs.d/init.el")
		(ignore (elpaca-process-queues)))
		 ;; (karna/org-colors-doom-one) ;; Reapply colors after reloading
	    :wk "Reload emacs config"r)
    "h t" '(consult-theme :wk "Load theme")
    "h v" '(describe-variable :wk "Describe variable")
    "h w" '(where-is :wk "Prints keybinding for command if set")
    "h x" '(describe-command :wk "Display full documentation for command"))

  (karna/leader-keys
    "m" '(:ignore t :wk "Org")
    "m a" '(org-agenda :wk "Org agenda")
    "m e" '(org-export-dispatch :wk "Org export dispatch")
    "m i" '(org-toggle-item :wk "Org toggle item")
    "m t" '(org-todo :wk "Org todo")
    "m B" '(org-babel-tangle :wk "Org babel tangle")
    "m T" '(org-todo-list :wk "Org todo list"))

  (karna/leader-keys
    "i" '(:ignore t :wk "Custom")
    "i a" '(karna/insert-auto-tangle-tag :wk "Insert auto-tangle tag"))

  (karna/leader-keys
    "q" '(:ignore t :wk "Quit")
    "q q" '(evil-quit :wk " Quit Emacs"))

  (karna/leader-keys
    "m b" '(:ignore t :wk "Tables")
    "m b -" '(org-table-insert-hline :wk "Insert hline in table"))

  (karna/leader-keys
    "m d" '(:ignore t :wk "Date/deadline")
    "m d t" '(org-time-stamp :wk "Org time stamp"))

  (karna/leader-keys
    "o" '(:ignore t :wk "Open")
    "o d" '(dashboard-open :wk "Dashboard")
    "o e" '(elfeed :wk "Elfeed RSS")
    "o f" '(make-frame :wk "Open buffer in new frame")
    "o p" '(open-python-right-side :wk "Open Python REPL")
    "o F" '(select-frame-by-name :wk "Select frame by name"))

  ;; projectile-command-map already has a ton of bindings
  ;; set for us, so no need to specify each individually.
  (karna/leader-keys
    "p" '(projectile-command-map :wk "Projectile")
    "P a" '(projectile-add-known-project :wk "Add root to known projects"))

  (karna/leader-keys
    "P" '(projectile-command-map :wk "Custom Previews")
    "P m" '(markdown-preview-mode :wk "Preview Markdown Document"))

  (karna/leader-keys
    "r" '(:ignore t :wk "Org-roam")
    "r c" '(completion-at-point :wk "Completion at point")
    "r f" '(org-roam-node-find :wk "Find node")
    "r g" '(org-roam-graph :wk "Show graph")
    "r t" '(org-roam-dailies-goto-today :wk "Show today note")
    "r i" '(org-roam-node-insert :wk "Insert node")
    "r n" '(org-roam-capture :wk "Capture to node")
    "r d" '(:prefix "d" :wk "Dailies")
    "r d c" '(:prefix "c" :wk "Capture")
    "r d c c" '(org-roam-dailies-capture-today :wk "Capture Today")
    "r d c y" '(org-roam-dailies-capture-yesterday :wk "Capture Yesterday")
    "r d c t" '(org-roam-dailies-capture-tomorrow :wk "Capture Tomorrow")
    "r d c d" '(org-roam-dailies-capture-date :wk "Capture Specific Date")
    "r d g" '(:prefix "g" :wk "Go to")
    "r d g g" '(org-roam-dailies-goto-today :wk "Go to Today")
    "r d g y" '(org-roam-dailies-goto-yesterday :wk "Go to Yesterday")
    "r d g t" '(org-roam-dailies-goto-tomorrow :wk "Go to Tomorrow")
    "r d g d" '(org-roam-dailies-goto-date :wk "Go to Specific Date")
    "r d g n" '(org-roam-dailies-goto-next-note :wk "Go to Next Date")
    "r d g d" '(org-roam-dailies-goto-previous-note :wk "Go to Previous Date")
    "r s" '(org-id-get-create :wk "Create Small node inside buffer")
    "r a" '(org-roam-alias-add :wk "Create alias for a roam")
    "r r" '(org-roam-buffer-toggle :wk "Toggle roam buffer"))


  (karna/leader-keys
    "s" '(:ignore t :wk "Search")
    "s d" '(dictionary-search :wk "Search dictionary")
    "s m" '(man :wk "Man pages")
    "s s" '(occur :wk "Search buffer")
    "s p" '(pdf-occur :wk "Pdf search lines matching STRING")
    "s t" '(tldr :wk "Lookup TLDR docs for a command")
    "s r" '(query-replace :wk "Search and replace")
    "s w" '(woman :wk "Similar to man but doesn't require man"))

  (karna/leader-keys
    "t" '(:ignore t :wk "Toggle")
    "t c" '(company-mode :wk "Toggle Company Mode")
    "t e" '(eshell-toggle :wk "Toggle eshell")
    "t f" '(flycheck-mode :wk "Toggle flycheck")
    "t l" '(display-line-numbers-mode :wk "Toggle line numbers")
    "t h" '(my/toggle-ef-theme :wk "Toggle ef-themes")
    "t n" '(treemacs :wk "Toggle Treemacs")
    ;;"t n" '(neotree-toggle :wk "Toggle neotree file viewer")
    "t o" '(org-mode :wk "Toggle org mode")
    "t r" '(rainbow-mode :wk "Toggle rainbow mode")
    "t t" '(tabnine-mode :wk "Toggle Tabnine mode")
    "t T" '(visual-line-mode :wk "Toggle truncated lines")
    "t v" '(vterm-toggle :wk "Toggle vterm"))

  (karna/leader-keys
    "w" '(:ignore t :wk "Windows/Words")
    ;; Window splits
    "w c" '(evil-window-delete :wk "Close window")
    "w n" '(evil-window-new :wk "New window")
    "w s" '(evil-window-split :wk "Horizontal split window")
    "w v" '(evil-window-vsplit :wk "Vertical split window")
    ;; Window motions
    "w h" '(evil-window-left :wk "Window left")
    "w j" '(evil-window-down :wk "Window down")
    "w k" '(evil-window-up :wk "Window up")
    "w l" '(evil-window-right :wk "Window right")
    "w w" '(evil-window-next :wk "Goto next window")
    "w <left>" '(evil-window-left :wk "Window left")
    "w <down>" '(evil-window-down :wk "Window down")
    "w <up>" '(evil-window-up :wk "Window up")
    "w <right>" '(evil-window-right :wk "Window right")
    ;; Move Windows
    "w H" '(buf-move-left :wk "Buffer move left")
    "w J" '(buf-move-down :wk "Buffer move down")
    "w K" '(buf-move-up :wk "Buffer move up")
    "w L" '(buf-move-right :wk "Buffer move right")
    ;; Words
    "w d" '(downcase-word :wk "Downcase word")
    "w u" '(upcase-word :wk "Upcase word")
    "w =" '(count-words :wk "Count words/lines for buffer"))
)

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") nil)
(global-set-key (kbd "<C-wheel-down>") nil)
(global-set-key (kbd "C-a") 'mark-whole-buffer) ;; Selects whole buffer to copy/delete

;; Binds `C-s` to compile and view the latex preview document.
(add-hook 'LaTeX-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-s") #'TeX-command-run-all)))
