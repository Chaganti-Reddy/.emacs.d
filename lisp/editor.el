;;; custom.el --- Custom Settings for Emacs -*- lexical-binding: t; -*-

;; Set the default browser function and program
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "zen-browser"
      browse-url-default-program "zen-browser")

;; Specify browser for Org mode HTML export (optional)
(setq org-html-htmlize-browser "zen-browser")

;; Configure file associations for Org mode
(setq org-file-apps
      '(("auto-mode" . emacs)          ;; Open files in Emacs by default
	("\\.mm\\'" . default)         ;; Use system default for .mm files
	("\\.x?html?\\'" . "zen-browser %s")  ;; Open HTML files in zen-browser
	("\\.pdf\\'" . "~/.local/bin/zathura %s"))) ;; Open PDFs in Zathura

;; Set default system file opener using xdg-open
(setcdr (assq 'system org-file-apps-gnu) "xdg-open %s")

;; Workaround for a known issue with xdg-open when opening files in Org mode
(advice-add 'org-open-file :around
	    (lambda (orig-fun &rest args)
	      (let ((process-connection-type nil))
		(apply orig-fun args))))

;; ----------------------------------------------------------------------------
;; RECENTF SETTINGS
;; ----------------------------------------------------------------------------

(require 'recentf)

;; Configure recentf settings
(setq recentf-save-file (dir-concat user-cache-directory "recentf")
      recentf-max-saved-items 200    ;; Store up to 200 recent files
      recentf-auto-cleanup 180)      ;; Auto-cleanup old entries every 180 seconds

;; Exclude specific files and directories from tracking
(setq recentf-exclude
      '("~/.cache/emacs/"             ;; Exclude everything inside ~/.cache/emacs/
	"/mnt/Karna/Git/Project-K/Org/Tasks.org" ;; Exclude this specific Org file
	"_region_\\.tex$"              ;; Ignore temporary LaTeX region files
	"^/tmp/"))                     ;; Exclude everything inside /tmp/

;; Enable recentf mode
(recentf-mode 1)

;; ----------------------------------------------------------------------------
;; BACKUP SETTINGS
;; ----------------------------------------------------------------------------

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

;; ----------------------------------------------------------------------------
;; FORMATTING SETTINGS
;; ----------------------------------------------------------------------------

(use-package format-all
  :ensure t
  :defer t
  :preface
  (defun ian/format-code ()
    "Auto-format the entire buffer. If in `prolog-mode', call `prolog-indent-buffer';  if the buffer is managed by Eglot and the LSP server supports document formatting, call `eglot-format-buffer'; otherwise, call `format-all-buffer'."
    (interactive)
    (cond
     ((derived-mode-p 'prolog-mode)
      (prolog-indent-buffer))
     ((and (eglot-managed-p)
	   (eglot--server-capable :documentFormattingProvider))
      (eglot-format-buffer))
     (t (format-all-buffer))))
  :hook (prog-mode . format-all-ensure-formatter))

;; ----------------------------------------------------------------------------
;; UNDO MANAGEMENT
;; ----------------------------------------------------------------------------

;; The `undo-fu` package provides a more intuitive undo system than the default Emacs undo.
(use-package undo-fu
  :ensure t
  :defer t
  :config
  (setq undo-fu-allow-undo-in-region t) ;; Allow undo in active region
  (setq undo-fu-ignore-keyboard-quit t)) ;; Prevent undo from resetting on C-g

;; The `undo-fu-session` package saves and restores undo history across Emacs sessions.
(use-package undo-fu-session
  :ensure t
  :defer t
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  (setq undo-fu-session-directory (dir-concat user-cache-directory "undo-fu-session/")) ;; Store undo history in cache
  :hook ((prog-mode conf-mode text-mode tex-mode) . undo-fu-session-mode))

;;; JARCHIVE

(use-package jarchive
  :ensure t
  :after eglot
  :config
  (jarchive-setup))

;; ----------------------------------------------------------------------------
;; BREADCRUMB FOR EMACS
;; ----------------------------------------------------------------------------

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

;; ----------------------------------------------------------------------------
;; DIMINISH
;; ----------------------------------------------------------------------------

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

;; ----------------------------------------------------------------------------
;; RAINBOW MODE
;; ----------------------------------------------------------------------------

(use-package rainbow-mode
  :ensure t
  :defer t
  :diminish
  :hook org-mode prog-mode)

;; ----------------------------------------------------------------------------
;; RAINBOW DELIMITERS
;; ----------------------------------------------------------------------------

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
	 (clojure-mode . rainbow-delimiters-mode)))

;; ----------------------------------------------------------------------------
;; DRAG STUFF
;; ----------------------------------------------------------------------------

(use-package drag-stuff
  :ensure t
  :defer t
  :diminish
  :init
  (drag-stuff-global-mode 1)
  (drag-stuff-define-keys))

;; ----------------------------------------------------------------------------
;; SOME EXTRA STUFF
;; ----------------------------------------------------------------------------

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

;; ----------------------------------------------------------------------------
;; HIGHLIGHT INDENTATION GUIDES
;; ----------------------------------------------------------------------------

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

;; ----------------------------------------------------------------------------
;; FLYCHECK FOR EMACS
;; ----------------------------------------------------------------------------

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


(provide 'editor)
;;; editor.el ends here
