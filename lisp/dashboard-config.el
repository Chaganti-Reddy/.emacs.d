;;; dashboard-config.el --- Dashboard config for Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; Dashboard using emacs dashboard and mode line using mood-line config.

;;; Code:

;; ----------------------------------------------------------------------------
;; DASHBOARD CONFIGURATION
;; ----------------------------------------------------------------------------

(use-package dashboard
  :ensure t
  :init
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))
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



;; ----------------------------------------------------------------------------
;; MODELINE
;; ----------------------------------------------------------------------------

;; (use-package doom-modeline
;;   :ensure t
;;   :init (doom-modeline-mode 1)
;;   :custom
;;   (inhibit-compacting-font-caches t)
;;   (doom-modeline-buffer-file-name-style 'relative-from-project)
;;   (doom-modeline-major-mode-icon nil)
;;   (doom-modeline-minor-modes nil)
;;   (doom-modeline-buffer-encoding nil)
;;   (doom-modeline-buffer-state-icon nil)
;;   (doom-modeline-lsp nil)
;;   :hook (after-init . doom-modeline-mode)
;;   :config
;;     (setq doom-modeline-height 15      ;; sets modeline height
;;           doom-modeline-bar-width 4    ;; sets right bar width
;;           doom-modeline-persp-name t   ;; adds perspective name to modeline
;;           doom-modeline-persp-icon t))

(use-package mood-line
  :ensure t
  :config
  (mood-line-mode)  ;; Enable mood-line
  (setq mood-line-format mood-line-format-default)

  ;; Use pretty Fira Code-compatible glyphs
  :custom
  (mood-line-glyph-alist mood-line-glyphs-fira-code))


;; Default format:
;;   * init.el  4:32 Top                                         ELisp  ! Issues: 2
;; (setq mood-line-format mood-line-format-default)

;; Extended format:
;;   * init.el  4:32:52 Top                    SPCx2  LF  UTF-8  ELisp  ! Issues: 2
;; (setq mood-line-format mood-line-format-default-extended)

;; Custom format:
;;   * init.el : ELisp                                     Top 4:32  |  ! Issues: 2
;; (setq mood-line-format
;;       (mood-line-defformat
;;        :left
;;        (((mood-line-segment-buffer-status) . " ")
;;         ((mood-line-segment-buffer-name)   . " : ")
;;         (mood-line-segment-major-mode))
;;        :right
;;        (((mood-line-segment-scroll)             . " ")
;;         ((mood-line-segment-cursor-position)    . "  ")
;;         ((when (mood-line-segment-checker) "|") . "  ")
;;         ((mood-line-segment-checker)            . "  "))))

;; The default set of glyphs:
;;   * myModifiedFile.js  Replace*3                 + main  JavaScript  ! Issues: 2
;; (setq mood-line-glyph-alist mood-line-glyphs-ascii)

;; A set of Fira Code-compatible Unicode glyphs:
;;   ● myModifiedFile.js  Replace×3                 + main  JavaScript  → Issues: 2
;; (setq mood-line-glyph-alist mood-line-glyphs-fira-code)

;; A set of Unicode glyphs:
;;   ● myModifiedFile.js  Replace✕3                 🞤 main  JavaScript  ⚑ Issues: 2
;; (setq mood-line-glyph-alist mood-line-glyphs-unicode)

;; ----------------------------------------------------------------------------
;; DIMINISH UNNECESSARY MINOR MODES
;; ----------------------------------------------------------------------------

(use-package minions
  :ensure t
  :config
  (minions-mode 1)
  (setq minions-mode-line-lighter ";")  ;; Customize separator for hidden minor mode
  )


(provide 'dashboard-config)
;;; dashboard-config.el ends here
