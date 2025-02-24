;;; packages/dashboard.el --- Dashboard Package -*- lexical-binding: t; -*-

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
  ;; (setq dashboard-startup-banner 'logo)
  (setq dashboard-startup-banner "~/.emacs.d/assets/emacs.png")
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

(setq dashboard-agenda-tags-format 'ignore)       

(setq dashboard-agenda-prefix-format  "%i %s  ")

(setq dashboard-agenda-item-icon "󰸗") ;; Nerd Font calendar icon


(provide 'packages/dashboard)
;;; packages/dashboard.el ends here
