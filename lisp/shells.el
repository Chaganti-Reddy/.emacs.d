;;; shells.el --- Shell Configuration -*- lexical-binding: t; -*-

;; -------------------------------------------------------------------------
;; ESHELL CONFIGURATION
;; -------------------------------------------------------------------------

(setopt eshell-highlight-prompt nil)

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
  (eshell-toggle-use-project-root t)
  (eshell-toggle-run-command nil)
  (eshell-toggle-init-function #'eshell-toggle-init-ansi-term))

(use-package eshell-syntax-highlighting
  :defer
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
  eshell-visual-commands'("zsh" "bash" "htop" "ssh" "top" "fish"))

;; -------------------------------------------------------------------------
;; VTERM
;; -------------------------------------------------------------------------

(use-package vterm
:ensure t
:defer t
:config
(setq shell-file-name "/bin/sh"
      vterm-max-scrollback 5000))

;; -------------------------------------------------------------------------
;;; VTERM TOGGLE
;; -------------------------------------------------------------------------

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


(provide 'shells)
;; shells.el ends here
