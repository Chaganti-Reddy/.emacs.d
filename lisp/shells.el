;;; shells.el --- Shell Configuration -*- lexical-binding: t; -*-

;; -------------------------------------------------------------------------
;; ESHELL CONFIGURATION
;; -------------------------------------------------------------------------

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

;; -------------------------------------------------------------------------
;; VTERM MULTI
;; -------------------------------------------------------------------------

(use-package multi-vterm
	:config
	(add-hook 'vterm-mode-hook
			(lambda ()
			(setq-local evil-insert-state-cursor 'box)
			(evil-insert-state)))
	(define-key vterm-mode-map [return]                      #'vterm-send-return)

	(setq vterm-keymap-exceptions nil)
	(evil-define-key 'insert vterm-mode-map (kbd "C-e")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-f")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-a")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-v")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-b")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-w")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-u")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-n")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-m")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-p")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-j")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-k")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-r")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-t")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-g")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-c")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-SPC")    #'vterm--self-insert)
	(evil-define-key 'normal vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
	(evil-define-key 'normal vterm-mode-map (kbd ",c")       #'multi-vterm)
	(evil-define-key 'normal vterm-mode-map (kbd ",n")       #'multi-vterm-next)
	(evil-define-key 'normal vterm-mode-map (kbd ",p")       #'multi-vterm-prev)
	(evil-define-key 'normal vterm-mode-map (kbd "i")        #'evil-insert-resume)
	(evil-define-key 'normal vterm-mode-map (kbd "o")        #'evil-insert-resume)
	(evil-define-key 'normal vterm-mode-map (kbd "<return>") #'evil-insert-resume))


(provide 'shells)
;; shells.el ends here
