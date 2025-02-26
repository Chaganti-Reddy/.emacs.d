;;; setup-git.el --- Git configuration -*- lexical-binding: t; -*-

;;; Commentary: 

;; My git configurations using Git-Timemachine, magit. 

;;; Code:

;; ----------------------------------------------------------------------------
;; Git Timemachine - Browse Git history interactively
;; ----------------------------------------------------------------------------
(use-package git-timemachine
  :defer t
  :hook (evil-normalize-keymaps . git-timemachine-hook)
  :config
  (when (featurep 'evil)
    (evil-define-key 'normal git-timemachine-mode-map (kbd "C-j") 'git-timemachine-show-previous-revision)
    (evil-define-key 'normal git-timemachine-mode-map (kbd "C-k") 'git-timemachine-show-next-revision)))

;; ----------------------------------------------------------------------------
;; Transient - Required for Magit popups and UI
;; ----------------------------------------------------------------------------
(use-package transient
  :defer t)

;; ----------------------------------------------------------------------------
;; Magit - The best Git porcelain for Emacs
;; ----------------------------------------------------------------------------
(use-package magit
  :after transient  ;; Ensure transient is loaded first
  :defer t
  :custom
  (magit-show-long-lines-warning nil)) ;; Disable warning for long lines

;; ----------------------------------------------------------------------------
;; Ediff - Visual diffing in Emacs
;; ----------------------------------------------------------------------------
(setq ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)

(defun karna/ediff-hook ()
  (with-eval-after-load 'ediff
    (define-key ediff-mode-map (kbd "j") 'ediff-next-difference)
    (define-key ediff-mode-map (kbd "k") 'ediff-previous-difference)))

(add-hook 'ediff-keymap-setup-hook 'karna/ediff-hook)


(provide 'setup-git)
;;; setup-git.el ends here
