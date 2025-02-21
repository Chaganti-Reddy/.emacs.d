;; packages/git.el --- Git Package -*- lexical-binding: t; -*-

;; ----------------------------------------------------------------------------
;; Git Timemachine 
;; ----------------------------------------------------------------------------

(use-package git-timemachine
  :after git-timemachine
  :defer t
  :hook (evil-normalize-keymaps . git-timemachine-hook)
  :config
    (evil-define-key 'normal git-timemachine-mode-map (kbd "C-j") 'git-timemachine-show-previous-revision)
    (evil-define-key 'normal git-timemachine-mode-map (kbd "C-k") 'git-timemachine-show-next-revision)
)

;; ----------------------------------------------------------------------------
;; Magit 
;; ----------------------------------------------------------------------------

;; Transient is required by Magit for handling popups and keybindings
(use-package transient
  :defer t)

;; Magit - A Git porcelain inside Emacs
(use-package magit
  :after transient  ;; Ensure transient is loaded first
  :defer t          ;; Load Magit when needed
  :custom
  (magit-show-long-lines-warning nil))  ;; Disable long lines warning in Magit
