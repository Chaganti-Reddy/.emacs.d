;;; workspace.el --- Perspective config -*- lexical-binding: t; -*-

;;; Commentary :
;; I am using perspective.el for my workspace management and this is my config.

;;; Code:

(use-package perspective
  :ensure t
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))  ;; Keybinding to open perspective menu
  (persp-suppress-no-prefix-key-warning t)  ;; Suppress annoying prefix key warning
  :config
  (persp-mode 1)  ;; Enable Perspective Mode

  ;; Automatically save perspectives on exit (uncomment if needed)
  ;; (add-hook 'kill-emacs-hook #'persp-state-save)

  ;; Keybinding to manually save perspectives
  (global-set-key (kbd "C-S-s") #'persp-state-save)

  ;; Ensure buffers are grouped by perspective in Ibuffer
  (add-hook 'ibuffer-hook #'persp-ibuffer-set-filter-groups))


(provide 'workspace)
;;; workspace.el ends here
