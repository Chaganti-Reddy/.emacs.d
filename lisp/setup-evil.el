;;; setup-evil.el --- Evil Mode Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Evil Mode configuration which uses vim like keybindings in emacs and makes easy to move around if you have already vim experience

;;; Code:

(use-package evil
  :ensure t
  :demand t
  :init
  (setq evil-want-integration t  ;; Ensure Evil integrates properly with Emacs
	evil-want-keybinding nil ;; Prevent conflicts with evil-collection
	evil-vsplit-window-right t
  evil-respect-visual-line-mode t
	evil-split-window-below t
	evil-undo-system 'undo-fu)  ;; To use Vim-like redo functionality use built in undo-redo
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  ;; Enable Evil keybindings for additional modes
  ;; (add-to-list 'evil-collection-mode-list 'help)
  (evil-collection-init))

(use-package evil-tutor
  :after evil 
  :ensure t)

;; Make RETURN follow links in Org mode while in Evil mode
;; (with-eval-after-load 'evil-maps
;;   (define-key evil-motion-state-map (kbd "SPC") nil)
;;   (define-key evil-motion-state-map (kbd "RET") nil)
;;  (define-key evil-motion-state-map (kbd "TAB") nil))

(setq org-return-follows-link t)

(use-package evil-tex
  :after evil
  :ensure t
  :hook (LaTeX-mode . evil-tex-mode))

;; ----------------------------------------------------------------------------
;; WHICH-KEY - Display available keybindings
;; ----------------------------------------------------------------------------

(use-package which-key
  :ensure t
  :init
  (which-key-mode 1)
  :config
  (setq which-key-side-window-location 'bottom
	which-key-side-window-slot -10
	which-key-side-window-max-height 0.25
	which-key-sort-order #'which-key-key-order-alpha
	which-key-sort-uppercase-first nil
	which-key-add-column-padding 1
	which-key-max-display-columns nil
	which-key-min-display-lines 8
	which-key-idle-delay 0.8
	which-key-max-description-length 25
	which-key-allow-imprecise-window-fit nil
	which-key-separator " → "))


(provide 'setup-evil)
;; setup-evil.el ends here
