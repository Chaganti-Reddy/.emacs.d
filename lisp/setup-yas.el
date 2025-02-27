;;; setup-yas.el --- Yasnippet Configurations -*- lexical-binding: t; -*-

;;; Commentary:

;; Yasnippet - Snippet system for emacs configurations

;;; Code:

(use-package yasnippet
  :ensure t
  :defer
  :hook ((prog-mode . yas-minor-mode)
	 (org-mode . yas-minor-mode)
	 (LaTeX-mode . yas-minor-mode))
  :custom
  (yas-snippet-dirs '("~/.emacs.d/snippets/")) ;; Custom snippet directory
  (yas-triggers-in-field t) ;; Allow nested expansion inside fields
  :config
  (yas-reload-all))

(use-package yasnippet-snippets
  :ensure t
  :defer
  :after yasnippet
  :config
  (yas-reload-all))


(provide 'setup-yas)
;;; setup-yas.el ends here
