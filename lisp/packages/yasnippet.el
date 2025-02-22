;;; packages/yasnippet.el --- Yasnippet Package -*- lexical-binding: t; -*-

(use-package yasnippet
  :ensure t
  :diminish
  :hook
  ((prog-mode . yas-minor-mode)
   (text-mode . yas-minor-mode))
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets/")) ;; Ensure your custom snippet directory is included
  (yas-reload-all))

(add-hook 'LaTeX-mode-hook #'yas-minor-mode)
(setq yas-triggers-in-field t)

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet
  :config
  (yas-reload-all)
  (yasnippet-snippets-initialize))


(provide 'packages/yasnippet)
;; packages/yasnippet.el ends here
