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

(add-hook 'latex-mode-hook #'yas-minor-mode)

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet
  :config
  (yas-reload-all)
  (yasnippet-snippets-initialize))

;; Keybinding to manually insert snippets
(global-set-key (kbd "C-c y") #'yas-insert-snippet)


(provide 'packages/yasnippet)
;; packages/yasnippet.el ends here
