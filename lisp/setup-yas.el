;;; setup-yas.el --- Yasnippet Configurations -*- lexical-binding: t; -*-

;;; Commentary:

;; Yasnippet - Snippet system for emacs configurations

;;; Code:

(use-package yasnippet
  :ensure t
  :hook ((prog-mode org-mode LaTeX-mode) . yas-minor-mode)
  :custom
  (yas-snippet-dirs (list (expand-file-name "~/.emacs.d/snippets/")))
  (yas-triggers-in-field t)
  :config
  ;; Load system snippets after yasnippet-snippets is loaded
  (with-eval-after-load 'yasnippet-snippets
    (add-to-list 'yas-snippet-dirs yasnippet-snippets-dir)))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet
  :config
  (yas-reload-all))


(provide 'setup-yas)
;;; setup-yas.el ends here
