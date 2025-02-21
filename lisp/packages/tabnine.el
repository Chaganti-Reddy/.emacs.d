;;; packages/tabnine.el --- TabNine Package -*- lexical-binding: t; -*-

(use-package tabnine
  :commands (tabnine-start-process tabnine-mode)
  :ensure t
  :diminish "⌬"
  :custom
  (tabnine-wait 1)
  (tabnine-minimum-prefix-length 2)
  ;; :hook
  ;; ((prog-mode . tabnine-mode)
  ;; (org-mode . tabnine-mode)
  ;; (LaTeX-mode . tabnine-mode)
  ;; (text-mode . tabnine-mode)
  ;; (kill-emacs . tabnine-kill-process))
  :config
  (add-to-list 'completion-at-point-functions #'tabnine-completion-at-point)
  (tabnine-start-process)
  :bind
  (:map tabnine-completion-map
    ("<tab>" . tabnine-accept-completion)
    ("M-f" . tabnine-accept-completion-by-word)
    ("M-<return>" . tabnine-accept-completion-by-line)
    ("C-g" . tabnine-clear-overlay)
    ("M-[" . tabnine-previous-completion)
    ("M-]" . tabnine-next-completion)))


(provide 'packages/tabnine)
;; packages/tabnine.el ends here
