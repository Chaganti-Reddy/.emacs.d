;;; setup-completions-default.el --- Completion System Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Completion system using Emacs 30 built in completion system

;;; Code:

(use-package completion-preview
  :ensure nil
  :hook ((org-mode LaTeX-mode) . completion-preview-mode)
  :bind
  ( :map completion-preview-active-mode-map
    ("C-n" . completion-preview-next-candidate)
    ("C-p" . completion-preview-prev-candidate)))

(provide 'setup-completions-default)

;;; setup-completions-default.el ends here
