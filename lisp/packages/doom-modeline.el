;;; packages/doom-modeline.el --- Doom Modeline Package -*- lexical-binding: t; -*-

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom
  (inhibit-compacting-font-caches t)
  (doom-modeline-buffer-file-name-style 'relative-from-project)
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-minor-modes nil)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-buffer-state-icon nil)
  (doom-modeline-lsp nil)
  :hook (after-init . doom-modeline-mode)
  :config
    (setq doom-modeline-height 25      ;; sets modeline height
	  doom-modeline-bar-width 5    ;; sets right bar width
	  doom-modeline-persp-name t   ;; adds perspective name to modeline
	  doom-modeline-persp-icon t))


(provide 'packages/doom-modeline)
;;; packages/doom-modeline.el ends here
