;;; tabnine-config.el --- Configuration for Tabnine -*- lexical-binding: t -*-

;;; Commentary:

;; Tabnine Inline AI completion system.

;;; Code:

(use-package tabnine
  :ensure t

  ;; Custom settings
  :custom
  (tabnine-wait 1)
  (tabnine-minimum-prefix-length 2)

  ;; Enable Tabnine in specific modes
  ;; :hook
  ;; ((prog-mode . tabnine-mode)
  ;;  (org-mode . tabnine-mode)
  ;;  (LaTeX-mode . tabnine-mode)
  ;;  (text-mode . tabnine-mode)
  ;;  (kill-emacs . tabnine-kill-process))

  ;; Configuration
  :config
  (tabnine-start-process)
  (add-to-list 'completion-at-point-functions #'tabnine-completion-at-point)

  ;; Keybindings for Tabnine completion
  :bind
  (:map tabnine-completion-map
	("<tab>" . tabnine-accept-completion)
	("M-f" . tabnine-accept-completion-by-word)
	("M-<return>" . tabnine-accept-completion-by-line)
	("C-g" . tabnine-clear-overlay)
	("M-[" . tabnine-previous-completion)
	("M-]" . tabnine-next-completion)))


(provide 'tabnine-config)
;;; tabnine-config.el ends here
