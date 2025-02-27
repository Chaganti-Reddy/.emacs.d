;;; setup-minibuffer.el --- Minibuffer Enhancements -*- lexical-binding: t; -*-

;;; Commentary:
;; This module enhances Emacs minibuffer with completion, navigation,
;; contextual actions, and search capabilities.

;;; Code:

;; ----------------------
;; VERTICO (Minibuffer UI)
;; ----------------------
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :hook (rfn-eshadow-update-overlay-hook . vertico-directory-tidy)
  :config
  (require 'vertico-directory) ;; Enable directory navigation
  (setq vertico-resize nil     ;; Disable dynamic resizing
	vertico-cycle t))    ;; Enable cycling through candidates

;; ----------------------
;; MARGINALIA (Annotations)
;; ----------------------
(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map
	 ("M-A" . marginalia-cycle))
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy
			   marginalia-annotators-light
			   nil))
  :init
  (marginalia-mode))

;; ----------------------
;; EMBARK (Contextual Actions)
;; ----------------------
(use-package embark
  :ensure t
  :bind (("M-h" . embark-act))
  :commands (embark-act embark-dwim embark-export embark-collect
			embark-bindings embark-prefix-help-command)
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
	       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
		 nil (window-parameters (mode-line-format . none)))))

;; EMBARK + CONSULT Integration
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook (embark-collection-mode . consult-preview-at-point-mode))


(provide 'setup-minibuffer)
;;; setup-minibuffer.el ends her
