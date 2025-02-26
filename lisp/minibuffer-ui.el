;;; minibuffer-ui.el --- Minibuffer Enhancements -*- lexical-binding: t; -*-

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

;; ----------------------
;; CONSULT (Enhanced Navigation)
;; ----------------------
(use-package consult
  :ensure t
  :bind (("M-s M-g" . consult-grep)      ;; Recursive grep
	 ("M-s M-f" . consult-fd)        ;; Search for file names
	 ("M-s M-o" . consult-outline)   ;; Search file outline (headings)
	 ("M-s M-l" . consult-line)      ;; Search current buffer
	 ("M-s M-b" . consult-buffer)))  ;; Switch buffers, bookmarks, recent files

;; CONSULT-DIR (Quick Directory Switching)
(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir)
	 :map vertico-map
	 ("C-x C-d" . consult-dir)
	 ("C-x C-j" . consult-dir-jump-file)))

;; ----------------------
;; WGREP (Editable Grep Results)
;; ----------------------
(use-package wgrep
  :ensure t
  :bind (:map grep-mode-map
	 ("e" . wgrep-change-to-wgrep-mode)
	 ("C-x C-q" . wgrep-change-to-wgrep-mode)
	 ("C-c C-c" . wgrep-finish-edit)))

;; ----------------------
;; SEARCH ENHANCEMENTS
;; ----------------------
(setq isearch-lazy-count t
      lazy-count-prefix-format "(%s/%s) "
      lazy-count-suffix-format nil
      search-whitespace-regexp ".*?") ;; Treat spaces as wildcards in searches


(provide 'minibuffer-ui)
;;; minibuffer-ui.el ends here
