;;; packages/minibuffer.el --- Minibuffer Package -*- lexical-binding: t; -*-

;; ----------------------------------------------------------------------------
;; VERTICO
;; ----------------------------------------------------------------------------

(use-package vertico
  :init
  (vertico-mode)
  :hook
  ;; Tidy paths automatically in the minibuffer.
  (rfn-eshadow-update-overlay-hook . vertico-directory-tidy)
  :config
  ;; Load the directory extension once Vertico is loaded.
  (require 'vertico-directory)
  ;; Show more candidates
  ;(setq vertico-count 20)
  ;; Grow/shrink the minibuffer dynamically.
  (setq vertico-resize nil)
  ;; Enable cycling through candidates.
  (setq vertico-cycle t))

;; ----------------------------------------------------------------------------
;; MARGINALIA
;; ----------------------------------------------------------------------------

(use-package marginalia
  :ensure t
  :defer t
  :bind (:map minibuffer-local-map
	 ("M-A" . marginalia-cycle))
  :custom
    (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init (marginalia-mode))

;; ----------------------------------------------------------------------------
;; EMBARK
;; ----------------------------------------------------------------------------

(use-package embark
  :ensure t
  :defer t
  :bind (("M-h" . embark-act))
  :commands (embark-act
	       embark-dwim
	       embark-export
	       embark-collect
	       embark-bindings
	       embark-prefix-help-command)

    :init
    (setq prefix-help-command #'embark-prefix-help-command)

    :config
    ;; Hide the mode line of the Embark live/completions buffers
    (add-to-list 'display-buffer-alist
		 '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
		   nil
		   (window-parameters (mode-line-format . none)))))

  ;;; EMBARK CONSULT

  (use-package embark-consult
     :ensure t
     :after (embark consult)
     :defer t
     :hook
     (embark-collection-mode . consult-preview-at-point-mode))


(provide 'packages/minibuffer)
;;; packages/minibuffer.el ends here
