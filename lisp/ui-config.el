;;; ui-config.el --- UI Configurations -*- lexical-binding: t; -*-

;; ----------------------------------------------------------------------------
;; THEMES
;; ----------------------------------------------------------------------------

(use-package ef-themes
  :ensure t
  :config
  (defvar my/current-ef-theme 'ef-winter
    "Stores the current theme to toggle between `ef-cyprus` and `ef-winter`.")

  (defun my/toggle-ef-theme ()
    "Toggle between `ef-cyprus` and `ef-winter` themes."
    (interactive)
    (setq my/current-ef-theme (if (eq my/current-ef-theme 'ef-cyprus)
				  'ef-winter
				'ef-cyprus))
    (ef-themes-select my/current-ef-theme)
    (message "Switched to %s" my/current-ef-theme))

  ;; Load default theme
  (ef-themes-select my/current-ef-theme))

;; ----------------------------------------------------------------------------
;; FONT SETTINGS
;; ----------------------------------------------------------------------------

(defvar my/default-font "JetBrainsMono Nerd Font"
  "Default font for Emacs.")

(add-to-list 'default-frame-alist `(font . ,(format "%s-12:bold" my/default-font)))

(set-face-attribute 'default nil
		    :font my/default-font
		    :height 120
		    :weight 'bold)

(set-face-attribute 'fixed-pitch nil
		    :font my/default-font
		    :height 130
		    :weight 'bold)

(set-face-attribute 'variable-pitch nil
		    :font my/default-font
		    :height 120
		    :weight 'bold)


;; Italics for comments & keywords
(set-face-attribute 'font-lock-comment-face nil :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil :slant 'italic)

(setq-default line-spacing 0)

(set-display-table-slot standard-display-table 'truncation (make-glyph-code ?…))
(set-display-table-slot standard-display-table 'wrap (make-glyph-code ?–))

;; ----------------------------------------------------------------------------
;;; ICONS
;; ----------------------------------------------------------------------------

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :ensure t
  :defer t
  :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))

(use-package all-the-icons-completion
  :ensure t
  :defer t
  :hook (marginalia-mode . #'all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package nerd-icons :defer t
  :custom
  (nerd-icons-color-icons t))

;;; BEACON

(use-package beacon
  :ensure t
  :defer t
  :init
  ;;(setq beacon-size 40)
  ;;(setq beacon-color "#ff00ff")
  (beacon-mode 1))


(provide 'ui-config)
;;; ui-config.el ends here
