;;; ui-config.el - Configuration for UI elements. -*- lexical-binding: t -*-

;;; Commentary:

;; UI Configuration for Emacs such as themes, fonts etc..,

;;; Code:

;; ----------------------------------------------------------------------------
;; EF THEMES CONFIGURATION
;; ----------------------------------------------------------------------------
(use-package ef-themes
  :ensure t
  :config
  (defcustom karna/current-ef-theme 'ef-winter
    "Stores the current theme to toggle between `ef-cyprus` and `ef-winter`."
    :type 'symbol
    :group 'karna)

  (defun karna/toggle-ef-theme ()
    "Toggle between `ef-cyprus` and `ef-winter` themes."
    (interactive)
    (setq karna/current-ef-theme
          (if (eq karna/current-ef-theme 'ef-cyprus)
              'ef-winter
            'ef-cyprus))
    (ef-themes-select karna/current-ef-theme)
    (message "Switched to %s" karna/current-ef-theme))

  ;; Load default theme on startup
  (custom-set-variables '(ef-themes-to-toggle (list karna/current-ef-theme)))
  (ef-themes-select karna/current-ef-theme))

;; ----------------------------------------------------------------------------
;; FONT SETTINGS
;; ----------------------------------------------------------------------------
(defvar karna/default-font "JetBrains Mono Nerd Font"
  "Default font for Emacs.")

(defvar karna/font-size 12
  "Default font size for Emacs.")

(let ((font-spec (format "%s-%d:semi-bold" karna/default-font karna/font-size)))
  (add-to-list 'default-frame-alist `(font . ,font-spec))
  (set-face-attribute 'default nil :font font-spec)
  (set-face-attribute 'fixed-pitch nil :font font-spec)
  (set-face-attribute 'variable-pitch nil :font font-spec))

;; Italics for comments & keywords
(set-face-attribute 'font-lock-comment-face nil :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil :slant 'italic)

(setq-default line-spacing 0)

(set-display-table-slot standard-display-table 'truncation (make-glyph-code ?…))
(set-display-table-slot standard-display-table 'wrap (make-glyph-code ?–))

;; ----------------------------------------------------------------------------
;; ICONS
;; ----------------------------------------------------------------------------
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :ensure t
  :if (display-graphic-p)
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package all-the-icons-completion
  :ensure t
  :if (display-graphic-p)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package nerd-icons
  :ensure t
  :if (display-graphic-p)
  :custom
  (nerd-icons-color-icons t))

(provide 'ui-config)
;;; ui-config.el ends here
