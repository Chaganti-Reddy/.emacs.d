;;; setup-roam.el --- Org Roam Config  -*- lexical-binding: t; -*-

;;; Commentary:

;; My ORG Roam configuration

;;; Code:

(use-package org-roam
  :ensure t
  :defer t
  :init
  (setq org-roam-v2-ack t)   ; Acknowledge the v2 upgrade message
  :custom
  (org-roam-db-autosync-mode t)         ; Auto-sync the Org Roam database
  (org-roam-completion-everywhere t)    ; Enable completion everywhere
  (org-roam-directory "/mnt/Karna/Git/Project-K/Org/Roam/")
  (org-roam-dailies-directory "/mnt/Karna/Git/Project-K/Org/Journal/")
  (org-roam-graph-viewer "/usr/bin/brave")

  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
			 "#+title: ${title}\n#+date: %U\n")
      :unnarrowed t)
     ("l" "programming language" plain
      "* Characteristics\n\n- Family: %?\n- Inspired by: \n\n* Reference:\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
			 "#+title: ${title}\n")
      :unnarrowed t)
     ("b" "book notes" plain
      (file "/mnt/Karna/Git/Project-K/Org/Templates/BooknoteTemplate.org")
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
			 "#+title: ${title}\n")
      :unnarrowed t)
     ("p" "project" plain
      "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
			 "#+title: ${title}\n#+filetags: Project")
      :unnarrowed t)
     ("r" "reference" plain "%?"
	       :if-new
	       (file+head "reference/${title}.org" "#+title: ${title}\n")
	       :immediate-finish t
	       :unnarrowed t)))
  :config
  (setf (alist-get "^\\*org-roam\\*$" display-buffer-alist
			 nil nil #'equal)
	      '((display-buffer-reuse-window
		 display-buffer-reuse-mode-window
		 display-buffer-below-selected)
		(window-height . 0.4
		 ;; (lambda (win)
		 ;;   (fit-window-to-buffer
		 ;;    win 30))
		 )))
    (defun my/org-roam-node-latex-preview (&rest _)
      (let ((major-mode 'org-mode))
	(org-latex-preview 'buffer)))

    (advice-add 'org-roam-node-insert-section :after
		#'my/org-roam-node-latex-preview)
  (org-roam-setup))

;; ─────────────────────────────────────────────────────
;; ORG ROAM UI - KNOWLEDGE GRAPH VISUALIZATION
;; ─────────────────────────────────────────────────────
(use-package simple-httpd :defer t)

(use-package org-roam-ui
  :ensure t
  :after org-roam
  :defer
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start t))

;; ─────────────────────────────────────────────────────
;; ORG JOURNAL CONFIGURATION
;; ─────────────────────────────────────────────────────
(use-package org-journal
  :ensure t
  :defer
  :custom
  (org-journal-dir "/mnt/Karna/Git/Project-K/Org/Journal/")
  (org-journal-date-prefix "* ")
  (org-journal-time-prefix "** ")
  (org-journal-date-format "%B %d, %Y (%A)")
  (org-journal-file-format "%Y-%m-%d.org"))

(provide 'setup-roam)
;;; setup-roam.el ends here
