;;; org-roam-config.el --- Org Roam Config -*- lexical-binding: t; -*-

(use-package org-roam
  :ensure t
  :defer t
  :init
  (setq org-roam-v2-ack t)   ; Acknowledge the v2 upgrade message
  :custom
  (org-roam-db-autosync-mode)           ; Automatically sync the Org Roam database
  (org-roam-completion-everywhere t)     ; Enable completion everywhere
  ;; (org-roam-dailies-capture-templates
  ;;     '(("d" "default" entry "* %<%I:%M %p>: %?"
  ;;        :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
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
     ("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
			  "#+title: ${title}\n#+filetags: Project")
      :unnarrowed t)))
  :config
  (org-roam-setup))

;;; ORG ROAM DIRECTORIES & GRAPH VIEWER

(with-eval-after-load 'org
  (setq org-roam-directory "/mnt/Karna/Git/Project-K/Org/Roam/"
	org-roam-graph-viewer "/usr/bin/zen-browser"))

(setq org-roam-dailies-directory "/mnt/Karna/Git/Project-K/Org/Journal/")

;;; ORG JOURNAL SETUP

(setq org-journal-dir "/mnt/Karna/Git/Project-K/Org/Journal/"
      org-journal-date-prefix "* "
      org-journal-time-prefix "** "
      org-journal-date-format "%B %d, %Y (%A) "
      org-journal-file-format "%Y-%m-%d.org")


(provide 'org-roam-config)
;; org-roam-config.el ends here
