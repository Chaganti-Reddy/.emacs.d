;;; setup-org.el --- Org Mode Config -*- lexical-binding: t; -*-

;;; Commentary:

;; Org mode config in emacs using some modern stuff

;;; Code:

;;; ============================================================
;;; ORG MODE CONFIGURATION (Optimized & Modernized)
;;; ============================================================

(use-package org
  :ensure nil
  :defer t
  :hook ((org-mode . visual-line-mode)
	 (org-mode . toc-org-enable)
	 (org-mode . org-modern-mode)
	 (org-mode . org-superstar-mode)
	 (org-mode . org-appear-mode)
	 (org-mode . hl-todo-mode)
	 (org-mode . org-auto-tangle-mode)
	 (org-mode . karna/org-mode-visual-fill))
  :config
  (setq org-src-preserve-indentation t
	org-src-fontify-natively t
	org-src-window-setup 'current-window
	org-edit-src-content-indentation 0
	org-src-tab-acts-natively t
	org-insert-heading-respect-content t
	org-checkbox-image "\\([%])\\"
	org-checkbox-hierarchical-statistics nil)

  ;; Prevent '<>' from auto-pairing in Org mode (fix for org-tempo)
  (add-hook 'org-mode-hook
	    (lambda ()
	      (setq-local electric-pair-inhibit-predicate
			  `(lambda (c)
			     (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c)))))))

;;; ============================================================
;;; ORG UI SETTINGS
;;; ============================================================

(use-package org-modern
  :ensure t
  :custom
  (org-modern-todo t)
  (org-modern-table t)
  (org-modern-variable-pitch t)
  (org-modern-block-fringe t)
  (org-modern-star '("◉" "○" "◆" "◇" "▶"))
  (org-modern-list '((?- . "•") (?+ . "➤") (?* . "▹")))
  :init (global-org-modern-mode))

(use-package org-superstar
  :ensure t
  :config
  (setq org-superstar-headline-bullets-list '("◉" "●" "○" "◆" "●" "○" "◆")
	org-superstar-itembullet-alist '((?+ . ?➤) (?- . ?✦))))

(use-package org-appear
  :ensure t)

(use-package toc-org
  :ensure t
  :defer t)

;;; ------------------------------------------------------------
;;; ORG HEADER FACES
;;; ------------------------------------------------------------

(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.4))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.3))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.2))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.15))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.1))))
 '(org-level-6 ((t (:inherit outline-5 :height 1.05))))
 '(org-level-7 ((t (:inherit outline-5 :height 1.00))))
 '(org-document-title ((t (:height 1.6 :weight bold))))
 '(org-block ((t (:inherit fixed-pitch :background "#282c34"))))
 '(org-table ((t (:inherit fixed-pitch :foreground "#98be65")))))

;;; ------------------------------------------------------------
;;; ORG REMARKS (Highlight & Annotate)
;;; ------------------------------------------------------------

(use-package org-remark
  :ensure t
  :bind (("C-c n m" . org-remark-mark)
	 ("C-c n l" . org-remark-mark-line)
	 :map org-remark-mode-map
	 ("C-c n o" . org-remark-open)
	 ("C-c n ]" . org-remark-view-next)
	 ("C-c n [" . org-remark-view-prev)
	 ("C-c n r" . org-remark-remove)
	 ("C-c n d" . org-remark-delete))
  :config
  (org-remark-global-tracking-mode +1)  ;; Moved inside :config to ensure `org-remark` is loaded
  ;; Optional modes
  (with-eval-after-load 'nov
    (org-remark-nov-mode +1))
  (with-eval-after-load 'info
    (org-remark-info-mode +1)))

;;; ------------------------------------------------------------
;;; HIGHLIGHT TODO KEYWORDS
;;; ------------------------------------------------------------

(use-package hl-todo
  :ensure t
    :hook ((prog-mode . hl-todo-mode))
  :config
  (setq hl-todo-highlight-punctuation ":"
	hl-todo-keyword-faces
	'(("TODO" . warning)
	  ("FIXME" . error)
	  ("HACK" . font-lock-constant-face)
	  ("REVIEW" . font-lock-keyword-face)
	  ("NOTE" . success)
	  ("DEPRECATED" . font-lock-doc-face))))

;;; ------------------------------------------------------------
;;; CENTER ORG MODE FOR BETTER READING
;;; ------------------------------------------------------------

(defun karna/org-mode-visual-fill ()
  "Enable centered text layout in Org mode."
  (setq visual-fill-column-width 180
	visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :ensure t)

;;; ------------------------------------------------------------
;;; ORG AUTO TANGLE
;;; ------------------------------------------------------------

(use-package org-auto-tangle
  :ensure t
  :diminish
  :config (setq org-auto-tangle-default t))

(defun karna/insert-auto-tangle-tag ()
  "Insert auto-tangle tag in a literate config."
  (interactive)
  (org-end-of-line)
  (newline)
  (insert "#+auto_tangle: t"))

;; ------------------------------------------------
;; ORG MERMAID
;; ------------------------------------------------

(use-package mermaid-mode
  :ensure t)

;; Install Mermaid CLI using - sudo npm install -g @mermaid-js/mermaid-cli
(use-package ob-mermaid
  :ensure t
  :config
  (setq ob-mermaid-cli-path "/usr/bin/mmdc") ;; Adjust this path to your mermaid-cli
  (org-babel-do-load-languages 'org-babel-load-languages
			       '((mermaid . t))))

;; ------------------------------------------------
;; ORG EVAL
;; ------------------------------------------------

(setq org-confirm-babel-evaluate nil
      org-babel-clojure-backend 'cider
      org-babel-lisp-eval-fn #'sly-eval)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((sqlite . t) (haskell . t) (emacs-lisp . t) (shell . t) (python . t)
   (C . t) (lua . t) (dot . t) (java . t)
   (lisp . t) (clojure . t) (scheme . t)
   (forth . t) (R . t)))

;; ------------------------------------------------
;; ORG MODE CORE SETTINGS
;; ------------------------------------------------

(setq org-directory "/mnt/Karna/Git/Project-K/Org/"
      org-default-notes-file (expand-file-name "notes.org" org-directory)
      org-ellipsis " ⬎ "
      org-superstar-headline-bullets-list '("◉" "●" "○" "◆" "●" "○" "◆")
      org-superstar-itembullet-alist '((?+ . ?➤) (?- . ?✦)) ; changes +/- symbols in item lists
      org-hide-emphasis-markers t
      ;; ex. of org-link-abbrev-alist in action
      ;; [[arch-wiki:Name_of_Page][Description]]
      org-link-abbrev-alist
      '(("google" . "http://www.google.com/search?q=")
	("arch-wiki" . "https://wiki.archlinux.org/index.php/")
	("ddg" . "https://duckduckgo.com/?q=")
	("wiki" . "https://en.wikipedia.org/wiki/"))
      org-table-convert-region-max-lines 20000)


(setq org-todo-keywords
      '((sequence "IDEA(i)"      ; Generate research ideas
		  "LIT(l)"       ; Conduct literature review
		  "CODE(c)"      ; Develop code/algorithms
		  "TEST(t)"      ; Test implementations or experiments
		  "WRITE(w)"     ; Document findings or draft manuscripts
		  "REVIEW(r)"    ; Revise based on feedback
		  "|"
		  "SUBMITTED(s)" ; Work submitted for review/publication
		  "PUBLISHED(p)" ; Work published (or defended)
		  "ABANDONED(x)") ; Project discontinued
	(sequence "TODO(T)"       ; Basic task: not yet started
		  "NEXT(n)"       ; Basic task: immediate next action
		  "|"
		  "DONE(d!)"))) ; Basic task: completed

;; --------------------------------------------------
;; ORG EXPORT
;; --------------------------------------------------

(setq org-export-backends '(md org ascii html icalendar latex odt)
      org-export-with-toc nil)

;; Load export backends
(require 'ox-md)
(require 'ox-org)

;; --------------------------------------------------
;; ORG TEMPO
;; --------------------------------------------------

;; | Typing the below + TAB | Expands to ...                          |
;; |------------------------+-----------------------------------------|
;; | <a                     | '#+BEGIN_EXPORT ascii' … '#+END_EXPORT  |
;; | <c                     | '#+BEGIN_CENTER' … '#+END_CENTER'       |
;; | <C                     | '#+BEGIN_COMMENT' … '#+END_COMMENT'     |
;; | <e                     | '#+BEGIN_EXAMPLE' … '#+END_EXAMPLE'     |
;; | <E                     | '#+BEGIN_EXPORT' … '#+END_EXPORT'       |
;; | <h                     | '#+BEGIN_EXPORT html' … '#+END_EXPORT'  |
;; | <l                     | '#+BEGIN_EXPORT latex' … '#+END_EXPORT' |
;; | <q                     | '#+BEGIN_QUOTE' … '#+END_QUOTE'         |
;; | <s                     | '#+BEGIN_SRC' … '#+END_SRC'             |
;; | <v                     | '#+BEGIN_VERSE' … '#+END_VERSE'         |

(require 'org-tempo)


;; ------------------------------------------------
;; ORG REFILE SETTINGS
;; ------------------------------------------------

(setq org-bookmark-names-plist nil) ;; Stop bookmarking on org captures and refiling

(setq org-refile-targets
      '(("Tasks.org" :maxlevel . 1)))

(setq org-hide-drawers '("PROPERTIES"))

;; Save Org buffers after refiling!
(advice-add 'org-refile :after 'org-save-all-org-buffers)

;; --------------------------------------------------
;; ORG AGENDA SETTINGS
;; --------------------------------------------------

(require 'org-habit)

(use-package org-super-agenda
  :ensure t
  :config
  (setq org-super-agenda-groups
	'((:name "📚 Study" :tag "study" :order 1)
	  (:name "🏡 Home" :tag "home" :order 2)
	  (:name "🔔 Reminders" :tag "remainder" :order 3)
	  (:name "💪 Gym / Habits" :tag "gym" :order 4)
	  (:name "📖 Literature Review" :tag "litreview" :order 5)
	  (:name "🧪 Experiments" :tag "experiment" :order 6)
	  (:name "✍️ Writing" :tag "writing" :order 7)
	  (:name "💡 Research" :tag "research" :order 8)
	  (:name "❌ Abandoned Projects" :tag "quit" :order 9)
	  (:name "Other" :auto-group t :order 100))) ; Catch-all group

  (setq org-tag-alist
	'((:startgroup)
	  ("study"    . ?s)  ; Study tasks
	  ("home"     . ?h)  ; Home tasks
	  ("remainder". ?r)  ; Reminders
	  ("gym"      . ?g)  ; Gym/habits
	  ("research" . ?R)  ; Research tasks
	  (:endgroup)
	  ("litreview" . ?l)  ; Literature review (sub-step of research)
	  ("experiment". ?e)  ; Experiments (sub-step of research)
	  ("writing"   . ?w)  ; Writing (sub-step of research)
	  ("quit"      . ?x))) ; Abandoned tasks
  )

(advice-add 'org-agenda-todo :after #'org-save-all-org-buffers)

(setq org-agenda-files (directory-files-recursively org-directory "\\.org$") )
(setq org-agenda-start-on-weekday nil) ; - to see from current day instead of from Monday
(setq org-agenda-start-day "-1d") ; - that's only for seeing week from yesterday, not from today

(setq org-agenda-start-with-log-mode t
      org-log-done 'time
      org-log-into-drawer t)

;; Fancy Priorities Settings
(setq org-fancy-priorities-list '("🟥" "🟧" "🟨")
      org-priority-faces
      '((?A :foreground "#ff6c6b" :weight bold)  ; High priority (🟥)
	(?B :foreground "#98be65" :weight bold)  ; Medium priority (🟧)
	(?C :foreground "#c678dd" :weight bold)) ; Low priority (🟨)
      org-agenda-block-separator 8411)

(setq org-agenda-custom-commands
      '(
	;; 📅 Dashboard: Overview of agenda + key tasks
	("d" "Dashboard"
	 ((agenda "" ((org-deadline-warning-days 7)))
	  (todo "NEXT" ((org-agenda-overriding-header "⚡ Next Actions")))
    (tags-todo "remainder" ((org-agenda-overriding-header "🔔 Reminders")))))

	;; 📚 Research Workflow: Organized research pipeline
  ("r" "Research Workflow"
   ((todo "IDEA"      ((org-agenda-overriding-header "💡 Research Ideas")))
    (todo "LIT"       ((org-agenda-overriding-header "📖 Literature Review")))
    (todo "CODE"      ((org-agenda-overriding-header "💻 Development / Coding")))
    (todo "TEST"      ((org-agenda-overriding-header "🧪 Testing / Experiments")))
    (todo "WRITE"     ((org-agenda-overriding-header "✍️ Writing / Documentation")))
    (todo "REVIEW"    ((org-agenda-overriding-header "📝 Review / Feedback")))
    (todo "SUBMITTED" ((org-agenda-overriding-header "🚀 Submitted Work")))
    (todo "PUBLISHED" ((org-agenda-overriding-header "🌍 Published Work")))
    (todo "ABANDONED" ((org-agenda-overriding-header "❌ Abandoned Projects")))))

	;; 📚 Study & Learning: Study-related tasks
	("s" "Study"
	 ((tags-todo "study" ((org-agenda-overriding-header "📚 Study Tasks")))))

	;; 🏡 Home Tasks: Household-related actions
	("h" "Home"
	 ((tags-todo "home" ((org-agenda-overriding-header "🏡 Home Tasks")))))

	;; 💪 Gym & Habits: Health and fitness tasks
	("g" "Gym / Habits"
	 ((tags-todo "gym" ((org-agenda-overriding-header "💪 Gym / Habits")))))

	;; ❌ Abandoned Projects
	("x" "Abandoned Projects"
	 ((tags-todo "quit" ((org-agenda-overriding-header "❌ Abandoned Projects")))))

	;; 🏷️ Priority-Based View: Tasks grouped by priority
	("p" "Priority View"
	 ((tags "PRIORITY=\"A\""
		((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
		 (org-agenda-overriding-header "🔥 High-Priority Tasks")))
	  (tags "PRIORITY=\"B\""
		((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
		 (org-agenda-overriding-header "⚡ Medium-Priority Tasks")))
	  (tags "PRIORITY=\"C\""
		((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
		 (org-agenda-overriding-header "🔹 Low-Priority Tasks")))))
	))

;; ─────────────────────────────────────────────────────
;; ORG ROAM CONFIGURATION
;; ─────────────────────────────────────────────────────

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
  (org-roam-graph-viewer "/usr/bin/zen-browser")

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
      :unnarrowed t)))
  :config
  (org-roam-setup))

;; ─────────────────────────────────────────────────────
;; ORG ROAM UI - KNOWLEDGE GRAPH VISUALIZATION
;; ─────────────────────────────────────────────────────
(use-package simple-httpd)

(use-package org-roam-ui
  :ensure t
  :after org-roam
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
  :custom
  (org-journal-dir "/mnt/Karna/Git/Project-K/Org/Journal/")
  (org-journal-date-prefix "* ")
  (org-journal-time-prefix "** ")
  (org-journal-date-format "%B %d, %Y (%A)")
  (org-journal-file-format "%Y-%m-%d.org"))


(provide 'setup-org)
;;; setup-org.el ends here 
