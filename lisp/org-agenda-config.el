;;; org-agenda-config.el --- ORG Agenda Config -*- lexical-binding: t; -*-

(require 'org-habit)

(use-package org-super-agenda
  :ensure t
  :config
  (setq org-super-agenda-groups
        '((:name "🔥 Urgent" :priority "A" :order 1) ; High-priority tasks
          (:name "📅 Today" :time-grid t :order 2) ; Tasks for today
          (:name "📚 Research" :tag "research" :order 3) ; Research group
          (:name "📖 Study" :tag "study" :order 4) ; Study tasks
          (:name "🏡 Home" :tag "home" :order 5) ; Home tasks
          (:name "🔔 Remainder" :tag "remainder" :order 6) ; Remainders
          (:name "💪 Gym" :tag "gym" :order 7) ; Gym/habits
          (:name "❌ Quit" :tag "quit" :order 99) ; Abandoned tasks
          (:name "Other" :auto-group t :order 100))) ; Catch-all group

  (setq org-tag-alist
        '((:startgroup)
          ("study"    . ?s) ; Study tasks
          ("home"     . ?h) ; Home tasks
          ("remainder". ?r) ; Remainders
          ("gym"      . ?g) ; Gym/habits
          ("research" . ?R) ; Research tasks
          (:endgroup)
          ("litreview" . ?l) ; Literature review (sub-step of research)
          ("experiment". ?e) ; Experiments (sub-step of research)
          ("writing"   . ?w) ; Writing (sub-step of research)
          ("quit" . ?x))) ; Abandoned tasks

  (org-super-agenda-mode))

(setq org-agenda-time-grid '((daily today require-timed)
                             (900 1200 1300 1700)))  ;; Times in 24-hour format without `:`


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

;; Org Agenda Custom Commands
(setq org-agenda-custom-commands
      '(
	;; Dashboard: Agenda view + Next Tasks + Active Projects
	("d" "Dashboard"
	 ((agenda "" ((org-deadline-warning-days 7)))
	  (todo "NEXT" ((org-agenda-overriding-header "Next Tasks")))
	  (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

	;; Next Tasks: Focused view on tasks marked as NEXT
	("n" "Next Tasks"
	 ((todo "NEXT" ((org-agenda-overriding-header "Next Tasks")))))

	;; Work & Location-Based Tasks: Filter tasks by location tags
	("w" "Work & Location Tasks"
	 ((tags-todo "+@lab")
	  (tags-todo "+@office")
	  (tags-todo "+@errand")
	  (tags-todo "+@home")))

	;; Low-Effort Tasks: Show NEXT tasks with low estimated effort
	("e" "Low-Effort Tasks"
	 ((tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
		     ((org-agenda-overriding-header "Low Effort Tasks")
		      (org-agenda-max-todos 20)
		      (org-agenda-files org-agenda-files)))))

	;; Unified Research Workflow
	("r" "Unified Research Workflow"
	 ((todo "IDEA"      ((org-agenda-overriding-header "Research Ideas")))
	  (todo "LIT"       ((org-agenda-overriding-header "Literature Review")))
	  (todo "CODE"      ((org-agenda-overriding-header "Development / Coding")))
	  (todo "TEST"      ((org-agenda-overriding-header "Testing / Experiments")))
	  (todo "WRITE"     ((org-agenda-overriding-header "Writing / Documentation")))
	  (todo "REVIEW"    ((org-agenda-overriding-header "Revision / Feedback")))
	  (todo "SUBMITTED" ((org-agenda-overriding-header "Submitted Work")))
	  (todo "PUBLISHED" ((org-agenda-overriding-header "Published Work")))
	  (todo "ABANDONED" ((org-agenda-overriding-header "Discontinued Projects")))))

	;; Priority-Based View: Unfinished tasks by custom priority tags
	("v" "Priority View"
	 ((tags "PRIORITY=\"A\""
		((org-agenda-skip-function
		  '(org-agenda-skip-entry-if 'todo 'done))
		 (org-agenda-overriding-header "High-Priority Tasks")))
	  (tags "PRIORITY=\"B\""
		((org-agenda-skip-function
		  '(org-agenda-skip-entry-if 'todo 'done))
		 (org-agenda-overriding-header "Medium-Priority Tasks")))
	  (tags "PRIORITY=\"C\""
		((org-agenda-skip-function
		  '(org-agenda-skip-entry-if 'todo 'done))
		 (org-agenda-overriding-header "Low-Priority Tasks")))
	  (agenda "")
	  (alltodo "")))))


(provide 'org-agenda-config)
;; org-agenda-config.el ends here
