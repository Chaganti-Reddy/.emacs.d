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
  :bind (("\C-cl" . org-store-link)
           ("\C-ca" . org-agenda)
           :map org-mode-map
           ("C-c C-x +" . my/org-strike-through-heading)
           ("C-,"   . nil)
           ("C-'"   . nil)
           ("C-c C-M-l" . org-toggle-link-display)
           ("C-S-<right>" . nil)
           ("C-S-<left>" . nil)
           :map org-cdlatex-mode-map
           ("`" . nil)
           (";" . cdlatex-math-symbol)
           :map org-link-navigation-repeat-map
           ("v" . org-link-preview))
  :hook (
	 (org-mode . toc-org-enable)
	 (org-mode . org-modern-mode)
   (org-mode . turn-on-org-cdlatex)
   (org-cdlatex-mode . my/org-cdlatex-settings)
   (org-mode . er/add-latex-in-org-mode-expansions)
   (org-mode . visual-line-mode)
	 (org-mode . karna/org-mode-visual-fill))
  :config
;; General preferences
  (setq-default org-adapt-indentation nil 
                org-cycle-include-plain-lists t 
                org-footnote-auto-label 'confirm
                org-agenda-file-menu-enabled nil
                org-checkbox-image "\\([%])\\"
                org-checkbox-hierarchical-statistics nil
                org-bookmark-names-plist nil
                org-image-actual-width nil
                ;; org-refile-targets '((nil :maxlevel . 2) (org-agenda-files :maxlevel . 3)) 
                org-refile-targets '((nil :maxlevel . 2)
                                     (org-agenda-files :maxlevel . 3)
                                     (org-agenda-files :todo . "PROJECT"))
                org-refile-target-verify-function nil
                org-refile-use-outline-path t
                org-refile-use-cache t
                org-refile-allow-creating-parent-nodes t
                org-outline-path-complete-in-steps nil
                org-use-tag-inheritance t
                org-tags-column 0
                org-special-ctrl-a/e t
                org-special-ctrl-k t
                org-use-fast-todo-selection 'expert
                org-catch-invisible-edits 'smart
                org-imenu-depth 7
                org-extend-today-until 3
                org-M-RET-may-split-line '((headline) (default . nil))
                org-fast-tag-selection-single-key 'expert
                org-link-elisp-confirm-function nil
                org-yank-image-save-method "figures"
                ;; org-indent-indentation-per-level 2 
                org-return-follows-link t)

  ;; Make org use `display-buffer' like every other Emacs citizen.
  (advice-add #'org-switch-to-buffer-other-window :override #'switch-to-buffer-other-window)

  (add-hook 'org-metareturn-hook
              (defun my/auto-checkbox (&rest _)
                (when (org-at-item-checkbox-p)
                  ;; Checkbox: Insert new item with checkbox.
                  (org-insert-todo-heading nil) t)))

  (defun my/org-beginning-of-defun (&optional arg)
      ";TODO: "
      (interactive "p")
      (if (not (texmathp))
          (org-backward-element)
        (let ((lx (save-mark-and-excursion
                    (LaTeX-backward-environment arg)
                    (point)))
              (beg (org-element-begin (org-element-context))))
          (if (> beg lx) (goto-char beg)
            (run-at-time 0 nil #'goto-char lx)
            lx))))

  (defun my/org-end-of-defun (&optional arg)
    ";TODO: "
    (interactive "p")
    (if (not (texmathp))
        (if (not (org-at-heading-p))
	    (org-forward-element)
	  (org-forward-element)
	  (forward-char -1))
      (goto-char (min (save-mark-and-excursion
                        (LaTeX-forward-environment (or arg 1))
                        (point))
                      (org-element-end (org-element-context))))))

  (defun my/org-strike-through-heading (&optional arg)
    "Strike through heading text of current Org item."
    (interactive "P")
    (save-excursion
      (unless (org-at-heading-p)
        (org-previous-visible-heading 1))
      (when (org-at-heading-p)
        (let ((org-special-ctrl-a/e t))
          (org-beginning-of-line)
          (insert "+")
          (org-end-of-line)
          (insert "+")))))

  (defun org-cdlatex-pbb (&rest _arg)
    "Execute `cdlatex-pbb' in LaTeX fragments.
  Revert to the normal definition outside of these fragments."
    (interactive "P")
    (if (org-inside-LaTeX-fragment-p)
        (call-interactively 'cdlatex-pbb)
      (let (org-cdlatex-mode)
        (call-interactively (key-binding (vector last-input-event))))))

  (define-key org-cdlatex-mode-map (kbd "(") #'org-cdlatex-pbb)
  (define-key org-cdlatex-mode-map (kbd "[") #'org-cdlatex-pbb)
  (define-key org-cdlatex-mode-map (kbd "{") #'org-cdlatex-pbb)

  (defun my/org-cdlatex-settings ()
      (define-key org-cdlatex-mode-map (kbd "$") 'cdlatex-dollar)
      ;; (ad-unadvise #'texmathp)
      (advice-remove 'texmathp #'org--math-always-on))

  (defun er/add-latex-in-org-mode-expansions ()
    ;; Make Emacs recognize \ as an escape character in org
    (modify-syntax-entry ?\\ "\\" org-mode-syntax-table)
    ;; Paragraph end at end of math environment
    (setq paragraph-start (concat paragraph-start "\\|\\\\end{\\([A-Za-z0-9*]+\\)}"))
    ;; (setq paragraph-separate (concat paragraph-separate "\\|\\\\end{\\([A-Za-z0-9*]+\\)}"))
    ;; Better forward/backward defun in Org
    (setq-local beginning-of-defun-function 'my/org-beginning-of-defun)
    ;; Latex mode expansions
    (with-eval-after-load 'expand-region
      (set (make-local-variable 'er/try-expand-list)
           (append (cl-set-difference er/try-expand-list
                                      '(er/mark-method-call
                                        er/mark-inside-pairs
                                        er/mark-outside-pairs))
                   '(LaTeX-mark-environment 
                     er/mark-LaTeX-inside-math
                     er/mark-latex-inside-pairs
                     er/mark-latex-outside-pairs
                     er/mark-LaTeX-math)))))

  ;; From the Org manual
  (defun org-summary-todo (n-done n-not-done)
       "Switch entry to DONE when all subentries are done, to TODO otherwise."
       (let (org-log-done org-log-states)   ; turn off logging
         (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

  ;; Prevent '<>' from auto-pairing in Org mode (fix for org-tempo)
  (add-hook 'org-mode-hook
	    (lambda ()
	      (setq-local electric-pair-inhibit-predicate
			  `(lambda (c)
			     (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c)))))))
(advice-add 'org-refile :after 'org-save-all-org-buffers)

(setq org-file-apps
      '(("auto-mode" . emacs)               ;; Open files in Emacs by default
        ("\\.mm\\'" . default)              ;; Use system default for .mm files
        ("\\.x?html?\\'" . "zen-browser %s") ;; Open HTML files in zen-browser
        ("\\.pdf\\'" . "~/.local/bin/zathura %s") ;; Open PDFs in Zathura
        )) 

;; Workaround for xdg-open issue when opening files in Org mode
(defun karna/org-open-file-wrapper (orig-fun &rest args)
  "Fix xdg-open issue in Org mode."
  (let ((process-connection-type nil))
    (apply orig-fun args)))

(advice-add 'org-open-file :around #'karna/org-open-file-wrapper)

(use-package org-id
  :ensure nil
  :defer t
  :config
  (setq org-id-link-to-org-use-id
        'create-if-interactive-and-no-custom-id
        org-id-link-consider-parent-id t
        org-id-search-archives nil))

(use-package org-fold
  :ensure nil
  :defer t 
  :config
  (setf (alist-get 'agenda org-fold-show-context-detail)
        'local))

(use-package org-footnote
  :ensure nil
  :defer t
  :config
  (setq org-footnote-section nil
        org-footnote-define-inline nil))

;; Custom DWIM functionality for `org-ctrl-c-ctrl-c'
(use-package org
  :ensure nil
  :defer t
  :after (org ox-latex org-latex-preview)
  :config
  (defvar my/org-output-buffers
    '("*Org Preview LaTeX Output*"
      "*Org Preview Convert Output*"
      "*Org PDF LaTeX Output*"
      "*Org Preview Preamble Precompilation*"
      "*Org LaTeX Precompilation*"))
  (defun my/org-output-next-buffer ()
    "Cycle through Org output buffers"
    (let* ((org-buffer (current-buffer))
           (bufnames
            (cl-sort
              (cl-remove-if-not
               #'buffer-live-p
               (cl-mapcar #'get-buffer my/org-output-buffers))
              #'> :key #'buffer-modified-tick))
           (next-output-buffer
            (lambda () (interactive)
              (setq bufnames
                    (nconc (cdr bufnames) (list (car bufnames))))
              (switch-to-buffer (car bufnames))))
           (back-to-org
            (lambda () (interactive)
              (while (memq (current-buffer) bufnames)
                (quit-window))
              (pop-to-buffer org-buffer))))
      (when bufnames
        (pop-to-buffer (car bufnames))
        (set-transient-map
         (define-keymap
           "C-c C-c" next-output-buffer
           "<remap> <keyboard-quit>" back-to-org)
         (lambda () (eq (current-buffer) (car bufnames)))))))
  (add-hook 'org-ctrl-c-ctrl-c-final-hook
            #'my/org-output-next-buffer 99))

;;; ============================================================
;;; ORG UI SETTINGS
;;; ============================================================

(use-package org-modern
  :defer t
  :ensure t
  :hook ((org-modern-mode . my/org-modern-spacing))
  :config
  (defun my/org-modern-spacing ()
    (setq-local line-spacing
                (if org-modern-mode
                    0.1 0.0)))
  :custom
  (org-modern-todo t)
  (org-modern-table nil)
  (org-modern-variable-pitch t)
  (org-modern-block-fringe t)
  (org-modern-star '("◉" "○" "◆" "◇" "▶"))
  (org-modern-list '((?- . "•") (?+ . "➤") (?* . "▹"))))

(use-package org-appear
  :disabled
  :ensure t
  :defer t
  :hook (org-mode . org-appear-mode)
  :config
  (setq-default org-hide-emphasis-markers t)
  (setq org-hidden-keywords t)
  (setq org-appear-autoemphasis t
        org-appear-autosubmarkers nil
        org-appear-autolinks nil
        org-appear-autokeywords t))

(use-package toc-org
  :ensure t
  :defer t)

;;; ------------------------------------------------------------
;;; CENTER ORG MODE FOR BETTER READING
;;; ------------------------------------------------------------

(defun karna/org-mode-visual-fill ()
  "Enable centered text layout in Org mode."
  (setq visual-fill-column-width 160
	visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :defer t            
  :ensure t)

;;; ------------------------------------------------------------
;;; ORG AUTO TANGLE
;;; ------------------------------------------------------------

(use-package org-auto-tangle
  :ensure t
  :defer t 
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
  :ensure t :defer t)

;; Install Mermaid CLI using - sudo npm install -g @mermaid-js/mermaid-cli
(use-package ob-mermaid
  :ensure t
  :defer t
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
      org-hide-emphasis-markers t
      org-hide-leading-stars t
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

(use-package htmlize :ensure t :defer t)

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

(use-package org-tempo
  :ensure nil
  :defer t
  :config
  (pcase-dolist (`(,key ,expansion)
                 '(("n" "name")
                   ("lh" "latex_header")
                   ("lc" "latex_class")
                   ("lco" "latex_name: class_options")
                   ("ao" "attr_org")
                   ("al" "attr_latex")
                   ("ah" "attr_html")
                   ("cap" "caption")))
    (setf (alist-get key org-tempo-keywords-alist
                     nil nil #'equal)
          expansion)))

;; --------------------------------------------------
;; ORG AGENDA SETTINGS
;; --------------------------------------------------

(require 'org-habit)

(use-package org-super-agenda
  :ensure t
  :hook (org-agenda-mode . org-super-agenda-mode)
  :config
  (setq org-super-agenda-groups
	'((:name "📚 Study" :tag "study" :order 1)
	  (:name "🏡 Home" :tag "home" :order 2)
	  (:name "🔔 Reminders" :tag "remainder" :order 3)
	  (:name "💪 Gym / Habits" :tag "gym" :order 4)
	  (:name "📖 Review" :tag "litreview" :order 5)
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
(setq org-agenda-window-setup 'current-window) 

(setq org-agenda-hide-tags-regexp ".") ;; alternative --  (setq org-agenda-hide-tags-regexp "\\(work\\|personal\\)")

(setq org-agenda-files
      (append (directory-files-recursively org-directory "\\.org$")
              ;; (directory-files-recursively "/mnt/Karna/Projects/" "\\.org$")
              ))

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

;; ----------------------------------------------------------------------------
;; Org LaTeX Preview Settings
;; ----------------------------------------------------------------------------
(use-package org-latex-preview
  :ensure nil
  :after org
  :config
  ;; Increase preview width
  (plist-put org-latex-preview-appearance-options
             :page-width 0.8)

  ;; Use dvisvgm to generate previews
  ;; You don't need this, it's the default:
  (setq org-latex-preview-process-default 'dvisvgm)
  
  ;; Turn on auto-mode, it's built into Org and much faster/more featured than
  ;; org-fragtog. (Remember to turn off/uninstall org-fragtog.)
  (add-hook 'org-mode-hook 'org-latex-preview-auto-mode)
  (setq org-latex-preview-center-mode t)

  ;; Block C-n, C-p etc from opening up previews when using auto-mode
  (setq org-latex-preview-auto-ignored-commands
        '(next-line previous-line mwheel-scroll
          scroll-up-command scroll-down-command))

  ;; Enable consistent equation numbering
  (setq org-latex-preview-numbered t)

  ;; Bonus: Turn on live previews.  This shows you a live preview of a LaTeX
  ;; fragment and updates the preview in real-time as you edit it.
  ;; To preview only environments, set it to '(block edit-special) instead
  (setq org-latex-preview-live t)

  ;; More immediate live-previews -- the default delay is 1 second
  (setq org-latex-preview-live-debounce 0.25))

;; ----------------------------------------------------------------------------
;; Org Citation Settings
;; ----------------------------------------------------------------------------

(use-package citeproc
  :ensure t
  :defer t)

(with-eval-after-load 'org
  (require 'oc-csl)
  (require 'oc-biblatex)
  (require 'oc-natbib))

;; ----------------------------------------------------------------------------
;; Org LaTeX Compiler & Export Settings
;; ----------------------------------------------------------------------------

;(setq org-latex-compiler "xelatex"
;      org-latex-pdf-process '("xelatex %f")
;      org-latex-listings t
;      org-preview-latex-image-directory "~/.cache/emacs/lxtimg/")

(define-key org-mode-map (kbd "C-p") 'org-latex-export-to-pdf)

;;;----------------------------------------------------------------
;; ** ORG-APPEARANCE
;;;----------------------------------------------------------------

;; Org settings to do with its default appearance
(use-package org
  :ensure nil
  :defer
  :hook ((org-mode . org-toggle-pretty-entities)
         (org-mode . visual-line-mode))
  :config
  (setq-default
   org-fontify-done-headline t
   org-fontify-quote-and-verse-blocks t
   org-fontify-whole-heading-line t
   org-hidden-keywords nil
   org-hide-leading-stars t
   org-startup-folded nil
   org-startup-indented nil
   org-startup-with-inline-images nil
   org-startup-with-latex-preview t
   ;; org-highlight-latex-and-related '(latex entities)
   org-highlight-latex-and-related '(latex)
   org-indent-mode-turns-on-hiding-stars nil
   org-use-sub-superscripts '{}
   org-pretty-entities nil
   org-image-align 'center
   ;; org-priority-faces '((?a . error) (?b . warning) (?c . success))
   org-pretty-entities-include-sub-superscripts t)
  
  ;; Pretty symbols
  ;; (add-hook 'org-mode-hook 'org-toggle-pretty-entities)
  ;; Org LaTeX options
  ;; (add-hook 'org-mode-hook 'turn-on-org-cdlatex)
  ;; Enable longline-truncation in org-mode buffers
  ;; (add-hook 'org-mode-hook 'toggle-truncate-lines)

  ;; Keyword faces for reftex labels and references in Org
  ;; ,(rx
  ;; (group "\\" (or "label" "ref" "cref" "eqref"))
  ;; (minimal-match "{" (group (1+ any)) "}"))
  (font-lock-add-keywords
   'org-mode
   '(("\\(\\(?:\\\\\\(?:label\\|cref\\|ref\\|eqref\\)\\)\\){\\(.+?\\)}"
      (1 font-lock-keyword-face)
      (2 font-lock-constant-face))))

  (setq org-entities-user '(("nbsp" "~" nil "&nbsp" " " "\x00A0" "∼")))
  
  (defun my/org-raise-scripts-no-braces (_)
    (when (and (eq (char-after (match-beginning 3)) ?{)
	       (eq (char-before (match-end 3)) ?}))
      (remove-text-properties (match-beginning 3) (1+ (match-beginning 3))
		              (list 'invisible nil))
      (remove-text-properties (1- (match-end 3)) (match-end 3)
		              (list 'invisible nil))))

  (advice-add 'org-raise-scripts :after #'my/org-raise-scripts-no-braces)

  (setq org-todo-keyword-faces
        '(;; ("TODO"    :foreground "#6e90c8" :weight bold)
          ("WAITING" :foreground "red" :weight bold)
          ("MAYBE"   :foreground "#6e8996" :weight bold)
          ("PROJECT" :foreground "#088e8e" :weight bold)
          ("SUSPENDED" :foreground "#6e8996" :weight bold))))

;; Settings to do with org-latex-preview
(use-package org-latex-preview
  :ensure nil
  :after org
  :hook ((org-mode . my/org-latex-preview-precompile-idle))
  :bind (:map org-mode-map
         ("C-c C-x SPC" . org-latex-preview-clear-cache)
         ("C-c i" . my/org-latex-preview-image-at-point)
         ("M-g m" . my/org-latex-next-env)
         ("M-g M" . my/org-latex-prev-env))
  :config
  ;; (setq org-element-use-cache nil)
  (setq org-latex-preview-auto-ignored-commands
        '(next-line previous-line
          mwheel-scroll pixel-scroll-precision
          scroll-up-command scroll-down-command
          scroll-other-window scroll-other-window-down))
  (plist-put org-latex-preview-appearance-options
             :matchers '("begin" "\\(" "\\["))
  (setq-default
   ;; org-latex-preview-header
   ;;    "\\documentclass{article}
   ;; \\usepackage[usenames]{color}
   ;; [DEFAULT-PACKAGES]
   ;; [PACKAGES]
   ;; \\setlength{\\textwidth}{0.8\\paperwidth}
   ;; \\addtolength{\\textwidth}{-2cm}"

   org-format-latex-options
   (progn (plist-put org-format-latex-options :background "Transparent")
          (plist-put org-format-latex-options :scale 1.0)
          (plist-put org-format-latex-options :zoom
                     (- (/ (face-attribute 'default :height) 100.0) 0.025)))

   org-latex-preview-appearance-options
   (progn (plist-put org-latex-preview-appearance-options :scale 1.0)
          (plist-put org-latex-preview-appearance-options :zoom
                     (- (/ (face-attribute 'default :height) 100.0) 0.025)))

   ;; org-latex-preview-live-throttle 0.5
   org-latex-preview-auto-track-inserts t
   org-latex-preview-process-active-indicator 'fringe)

  ;; Get the image at point
  (defun my/org-latex-preview-image-at-point (&optional arg)
    (interactive "P")
    (if-let* ((imgpath (thread-first
                         (point) overlays-at last car
                         (overlay-get 'preview-image)
                         cdr (plist-get :file)))
              (_ (file-exists-p imgpath)))
        (prog1 (kill-new imgpath)
            (when arg (start-process "olpsink" nil "dragon" (expand-file-name imgpath)))
            (message "Image path copied to kill-ring."))
      (message "No LaTeX preview image at point!")))
  
  ;; Utility command to navigate math fragments
  (defun my/org-latex-next-env (&optional arg)
    (interactive "p")
    (save-match-data
      (re-search-forward org-latex-preview--tentative-math-re
                         nil t (or arg 1))))

  (defun my/org-latex-prev-env (&optional arg)
    (interactive "p")
    (my/org-latex-next-env (- (or arg 1))))
  
  (defvar-keymap my/org-latex-env-map
    :repeat t
    "m" 'my/org-latex-next-env
    "M" 'my/org-latex-prev-env
    "n" 'my/org-latex-next-env
    "p" 'my/org-latex-prev-env)
  (put 'my/org-latex-next-env
       'repeat-map 'my/org-latex-env-map)
  (put 'my/org-latex-prev-env
       'repeat-map 'my/org-latex-env-map)
  
  (defun my/org-latex-preview-show-error ()
    (display-local-help t))
  (add-hook 'org-ctrl-c-ctrl-c-final-hook
            #'my/org-latex-preview-show-error -10)

  ;; Precompilation freezes emacs, do it in the background when possible.
  (defun my/org-latex-preview-precompile-idle (&optional beg end _)
    (when (and (featurep 'async) (not (or beg end)))
      (run-with-idle-timer
       2 nil #'my/org-latex-preview-precompile-async
       (current-buffer))))

  (advice-add 'org-latex-preview-clear-cache :after
              #'my/org-latex-preview-precompile-idle)

  (defun my/org-latex-preview-precompile-async (&optional org-buf)
    (when (and (buffer-live-p org-buf)
               (window-live-p (get-buffer-window org-buf 'all-frames)))
      (with-current-buffer org-buf
        (when org-latex-preview-process-precompiled
          (let* ((org-location (org-find-library-dir "org"))
                 (compiler-keywords
                  (org-collect-keywords
                   '("LATEX_PREVIEW_COMPILER" "LATEX_COMPILER")
                   '("LATEX_PREVIEW_COMPILER" "LATEX_COMPILER")))
                 (compiler
                  (or (cdr (assoc "LATEX_PREVIEW_COMPILER" compiler-keywords))
                      (and (boundp 'org-latex-preview-compiler)
                           org-latex-preview-compiler)
                      (cdr (assoc "LATEX_COMPILER" compiler-keywords))
                      org-latex-compiler))
                 (header (concat
                          (or org-latex-preview--preamble-content
                              (org-latex-preview--get-preamble))
                          org-latex-preview--include-preview-string))
                 (relative-file-p
                  (string-match-p "\\(?:\\\\input{\\|\\\\include{\\)[^/]" header))
                 (remote-file-p (file-remote-p default-directory))
                 (info (list :latex-compiler compiler
                             :precompile-format-spec
                             (let ((org-tex-compiler
                                    (cdr (assoc compiler org-latex-preview-compiler-command-map))))
                               `((?l . ,org-tex-compiler)
                                 (?L . ,(car (split-string org-tex-compiler)))))))
                 (preamble-hash (thread-first
                                  header
                                  (concat
                                   compiler
                                   (if (not relative-file-p)
                                       "-temp" default-directory))
                                  (sha1))))
            (when (and (equal compiler "pdflatex")
                       (not remote-file-p)
                       (not (cadr (org-persist-read "LaTeX format file cache"
                                                    (list :key preamble-hash)
                                                    nil nil :read-related t))))
              (async-start
               `(lambda ()
                  (add-to-list 'load-path ,org-location)
                  (require 'ox)
                  (org-latex--precompile-preamble
                   ',info ,header
                   ,(expand-file-name preamble-hash temporary-file-directory)))
               `(lambda (dump-file)
                 (let ((inhibit-message t))
                   (org-persist--load-index)
                   (cadr
                    (org-persist-register `(,"LaTeX format file cache"
                                            (file ,dump-file))
                                          (list :key ,preamble-hash)
                                          :write-immediately t))
                   (message "Precompiled in background: %S"
                            (file-name-base dump-file)))))))))))

  ;; org-html-format-latex from Org 9.6 - this is needed because the new version does not work with ox-hugo
;; (defun org-html-format-latex (latex-frag processing-type info)
;;     "Format a LaTeX fragment LATEX-FRAG into HTML.
;; PROCESSING-TYPE designates the tool used for conversion.  It can
;; be `mathjax', `verbatim', `html', nil, t or symbols in
;; `org-preview-latex-process-alist', e.g., `dvipng', `dvisvgm' or
;; `imagemagick'.  See `org-html-with-latex' for more information.
;; INFO is a plist containing export properties."
;;     (let ((cache-relpath "") (cache-dir ""))
;;       (unless (or (eq processing-type 'mathjax)
;;                   (eq processing-type 'html))
;;         (let ((bfn (or (buffer-file-name)
;;                        (make-temp-name
;;                         (expand-file-name "latex" temporary-file-directory))))
;;               (latex-header
;;                (let ((header (plist-get info :latex-header)))
;;                  (and header
;;                       (concat (mapconcat
;;                                (lambda (line) (concat "#+LATEX_HEADER: " line))
;;                                (org-split-string header "\n")
;;                                "\n")
;;                               "\n")))))
;;           (setq cache-relpath
;;                 (concat (file-name-as-directory org-preview-latex-image-directory)
;;                         (file-name-sans-extension
;;                          (file-name-nondirectory bfn)))
;;                 cache-dir (file-name-directory bfn))
;;           ;; Re-create LaTeX environment from original buffer in
;;           ;; temporary buffer so that dvipng/imagemagick can properly
;;           ;; turn the fragment into an image.
;;           (setq latex-frag (concat latex-header latex-frag))))
;;       (with-temp-buffer
;;         (insert latex-frag)
;;         (org-format-latex cache-relpath nil nil cache-dir nil
;;                           "Creating LaTeX Image..." nil processing-type)
;;         (buffer-string))))
  )

;; code for centering LaTeX previews -- a terrible idea
(use-package org-latex-preview
  :ensure nil
  :after org-latex-preview
  :config
  (defun my/org-latex-preview-uncenter (ov)
    ;; (overlay-put ov 'justify (overlay-get ov 'before-string))
    (overlay-put ov 'before-string nil))
  (defun my/org-latex-preview-recenter (ov)
    (overlay-put ov 'before-string (overlay-get ov 'justify))
    ;; (overlay-put ov 'justify nil))
    )
  (defun my/org-latex-preview-center (ov)
    (save-excursion
      (goto-char (overlay-start ov))
      (when-let* ((elem (org-element-context))
                  ((or (eq (org-element-type elem) 'latex-environment)
                       (string-match-p "^\\\\\\[" (org-element-property :value elem))))
                  (img (overlay-get ov 'display))
                  (prop `(space :align-to (- center (0.55 . ,img))))
                  (justify (propertize " " 'display prop 'face 'default)))
        (overlay-put ov 'justify justify)
        (overlay-put ov 'before-string (overlay-get ov 'justify)))))
  (define-minor-mode org-latex-preview-center-mode
    "Center equations previewed with `org-latex-preview'."
    :global nil
    (if org-latex-preview-center-mode
        (progn
          (add-hook 'org-latex-preview-overlay-open-functions
                    #'my/org-latex-preview-uncenter nil :local)
          (add-hook 'org-latex-preview-overlay-close-functions
                    #'my/org-latex-preview-recenter nil :local)
          (add-hook 'org-latex-preview-overlay-update-functions
                    #'my/org-latex-preview-center nil :local))
      (remove-hook 'org-latex-preview-overlay-close-functions
                    #'my/org-latex-preview-recenter)
      (remove-hook 'org-latex-preview-overlay-update-functions
                    #'my/org-latex-preview-center)
      (remove-hook 'org-latex-preview-overlay-open-functions
                    #'my/org-latex-preview-uncenter))))



;; From alphapapa's unpackaged: https://github.com/alphapapa/unpackaged.el#org-return-dwim
(use-package org
  :ensure nil
  :defer t
  :bind (:map org-mode-map
         ("RET" . my/org-return-dwim))
  :config
  
  (defun my/org-element-descendant-of (type element)
    "Return non-nil if ELEMENT is a descendant of TYPE.
TYPE should be an element type, like `item' or `paragraph'.
ELEMENT should be a list like that returned by `org-element-context'."
    ;; MAYBE: Use `org-element-lineage'.
    (when-let* ((parent (org-element-property :parent element)))
      (or (eq type (car parent))
          (my/org-element-descendant-of type parent))))

  (defun my/org-return-dwim (&optional default)
    "A helpful replacement for `org-return'.  With prefix, call `org-return'.

On headings, move point to position after entry content.  In
lists, insert a new item or end the list, with checkbox if
appropriate.  In tables, insert a new row or end the table."
    ;; Inspired by John Kitchin:
    ;; http://kitchingroup.cheme.cmu.edu/blog/2017/04/09/A-better-return-in-org-mode/
    (interactive "P")
    (if default
        (org-return)
      (cond
       ;; Act depending on context around point.
       
       ((and (eq 'link (car (org-element-context)))
             org-return-follows-link)
        ;; Link: Open it.
        (org-open-at-point-global))

       ;; ((or (eq
       ;;       (get-char-property (min (1+ (point)) (point-max)) 'org-overlay-type)
       ;;       'org-latex-overlay)
       ;;      (let ((context (org-element-context)))
       ;;        (and (memq (org-element-type context)
       ;;                   '(latex-fragment latex-environment))
       ;;             (eq (point)
       ;;                 (save-excursion
       ;;                   (goto-char (org-element-property :end context))
       ;;                   (skip-chars-backward "\n\r\t ")
       ;;                   (point))))))
       ;;  (org-latex-preview))

       ((org-at-heading-p)
        ;; Heading: Move to position after entry content.
        ;; NOTE: This is probably the most interesting feature of this function.
        (let ((heading-start (org-entry-beginning-position)))
          (goto-char (org-entry-end-position))
          (cond ((and (org-at-heading-p)
                      (= heading-start (org-entry-beginning-position)))
                 ;; Entry ends on its heading; add newline after
                 (end-of-line)
                 (insert "\n\n"))
                (t
                 ;; Entry ends after its heading; back up
                 (forward-line -1)
                 (end-of-line)
                 (when (org-at-heading-p)
                   ;; At the same heading
                   (forward-line)
                   (insert "\n")
                   (forward-line -1))
                 ;; FIXME: looking-back is supposed to be called with more arguments.
                 (while (not (looking-back (rx (repeat 3 (seq (optional blank) "\n")))))
                   (insert "\n"))
                 (forward-line -1)))))

       ((org-in-item-p)
        ;; Plain list.  Yes, this gets a little complicated...
        (let ((context (org-element-context)))
          (if (or (eq 'plain-list (car context))  ; First item in list
                  (and (eq 'item (car context))
                       (not (eq (org-element-property :contents-begin context)
                                (org-element-property :contents-end context))))
                  (my/org-element-descendant-of 'item context))  ; Element in list item, e.g. a link
              ;; Non-empty item: Add new item.
              (if (org-at-item-checkbox-p)
                  (org-insert-todo-heading nil)
                (org-insert-item))
            ;; Empty item: Close the list.
            ;; TODO: Do this with org functions rather than operating on the
            ;; text. Can't seem to find the right function.
            (delete-region (line-beginning-position) (line-end-position))
            (insert "\n"))))

       ((when (fboundp 'org-inlinetask-in-task-p)
          (org-inlinetask-in-task-p))
        ;; Inline task: Don't insert a new heading.
        (org-return))

       ((org-at-table-p)
        (cond ((save-excursion
                 (beginning-of-line)
                 ;; See `org-table-next-field'.
                 (cl-loop with end = (line-end-position)
                          for cell = (org-element-table-cell-parser)
                          always (equal (org-element-property :contents-begin cell)
                                        (org-element-property :contents-end cell))
                          while (re-search-forward "|" end t)))
               ;; Empty row: end the table.
               (delete-region (line-beginning-position) (line-end-position))
               (org-return))
              (t
               ;; Non-empty row: call `org-return'.
               (org-return))))
       (t
        ;; All other cases: call `org-return'.
        (org-return))))))

(defun my/set-TeX-master ()
  (setq TeX-master
        '("
\\documentclass{article}
\\usepackage{amsmath, amssymb, tikz}
\\begin{document}
  % Preview will insert LaTeX fragments here
\\end{document}")))

(use-package preview-auto
  :after latex
  :defer t
  :hook (LaTeX-mode . preview-auto-setup)
  :config
  (setq preview-protect-point t)
  (setq preview-locating-previews-message nil)
  (setq preview-leave-open-previews-visible t)
  :custom
  (preview-auto-interval 0.1)

  ;; Uncomment the following only if you have followed the above
  ;; instructions concerning, e.g., hyperref:

  (preview-LaTeX-command-replacements
   '(preview-LaTeX-disable-pdfoutput))
  )

;;;----------------------------------------------------------------
;; ** ORG-TREE-SLIDE
;;;----------------------------------------------------------------
;; Presentations from within org-mode.
(use-package org-tree-slide
  :ensure t
  :after org
  :commands my/org-presentation-mode
  ;; :hook (org-tree-slide-after-narrow . my/org-tree-slide-enlarge-latex-preview)
  :config
  (setq org-tree-slide-never-touch-face nil
        org-tree-slide-skip-outline-level 8
        org-tree-slide-heading-emphasis nil
        org-tree-slide-cursor-init nil
        org-tree-slide-slide-in-effect nil
        org-tree-slide-activate-message
        (propertize "ORG PRESENTATION STARTED" 'face 'success)
        org-tree-slide-deactivate-message
        (propertize "ORG PRESENTATION STOPPED" 'face 'error))
  
  ;; (defun my/org-tree-slide-enlarge-latex-preview ()
  ;;   (dolist (ov (overlays-in (point-min) (point-max)))
  ;;     (if (eq (overlay-get ov 'org-overlay-type)
  ;;             'org-latex-overlay)
  ;;         (overlay-put
  ;;          ov 'display
  ;;          (cons 'image 
  ;;                (plist-put
  ;;                 (cdr (overlay-get ov 'display))
  ;;                 :scale (+ 1.0 (* 0.2 text-scale-mode-amount))))))))

  (defvar olivetti-style)
  (define-minor-mode my/org-presentation-mode
    "Parameters for plain text presentations with `org-mode'."
    :init-value nil
    :global nil
    (if my/org-presentation-mode
        (progn
          (unless (eq major-mode 'org-mode)
            (user-error "Not in an Org buffer"))
          (visual-fill-column-mode -1)
          (org-tree-slide-mode 1)
          (setq olivetti-style nil)
          (setq line-spacing 0.12)
          ;; (setq olivetti-margin-width 14)
          ;; (setq olivetti-body-width 0.7)
          (text-scale-increase 3)
          (my/olivetti-mode 1))
      (org-tree-slide-mode -1)
      (my/olivetti-mode -1)
      (visual-fill-column-mode 1)
      (text-scale-decrease 3)
      (org-fold-show-all)
      (text-scale-mode -1)))

  :bind (("C-c P"      . my/org-presentation-mode)
         :map org-tree-slide-mode-map
         ("<next>" . org-tree-slide-move-next-tree)
         ("<prior>" . org-tree-slide-move-previous-tree)
         ("<home>" . 'org-tree-slide-display-header-toggle)
         ("<C-down>"  . org-tree-slide-display-header-toggle)
         ("<C-right>" . org-tree-slide-move-next-tree)
         ("<C-left>"  . org-tree-slide-move-previous-tree)))

(provide 'setup-org)
;;; setup-org.el ends here 
