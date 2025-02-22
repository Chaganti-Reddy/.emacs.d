;;; org-config.el --- Org Config -*- lexical-binding: t; -*-

;; ------------------------------------------------
;; TOC & INDENTATION
;; ------------------------------------------------

(use-package toc-org
    :ensure t
    :defer t
    :commands toc-org-enable
    :init (add-hook 'org-mode-hook 'toc-org-enable))

(setq org-src-preserve-indentation t
      org-src-fontify-nativelt t
      org-src-window-setup 'current-window
      org-edit-src-content-indentation 0
      org-src-tab-acts-natively t)

;; Prevent '<>' from auto-pairing in Org mode (fix for org-tempo)
(add-hook 'org-mode-hook
	  (lambda ()
	    (setq-local electric-pair-inhibit-predicate
			`(lambda (c)
			   (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))

(eval-after-load 'org-indent '(diminish 'org-indent-mode)) ;; Removes "Ind" from showing in the modeline.

;; ------------------------------------------------
;;; ORG UI SETTINGS
;; ------------------------------------------------

(use-package org-modern
  :ensure t
  :hook ((org-mode . org-modern-mode)
	 (org-agenda-finalize-hook . org-modern-agenda))
  :custom ((org-modern-todo t)
	   (org-modern-table t)
	   (org-modern-variable-pitch t) ;; Enables proportional fonts for text
	   (org-modern-block-fringe t))
  :commands (org-modern-mode org-modern-agenda)
  :config (setq org-modern-star '("◉" "○" "◆" "◇" "▶")
     org-modern-list '((?- . "•") (?+ . "➤") (?* . "▹")))
  :init (global-org-modern-mode))

;; --------------------------------------------------
;; ORG TRANSCLUSION
;; --------------------------------------------------

(use-package org-transclusion
  :after org
  :bind ("C-c M-t" . org-transclusion-add))

;; --------------------------------------------------
;; ORG CUSTOM HEADER FACES
;; --------------------------------------------------

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

;; --------------------------------------------------
;; ORG SUPERSTAR (ALTERNATIVE TO ORG BULLETS)
;; --------------------------------------------------

(use-package org-superstar
  :ensure t
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-headline-bullets-list '("◉" "●" "○" "◆" "●" "○" "◆")
	org-superstar-itembullet-alist '((?+ . ?➤) (?- . ?✦))))

(setq org-checkbox-hierarchical-statistics nil
      org-checkbox-image "\\([%])\\")

;; Prettify Org mode with `org-appear`
(use-package org-appear
  :ensure t
  :hook (org-mode . org-appear-mode))

;; ------------------------------------------------
;; ORG REMARKS
;; ------------------------------------------------

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

;; ------------------------------------------------
;; HIGHLIGHT TODO
;; ------------------------------------------------

(use-package hl-todo
  :ensure t
  :hook ((org-mode . hl-todo-mode)
	 (prog-mode . hl-todo-mode))
  :config
  (setq hl-todo-highlight-punctuation ":"
	hl-todo-keyword-faces
	`(("TODO" warning bold)
	  ("FIXME" error bold)
	  ("HACK" font-lock-constant-face bold)
	  ("REVIEW" font-lock-keyword-face bold)
	  ("NOTE" success bold)
	  ("DEPRECATED" font-lock-doc-face bold))))

;; --------------------------------------------------
;; CENTER ORG MODE
;; --------------------------------------------------

(defun karna/org-mode-visual-fill ()
  (setq visual-fill-column-width 180
	visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . karna/org-mode-visual-fill))

;; ------------------------------------------------
;; ORG AUTO TANGLE
;; ------------------------------------------------

(use-package org-auto-tangle
  :defer t
  :diminish
  :hook (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-default t))

(defun karna/insert-auto-tangle-tag ()
  "Insert auto-tangle tag in a literate config."
  (interactive)
  (org-end-of-line)
  (newline)
  (insert "#+auto_tangle: t")
  (evil-force-normal-state))

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
      org-log-done 'time
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

;; ------------------------------------------------
;; ORG REFILE SETTINGS
;; ------------------------------------------------

(setq org-bookmark-names-plist nil) ;; Stop bookmarking on org captures and refiling

(setq org-refile-targets
      '(("Archive.org" :maxlevel . 1)
	("Tasks.org" :maxlevel . 1)))

(setq org-hide-drawers '("PROPERTIES"))

;; Save Org buffers after refiling!
(advice-add 'org-refile :after 'org-save-all-org-buffers)


(provide 'org-config)
;; org-config.el ends here
