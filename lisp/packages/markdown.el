;;; packages/markdown.el --- Markdown Setup -*- lexical-binding: t; -*-

;;; ============================================================
;;; MARKDOWN SETUP
;;; ============================================================

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode))

(eval-after-load "org"
  '(require 'ox-md nil t))

;; -----------------------------------------------------------
;; OX-GFM FOR GITHUB-FLAVORED MARKDOWN EXPORT
;; -----------------------------------------------------------
(use-package ox-gfm
  :ensure t
  :after org
  :defer t)

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist
	     '("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode))

(autoload 'gfm-mode "markdown-mode"
   "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;; ----------------------------------------------------------------
;; OX-HUGO
;; ----------------------------------------------------------------

(use-package ox-hugo
  :ensure ( :host github
	      :repo "kaushalmodi/ox-hugo"
	      :branch "main")
  :after ox)

;; Function to insert the current date
(defun insert-current-date ()
  "Insert the current date in the format YYYY-MM-DD at the point."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

;; Bind the function to C-c d in Org mode
(add-hook 'org-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c d") 'insert-current-date)))

;; Function to insert the current time in 24-hour format
(defun insert-current-time ()
  "Insert the current time in the format HH:MM at the point."
  (interactive)
  (insert (format-time-string "%H:%M")))

;; Bind the function to C-c t in Org mode
(add-hook 'org-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c t") 'insert-current-time)))

;; ------------------------------------------------------------
;; MARKDOWN PREVIEW MODE
;; ------------------------------------------------------------

(use-package markdown-preview-mode
  :ensure t
  :commands (markdown-preview-mode
	     markdown-preview-open-browser
	     markdown-preview-cleanup)
  :init
  ;; Set your Markdown processor (by default it uses "markdown")
  ;; If you want to use multimarkdown, make sure it's installed and in your PATH.
  ;(setq markdown-command "multimarkdown")
  :config
  ;; Optional: add extra JavaScript (e.g., MathJax)
  (add-to-list 'markdown-preview-javascript
	       "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-MML-AM_CHTML"))

;; Additional CSS
;; (setq markdown-preview-stylesheets
;;       (list "http://thomasf.github.io/solarized-css/solarized-light.min.css"))

;; ------------------------------------------------------------
;; MARKDOWN LATEX PREVIEW
;; ------------------------------------------------------------

(use-package preview-tailor
  :ensure t
  :after preview
  :config
  (preview-tailor-init)
  :hook
  (kill-emacs . preview-tailor-save))

;; Preview Latex in markdown buffer as well using a temporary TeX file with preview auto mode.

(defun my-markdown-preview-hook ()
  "Setup LaTeX preview for Markdown mode with a fresh temporary TeX file."
  (setq-local preview-tailor-local-multiplier 0.7)

  ;; Always create a new temporary LaTeX file
  (setq-local my-preview-master (make-temp-file "preview-master" nil ".tex"))
  (with-temp-file my-preview-master
    (insert "\\documentclass{article}\n"
	    "\\usepackage{amsmath, amssymb}\n"
	    "\\begin{document}\n"
	    "% Your LaTeX preview content will be inserted here\n"
	    "\\end{document}\n"))

  (setq-local TeX-master my-preview-master)
  (preview-auto-mode))

(add-hook 'markdown-mode-hook 'my-markdown-preview-hook)


(provide 'packages/markdown)
;; packages/markdown.el ends here
