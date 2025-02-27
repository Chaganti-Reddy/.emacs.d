;;; setup-md.el --- Markdown and Org-to-Markdown Export Setup -*- lexical-binding: t; -*-

;;; Commentary:
;; This file configures Markdown support in Emacs, including preview features,
;; Org mode Markdown export, Hugo integration, and LaTeX preview in Markdown.

;;; Code:

;; ============================================================
;; MARKDOWN MODE CONFIGURATION
;; ============================================================

(use-package markdown-mode
  :ensure t
  :defer
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode)))

(use-package markdown-preview-mode
  :ensure t
  :defer
  :commands (markdown-preview-mode
             markdown-preview-open-browser
             markdown-preview-cleanup)
  :config
  ;; Add MathJax support for equations in Markdown previews
  (add-to-list 'markdown-preview-javascript
               "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-MML-AM_CHTML"))

;; Optional: Custom stylesheet for preview
;; (setq markdown-preview-stylesheets
;;       (list "http://thomasf.github.io/solarized-css/solarized-light.min.css"))

;; ============================================================
;; ORG MODE EXPORT TO MARKDOWN (OX-MD & OX-GFM)
;; ============================================================

(with-eval-after-load 'org
  (require 'ox-md nil t)) ;; Standard Markdown export

(use-package ox-gfm
  :ensure t
  :defer
  :after org
  :defer t) ;; GitHub Flavored Markdown (GFM) export

;; ============================================================
;; OX-HUGO FOR ORG → HUGO BLOG EXPORT
;; ============================================================

(use-package ox-hugo
  :ensure t
  :defer
  :after ox)

;; ============================================================
;; UTILITY FUNCTIONS FOR DATE & TIME INSERTION
;; ============================================================

(defun karna/insert-current-date ()
  "Insert the current date in YYYY-MM-DD format."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun karna/insert-current-time ()
  "Insert the current time in HH:MM format."
  (interactive)
  (insert (format-time-string "%H:%M")))

;; Bind them to Org mode
(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c d") #'karna/insert-current-date)
            (local-set-key (kbd "C-c t") #'karna/insert-current-time)))

;; ============================================================
;; MARKDOWN LATEX PREVIEW
;; ============================================================

(use-package preview-auto
  :disabled
  :defer
  :after latex
  :custom
  (preview-protect-point t)                    ;; Prevents cursor from jumping when updating previews
  (preview-locating-previews-message nil)      ;; Disables preview messages for cleaner output
  (preview-leave-open-previews-visible t)      ;; Keeps previews visible when buffer is switched
  (preview-auto-interval 0.1)                  ;; Faster updates for inline previews
  (preview-LaTeX-command-replacements '(preview-LaTeX-disable-pdfoutput))) ;; Avoids PDF output issues

(use-package preview-tailor
  :disabled
  :ensure t
  :after preview
  :hook (kill-emacs . preview-tailor-save)
  :config
  (preview-tailor-init))

;; ============================================================
;; FUNCTION TO ENABLE LATEX PREVIEW IN MARKDOWN
;; ============================================================

;(defun karna/setup-markdown-latex-preview ()
;  "Setup LaTeX preview in Markdown mode using a temporary TeX file."
;  (setq-local preview-tailor-local-multiplier 0.7)
;
;  ;; Generate a new temporary LaTeX file for previews
;  (let ((tex-file (make-temp-file "preview-master" nil ".tex")))
;    (with-temp-file tex-file
;      (insert "\\documentclass{article}\n"
;              "\\usepackage{amsmath, amssymb}\n"
;              "\\begin{document}\n"
;              "% Your LaTeX preview content will be inserted here\n"
;              "\\end{document}\n"))
;    (setq-local TeX-master tex-file))
;
;  (preview-auto-mode 1)) ;; Ensure `preview-auto-mode` is activated
;
;(add-hook 'markdown-mode-hook #'karna/setup-markdown-latex-preview)

(provide 'setup-md)

;;; setup-md.el ends here
