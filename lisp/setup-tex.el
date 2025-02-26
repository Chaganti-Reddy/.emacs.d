;;; setup-tex.el --- LaTeX & AUCTeX Setup -*- lexical-binding: t; -*-

;; ----------------------------------------------------------------------------
;; AUCTeX Setup
;; ----------------------------------------------------------------------------

(use-package tex
  :ensure auctex
  :defer t
  :mode ("\\.tex\\'" . LaTeX-mode)
  :hook ((LaTeX-mode . LaTeX-math-mode)
         (LaTeX-mode . visual-line-mode)
         (LaTeX-mode . prettify-symbols-mode)
         (LaTeX-mode . rainbow-delimiters-mode)
         (LaTeX-mode . TeX-fold-mode))
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-PDF-mode t)
  (TeX-save-query nil)
  (TeX-show-compilation nil)
  (reftex-plug-into-AUCTeX t)
  (LaTeX-indent-level 2)
  (LaTeX-item-indent 2)
  (TeX-electric-math '("$" . "$"))
  (preview-scale-function 1.2)
  (preview-auto-cache-preamble t)
  (preview-default-option-list '("displaymath" "graphics" "textmath"
                                 "footnotes" "sections" "showlabels"
                                 "psfixbb" "floats"))
  (TeX-master
   (lambda ()
     (let ((default-master (expand-file-name "main.tex"
                                             (or (file-name-directory (buffer-file-name))
                                                 default-directory))))
       (if (file-exists-p default-master)
           "main.tex"
         (progn
           (message "main.tex not found, please select a master file")
           (read-file-name "Choose master file: " nil nil t))))))
  :config
  (setq-default TeX-master nil))

;; ----------------------------------------------------------------------------
;; Function to Clear LaTeX Build Files
;; ----------------------------------------------------------------------------

(defun karna/clear-latex-build ()
  "Remove all LaTeX compilation files except .tex and .pdf."
  (interactive)
  (when (yes-or-no-p "Delete all LaTeX build files except .tex and .pdf?")
    (call-process "/bin/sh" nil nil nil "-c"
                  "rm -rf auto *.prv *.fmt *.aux *.bbl *.blg *.log *.out *.toc *.lof *.lot *.synctex.gz *.fls *.fdb_latexmk _region_.tex")
    (message "LaTeX build files deleted.")))

;; ----------------------------------------------------------------------------
;; RefTeX - Reference & Citation Management
;; ----------------------------------------------------------------------------

(use-package reftex
  :ensure nil  ;; Built-in
  :defer t
  :hook (LaTeX-mode . turn-on-reftex)
  :custom
  (reftex-enable-partial-scans t)
  (reftex-save-parse-info t)
  (reftex-use-multiple-selection-buffers t)
  (reftex-toc-split-windows-horizontally t)
  (reftex-toc-split-windows-fraction 0.2))

;; ----------------------------------------------------------------------------
;; Citar - Bibliographic Management for Org & LaTeX
;; ----------------------------------------------------------------------------

(use-package citar
  :ensure t
  :defer t
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography))

(use-package citar-embark
  :ensure t
  :after (citar embark)
  :hook (citar-mode . citar-embark-mode)
  :custom
  (citar-at-point-function 'embark-act))

;; ----------------------------------------------------------------------------
;; AUCTeX-Latexmk - Automate LaTeX Compilation
;; ----------------------------------------------------------------------------

(use-package auctex-latexmk
  :ensure t
  :defer t
  :hook (LaTeX-mode . auctex-latexmk-setup)
  :custom
  (auctex-latexmk-inherit-TeX-PDF-mode t))

;; ----------------------------------------------------------------------------
;; CDLaTeX - Faster LaTeX Input
;; ----------------------------------------------------------------------------

(use-package cdlatex
  :ensure t
  :defer t
  :hook (LaTeX-mode . turn-on-cdlatex))

;; ----------------------------------------------------------------------------
;; Zathura PDF Viewer Setup with SyncTeX
;; ----------------------------------------------------------------------------

(with-eval-after-load 'tex
  (add-to-list 'TeX-view-program-list
               `("Zathura"
                 (,(concat (expand-file-name "~/.local/bin/zathura")
                           " --synctex-forward %n:0:%b -x \"emacsclient +%{line} %{input}\" %o"))
                 "zathura"))
  (setq TeX-view-program-selection '((output-pdf "Zathura"))
        TeX-source-correlate-mode t
        TeX-source-correlate-method 'synctex
        TeX-source-correlate-start-server t))

(add-hook 'TeX-after-initialization-hook
          (lambda () (setq TeX-source-correlate-start-server t)))

(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

;; ----------------------------------------------------------------------------
;; Org LaTeX Compiler & Export Settings
;; ----------------------------------------------------------------------------

(setq org-latex-compiler "xelatex"
      org-latex-pdf-process '("xelatex %f")
      org-latex-listings t
      org-preview-latex-image-directory "~/.cache/emacs/lxtimg/")

(define-key org-mode-map (kbd "M-p") 'org-latex-export-to-pdf)

;; ----------------------------------------------------------------------------
;; Custom LaTeX Classes for Org Export
;; ----------------------------------------------------------------------------

(with-eval-after-load 'ox-latex
  (dolist (class
           '(("IEEEtran" "\\documentclass[conference]{IEEEtran}"
              ("\\section{%s}" . "\\section*{%s}")
              ("\\subsection{%s}" . "\\subsection*{%s}")
              ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
              ("\\paragraph{%s}" . "\\paragraph*{%s}")
              ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
             ("article" "\\documentclass[11pt]{article}"
              ("\\section{%s}" . "\\section*{%s}")
              ("\\subsection{%s}" . "\\subsection*{%s}")
              ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
              ("\\paragraph{%s}" . "\\paragraph*{%s}")
              ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
             ("report" "\\documentclass[11pt]{report}"
              ("\\part{%s}" . "\\part*{%s}")
              ("\\chapter{%s}" . "\\chapter*{%s}")
              ("\\section{%s}" . "\\section*{%s}")
              ("\\subsection{%s}" . "\\subsection*{%s}")
              ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
             ("book" "\\documentclass[11pt]{book}"
              ("\\part{%s}" . "\\part*{%s}")
              ("\\chapter{%s}" . "\\chapter*{%s}")
              ("\\section{%s}" . "\\section*{%s}")
              ("\\subsection{%s}" . "\\subsection*{%s}")
              ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))
    (add-to-list 'org-latex-classes class)))

;; ----------------------------------------------------------------------------
;; Org LaTeX Preview Settings
;; ----------------------------------------------------------------------------

(setq org-format-latex-options
      (plist-put org-format-latex-options :scale 2.0)
      org-latex-create-formula-image-program 'dvisvgm
      org-preview-latex-default-process 'dvisvgm
      org-startup-with-latex-preview t)

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
;; Org-Fragtog - Auto Preview LaTeX Fragments
;; ----------------------------------------------------------------------------

(use-package org-fragtog
  :ensure t
  :hook (org-mode . org-fragtog-mode))

(provide 'setup-tex)
;;; setup-tex.el ends here
