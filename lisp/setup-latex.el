;;; setup-latex.el --- LaTeX & AUCTeX Setup -*- lexical-binding: t; -*-

;; ----------------------------------------------------------------------------
;; AUCTeX Setup
;; ----------------------------------------------------------------------------

(use-package tex
  :ensure auctex 
  :defer t
  :mode ("\\.tex\\'" . LaTeX-mode)
  :hook ((LaTeX-mode . LaTeX-math-mode)
         (LaTeX-mode . visual-line-mode)
         (LaTeX-mode . electric-pair-mode)
         (LaTeX-mode . prettify-symbols-mode)
         (LaTeX-mode . rainbow-delimiters-mode)
         (LaTeX-mode . my/latex-with-outline)
         (LaTeX-mode . TeX-fold-mode))
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-electric-escape nil)
  (TeX-PDF-mode t)
  (TeX-save-query nil)
  (TeX-show-compilation nil)
  (reftex-plug-into-AUCTeX t)
  (LaTeX-indent-level 2)
  (LaTeX-item-indent 2)
  (TeX-electric-math '("$" . "$"))
  ;; (preview-scale-function 1.2)
  (preview-auto-cache-preamble t)
  (preview-default-option-list '("displaymath" "graphics" "textmath"
                                 "footnotes" "sections" "showlabels"
                                 "psfixbb" "floats" "tikzpicture"))
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
  (defun my/latex-with-outline ()
      (add-to-list 'minor-mode-overriding-map-alist
                   `(outline-minor-mode . ,outline-minor-mode-map))
      (outline-minor-mode 1))
  (defun TeX-insert-smallmatrix () (interactive)
         (insert "[\\begin{smallmatrix}  \\end{smallmatrix}]")
         (backward-char 19))
  (defun TeX-insert-bmatrix () (interactive)
         (insert "\\begin{bmatrix}  \\end{bmatrix}")
         (backward-char 14))
  (with-eval-after-load 'latex
      (define-key LaTeX-mode-map (kbd "M-RET") 'LaTeX-insert-item))

  (with-eval-after-load 'latex
    (define-key TeX-mode-map [C-down-mouse-1] 'TeX-view-mouse))

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

(use-package latex-extra
  :disabled
  :after latex
  :defines (latex-extra-mode)
  :hook (LaTeX-mode . latex-extra-mode))

;; ----------------------------------------------------------------------------
;; Zathura PDF Viewer Setup with SyncTeX
;; ----------------------------------------------------------------------------

(with-eval-after-load 'latex
  (add-to-list 'TeX-view-program-list
               `("Zathura"
                 (,(concat (expand-file-name "~/.local/bin/zathura")
                           " --synctex-forward %n:0:%b -x \"emacsclient +%{line} %{input}\" %o"))
                 "zathura"))
  (setq-default TeX-source-correlate-mode t)
  (setq-default TeX-source-correlate-start-server t)
  (setq TeX-newline-function 'reindent-then-newline-and-indent)
  (setq TeX-view-program-selection '((output-pdf "Zathura"))
        TeX-source-correlate-method 'synctex))

(defun karna/toggle-tex-pdf-viewer ()
  "Toggle between PDF Tools and Zathura for TeX viewing in AUCTeX."
  (interactive)
  (let* ((current-viewer (cadr (assoc 'output-pdf TeX-view-program-selection)))
         (new-viewer (if (equal current-viewer "PDF Tools")
                         "Zathura"
                       "PDF Tools")))
    (setq TeX-view-program-selection
          `((output-pdf ,new-viewer))) ;; Proper format
    (message "TeX viewer set to: %s" new-viewer)))

(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

(use-package preview
  :ensure nil
  :defer
  :after latex
  :hook (LaTeX-mode . my/preview-scale-larger)
  :config
  (define-key LaTeX-mode-map (kbd "C-c C-x") preview-map)
  (defun my/preview-scale-larger ()
    "Increase the size of `preview-latex' images"
    (setq preview-scale-function 
          (lambda nil (* 1.1 (funcall (preview-scale-from-face)))))))

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
  (reftex-plug-into-AUCTeX t)
  (reftex-ref-style-default-list '("Default" "AMSmath" "Cleveref"))
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
  :defer
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
;; CDLaTeX - Faster LaTeX Input
;; ----------------------------------------------------------------------------

(use-package cdlatex
  :after latex
  :ensure t
  ;; :commands turn-on-cdlatex
  :hook ((LaTeX-mode . cdlatex-mode)
         (LaTeX-mode . cdlatex-electricindex-mode))
  :bind (:map cdlatex-mode-map
         ("[" . nil) ("(" . nil) ("{" . nil)
         ("<tab>" . cdlatex-tab))
  :defines (cdlatex-math-symbol-prefix cdlatex-command-alist)
  :config
  (setq cdlatex-math-symbol-prefix ?\;)
  (define-key cdlatex-mode-map
              (cdlatex-get-kbd-vector cdlatex-math-symbol-prefix)
              #'cdlatex-math-symbol)
  (dolist (cmd '(("vc" "Insert \\vect{}" "\\vect{?}"
                  cdlatex-position-cursor nil nil t)
                 ("tfr" "Insert \\tfrac{}{}" "\\tfrac{?}{}"
                  cdlatex-position-cursor nil nil t)
                 ("sfr" "Insert \\sfrac{}{}" "\\sfrac{?}{}"
                  cdlatex-position-cursor nil nil t)
                 ("abs" "Insert \\abs{}" "\\abs{?}"
                  cdlatex-position-cursor nil nil t)
                 ("equ*" "Insert equation* env"
                  "\\begin{equation*}\n?\n\\end{equation*}"
                  cdlatex-position-cursor nil t nil)
                 ("sn*" "Insert section* env"
                  "\\section*{?}"
                  cdlatex-position-cursor nil t nil)
                 ("ss*" "Insert subsection* env"
                  "\\subsection*{?}"
                  cdlatex-position-cursor nil t nil)
                 ("sss*" "Insert subsubsection* env"
                  "\\subsubsection*{?}"
                  cdlatex-position-cursor nil t nil)))
    (push cmd cdlatex-command-alist))

  (setq cdlatex-env-alist
        '(("align" "\\begin{align}
?
\\end{align}" "\\\\AUTOLABEL
?")
          ("equation" "\\begin{equation}
?
\\end{equation}" nil)))
  
  (setq cdlatex-math-symbol-alist '((?F ("\\Phi"))
                                    (?o ("\\omega" "\\mho" "\\mathcal{O}"))
                                    (?. ("\\cdot" "\\circ"))
                                    (?6 ("\\partial"))
                                    (?v ("\\vee" "\\forall"))
                                    (?^ ("\\uparrow" "\\Updownarrow" "\\updownarrow"))))
  (setq cdlatex-math-modify-alist '((?k "\\mathfrak" "\\textfrak" t nil nil)
                                    (?b "\\mathbf" "\\textbf" t nil nil)
                                    (?B "\\mathbb" "\\textbf" t nil nil)
                                    (?t "\\text" nil t nil nil)))
  (setq cdlatex-paired-parens "$[{(")
  (cdlatex-reset-mode))

;; Make cdlatex play nice inside org tables
(use-package lazytab
  :load-path "plugins/lazytab/";; 
  :bind (:map orgtbl-mode-map
              ("<tab>" . lazytab-org-table-next-field-maybe)
              ("TAB" . lazytab-org-table-next-field-maybe))
  :after cdlatex
  :demand t
  :config
  (add-hook 'cdlatex-tab-hook #'lazytab-cdlatex-or-orgtbl-next-field 90)
  (dolist (cmd '(("smat" "Insert smallmatrix env"
                  "\\left( \\begin{smallmatrix} ? \\end{smallmatrix} \\right)"
                  lazytab-position-cursor-and-edit
                  nil nil t)
                 ("bmat" "Insert bmatrix env"
                  "\\begin{bmatrix} ? \\end{bmatrix}"
                  lazytab-position-cursor-and-edit
                  nil nil t)
                 ("pmat" "Insert pmatrix env"
                  "\\begin{pmatrix} ? \\end{pmatrix}"
                  lazytab-position-cursor-and-edit
                  nil nil t)
                 ("tbl" "Insert table"
                  "\\begin{table}\n\\centering ? \\caption{}\n\\end{table}\n"
                  lazytab-position-cursor-and-edit
                  nil t nil)))
    (push cmd cdlatex-command-alist))
  (cdlatex-reset-mode))


(provide 'setup-latex)
;;; setup-latex.el ends here
