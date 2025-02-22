;;; packages/latex.el --- LaTeX Package -*- lexical-binding: t; -*-

;; ----------------------------------------------------------------------------
;; PDF TOOLS
;; ----------------------------------------------------------------------------

(use-package pdf-tools
  :ensure t
  :defer t
  :commands (pdf-loader-install)
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :custom
  (pdf-view-display-size 'fit-width)
  (pdf-annot-activate-created-annotations t)
  :bind (:map pdf-view-mode-map
	      ("j" . pdf-view-next-line-or-next-page)
	      ("k" . pdf-view-previous-line-or-previous-page)
	      ("C-=" . pdf-view-enlarge)
	      ("C--" . pdf-view-shrink)
	      ("C-s" . isearch-forward)
	      ("C-r" . isearch-backward))
  :init
  (pdf-loader-install)
  :hook (pdf-view-mode . (lambda ()
			   (display-line-numbers-mode -1)
			   (blink-cursor-mode -1)
			   (doom-modeline-mode -1)))
  :config
  (add-to-list 'revert-without-query ".pdf"))

;; ----------------------------------------------------------------------------
;; EPUB
;; ----------------------------------------------------------------------------

(use-package nov
  :ensure t
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (setq nov-variable-pitch nil) ;; Use fixed-width font
  (setq nov-text-width t)) ;; Adjust text width dynamically

;; ----------------------------------------------------------------------------
;; LATEX PREVIEW SETTINGS
;; ----------------------------------------------------------------------------

(use-package tex
  :ensure auctex
  :defer t
  :mode ("\\.tex\\'" . LaTeX-mode)
  :hook
  ((LaTeX-mode . LaTeX-math-mode)
   (LaTeX-mode . TeX-fold-mode))
  :config
  ;; Basic AUCTeX settings.
  (setq TeX-auto-save t
	TeX-parse-self t
	TeX-PDF-mode t
	TeX-save-query nil)
  ;; Integration with RefTeX.
  (setq reftex-plug-into-AUCTeX t)
  ;; Master file setup: default to main.tex if it exists; otherwise prompt.
  (setq-default TeX-master
		(lambda ()
		  (let ((default-master (concat (file-name-directory (or (buffer-file-name) default-directory))
						"main.tex")))
		    (if (file-exists-p default-master)
			"main.tex"
		      (progn
			(message "main.tex not found, please select a master file")
			(read-file-name "Choose master file: " nil nil t))))))
  ;; LaTeX indentation and electric settings.
  (setq LaTeX-indent-level 2
	LaTeX-item-indent 2
	TeX-electric-math '("$" . "$"))
  ;; LaTeX preview settings.
  (setq preview-auto-cache-preamble t
  ;; preview-default-option-list '("floats" "graphics")
  preview-default-option-list '("displaymath" "graphics" "textmath" "footnotes" "sections" "showlabels" "psfixbb" "floats")
	TeX-show-compilation nil))

(add-hook 'LaTeX-mode-hook #'rainbow-delimiters-mode)

(defun clear-latex-build ()
  "Remove all LaTeX compilation files except .tex and .pdf."
  (interactive)
  (when (y-or-n-p "Delete all LaTeX build files except .tex and .pdf? (y/n) ")
    (call-process "/bin/sh" nil nil nil "-c"
		  "rm -rf auto *.prv *.fmt *.aux *.bbl *.blg *.log *.out *.toc *.lof *.lot *.synctex.gz *.fls *.fdb_latexmk _region_.tex")
    (message "LaTeX build files deleted.")))

;;; LATEX FRAGMENT SCALE

(setq preview-scale-function 1)

;; ----------------------------------------------------------------------------
;; REFTEX
;; ----------------------------------------------------------------------------

(use-package reftex
  :ensure nil  ;; RefTeX is built-in
  :defer t
  :preface
  ;; Explicit autoloads if desired (usually not needed)
  (autoload 'reftex-mode "reftex" "RefTeX Minor Mode" t)
  (autoload 'turn-on-reftex "reftex" "RefTeX Minor Mode" nil)
  (autoload 'reftex-citation "reftex-cite" "Make citation" nil)
  (autoload 'reftex-index-phrase-mode "reftex-index" "Phrase mode" t)
  :hook (LaTeX-mode . turn-on-reftex)
  :config
  (setq reftex-plug-into-AUCTeX t
	reftex-enable-partial-scans t
	reftex-save-parse-info t
	reftex-use-multiple-selection-buffers t
	reftex-toc-split-windows-horizontally t
	reftex-toc-split-windows-fraction 0.2))

;; ----------------------------------------------------------------------------
;; ZATHURA PREVIEW
;; ----------------------------------------------------------------------------

(with-eval-after-load 'tex
  (add-to-list 'TeX-view-program-list
	       `("Zathura"
		 (,(concat (expand-file-name "~/.local/bin/zathura") " "
			   (when (boundp 'mode-io-correlate)
			     " --synctex-forward %n:0:%b -x \"emacsclient +%{line} %{input}\" ")
			   " %o"))
		 "zathura"))
  (setq TeX-view-program-selection '((output-pdf "Zathura"))
	TeX-source-correlate-start-server t
	TeX-source-correlate-mode t
	TeX-source-correlate-method 'synctex))

;; ----------------------------------------------------------------------------
;; CITAR
;; ----------------------------------------------------------------------------

(use-package citar
  :ensure t
  :defer t
  :init
  (setq org-cite-insert-processor 'citar
	org-cite-follow-processor 'citar
	org-cite-activate-processor 'citar
	citar-bibliography org-cite-global-bibliography)
	;; citar-notes-paths '("~/Path/To/NotesDir")
	)

(use-package citar-embark
  :after (citar embark)
  :ensure t
  :defer t
  :init
  (setq citar-at-point-function 'embark-act)
  :config
  (citar-embark-mode 1))

;; ----------------------------------------------------------------------------
;; AUCTEX-LATEXMK
;; ----------------------------------------------------------------------------

(use-package auctex-latexmk
  :ensure t
  :defer t
  :config
  (auctex-latexmk-setup)
  (setq auctex-latexmk-inherit-TeX-PDF-mode t))

;; ----------------------------------------------------------------------------
;; CDLATEX
;; ----------------------------------------------------------------------------

(use-package cdlatex
  :ensure t
  :defer t
  :hook (LaTeX-mode . turn-on-cdlatex))

;;; PREVIEW AUTO
(use-package preview-auto
  :after latex
  :hook (LaTeX-mode . preview-auto-mode)
  :config
  (setq preview-protect-point t)
  (setq preview-locating-previews-message nil)
  (setq preview-leave-open-previews-visible t)
  :custom
  (preview-auto-interval 0.1)

  ;; Uncomment the following only if you have followed the above
  ;; instructions concerning, e.g., hyperref:

  (preview-LaTeX-command-replacements '(preview-LaTeX-disable-pdfoutput)))


(provide 'packages/latex)
;; packages/latex.el ends here
