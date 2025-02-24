;;; packages/org-latex.el --- Org LaTeX Setup -*- lexical-binding: t; -*-

;; ------------------------------------------------------------
;; LATEX COMPILER & BASIC PREVIEW SETTINGS
;; ------------------------------------------------------------

(setq org-latex-compiler "xelatex")
(setq org-latex-pdf-process '("xelatex %f"))
(setq org-latex-listings t)
(setq org-preview-latex-image-directory "~/.cache/emacs/lxtimg/")
(setq org-latex-preview-lxtpng-directory "~/.cache/emacs/lxtimg/")

(define-key org-mode-map (kbd "M-p") 'org-latex-export-to-pdf)

;; ------------------------------------------------------------
;; CUSTOM LATEX CLASSES FOR ORG EXPORT
;; ------------------------------------------------------------

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
	      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
	     ("org-plain-latex" "\\documentclass{article}
	       [NO-DEFAULT-PACKAGES]
	       [PACKAGES]
	       [EXTRA]"
	      ("\\section{%s}" . "\\section*{%s}")
	      ("\\subsection{%s}" . "\\subsection*{%s}")
	      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	      ("\\paragraph{%s}" . "\\paragraph*{%s}")
	      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
    (add-to-list 'org-latex-classes class)))

;; ------------------------------------------------------------
;; ORG LATEX PREVIEW
;; ------------------------------------------------------------

;; Increase LaTeX preview size
(setq org-format-latex-options
      (plist-put org-format-latex-options :scale 2.0)) ;; Adjust as needed

;; Use dvisvgm for SVG-based previews (default)
(setq org-latex-create-formula-image-program 'dvisvgm) ;; Use dvisvgm for better compatibility
(setq org-preview-latex-default-process 'dvisvgm) ;; Default to dvisvgm

(setq org-startup-with-latex-preview t)


;; Prevent navigation commands from triggering LaTeX previews
(setq org-latex-preview-auto-ignored-commands
      '(next-line previous-line mwheel-scroll
	scroll-up-command scroll-down-command))

;; Enable consistent equation numbering
(setq org-latex-preview-numbered t)

;; Enable live previews for real-time LaTeX updates
(setq org-latex-preview-live t)

;; Reduce delay for faster live previews
(setq org-latex-preview-live-debounce 0.25)

;; ;; Stolen from the package ov (Center Previews)

;; (defun ov-at (&optional point)
;;   "Get an overlay at POINT.
;; POINT defaults to the current `point'."
;;   (or point (setq point (point)))
;;   (car (overlays-at point)))
;; ;; https://www.reddit.com/r/emacs/comments/169keg7/comment/jzierha/?utm_source=share&utm_medium=web2x&context=3
;; (defun org-justify-fragment-overlay (beg end image &optional imagetype)
;;   "Only equations at the beginning and also end of a line are justified."
;;   (if
;;    (and (= beg (line-beginning-position)) (= end (line-end-position)))
;;    (let* ((ov (ov-at))
;;   (disp (overlay-get ov 'display)))
;;      (overlay-put ov 'line-prefix `(space :align-to (- center (0.5 . ,disp)))))))
;; (advice-add 'org--make-preview-overlay :after 'org-justify-fragment-overlay)

;; ;; Automatically refresh LaTeX previews on save or edits

;; (add-hook 'org-mode-hook
;;	  (lambda ()
;;	    (add-hook 'after-save-hook 'org-latex-preview nil 'local)
;;	    (add-hook 'after-change-functions
;;		      (lambda (&rest _) (org-latex-preview)) nil 'local)))

;; ------------------------------------------------------------
;; CITATION
;; ------------------------------------------------------------

(use-package citeproc
  :ensure t
  :defer t)


(with-eval-after-load 'org
  (require 'oc-csl)
(require 'oc-biblatex)
(require 'oc-natbib))
;; (setq org-cite-global-bibliography '("~/Path/To/bibliographyFile"))

;;; ORG FRAGTOP

(use-package org-fragtog
  :ensure t
  :defer t)
(add-hook 'org-mode-hook 'org-fragtog-mode)


(provide 'packages/org-latex)
;; packages/org-latex.el ends here
