;;; setup-doc.el --- PDF and EPUB support in Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; This file configures support for viewing PDFs with pdf-tools
;; and EPUBs with nov-mode in Emacs.

;;; Code:

;; --------------------------------------------------------------
;; PDF TOOLS
;; --------------------------------------------------------------

(use-package pdf-tools
  :ensure t
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
			   (setq mode-line-format nil)))  ;; Hide modeline
  :config
  (add-to-list 'revert-without-query "\\.pdf\\'"))  ;; Proper regex for auto-revert

;; --------------------------------------------------------------
;; EPUB READER (NOV MODE)
;; --------------------------------------------------------------

(use-package nov
  :ensure t
  :mode ("\\.epub\\'" . nov-mode)
  :custom
  (nov-variable-pitch nil)  ;; Use a fixed-width font
  (nov-text-width t))       ;; Adjust text width dynamically

(provide 'setup-doc)
;;; setup-doc.el ends here
