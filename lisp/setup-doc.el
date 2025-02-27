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
  :defer 
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

;;;----------------------------------------------------------------
;; ** NOV.EL
;;;----------------------------------------------------------------
(use-package nov
  :ensure t
  :defer
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  :hook ((nov-mode . my/nov-display-setup)
         (nov-mode . er/add-text-mode-expansions))
  :bind (:map nov-mode-map
         ("u" . my/scroll-down-half)
         ("d" . my/scroll-up-half))
  :config
  (use-package setup-reading
    :disabled
    :hook (nov-post-html-render . my/reader-center-images))
  
  (setq nov-text-width 72
        nov-save-place-file (dir-concat user-cache-directory "nov-places"))
  ;; Pinched from https://tecosaur.github.io/emacs-config/config.html
  (defun my/nov-display-setup ()
    (face-remap-add-relative 'variable-pitch
                             :family "Merriweather"
                             :height 1.0
                             :width 'semi-expanded)
    ;; (face-remap-add-relative 'default :height 1.1)
    (setq-local line-spacing 0.2
                next-screen-context-lines 4
                shr-use-colors t)
    (require 'visual-fill-column nil t)
    (setq-local visual-fill-column-center-text t
                visual-fill-column-width (1+ nov-text-width))
    (visual-fill-column-mode 1)))

(provide 'setup-doc)
;;; setup-doc.el ends here
