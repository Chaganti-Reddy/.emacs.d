;;; packages/cape.el --- Cape Package -*- lexical-binding: t; -*-

(use-package cape
  :ensure t
  :init
  ;; (dolist (fn '(cape-file
	;;	cape-keyword
	;;	cape-dabbrev
  ;;	cape-abbrev
	;;	cape-dict
	;;	;; cape-emoji
	;;	cape-sgml))
  ;;  (add-hook 'completion-at-point-functions fn 'append))

  ;; General completion functions for all programming modes
  (add-hook 'prog-mode-hook
	    (lambda ()
	      (add-hook 'completion-at-point-functions #'cape-keyword 'append)
	      (add-hook 'completion-at-point-functions #'cape-dabbrev 'append)
	      (add-hook 'completion-at-point-functions #'cape-file 'append)))

  ;; Elisp-specific completions
  (add-hook 'emacs-lisp-mode-hook
	    (lambda ()
	      (add-hook 'completion-at-point-functions #'cape-elisp-symbol 'append)
	      (add-hook 'completion-at-point-functions #'cape-elisp-block 'append)
	      (add-hook 'completion-at-point-functions #'cape-file 'append)))

  ;; Org mode completions
  (add-hook 'org-mode-hook
	    (lambda ()
	      (add-hook 'completion-at-point-functions #'cape-dabbrev 'append)
	      (add-hook 'completion-at-point-functions #'cape-keyword 'append)
	      (add-hook 'completion-at-point-functions #'cape-abbrev 'append)
	      (add-hook 'completion-at-point-functions #'cape-file 'append)))

  ;; LaTeX-specific completions
  (add-hook 'latex-mode-hook
	    (lambda ()
	      (add-hook 'completion-at-point-functions #'cape-tex 'append)
	      (add-hook 'completion-at-point-functions #'cape-dabbrev 'append)
	      (add-hook 'completion-at-point-functions #'cape-keyword 'append)
	      (add-hook 'completion-at-point-functions #'cape-file 'append)))

   (add-hook 'LaTeX-mode-hook
	    (lambda ()
	      (add-hook 'completion-at-point-functions #'cape-tex 'append)
	      (add-hook 'completion-at-point-functions #'cape-dabbrev 'append)
	      (add-hook 'completion-at-point-functions #'cape-keyword 'append)
	      (add-hook 'completion-at-point-functions #'cape-file 'append)))

  ;; SGML/HTML/XML modes
  (add-hook 'sgml-mode-hook
	    (lambda ()
	      (add-hook 'completion-at-point-functions #'cape-sgml 'append)
	      (add-hook 'completion-at-point-functions #'cape-dabbrev 'append)
	      (add-hook 'completion-at-point-functions #'cape-file 'append)))

  ;; Text mode completions
  (add-hook 'text-mode-hook
	    (lambda ()
	      (add-hook 'completion-at-point-functions #'cape-dabbrev 'append)
	      (add-hook 'completion-at-point-functions #'cape-abbrev 'append)
	      (add-hook 'completion-at-point-functions #'cape-file 'append))))

;; ----------------------------------------------------------------
;; CAPF AUTOSUGGEST
;; ----------------------------------------------------------------

(use-package capf-autosuggest
  :ensure t
  :defer t
  :hook ((eshell-mode . capf-autosuggest-mode))
  :custom
  (capf-autosuggest-dwim-next-line nil))


(provide 'packages/cape)
;; packages/cape.el ends here
