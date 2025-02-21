;;; packages/orderless.el --- Orderless Package -*- lexical-binding: t; -*-

(use-package orderless
  :ensure t
  :defer t
  :custom
  ;; (orderless-style-dispatchers '(orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(basic substring initials flex orderless))
  (completion-category-defaults nil)

  (setq completion-category-overrides
	;; NOTE 2021-10-25: I am adding `basic' because it works better as a
	;; default for some contexts.  Read:
	;; <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=50387>.
	;;
	;; `partial-completion' is a killer app for files, because it
	;; can expand ~/.l/s/fo to ~/.local/share/fonts.
	;;
	;; If `basic' cannot match my current input, Emacs tries the
	;; next completion style in the given order.  In other words,
	;; `orderless' kicks in as soon as I input a space or one of its
	;; style dispatcher characters.
	'((file (styles . (basic partial-completion orderless)))
	  (bookmark (styles . (basic substring)))
	  (library (styles . (basic substring)))
	  (embark-keybinding (styles . (basic substring)))
	  (imenu (styles . (basic substring orderless)))
	  (consult-location (styles . (basic substring orderless)))
	  (kill-ring (styles . (orderless)))
	  (eglot (styles . (orderless flex))))))

(setq completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq-default case-fold-search t)   ; For general regexp
(setq read-file-name-completion-ignore-case t)


(provide 'packages/orderless)
;; packages/orderless.el ends here
