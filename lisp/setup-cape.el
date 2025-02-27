;;; setup-cape.el --- Completion At Point Extensions (CAPE) -*- lexical-binding: t; -*-

;;; Commentary:
;; This file configures CAPE (Completion At Point Extensions) to enhance Emacs completion.

;;; Code:

(use-package cape
  :ensure t
  :defer
  :init
  (dolist (hook '((prog-mode-hook . (cape-keyword cape-dabbrev cape-file))
                  (emacs-lisp-mode-hook . (cape-elisp-symbol cape-elisp-block cape-file))
                  (org-mode-hook . (cape-dabbrev cape-keyword cape-abbrev cape-file))
                  (LaTeX-mode-hook . (cape-tex cape-dabbrev cape-keyword cape-file))
                  (sgml-mode-hook . (cape-sgml cape-dabbrev cape-file))
                  (text-mode-hook . (cape-dabbrev cape-abbrev cape-file))))
    (dolist (fn (cdr hook))
      (add-hook (car hook) (lambda ()
                             (add-hook 'completion-at-point-functions fn 'append))))))


(setq ispell-program-name "hunspell")
(setq ispell-dictionary "en_US")
(setq ispell-alternate-dictionary "/usr/share/hunspell/en_US.dic")

(provide 'setup-cape)
;;; setup-cape.el ends here
