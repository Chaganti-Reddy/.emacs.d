;;; popups.el --- popups configuration -*- lexical-binding: t; -*-

;;; Commentary: 
;; Popper is an emacs package built by karthink which makes emacs buffers into popus for more space and easy usage and also vice-versa.

;; https://github.com/karthink/popper
;; https://youtu.be/E-xUNlZi3rI?si=56ebEfcqFmbi9oyi

;;; Code:

(use-package popper
  :ensure t ; or :straight t
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("^\\*Messages\\*"
          "[Oo]utput\\*$"
          "\\*Async Shell Command\\*"
          "^\\*Backtrace\\*"
          "\\*Completions\\*"
          "^\\*Compile-Log\\*"
          "^\\*Warnings\\*"
          "\\*Shell Command Output\\*"
          "^\\*evil-registers\\*"
          "^Calc:"
          "^\\*ielm\\*"
          "^\\*TeX Help\\*"
          help-mode
          Custom-mode 
          messages-mode 
          occur-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))                ; For echo area hints

(setq popper-group-function #'popper-group-by-project) ; project.el projects

;; (setq popper-group-function #'popper-group-by-projectile) ; projectile projects

;; (setq popper-group-function #'popper-group-by-directory) ; group by project.el
                                                         ; project root, with
                                                         ; fall back to
                                                         ; default-directory
;; (setq popper-group-function #'popper-group-by-perspective) ; group by perspective

;; Match eshell, shell, term and/or vterm buffers
(setq popper-reference-buffers
      (append popper-reference-buffers
              '("^\\*eshell.*\\*$" eshell-mode ;eshell as a popup
                "^\\*shell.*\\*$"  shell-mode  ;shell as a popup
                "^\\*term.*\\*$"   term-mode   ;term as a popup
                "^\\*vterm.*\\*$"  vterm-mode  ;vterm as a popup
                )))
    
(setq popper-mode-line '(:eval (propertize " POP" 'face 'mode-line-emphasis)))


(provide 'popups)
;;; popups.el ends here
