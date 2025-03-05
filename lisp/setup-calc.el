;;; setup-calc.el --- Emacs Calc Setup -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs Calc configuration 

;;; Code:

(use-package calc
  :ensure nil
  :defer t
  :bind (("C-x c" . calc)
         ("C-S-e" . latex-math-from-calc)
         ("C-c e" . calc-embedded)
         )
  :config
  (use-package calc-embed
    :ensure nil
    :defer t
    :bind (:map calc-override-minor-modes-map
           ("'" . calc-algebraic-entry)))
  (use-package calc-yank
    :ensure nil
    :defer
    :config
    (define-advice calc-finish-stack-edit (:around (orig-fn &rest args) pop-to-buffer)
      (cl-letf (((symbol-function 'switch-to-buffer)
                 #'pop-to-buffer))
        (apply orig-fn args))))
  (setq calc-make-windows-dedicated t)
  (defun latex-math-from-calc ()
    "Evaluate `calc' on the contents of line at point."
    (interactive)
    (let ((lang (if (memq major-mode '(org-mode latex-mode LaTex-mode))
                  'latex 'normal)))
      (cond ((region-active-p)
             (let* ((beg (region-beginning))
                    (end (region-end))
                    (string (buffer-substring-no-properties beg end)))
               (kill-region beg end)
               (insert (calc-eval `(,string calc-language ,lang
                                            calc-prefer-frac t
                                            calc-angle-mode rad)))))
            (t (let ((l (thing-at-point 'line)))
                 (end-of-line 1) (kill-line 0)
                 (insert (calc-eval `(,l
                                      calc-language ,lang
                                      calc-prefer-frac t
                                      calc-angle-mode rad)))))))))

(use-package calc
  :ensure nil
  :defer t
  :after org-latex-preview
  :hook (calc-mode . my/olp-calc-settings)
  :config
  (defun my/olp-calc-settings ()
    (setq-local org-latex-preview-numbered nil
                org-latex-preview-auto-ignored-commands
                '(mwheel-scroll pixel-scroll-precision
                  scroll-up-command scroll-down-command
                  scroll-other-window scroll-other-window-down))))

(use-package literate-calc-mode
  :disabled
  :ensure t
  :defer t
  :hook (org-mode . literate-calc-minor-mode))

;; Window pop to show entries when using calc embedded
(defvar calc-embedded-trail-window nil)
(defvar calc-embedded-calculator-window nil)

(defun karna/calc-embedded-with-side-panel (&rest _)
  "Manage Calc embedded mode windows dynamically."
  (when calc-embedded-trail-window
    (ignore-errors
      (delete-window calc-embedded-trail-window))
    (setq calc-embedded-trail-window nil))
  (when calc-embedded-calculator-window
    (ignore-errors
      (delete-window calc-embedded-calculator-window))
    (setq calc-embedded-calculator-window nil))
  (when (and calc-embedded-info
             (> (* (window-width) (window-height)) 1200))
    (let ((main-window (selected-window))
          (vertical-p (> (window-width) 80)))
      (select-window
       (setq calc-embedded-trail-window
             (if vertical-p
                 (split-window-horizontally (- (max 30 (/ (window-width) 3))))
               (split-window-vertically (- (max 8 (/ (window-height) 4)))))))
      (switch-to-buffer "*Calc Trail*")
      (select-window
       (setq calc-embedded-calculator-window
             (if vertical-p
                 (split-window-vertically -6)
               (split-window-horizontally (- (/ (window-width) 2))))))
      (switch-to-buffer "*Calculator*")
      (select-window main-window))))

(advice-add 'calc-do-embedded :after #'karna/calc-embedded-with-side-panel)


(provide 'setup-calc)
;;; setup-calc.el ends here
