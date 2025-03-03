;;; setup-windows.el --- Editor Settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Editor Settings for Windows and their placements 

;;; Code:

(defun karna/set-display-buffer-rule (buffers side size)
  "Dynamically add rules to `display-buffer-alist`.
BUFFERS: A list of buffer names (strings) or major modes (symbols).
SIDE: The window side to display the buffer ('bottom, 'top, 'left, or 'right).
SIZE: The height (if 'top or 'bottom) or width (if 'left or 'right) as a decimal fraction (e.g., 0.3 for 30%)."
  (let ((buffer-patterns (mapcar (lambda (b) (if (symbolp b) b (regexp-quote b))) buffers)))
    (add-to-list 'display-buffer-alist
                 `(,(if (seq-some #'symbolp buffers)
                        (lambda (buffer-name _action)
                          (with-current-buffer buffer-name
                            (apply #'derived-mode-p buffer-patterns)))
                      (regexp-opt buffers))
                   (display-buffer-in-side-window)
                   (side . ,side)
                   (,(if (memq side '(top bottom)) 'window-height 'window-width) . ,size)))))

;; Example Usage: Just define variables with your preferences
(karna/set-display-buffer-rule
 '("*Warnings*" "*Compile-Log*" "*Async Shell Command*" "*Org PDF LaTeX Output*" "*Preview-Ghostscript-Error*" "*Org LaTeX Precompilation*" "*Backtrace*" "*Completions*" "*Shell Command Output*" "*evil-registers*" "*gptel-ask*" "*TeX Help*" "^Calc:" "[Oo]utput\\*$")
 'bottom 0.3)

(karna/set-display-buffer-rule
 '(Custom-mode messages-mode compilation-mode messages-buffer-mode TeX-output-mode)
 'bottom 0.3)

(karna/set-display-buffer-rule
 '("*eshell*" "*vterm*")
 'right 0.5)

(karna/set-display-buffer-rule
 '(help-mode occur-mode inferior-emacs-lisp-mode)
 'left 0.3)

(karna/set-display-buffer-rule
 '(emacs-news-mode pdf-view-mode inferior-python-mode inferior-octave-mode)
 'right 0.5)


(provide 'setup-windows)
;;; setup-windows.el ends here
