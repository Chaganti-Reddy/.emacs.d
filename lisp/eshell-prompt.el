;;; eshell-prompt.el --- a fancy shell prompt for eshell

;;; Code:

;; fancy-shell
;; A fancy shell prompt for eshell.

(defun fancy-shell ()
  "A pretty shell with git status"
  (let* ((cwd (abbreviate-file-name (eshell/pwd)))
         (ref (magit-get-shortname "HEAD"))
         (stat (magit-file-status))
         (x-stat eshell-last-command-status)
         (git-chunk
          (if ref
              (format "%s%s%s "
                      (propertize (if stat "[" "(") 'font-lock-face (list :foreground (if stat "#e81050" "#9bee8b")))
                      (propertize ref 'font-lock-face '(:foreground "#c897ff"))
                      (propertize (if stat "]" ")") 'font-lock-face (list :foreground (if stat "#e81050" "#9bee8b"))))
            "")))
    (propertize
     (format "\n%s %s %s$ "
             (if (< 0 x-stat) (format (propertize "!%s" 'font-lock-face '(:foreground "#e81050")) x-stat)
               (propertize "➤" 'font-lock-face (list :foreground (if (< 0 x-stat) "#e81050" "#9bee8b"))))
             (propertize cwd 'font-lock-face '(:foreground "#45babf"))
             git-chunk)
     'read-only t
     'front-sticky   '(font-lock-face read-only)
     'rear-nonsticky '(font-lock-face read-only))))


(provide 'eshell-prompt)
;;; eshell-prompt.el ends here
