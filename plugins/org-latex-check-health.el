;;; org-latex-check-health.el --- Custom function -*- lexical-binding: t; -*-

;;; Commentary:

;; Custom function to check the org latex heath(written by karthink).

;;; Code:

(defun org-latex-preview-check-health (&optional inter)
  "Inspect the relevent system state and setup.
INTER signals whether the function has been called interactively."
  (interactive (list t))
  ;; Collect information
  (let* ((diag `(:interactive ,inter)))
    (plist-put diag :org-version org-version)
    ;; modified variables
    (plist-put diag :modified
               (let ((list))
                 (mapatoms
                  (lambda (v)
                    (and (boundp v)
                         (string-match "\\`\\(org-latex-\\|org-persist-\\)" (symbol-name v))
                         (or (and (symbol-value v)
                                  (string-match "\\(-hook\\|-function\\)\\'" (symbol-name v)))
                             (and
                              (get v 'custom-type) (get v 'standard-value)
                              (not (equal (symbol-value v)
                                          (eval (car (get v 'standard-value)) t)))))
                         (push (cons v (symbol-value v)) list))))
                 list))
    ;; Executables
    ;; latex processors
    (dolist (processor org-latex-compilers)
      (when-let ((path (executable-find processor)))
        (let ((version (with-temp-buffer
                         (thread-last
                           (concat processor " --version")
                           (shell-command-to-string)
                           (insert))
                         (goto-char (point-min))
                         (buffer-substring (point) (line-end-position)))))
          (push (list processor version path) (plist-get diag :latex-processors)))))
    ;; Image converters
    (dolist (converter '("dvipng" "dvisvgm" "convert"))
      (when-let ((path (executable-find converter)))
        (let ((version (with-temp-buffer
                         (thread-last
                           (concat converter " --version")
                           (shell-command-to-string)
                           (insert))
                         (goto-char (point-min))
                         (buffer-substring (point) (line-end-position)))))
          (push (list converter version path) (plist-get diag :image-converters)))))
    (when inter
      (with-current-buffer (get-buffer-create "*Org LaTeX Preview Report*")
        (let ((inhibit-read-only t))
          (erase-buffer)

          (insert (propertize "Your LaTeX preview process" 'face 'outline-1))
          (insert "\n\n")

          (let* ((latex-available (cl-member org-latex-compiler
                                             (plist-get diag :latex-processors)
                                             :key #'car :test #'string=))
                 (precompile-available
                  (and latex-available
                       (not (member org-latex-compiler '("lualatex" "xelatex")))))
                 (proc-info (alist-get
                             org-latex-preview-process-default
                             org-latex-preview-process-alist))
                 (image-converter (cadr (plist-get proc-info :programs)))
                 (image-converter
                  (cl-find-if
                   (lambda (c)
                     (string= image-converter c))
                   (plist-get diag :image-converters)
                   :key #'car))
                 (image-output-type (plist-get proc-info :image-output-type)))
            (if org-latex-preview-process-precompiled
                (insert "Precompile with "
                        (propertize (map-elt org-latex-precompile-compiler-map
                                             org-latex-compiler)
                                    'face
                                    (list
                                     (if precompile-available
                                         '(:inherit success :box t)
                                       '(:inherit error :box t))
                                     'org-block))
                        " → "))
            (insert "LaTeX Compile with "
                    (propertize org-latex-compiler 'face
                                (list
                                 (if latex-available
                                     '(:inherit success :box t)
                                   '(:inherit error :box t))
                                 'org-block))
                    " → ")
            (insert "Convert to "
                    (propertize (upcase image-output-type) 'face '(:weight bold))
                    " with "
                    (propertize (car image-converter) 'face
                                (list
                                 (if image-converter
                                     '(:inherit success :box t)
                                   '(:inherit error :box t))
                                 'org-block))
                    "\n\n")
            (insert (propertize org-latex-compiler 'face 'outline-3)
                    "\n"
                    (if latex-available
                        (concat
                          (propertize
                           (mapconcat #'identity (map-nested-elt diag `(:latex-processors ,org-latex-compiler))
                                      "\n")
                           'face 'org-block)
                          "\n"
                          (when (and latex-available (not precompile-available))
                            (propertize
                             (format "\nWarning: Precompilation not available with %S!\n" org-latex-compiler)
                             'face 'warning)))
                      (propertize "Not found in path!\n" 'face 'error))
                    "\n")
            
            (insert (propertize (cadr (plist-get proc-info :programs)) 'face 'outline-3)
                    "\n"
                    (if image-converter
                        (propertize
                         (concat
                          (mapconcat #'identity (cdr image-converter) "\n")
                          "\n")
                         'face 'org-block)
                      (propertize "Not found in path!\n" 'face 'error))
                    "\n")
            ;; dvisvgm version check
            (when (equal (car-safe image-converter)
                         "dvisvgm")
              (let* ((version-string (cadr image-converter))
                     (dvisvgm-ver (progn
                                    (string-match "\\([0-9.]+\\)" version-string)
                                    (match-string 1 version-string))))

                (when (version< dvisvgm-ver "3.0")
                  (insert (propertize
                           (format "Warning: dvisvgm version %s < 3.0, displaymath will not be centered."
                                   dvisvgm-ver)
                           'face 'warning)
                          "\n\n"))
                (unless (string-match-p " RSVG" system-configuration-features)
                  (insert (propertize
                           "Error: Emacs was not compiled with SVG support,
images cannot be displayed with dvisvgm"
                           'face 'error)))))
            ;; png support check
            (when (member (car-safe image-converter)
                          '("dvipng" "convert"))
              (unless (string-match-p " PNG" system-configuration-features)
                (insert (propertize
                         (format "Error: Emacs was not compiled with PNG support,
images cannot be displayed with %s"
                                 (car-safe image-converter))))))
            (when (not (and latex-available image-converter))
              (insert "path: " (getenv "PATH") "\n\n")))
          ;; Settings
          (insert (propertize "LaTeX preview options" 'face 'outline-2)
                  "\n")

          (pcase-dolist (`(,var . ,msg)
                         `((,org-latex-preview-process-precompiled . "Precompilation           ")
                           (,org-latex-preview-numbered . "Equation renumbering     ")
                           (,org-latex-preview-cache  . "Caching with org-persist ")))
            (insert (propertize "• " 'face 'org-list-dt)
                    msg
                    (if var
                        (propertize "ON" 'face '(success bold org-block))
                      (propertize "OFF" 'face '(error bold org-block)))
                    "\n"))
          (insert "\n"
                  (propertize "LaTeX preview sizing" 'face 'outline-2) "\n"
                  (propertize "•" 'face 'org-list-dt)
                  " Page width  "
                  (propertize
                   (format "%S" (plist-get org-latex-preview-appearance-options :page-width))
                   'face '(org-code org-block))
                  "   (display equation width in LaTeX)\n"
                  (propertize "•" 'face 'org-list-dt)
                  " Scale       "
                  (propertize
                   (format "%.2f" (plist-get org-latex-preview-appearance-options :scale))
                   'face '(org-code org-block))
                  "  (PNG pixel density multiplier)\n"
                  (propertize "•" 'face 'org-list-dt)
                  " Zoom        "
                  (propertize
                   (format "%.2f" (plist-get org-latex-preview-appearance-options :zoom))
                   'face '(org-code org-block))
                  "  (display scaling factor)\n\n")
          (insert (propertize "LaTeX preview preamble" 'face 'outline-2) "\n")
          (let ((major-mode 'org-mode))
            (let ((point-1 (point)))
              (insert org-latex-preview-preamble "\n")
              (org-src-font-lock-fontify-block 'latex point-1 (point))
              (add-face-text-property point-1 (point) '(:inherit org-block :height 0.9)))
            (insert "\n")
            ;; Diagnostic output
            (insert (propertize "Diagnostic info (copied)" 'face 'outline-2)
                    "\n\n")
            (let ((point-1 (point)))
              (pp diag (current-buffer))
              (org-src-font-lock-fontify-block 'emacs-lisp point-1 (point))
              (add-face-text-property point-1 (point) '(:height 0.9))))
          (gui-select-text (prin1-to-string diag))
          (special-mode))
        (setq-local
         revert-buffer-function
         (lambda (&rest _)
           (call-interactively #'org-latex-preview-check-health)
           (message "Refreshed LaTeX preview diagnostic")))
        (let ((message-log-max nil))
          (toggle-truncate-lines 1))
        (goto-char (point-min))
        (display-buffer (current-buffer))))
    diag))


(provide 'org-latex-check-health)

;;; org-latex-check-health.el ends here
