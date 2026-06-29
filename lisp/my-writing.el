;;; my-writing.el --- Org (research/LaTeX/calc) + Markdown -*- lexical-binding: t; -*-

;;; ---------------------------------------------------------------------------
;;; Org Latest
;;; ---------------------------------------------------------------------------
;; :ensure can't upgrade a built-in, so install Org 9.8 explicitly once.
(when (and (featurep 'package)
           (not (package-installed-p 'org '(9 8))))
  (ignore-errors (package-install 'org)))

(use-package org
  :ensure nil
  :defer t
  :init
  (setq org-directory (if IS-WINDOWS "D:/Org" "~/Org")
        org-id-locations-file (my/var "org-id-locations")
        org-persist-directory (my/var "org-persist/" t)
        org-preview-latex-image-directory (my/var "org-latex-preview/" t)
        org-modules nil)
  :hook ((org-mode . visual-line-mode)
         (org-mode . org-cdlatex-mode))     ; fast math input (e.g. `ab' -> a_b)
  :custom
  (org-startup-indented t)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)                   ; \alpha renders as the glyph
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-src-content-indentation 0)
  (org-confirm-babel-evaluate nil)
  (org-return-follows-link t)
  (org-startup-with-latex-preview nil)
  :config
  (setq org-format-latex-options
        (plist-put
         (plist-put
          (plist-put org-format-latex-options :scale 1.2)
          :foreground "#d6d6d4")
         :background "Transparent"))
  (when (executable-find "dvisvgm")
    (setq org-preview-latex-default-process 'dvisvgm))
  ;; Babel langs (calc evaluates `#+begin_src calc'; embedded calc is C-x * e).
  (setq org-babel-load-languages
        '((emacs-lisp . t) (python . t) (C . t) (shell . t) (calc . t)))
  (run-with-idle-timer
   0.5 nil
   (lambda ()
     (org-babel-do-load-languages
      'org-babel-load-languages org-babel-load-languages))))

(use-package org-modern
  :after org
  :hook ((org-mode . org-modern-mode)
         (org-modern-mode . (lambda () (setq-local line-spacing 0.1))))
  :custom
  (org-modern-star 'replace)
  (org-modern-hide-stars nil)
  (org-modern-table nil)
  (org-modern-keyword "‣ ")
  (org-modern-horizontal-rule t)
  (org-modern-todo t))

;; cdlatex: fast math input (TAB templates, `;' math-symbol prefix). Powers
;; `org-cdlatex-mode' and LaTeX-mode. Symbol/modify maps from karthink.
(use-package cdlatex
  :defer t
  :hook (LaTeX-mode . cdlatex-mode)
  :config
  (setq cdlatex-math-symbol-prefix ?\;
        cdlatex-paired-parens "$[{("
        cdlatex-math-symbol-alist
        '((?F ("\\Phi")) (?o ("\\omega" "\\mho")) (?. ("\\cdot" "\\circ"))
          (?6 ("\\partial")) (?v ("\\vee" "\\forall"))
          (?^ ("\\uparrow" "\\Updownarrow")))
        cdlatex-math-modify-alist
        '((?k "\\mathfrak") (?b "\\mathbf") (?B "\\mathbb") (?t "\\text"))))

;; Auto-toggle a fragment's LaTeX preview as point enters/leaves it -> live feel.
(use-package org-fragtog
  :after org
  :hook (org-mode . org-fragtog-mode))

;; Render existing fragments shortly AFTER opening (on idle, not on the open
;; path). Without this, `org-startup-with-latex-preview nil' means a freshly
;; opened file shows no previews until you move the cursor through each fragment.
(defun my/org-preview-buffer-on-idle ()
  "Preview all LaTeX fragments in this Org buffer once Emacs is idle."
  (when (derived-mode-p 'org-mode)
    (let ((buf (current-buffer)))
      (run-with-idle-timer
       0.4 nil
       (lambda ()
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (ignore-errors (org-latex-preview '(16))))))))))  ; 16 = whole buffer
(add-hook 'org-mode-hook #'my/org-preview-buffer-on-idle)

(add-hook 'emacs-startup-hook
          (lambda ()
            (run-with-idle-timer 1.5 nil (lambda () (require 'org nil t)))))

;; Auto-tangle on save when the file has `#+auto_tangle: t' (built-in babel,
;; no extra package). Bind `before-save-hook' locally in org buffers.
(defun my/org-auto-tangle ()
  "Tangle the current Org file on save if it declares `#+auto_tangle: t'."
  (when (and (derived-mode-p 'org-mode)
             (save-excursion
               (goto-char (point-min))
               (re-search-forward "^#\\+auto_tangle:[ \t]*t\\b" nil t)))
    (let ((before-save-hook nil)) (org-babel-tangle))))
(add-hook 'org-mode-hook
          (lambda () (add-hook 'before-save-hook #'my/org-auto-tangle nil t)))

;;; ---------------------------------------------------------------------------
;;; Markdown
;;; ---------------------------------------------------------------------------
(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"        . gfm-mode))
  :init (setq markdown-command "pandoc")    ; used by C-c C-c v live preview
  :config
  (setq markdown-fontify-code-blocks-natively t
        markdown-enable-math t))

;;; ---------------------------------------------------------------------------
;;; LaTeX (AUCTeX) -- needs a TeX distribution (TeX Live / MiKTeX) for compile.
;;; ---------------------------------------------------------------------------
(use-package auctex
  :ensure t
  :mode ("\\.tex\\'" . LaTeX-mode)
  :hook ((LaTeX-mode . cdlatex-mode)
         (LaTeX-mode . turn-on-reftex)
         (LaTeX-mode . prettify-symbols-mode)
         (LaTeX-mode . visual-line-mode))
  :init
  (setq TeX-auto-save t
        TeX-parse-self t                  
        TeX-electric-escape nil
        TeX-source-correlate-method 'synctex 
        TeX-source-correlate-start-server t
        reftex-plug-into-AUCTeX t
        reftex-default-bibliography (list (my/emacs-path "bib/refs.bib")))
  :config
  (setq-default TeX-master nil)
  (setq TeX-PDF-mode t)                      ; produce PDF via pdflatex
  ;; Windows: open in the OS-default PDF viewer (no SyncTeX).
  ;; Linux/macOS: in-Emacs pdf-tools with full SyncTeX.
  (unless IS-WINDOWS
    (setq TeX-source-correlate-mode t
          TeX-view-program-selection '((output-pdf "PDF Tools")))))

;; In-Emacs PDF viewer (Linux/macOS). epdfinfo build on Windows is painful, so
;; Windows just uses the OS-default viewer.
(use-package pdf-tools
  :if (not IS-WINDOWS)
  :defer t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config (pdf-tools-install :no-query))  

;;; ---------------------------------------------------------------------------
;;; Calc -- symbolic math + LaTeX (karthink's `latex-math-from-calc')
;;; ---------------------------------------------------------------------------
;; Select an expression (or sit on a line), hit C-S-e: calc evaluates it and
;; replaces it with the result, formatted as LaTeX inside LaTeX/Org math.
(defun latex-math-from-calc ()
  "Evaluate the region (or current line) with Calc; replace it with the result."
  (interactive)
  (if (region-active-p)
      (let ((beg (region-beginning)) (end (region-end)))
        (kill-region beg end)
        (insert (calc-eval `(,(current-kill 0)
                             calc-language ,(if (derived-mode-p 'latex-mode 'LaTeX-mode)
                                                'latex 'normal)
                             calc-prefer-frac t calc-angle-mode rad))))
    (let ((line (thing-at-point 'line t)))
      (end-of-line) (kill-line 0)
      (insert (calc-eval `(,line
                           calc-language ,(if (derived-mode-p 'latex-mode 'LaTeX-mode)
                                              'latex 'normal)
                           calc-prefer-frac t calc-angle-mode rad))))))
(use-package calc
  :ensure nil
  :bind (("C-x c" . calc)                       ; calculator (also C-x * c)
         ("C-S-e" . latex-math-from-calc))      ; evaluate math in-place
  :config (setq calc-make-windows-dedicated t))
;; Inline algebraic evaluation in any buffer: C-x * e  (calc-embedded, built-in).

;; casual: a discoverable transient menu for Calc (and more) -- press `C-o' in
;; the Calc buffer to see every operation. Great while learning Calc.
(use-package casual
  :defer t
  :bind (:map calc-mode-map ("C-o" . casual-calc-tmenu)))

(provide 'my-writing)
;;; my-writing.el ends here
