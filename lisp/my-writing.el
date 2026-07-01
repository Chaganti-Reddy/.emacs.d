;;; my-writing.el --- Org (research/LaTeX/calc) + Markdown -*- lexical-binding: t; -*-

;;; ---------------------------------------------------------------------------
;;; Org 9.8 (from GNU ELPA -- upgrades the built-in)
;;; ---------------------------------------------------------------------------
;; :ensure can't upgrade a built-in, so install Org 9.8 explicitly once. The
;; ELPA org dir is put on `load-path' ahead of built-in Org in early-init, so
;; the newer version loads first.
(when (and (featurep 'package)
           (not (package-installed-p 'org '(9 8))))
  (let ((package-native-compile nil)
        (native-comp-jit-compilation nil))
    (ignore-errors (package-install 'org))))
(when-let* ((dir (car (file-expand-wildcards
                       (expand-file-name "org-[0-9]*" package-user-dir)))))
  (add-to-list 'load-path dir)
  (add-to-list 'native-comp-jit-compilation-deny-list
               (concat "\\`" (regexp-quote (file-name-as-directory dir))))
  (dolist (f (directory-files-recursively dir "\\.elc\\'"))
    (ignore-errors (delete-file f))))

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
         (org-mode . org-cdlatex-mode)      ; fast math input (e.g. `ab' -> a_b)
         (org-mode . (lambda () (setq-local tab-always-indent t))))
  :custom
  (org-startup-indented t)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)                   ; \alpha renders as the glyph
  (org-pretty-entities-include-sub-superscripts t)
  (org-image-align 'center)                 ; center inline images + LaTeX previews
  (org-support-shift-select t)
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-src-content-indentation 0)
  (org-confirm-babel-evaluate nil)
  (org-return-follows-link t)
  (org-startup-with-latex-preview nil)
  (org-ellipsis " ▾")
  (org-fontify-quote-and-verse-blocks t)
  (org-fontify-whole-heading-line t)
  (org-fontify-done-headline t)
  (org-highlight-latex-and-related '(latex script entities))
  :config
  (setq org-use-sub-superscripts '{})
  (font-lock-add-keywords
   'org-mode
   '(("\\(\\\\\\(?:label\\|c?ref\\|eqref\\)\\){\\(.+?\\)}"
      (1 font-lock-keyword-face) (2 font-lock-constant-face))))
  (advice-add 'org-try-cdlatex-tab :around
              (lambda (orig &rest _) (ignore-errors (funcall orig))))
  (setq org-format-latex-options
        (plist-put
         (plist-put
          (plist-put org-format-latex-options :scale 1.0)
          :foreground 'default)
         :background 'default))
  (setq org-preview-latex-default-process
        (if IS-WINDOWS
            (cond ((executable-find "dvipng")  'dvipng)
                  ((executable-find "dvisvgm") 'dvisvgm)
                  (t org-preview-latex-default-process))
          (cond ((executable-find "dvisvgm") 'dvisvgm)
                ((executable-find "dvipng")  'dvipng)
                (t org-preview-latex-default-process))))
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

(defun my/scale-org-headings (&rest _)
  (when (facep 'org-level-1)
    (dolist (s '((org-level-1 . 1.4) (org-level-2 . 1.25) (org-level-3 . 1.18)
                 (org-level-4 . 1.12) (org-level-5 . 1.08)
                 (org-level-6 . 1.05) (org-level-7 . 1.0) (org-level-8 . 1.0)))
      (set-face-attribute (car s) nil :height (cdr s) :weight 'semibold))
    (set-face-attribute 'org-document-title nil :height 1.6 :weight 'bold)))
(with-eval-after-load 'org-faces (my/scale-org-headings))
(add-hook 'enable-theme-functions #'my/scale-org-headings)

;; cdlatex: fast math input (TAB templates, `;' math-symbol prefix). Powers
;; `org-cdlatex-mode' and LaTeX-mode.
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

;; org-appear: reveal hidden emphasis/link/sub-super markers ONLY on the element
;; at point (so editing *bold*/[[links]]/_x_ isn't blind). Pairs with
;; `org-hide-emphasis-markers' + `org-pretty-entities'.
(use-package org-appear
  :after org
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autoemphasis t)
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t))

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

;; org-capture: fast timestamped note into an inbox file (C-c c). Lightweight --
;; no agenda/PKM machinery.
(setq org-capture-templates
      '(("n" "Note" entry (file "inbox.org")
         "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n" :empty-lines 1)))
(global-set-key (kbd "C-c c") #'org-capture)

;; engrave-faces: export Org src blocks to PDF in your Emacs THEME colors (no
;; Python/minted/-shell-escape needed -- Windows-friendly). Used by ox-latex.
(use-package engrave-faces :defer t)
(with-eval-after-load 'ox-latex
  (setq org-latex-src-block-backend 'engraved))

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
         (LaTeX-mode . visual-line-mode)
         (LaTeX-mode . outline-minor-mode))
  :init
  (setq outline-minor-mode-cycle t)
  (setq TeX-auto-save t
        TeX-parse-self t                  
        TeX-electric-escape nil
        TeX-source-correlate-method 'synctex 
        TeX-source-correlate-start-server t
        reftex-plug-into-AUCTeX t
        reftex-default-bibliography (list (my/emacs-path "bib/refs.bib")))
  :config
  (setq-default TeX-master nil)
  (setq TeX-PDF-mode t)
  (when IS-WINDOWS
    (setq-default TeX-PDF-from-DVI "Dvipdfmx"))
  (setq prettify-symbols-unprettify-at-point 'right-edge)
  ;; Windows: open in the OS-default PDF viewer (no SyncTeX).
  ;; Linux/macOS: in-Emacs pdf-tools with full SyncTeX.
  (unless IS-WINDOWS
    (setq TeX-source-correlate-mode t
          TeX-view-program-selection '((output-pdf "PDF Tools")))))

(with-eval-after-load 'preview
  (setq preview-scale-function
        (lambda () (* 1.25 (funcall (preview-scale-from-face)))))
  (when (and IS-WINDOWS (executable-find "dvipng"))
    (setq preview-image-type 'dvi*
          preview-dvi*-image-type 'png))
  (let ((gs (or (executable-find "mgs") (executable-find "gswin64c")
                (executable-find "gswin32c") (executable-find "gs"))))
    (when gs (setq preview-gs-command gs)))
  (setq preview-auto-cache-preamble t)
  (define-key LaTeX-mode-map (kbd "C-c C-x") preview-map))

;; --- LaTeX live auto-preview (the `org-fragtog' analogue for .tex) ---------
;; AUCTeX auto-REVEALS a preview's source when point enters it and re-hides it
;; on leave (`preview-auto-reveal', default on), but never REGENERATES after an
;; edit. This mode adds that -- the smart, targeted way:
;;   * preview ONLY math constructs (inline $...$, \(\), \[\], display envs),
;;     never prose, never the whole section;
;;   * preview ONLY the one you actually edited, and only once you LEAVE it
;;     (so the formula never collapses under you mid-edit);
;;   * via `preview-at-point', so AUCTeX finds the construct's exact bounds --
;;     no region mis-alignment, no swallowed text.
;; `texmathp' tells us when point is in math and (via `texmathp-why') where the
;; construct began; we preview that spot on the transition math -> not-math.
(defvar-local my/latex-preview-timer nil)
(defvar-local my/latex-preview--math-start nil
  "Buffer position where the math construct point is in began, else nil.")
(defvar-local my/latex-preview--dirty nil
  "Non-nil once the current math construct has been edited.")

(defun my/latex-auto-preview--after-change (&rest _)
  "Mark the math construct at point as edited (so leaving it re-previews)."
  (when (and (fboundp 'texmathp) (texmathp))
    (setq my/latex-preview--dirty t)))

(defun my/latex-auto-preview--post-command ()
  "Track the math construct at point; preview it after an edited one is left."
  (when (bound-and-true-p my/latex-auto-preview-mode)
    (if (and (fboundp 'texmathp) (texmathp))
        ;; Inside math: remember where this construct starts.
        (setq my/latex-preview--math-start
              (and (consp texmathp-why) (cdr texmathp-why)))
      ;; Outside math: if we just left a construct we edited, preview just it.
      (when (and my/latex-preview--math-start my/latex-preview--dirty)
        (let ((pos my/latex-preview--math-start))
          (setq my/latex-preview--math-start nil
                my/latex-preview--dirty nil)
          (when (timerp my/latex-preview-timer) (cancel-timer my/latex-preview-timer))
          (setq my/latex-preview-timer
                (run-with-idle-timer 0.3 nil
                                     #'my/latex-auto-preview--at
                                     (current-buffer) pos)))))))

(defun my/latex-auto-preview--at (buf pos)
  "Preview the single LaTeX construct around POS in BUF."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when (bound-and-true-p my/latex-auto-preview-mode)
        (save-excursion
          (goto-char (min pos (point-max)))
          (ignore-errors (preview-at-point)))))))

(define-minor-mode my/latex-auto-preview-mode
  "Live-render the LaTeX math construct you just edited, on leaving it.
Targets one construct at a time via `preview-at-point' and pairs with AUCTeX's
`preview-auto-reveal' for an `org-fragtog'-like feel."
  :lighter " LivePrv"
  (if my/latex-auto-preview-mode
      (progn
        (add-hook 'after-change-functions #'my/latex-auto-preview--after-change nil t)
        (add-hook 'post-command-hook #'my/latex-auto-preview--post-command nil t)
        ;; Render math already in the file shortly after opening (whole buffer
        ;; once -- this is the only bulk pass; edits after are per-construct).
        (let ((buf (current-buffer)))
          (run-with-idle-timer
           0.8 nil
           (lambda ()
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (when (bound-and-true-p my/latex-auto-preview-mode)
                   (ignore-errors (preview-buffer)))))))))
    (remove-hook 'after-change-functions #'my/latex-auto-preview--after-change t)
    (remove-hook 'post-command-hook #'my/latex-auto-preview--post-command t)
    (when (timerp my/latex-preview-timer) (cancel-timer my/latex-preview-timer))))

(add-hook 'LaTeX-mode-hook #'my/latex-auto-preview-mode)

;; In-Emacs PDF viewer (Linux/macOS). epdfinfo build on Windows is painful, so
;; Windows just uses the OS-default viewer.
(use-package pdf-tools
  :if (not IS-WINDOWS)
  :defer t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config (pdf-tools-install :no-query))  

;;; ---------------------------------------------------------------------------
;;; Calc -- symbolic math + LaTeX
;;; ---------------------------------------------------------------------------
;; Select an expression (or sit on a line), hit C-S-e: calc evaluates it and
;; replaces it with the result, formatted as LaTeX inside LaTeX/Org math.
(defun my/calc-strip-math (s)
  "Strip surrounding math delimiters ($, \\(\\), \\[\\]) and whitespace from S.
Calc rejects `$', so leaving them in produces a parse error."
  (string-trim (replace-regexp-in-string "\\$\\|\\\\[][()]" "" s)))

(defun my/math-region-at-point ()
  "If point sits inside inline $...$ math, return (BEG . END) of its CONTENT.
Otherwise nil. Keeps the `$' delimiters in place; only the inside is replaced."
  (save-excursion
    (let* ((p (point)) (bol (line-beginning-position)) (eol (line-end-position))
           (open  (save-excursion (and (search-backward "$" bol t) (1+ (point)))))
           (close (save-excursion (and (search-forward  "$" eol t) (1- (point))))))
      (when (and open close (<= open p) (>= close p) (< open close))
        (cons open close)))))

(defun latex-math-from-calc ()
  "Evaluate math with Calc and replace it in place.
Uses the region if active, else the $...$ fragment at point, else the line."
  (interactive)
  (let* ((bounds (cond ((region-active-p)
                        (cons (region-beginning) (region-end)))
                       ((my/math-region-at-point))
                       (t (cons (line-beginning-position) (line-end-position)))))
         (input  (my/calc-strip-math
                  (buffer-substring-no-properties (car bounds) (cdr bounds))))
         (result (calc-eval
                  (list input
                        'calc-language (if (derived-mode-p 'latex-mode 'LaTeX-mode 'org-mode)
                                           'latex 'normal)
                        'calc-prefer-frac t 'calc-angle-mode 'rad))))
    ;; calc-eval returns (POS "message") on a parse error, not a string.
    (unless (stringp result)
      (user-error "Calc: %s" (if (consp result) (cadr result) "cannot evaluate")))
    (delete-region (car bounds) (cdr bounds))
    (insert result)))
(use-package calc
  :ensure nil
  :bind (("C-x c" . calc)                       ; calculator (also C-x * c)
         ("C-S-e" . latex-math-from-calc))      ; evaluate math in-place
  :config
  ;; By default Calc builds its stack + trail windows itself (via `split-window'),
  ;; bypassing `display-buffer-alist'. Setting these two hooks makes it route each
  ;; buffer through `display-buffer' instead, so our alist rules apply -- stack and
  ;; trail then dock together on the right, stacked. (See calc.el `calc-window-hook'.)
  (setq calc-make-windows-dedicated t
        calc-window-hook       (lambda () (display-buffer (current-buffer)))
        calc-trail-window-hook (lambda () (display-buffer (current-buffer)))))
;; Inline algebraic evaluation in any buffer: C-x * e  (calc-embedded, built-in).

;; casual: a discoverable transient menu for Calc (and more) -- press `C-o' in
;; the Calc buffer to see every operation. Great while learning Calc.
(use-package casual
  :defer t
  :bind (:map calc-mode-map ("C-o" . casual-calc-tmenu)))

(provide 'my-writing)
;;; my-writing.el ends here
