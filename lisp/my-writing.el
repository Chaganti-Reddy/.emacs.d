;;; my-writing.el --- Org (research/LaTeX/calc) + Markdown -*- lexical-binding: t; -*-

;;; ---------------------------------------------------------------------------
;;; Org (tecosaur dev branch, via elpaca)
;;; ---------------------------------------------------------------------------
(defun my/org-enable-live-latex-preview ()
  "Enable tecosaur Org's live LaTeX preview minor mode in this buffer.
Handles the dev-branch rename (`org-latex-preview-auto-mode' ->
`org-latex-preview-mode'); no-op on stock Org that lacks both."
  (cond ((fboundp 'org-latex-preview-mode)      (org-latex-preview-mode 1))
        ((fboundp 'org-latex-preview-auto-mode) (org-latex-preview-auto-mode 1))))

(defun my/org-enable-cdlatex ()
  "Enable `org-cdlatex-mode' without letting its failure abort `org-mode-hook'.
`org-cdlatex-mode' hard-requires texmathp/AUCTeX; if that is ever missing it
signals, which would skip every later hook (live preview, etc.). Swallow it."
  (when (fboundp 'org-cdlatex-mode)
    (ignore-errors (org-cdlatex-mode 1))))

(setq org-directory
      (if (and IS-WINDOWS (file-directory-p "D:/")) "D:/Org" "~/Org"))

(use-package async :defer t)

(use-package org
  :ensure (org
           :remotes ("tecosaur"
                     :repo "https://git.tecosaur.net/tec/org-mode.git"
                     :branch "dev")
           :files (:defaults ("etc/styles/" "etc/styles/*" "doc/*.texi"))
           :build t
           :pre-build
           (progn
             (with-temp-file "org-version.el"
               (require 'lisp-mnt)
               (let ((version
                      (with-temp-buffer
                        (insert-file-contents "lisp/org.el")
                        (lm-header "version")))
                     (git-version
                      (string-trim
                       (with-temp-buffer
                         (call-process "git" nil t nil "rev-parse" "--short" "HEAD")
                         (buffer-string)))))
                 (insert
                  (format "(defun org-release () \"The release version of Org.\" %S)\n" version)
                  (format "(defun org-git-version () \"The truncate git commit hash of Org mode.\" %S)\n" git-version)
                  "(provide 'org-version)\n")))
             (require 'elpaca-menu-org)
             (elpaca-menu-org--build))
           :pin nil)
  :defer t
  :init
  (setq org-id-locations-file (my/var "org-id-locations")
        org-persist-directory (my/var "org-persist/" t)
        org-preview-latex-image-directory (my/var "org-latex-preview/" t)
        org-modules nil)
  :hook ((org-mode . visual-line-mode)
         (org-mode . my/org-enable-cdlatex) ; fast math input (e.g. `ab' -> a_b)
         (org-mode . my/org-enable-live-latex-preview)
         (org-mode . (lambda () (setq-local tab-always-indent t))))
  :custom
  (org-startup-indented t)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)                   ; \alpha renders as the glyph
  (org-pretty-entities-include-sub-superscripts t)
  (org-image-align 'center)
  (org-support-shift-select t)
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-src-content-indentation 0)
  (org-confirm-babel-evaluate nil)
  (org-return-follows-link t)
  (org-startup-with-latex-preview t)
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
  (with-eval-after-load 'org-latex-preview
    (setq org-latex-preview-numbered t
          org-latex-preview-mode-display-live '(block edit-special)
          org-latex-preview-mode-update-delay 0.3
          org-latex-preview-mode-track-inserts t
          org-latex-preview-process-active-indicator 'face
          org-latex-preview-process-precompile t
          org-latex-preview-cache 'persist
          org-latex-preview-persist-expiry 30
          org-latex-preview-mode-ignored-commands
          '(next-line previous-line scroll-up-command scroll-down-command
            mwheel-scroll scroll-other-window scroll-other-window-down))
    (plist-put org-latex-preview-appearance-options :scale 1.0)
    (plist-put org-latex-preview-appearance-options :zoom
               (+ (/ my/font-size 10.0) 0.15))
    (plist-put org-latex-preview-appearance-options :page-width 0.8)
    (plist-put org-latex-preview-appearance-options :foreground 'auto)
    (when (fboundp 'org-latex-preview--tex-styled)
      (advice-add 'org-latex-preview--tex-styled :filter-args
                  (lambda (args)
                    (list (nth 0 args) (nth 1 args)
                          (plist-put (copy-sequence (nth 2 args))
                                     :continue-color nil)))))
    (defun my/org-latex-preview-image-at-point (&optional _arg)
      "Copy the preview image file for the fragment at point to the kill-ring."
      (interactive "P")
      (if-let* ((img (thread-first (point) (overlays-at) last car
                       (overlay-get 'preview-image) cdr (plist-get :file)))
                ((file-exists-p img)))
          (progn (kill-new img) (message "Preview image path copied."))
        (message "No LaTeX preview image at point.")))
    (defun my/org-latex-next-env (&optional arg)
      "Jump to the next LaTeX math environment/fragment."
      (interactive "p")
      (save-match-data
        (re-search-forward org-latex-preview--tentative-math-re nil t (or arg 1))))
    (defun my/org-latex-prev-env (&optional arg)
      "Jump to the previous LaTeX math environment/fragment."
      (interactive "p")
      (my/org-latex-next-env (- (or arg 1))))
    (defvar-keymap my/org-latex-env-map :repeat t
      "m" #'my/org-latex-next-env "M" #'my/org-latex-prev-env
      "n" #'my/org-latex-next-env "p" #'my/org-latex-prev-env)
    (put 'my/org-latex-next-env 'repeat-map 'my/org-latex-env-map)
    (put 'my/org-latex-prev-env 'repeat-map 'my/org-latex-env-map)
    (defun my/org-latex-preview-show-error ()
      "Surface a LaTeX preview compile error via local help."
      (display-local-help t))
    (add-hook 'org-ctrl-c-ctrl-c-final-hook #'my/org-latex-preview-show-error -10)
    (define-key org-mode-map (kbd "C-c i w")     #'my/org-latex-preview-image-at-point)
    (define-key org-mode-map (kbd "M-g m")       #'my/org-latex-next-env)
    (define-key org-mode-map (kbd "M-g M")       #'my/org-latex-prev-env)
    (define-key org-mode-map (kbd "C-c C-x SPC") #'org-latex-preview-clear-cache)
    ;; --- Live preview for freshly-typed $...$ / $$...$$ --------------------
    (defun my/org-latex-preview--track-dollar (beg end _)
      "Track newly inserted $...$ / $$...$$ fragments for live preview."
      (when (and org-latex-preview-mode-track-inserts
                 (fboundp 'org-latex-preview-mode--maybe-track-element-here))
        (let ((initial-point (point)) frags)
          (save-excursion
            (goto-char beg)
            (while (search-forward "$" end t)
              (push (org-latex-preview-mode--maybe-track-element-here
                     'latex-fragment initial-point)
                    frags)))
          (when (setq frags (delq nil frags))
            (org-latex-preview--place-from-elements
             org-latex-preview-process-default frags)))))
    (when (fboundp 'org-latex-preview-mode--detect-fragments-in-change)
      (advice-add 'org-latex-preview-mode--detect-fragments-in-change :after
                  #'my/org-latex-preview--track-dollar))
    ;; --- Background preamble precompilation ---------------------
    (defun my/org-latex-preview-precompile-async (org-buf)
      "Precompile the Org LaTeX-preview preamble for ORG-BUF in the background."
      (when (buffer-live-p org-buf)
        (with-current-buffer org-buf
          (when (and org-latex-preview-process-precompile
                     (require 'async nil t))
            (let* ((org-location (org-find-library-dir "org"))
                   (compiler (or (cdr (assoc "LATEX_COMPILER"
                                             (org-collect-keywords '("LATEX_COMPILER"))))
                                 org-latex-compiler))
                   (header (concat (or org-latex-preview--preamble-content
                                       (org-latex-preview--get-preamble))
                                   org-latex-preview--include-preview-string))
                   (relative-file-p
                    (string-match-p "\\(?:\\\\input{\\|\\\\include{\\)[^/]" header))
                   (remote-file-p (file-remote-p default-directory))
                   (info (list :latex-compiler compiler
                               :precompile-format-spec
                               (let ((c (cdr (assoc compiler
                                                    org-latex-preview-compiler-command-map))))
                                 `((?l . ,c) (?L . ,(car (split-string (or c " "))))))))
                   (preamble-hash
                    (sha1 (concat header compiler
                                  (if relative-file-p default-directory "-temp")))))
              (when (and (equal compiler "pdflatex")
                         (not remote-file-p)
                         (not (cadr (org-persist-read "LaTeX format file cache"
                                                      (list :key preamble-hash) nil nil
                                                      :read-related t))))
                (async-start
                 `(lambda ()
                    (add-to-list 'load-path ,org-location)
                    (require 'ox)
                    (org-latex--precompile-preamble
                     ',info ,header
                     ,(expand-file-name preamble-hash temporary-file-directory)))
                 `(lambda (dump-file)
                    (let ((inhibit-message t))
                      (org-persist--load-index)
                      (org-persist-register
                       `("LaTeX format file cache" (file ,dump-file))
                       (list :key ,preamble-hash) :write-immediately t)
                      (message "Org LaTeX preamble precompiled in background."))))))))))
    (defun my/org-latex-preview-precompile-idle (&optional beg end _)
      "On idle, kick off background preamble precompile for the current buffer.
Signature also fits `org-latex-preview-clear-cache' advice (BEG END)."
      (unless (or beg end)
        (run-with-idle-timer
         2 nil #'my/org-latex-preview-precompile-async (current-buffer))))
    (advice-add 'org-latex-preview-clear-cache :after
                #'my/org-latex-preview-precompile-idle)
    (add-hook 'org-mode-hook #'my/org-latex-preview-precompile-idle))
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
  (org-modern-table t)
  (org-modern-timestamp t)
  (org-modern-tag t)
  (org-modern-priority t)
  (org-modern-todo t)
  (org-modern-checkbox '((?X . "☑") (?- . "◧") (?\s . "☐")))
  (org-modern-progress 12)
  (org-modern-list '((?+ . "◦") (?- . "–") (?* . "•")))
  (org-modern-keyword "‣ ")
  (org-modern-block-name t)
  (org-modern-horizontal-rule t))

(use-package org-modern-indent
  :ensure (org-modern-indent :host github :repo "jdtsmith/org-modern-indent")
  :after org-modern
  :hook (org-mode . org-modern-indent-mode))

(defun my/scale-org-headings (&rest _)
  (when (facep 'org-level-1)
    (dolist (s '((org-level-1 . 1.4) (org-level-2 . 1.25) (org-level-3 . 1.18)
                 (org-level-4 . 1.12) (org-level-5 . 1.08)
                 (org-level-6 . 1.05) (org-level-7 . 1.0) (org-level-8 . 1.0)))
      (set-face-attribute (car s) nil :height (cdr s) :weight 'semibold))
    (set-face-attribute 'org-document-title nil :height 1.6 :weight 'bold)))
(with-eval-after-load 'org-faces (my/scale-org-headings))
(add-hook 'enable-theme-functions #'my/scale-org-headings)

;; cdlatex: fast math input (TAB templates, `;' math-symbol prefix).
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

;; Auto-tangle on save when the file has `#+auto_tangle: t'. Bind `before-save-hook' locally in org buffers.
(defun my/org-auto-tangle ()
  "Tangle the current Org file on save if it declares `#+auto_tangle: t'."
  (when (and (derived-mode-p 'org-mode)
             (save-excursion
               (goto-char (point-min))
               (re-search-forward "^#\\+auto_tangle:[ \t]*t\\b" nil t)))
    (let ((before-save-hook nil)) (org-babel-tangle))))
(add-hook 'org-mode-hook
          (lambda () (add-hook 'before-save-hook #'my/org-auto-tangle nil t)))

;; org-capture: fast timestamped note into an inbox file (C-c c).
(setq org-capture-templates
      '(("n" "Note" entry (file "inbox.org")
         "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n" :empty-lines 1)
        ("t" "Todo" entry (file "inbox.org")
         "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n" :empty-lines 1)))
(global-set-key (kbd "C-c c") #'org-capture)

;;; ---------------------------------------------------------------------------
;;; Org Agenda
;;; ---------------------------------------------------------------------------
(use-package org-agenda
  :ensure nil
  :defer t
  :bind ("C-c A" . org-agenda)
  :hook (org-agenda-mode . hl-line-mode)
  :config
  (setq org-agenda-files (when (file-directory-p org-directory) (list org-directory))
        org-agenda-window-setup 'current-window
        org-agenda-restore-windows-after-quit t
        org-agenda-start-on-weekday nil
        org-agenda-span 'week
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-block-separator ?─
        org-agenda-tags-column 'auto
        org-agenda-todo-keyword-format "%-1s"
        org-agenda-breadcrumbs-separator " ▸ "
        org-agenda-scheduled-leaders '("" "Sched.%2dx: ")
        org-agenda-deadline-leaders '("Deadline: " "In %d d: " "%d d ago: ")
        org-agenda-current-time-string "◀── now ──────────────────────────────"
        org-agenda-time-grid
        '((daily today require-timed)
          (700 900 1100 1300 1500 1700 1900 2100)
          " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
        org-agenda-show-current-time-in-grid t))

;;; ---------------------------------------------------------------------------
;;; Denote -- lightweight, plain-text, linked notes (NO database).
;;; ---------------------------------------------------------------------------
(use-package denote
  :defer t
  :bind (("C-c n n" . denote)                    ; new note
         ("C-c n c" . denote-region)             ; region -> new note
         ("C-c n N" . denote-type)               ; new note, choose file type
         ("C-c n d" . denote-date)               ; new note for a date
         ("C-c n s" . denote-subdirectory)       ; new note in a subdir
         ("C-c n z" . denote-signature)          ; new note with a signature
         ("C-c n o" . denote-open-or-create)     ; jump to / create a note
         ("C-c n l" . denote-link)               ; insert a link to a note
         ("C-c n L" . denote-add-links)          ; insert links matching a regexp
         ("C-c n b" . denote-backlinks)          ; buffer of notes linking here
         ("C-c n r" . denote-rename-file)
         ("C-c n R" . denote-rename-file-using-front-matter))
  :hook (dired-mode . denote-dired-mode)         ; fontify denote names in dired
  :init
  (setq denote-directory (expand-file-name "notes/" org-directory))
  :config
  (setq denote-known-keywords '("emacs" "work" "research" "ideas" "journal")
        denote-infer-keywords t
        denote-sort-keywords t
        denote-file-type 'org
        denote-prompts '(title keywords)
        denote-date-prompt-use-org-read-date t
        denote-backlinks-show-context t
        denote-dired-directories (list denote-directory))
  (denote-rename-buffer-mode 1))

;; Journaling on top of denote.
(use-package denote-journal
  :defer t
  :bind ("C-c n j" . denote-journal-new-or-existing-entry)
  :init
  (setq denote-journal-directory
        (expand-file-name "journal/" (expand-file-name "notes/" org-directory))
        denote-journal-keyword "journal"))

;; engrave-faces: export Org src blocks to PDF in your Emacs THEME colors (no
;; Python/minted/-shell-escape needed -- Windows-friendly). Used by ox-latex.
(use-package engrave-faces :defer t)
(with-eval-after-load 'ox-latex
  (setq org-latex-src-block-backend 'engraved))

;;; ---------------------------------------------------------------------------
;;; Markdown
;;; ---------------------------------------------------------------------------
(use-package csv-mode
  :mode "\\.csv\\'")

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
        (lambda () (* 1.20 (funcall (preview-scale-from-face)))))
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
        (setq my/latex-preview--math-start
              (and (consp texmathp-why) (cdr texmathp-why)))
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
    (unless (stringp result)
      (user-error "Calc: %s" (if (consp result) (cadr result) "cannot evaluate")))
    (delete-region (car bounds) (cdr bounds))
    (insert result)))
(use-package calc
  :ensure nil
  :bind (("C-x c" . calc)                       ; calculator (also C-x * c)
         ("C-S-e" . latex-math-from-calc))      ; evaluate math in-place
  :config
  (setq calc-make-windows-dedicated t
        calc-window-hook       (lambda () (display-buffer (current-buffer)))
        calc-trail-window-hook (lambda () (display-buffer (current-buffer)))))
;; Inline algebraic evaluation in any buffer: C-x * e  (calc-embedded, built-in).

(use-package casual :defer t)
(with-eval-after-load 'calc
  (define-key calc-mode-map (kbd "C-o") #'casual-calc-tmenu))
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-o") #'casual-dired-tmenu))
(with-eval-after-load 'info
  (define-key Info-mode-map (kbd "C-o") #'casual-info-tmenu))
(with-eval-after-load 'isearch
  (define-key isearch-mode-map (kbd "C-o") #'casual-isearch-tmenu))
(with-eval-after-load 'ibuffer
  (define-key ibuffer-mode-map (kbd "C-o") #'casual-ibuffer-tmenu))
(with-eval-after-load 'bookmark
  (define-key bookmark-bmenu-mode-map (kbd "C-o") #'casual-bookmarks-tmenu))
(with-eval-after-load 're-builder
  (define-key reb-mode-map (kbd "C-o") #'casual-re-builder-tmenu))
(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "C-o") #'casual-agenda-tmenu))
(global-set-key (kbd "C-c O") #'casual-editkit-main-tmenu) 
(global-set-key (kbd "M-g a") #'casual-avy-tmenu)

(provide 'my-writing)
;;; my-writing.el ends here
