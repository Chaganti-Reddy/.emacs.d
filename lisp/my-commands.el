;;; my-commands.el --- Custom utility commands & their keys -*- lexical-binding: t; -*-

;; --- Config helpers ---------------------------------------------------------
(defun my/reload-init ()
  "Reload the configuration without restarting."
  (interactive) (load-file user-init-file) (message "Config reloaded."))
(defun my/open-init ()
  "Open the init file."
  (interactive) (find-file user-init-file))

;; --- Line editing (VS-Code parity) ------------------------------------------
(defun my/open-line-below ()
  "Open an indented line below point, regardless of column."
  (interactive) (end-of-line) (newline-and-indent))
(defun my/open-line-above ()
  "Open an indented line above point."
  (interactive)
  (beginning-of-line) (newline-and-indent) (forward-line -1) (indent-according-to-mode))
;; Drag the current line OR the active region up/down (region-aware).
(defun my/move-text-internal (arg)
  "Move region (if active) or current line ARG lines down (negative = up)."
  (cond
   ((region-active-p)
    (when (> (point) (mark)) (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg) (move-to-column column t)
      (set-mark (point)) (insert text) (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp))) (transpose-lines arg))
        (forward-line -1))
      (move-to-column column t)))))
(defun my/move-line-up ()   (interactive) (my/move-text-internal -1))
(defun my/move-line-down () (interactive) (my/move-text-internal 1))
;; After M-<up>/M-<down>, tap <up>/<down> to keep dragging (repeat-mode).
(defvar-keymap my/move-line-repeat-map
  :repeat t
  "<up>"   #'my/move-line-up
  "<down>" #'my/move-line-down)

(defun my/join-line ()
  "Join the next line onto this one (Vim J)."
  (interactive) (forward-line 1) (join-line))

;; --- File / buffer commands -------------------------------------------------
(defun my/rename-file-and-buffer ()
  "Rename the visited file and its buffer."
  (interactive)
  (let ((file (buffer-file-name)))
    (if (and file (file-exists-p file))
        (let ((new (read-file-name "New name: " file)))
          (rename-file file new t) (set-visited-file-name new t t)
          (message "Renamed to %s" new))
      (message "Buffer is not visiting a file."))))
(defun my/delete-file ()
  "Delete the visited file and kill its buffer."
  (interactive)
  (let ((file (buffer-file-name)))
    (if (and file (file-exists-p file))
        (when (yes-or-no-p (format "Delete %s? " file))
          (delete-file file) (kill-buffer) (message "Deleted."))
      (message "Buffer is not visiting a file."))))
(defun my/copy-path ()
  "Copy the visited file's absolute path to the clipboard."
  (interactive)
  (if-let* ((file (buffer-file-name)))
      (progn (kill-new file) (message "Copied: %s" file))
    (message "Buffer is not visiting a file.")))
(defun my/unfill-paragraph ()
  "Join a wrapped paragraph into one line (opposite of M-q)."
  (interactive) (let ((fill-column most-positive-fixnum)) (fill-paragraph nil)))

;; --- Transparency toggle (uses early-init constants) ------------------------
(defun my/toggle-transparency ()
  "Toggle frame transparency between opaque and `my/frame-alpha'."
  (interactive)
  (let ((cur (frame-parameter nil my/alpha-param)))
    (set-frame-parameter nil my/alpha-param
                         (if (and cur (/= cur 100)) 100 my/frame-alpha))))

;; --- Theme toggle: modus-vivendi (default) <-> gruber-darker ---------------
(defconst my/themes '(modus-vivendi gruber-darker)
  "Themes cycled by `my/toggle-theme'; the first one is the default.")
(defun my/toggle-theme ()
  "Cycle to the next theme in `my/themes', disabling the current one."
  (interactive)
  (let* ((cur  (car custom-enabled-themes))
         (next (or (cadr (memq cur my/themes)) (car my/themes))))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme next t)
    (message "Theme: %s" next)))

;; --- Scratch / temp files by language --------------------------------------
(defconst my/temp-root (my/var "scratch/" t) "Root for throwaway experiment files.")
(defun my/new-temp-file (name)
  "Create NAME under a language-named subfolder of `my/temp-root' and open it."
  (interactive "sFile name: ")
  (let* ((ext (downcase (or (file-name-extension name) "")))
         (folder (pcase ext
                   ("py" "python") ("rs" "rust") ("go" "go")
                   ((or "js" "ts" "tsx") "web") ("c" "c") ((or "cpp" "cc") "cpp")
                   ("java" "java") (_ "misc")))
         (dir (expand-file-name folder my/temp-root)))
    (make-directory dir t)
    (find-file (expand-file-name name dir))))

;; --- Smart polyglot compile/run --------------------------------------------
(defun my/smart-compile ()
  "Compile or run the current file/project, choosing a sensible command."
  (interactive)
  (save-some-buffers t)
  (let* ((name (when buffer-file-name (file-name-nondirectory buffer-file-name)))
         (base (when name (file-name-sans-extension name)))
         (cmd (cond
               ((or (file-exists-p "Makefile") (file-exists-p "makefile")) "make -k")
               ((locate-dominating-file default-directory "Cargo.toml") "cargo run")
               ((locate-dominating-file default-directory "go.mod") "go run .")
               ((derived-mode-p 'python-mode 'python-ts-mode) (format "python \"%s\"" name))
               ((derived-mode-p 'c-mode 'c-ts-mode)
                (format "gcc \"%s\" -o \"%s\" && \"./%s\"" name base base))
               ((derived-mode-p 'c++-mode 'c++-ts-mode)
                (format "g++ -std=c++20 \"%s\" -o \"%s\" && \"./%s\"" name base base))
               ((derived-mode-p 'js-mode 'js-ts-mode 'typescript-ts-mode)
                (format "node \"%s\"" name))
               ((derived-mode-p 'sh-mode 'bash-ts-mode) (format "bash \"%s\"" name))
               (t compile-command))))
    (compile cmd)))

;; --- Buffer switching / killing (hide internal & noisy buffers) -------------
(defvar my/hidden-buffer-regexp
  "\\`\\*\\(Messages\\|Warnings\\|Compile-Log\\|Echo Area\\|Async\\|Completions\\|\
Flymake log\\|EGLOT\\|tramp/?\\|epc con\\).*\\*\\'"
  "Buffers whose names match this (or start with a space) are hidden from
`my/switch-to-buffer' and from buffer cycling.")

(defun my/hidden-buffer-p (buf)
  "Non-nil if BUF is internal/noisy and should be hidden from selection."
  (let ((name (buffer-name buf)))
    (or (string-prefix-p " " name)
        (string-match-p my/hidden-buffer-regexp name))))

(defun my/buffer-skip-p (win buf _bury-or-kill)
  "Predicate for `switch-to-prev/next-buffer-skip' (VSCode-like scoping).
Skip hidden/noisy buffers always; and when WIN currently shows a buffer that
belongs to a project, also skip any BUF outside that project -- so `M-['/`M-]'
cycle only within the current project. Outside a project, cycle all (non-hidden)."
  (or (my/hidden-buffer-p buf)
      (and (fboundp 'project-current)
           (when-let* ((cur  (window-buffer win))
                       (proj (with-current-buffer cur (project-current nil))))
             (not (memq buf (project-buffers proj)))))))

(defun my/switch-to-buffer (&optional all)
  "Switch buffers, omitting internal/noisy ones; RET defaults to the last buffer.
In a project, restrict candidates to that project's buffers (VSCode-like).
With prefix ALL (\\[universal-argument]) -- or when not in a project -- offer
every buffer."
  (interactive "P")
  (let* ((proj (unless all (and (fboundp 'project-current) (project-current nil))))
         (bufs (seq-remove #'my/hidden-buffer-p
                           (if proj (project-buffers proj) (buffer-list)))))
    (switch-to-buffer
     (completing-read
      (if proj "Buffer (project): " "Buffer: ")
      (mapcar #'buffer-name bufs)
      nil nil nil nil
      (buffer-name (other-buffer (current-buffer) t))))))

(defun my/next-buffer ()
  "Like `next-buffer', but say so when there's no other (project) buffer."
  (interactive)
  (let ((b (current-buffer)))
    (next-buffer)
    (when (eq b (current-buffer)) (message "No next buffer in this project"))))
(defun my/previous-buffer ()
  "Like `previous-buffer', but say so when there's no other (project) buffer."
  (interactive)
  (let ((b (current-buffer)))
    (previous-buffer)
    (when (eq b (current-buffer)) (message "No previous buffer in this project"))))

(defun my/tab-next ()
  "Like `tab-next', but say so when there is only one tab."
  (interactive)
  (if (<= (length (funcall tab-bar-tabs-function)) 1)
      (message "No other tab")
    (tab-next)))
(defun my/tab-previous ()
  "Like `tab-previous', but say so when there is only one tab."
  (interactive)
  (if (<= (length (funcall tab-bar-tabs-function)) 1)
      (message "No other tab")
    (tab-previous)))

(defun my/kill-this-buffer ()
  "Kill the current buffer without prompting for which one."
  (interactive) (kill-buffer (current-buffer)))

(defun my/kill-buffer-and-window ()
  "Kill the current buffer and close its window unless it is the only one."
  (interactive)
  (if (one-window-p) (kill-buffer) (kill-buffer-and-window)))

;; --- Smart C-<backspace> (VS-Code style, no kill-ring pollution) ------------
(defun my/backward-delete-word (arg)
  "Delete backward to the previous word boundary without touching the kill ring.
Eats any trailing whitespace plus the word before it, never crosses the line
start, and at column 0 just removes the newline.  Respects `subword-mode'."
  (interactive "p")
  (cond
   ((use-region-p) (delete-region (region-beginning) (region-end)))
   ((bolp) (delete-char -1))
   (t
    (let ((start (point))
          (bol (line-beginning-position)))
      (save-excursion
        (skip-chars-backward " \t" bol)
        (when (and (> (point) bol)            ; still on this line: eat a word
                   (= (point) start))          ; (no whitespace was skipped)
          (if (and (bound-and-true-p subword-mode) (fboundp 'subword-backward))
              (subword-backward arg)
            (backward-word arg)))
        (when (< (point) bol) (goto-char bol))
        (delete-region (point) start))))))

;; --- Indent region left/right, keeping the selection active -----------------
(defconst my/shift-width 4 "Columns to shift with `my/shift-region-*'.")
(defun my/shift-region-left ()
  "Shift the region (or line) left, keeping it highlighted."
  (interactive)
  (let ((deactivate-mark nil)
        (beg (if (use-region-p) (region-beginning) (line-beginning-position)))
        (end (if (use-region-p) (region-end) (line-end-position))))
    (indent-rigidly beg end (- my/shift-width))))
(defun my/shift-region-right ()
  "Shift the region (or line) right, keeping it highlighted."
  (interactive)
  (let ((deactivate-mark nil)
        (beg (if (use-region-p) (region-beginning) (line-beginning-position)))
        (end (if (use-region-p) (region-end) (line-end-position))))
    (indent-rigidly beg end my/shift-width)))
;; After `C-c <' or `C-c >', just tap < / > to keep shifting (repeat-mode).
(defvar-keymap my/shift-repeat-map
  :repeat t
  "<" #'my/shift-region-left
  ">" #'my/shift-region-right)

;; --- Project TODO/FIXME search ------------
(defconst my/todo-regexp "\\(TODO\\|FIXME\\|HACK\\|BUG\\|XXX\\|NOTE\\):"
  "Annotation tags searched by `my/project-todos'.")
(defun my/project-todos ()
  "Search the current project for annotation tags like TODO:, FIXME:."
  (interactive)
  (if (project-current)
      (project-find-regexp my/todo-regexp)
    (message "Not in a project.")))

;; --- Insert date / time -----------------------------------------------------
(defun my/insert-date () (interactive) (insert (format-time-string "%Y-%m-%d")))
(defun my/insert-time () (interactive) (insert (format-time-string "%H:%M")))

;; --- Doctor: what external tools are installed (fresh-machine checklist) -----
(defun my/doctor ()
  "Report which external programs/grammars/fonts this config relies on."
  (interactive)
  (let ((rows
         (list
          (cons "git"                         (executable-find "git"))
          (cons "ripgrep (rg)"                (executable-find "rg"))
          (cons "clangd  (C/C++)"             (executable-find "clangd"))
          (cons "pyright (Python)"            (or (executable-find "pyright-langserver")
                                                  (executable-find "pyright")))
          (cons "rust-analyzer (Rust)"        (executable-find "rust-analyzer"))
          (cons "gopls   (Go)"                (executable-find "gopls"))
          (cons "typescript-language-server"  (executable-find "typescript-language-server"))
          (cons "jdtls   (Java)"              (executable-find "jdtls"))
          (cons "pdflatex (LaTeX)"            (executable-find "pdflatex"))
          (cons "dvisvgm (org math preview)"  (executable-find "dvisvgm"))
          (cons "pandoc  (markdown)"          (executable-find "pandoc"))
          (cons "node    (JS/Expo)"           (executable-find "node"))
          (cons (format "font: %s" my/font-family)
                (and (display-graphic-p) (find-font (font-spec :family my/font-family))))
          (cons "tree-sitter python grammar"  (and (fboundp 'treesit-ready-p)
                                                   (treesit-ready-p 'python t))))))
    (with-current-buffer (get-buffer-create "*my-doctor*")
      (erase-buffer)
      (insert "Config dependencies  ([--] = missing, install to enable):\n\n")
      (dolist (r rows)
        (insert (format "  %s  %s\n" (if (cdr r) "[OK]" "[--]") (car r))))
      (special-mode)
      (display-buffer (current-buffer)))))

;; --- On-save: make #! scripts executable ------------------------------------
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

;; --- Keys (free chords + C-c leader, nothing core is shadowed) --------------
(global-set-key (kbd "<C-return>")   #'my/open-line-below)
(global-set-key (kbd "<C-S-return>") #'my/open-line-above)
(global-set-key (kbd "<M-up>")       #'my/move-line-up)
(global-set-key (kbd "<M-down>")     #'my/move-line-down)
(global-set-key (kbd "C-<f5>")       #'my/reload-init)
(global-set-key (kbd "<f6>")         #'my/new-temp-file)
(global-set-key (kbd "<f8>")         #'next-error)
(global-set-key (kbd "<C-f9>")       #'my/smart-compile)
(global-set-key (kbd "<C-S-f9>")     #'recompile)
(global-set-key (kbd "C-S-p")        #'execute-extended-command)
(global-set-key (kbd "C-S-f")        #'project-find-regexp)
(global-set-key (kbd "C-c s d")      #'project-find-file)
(global-set-key (kbd "C-<backspace>") #'my/backward-delete-word)
(global-set-key (kbd "C-x b")        #'my/switch-to-buffer)
(global-set-key (kbd "C-x k")        #'my/kill-this-buffer)
(global-set-key (kbd "C-x K")        #'my/kill-buffer-and-window)
(global-set-key (kbd "C-c <")        #'my/shift-region-left)
(global-set-key (kbd "C-c >")        #'my/shift-region-right)
(global-set-key (kbd "C-c r")        #'recentf-open)
(global-set-key (kbd "C-c T")        #'my/project-todos)
(global-set-key (kbd "C-c o")        #'my/open-init)
(global-set-key (kbd "C-c R")        #'my/rename-file-and-buffer)
(global-set-key (kbd "C-c D")        #'my/delete-file)
(global-set-key (kbd "C-c y")        #'my/copy-path)
(global-set-key (kbd "C-c J")        #'my/join-line)
(global-set-key (kbd "C-c d")        #'duplicate-dwim)
(global-set-key (kbd "M-Q")          #'my/unfill-paragraph)
(global-set-key (kbd "C-c t t")      #'my/toggle-transparency)
(global-set-key (kbd "C-c t T")      #'my/toggle-theme)
(global-set-key (kbd "C-c i d")      #'my/insert-date)
(global-set-key (kbd "C-c i t")      #'my/insert-time)

(provide 'my-commands)
;;; my-commands.el ends here
