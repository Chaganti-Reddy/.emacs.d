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
(defun my/move-line-up ()
  "Drag the current line up."
  (interactive) (transpose-lines 1) (forward-line -2) (indent-according-to-mode))
(defun my/move-line-down ()
  "Drag the current line down."
  (interactive) (forward-line 1) (transpose-lines 1) (forward-line -1) (indent-according-to-mode))
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

;; --- Insert date / time -----------------------------------------------------
(defun my/insert-date () (interactive) (insert (format-time-string "%Y-%m-%d")))
(defun my/insert-time () (interactive) (insert (format-time-string "%H:%M")))

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
(global-set-key (kbd "<f9>")         #'recompile)
(global-set-key (kbd "<C-f9>")       #'my/smart-compile)
(global-set-key (kbd "C-S-p")        #'execute-extended-command)
(global-set-key (kbd "C-S-f")        #'project-find-regexp)
(global-set-key (kbd "C-c o")        #'my/open-init)
(global-set-key (kbd "C-c r")        #'my/rename-file-and-buffer)
(global-set-key (kbd "C-c K")        #'my/delete-file)
(global-set-key (kbd "C-c y")        #'my/copy-path)
(global-set-key (kbd "C-c J")        #'my/join-line)
(global-set-key (kbd "C-c d")        #'duplicate-dwim)
(global-set-key (kbd "M-Q")          #'my/unfill-paragraph)
(global-set-key (kbd "C-c t t")      #'my/toggle-transparency)
(global-set-key (kbd "C-c i d")      #'my/insert-date)
(global-set-key (kbd "C-c i t")      #'my/insert-time)

(provide 'my-commands)
;;; my-commands.el ends here
