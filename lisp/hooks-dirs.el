;;; hooks-dirs.el --- Hooks and Cache Dirs Settings -*- lexical-binding: t; -*-

(add-hook 'after-init-hook #'global-auto-revert-mode)  ;; Automatically refresh buffers
(add-hook 'after-init-hook #'recentf-mode)             ;; Track recently opened files
(add-hook 'after-init-hook #'savehist-mode)            ;; Preserve minibuffer history
(add-hook 'after-init-hook #'save-place-mode)          ;; Remember cursor positions

;;----------------------------------------------------------------------------
;; Custom Directories for Cache Files
;;----------------------------------------------------------------------------

(defun dir-concat (dir file)
  (concat (file-name-as-directory dir) file))

;; Define the cache directory for Emacs files.
(defvar user-cache-directory "~/.cache/emacs/"
  "Directory for files created by Emacs.")

;; Set the file to save cursor positions.
(setq save-place-file (dir-concat user-cache-directory "places"))
;; Set the file for transient history.
(setq transient-history-file (dir-concat user-cache-directory "transient-history.el"))
;; Set the file for bookmarks.
(setq bookmark-default-file (dir-concat user-emacs-directory ".cache/bookmarks"))
;; Set the prefix for auto-save list files.
(setq auto-save-list-file-prefix (dir-concat user-cache-directory "auto-save-list/.saves-"))
;; Set the file for save history.
(setq savehist-file (dir-concat user-cache-directory "savehist"))
;; Set the file for TRAMP persistency.
(setq tramp-persistency-file-name (dir-concat user-cache-directory "tramp"))
;; Set the file for the LSP session.
(setq lsp-session-file (dir-concat user-cache-directory "lsp-session"))
;; Set the file for DAP breakpoints.
(setq dap-breakpoints-file (dir-concat user-cache-directory "dap-breakpoints"))
;; Set the file for Projectile's known projects.
(setq projectile-known-projects-file (dir-concat user-emacs-directory ".cache/projectile-bookmarks.eld"))
;; Set the directory for eshell.
(setq eshell-directory-name (dir-concat user-cache-directory "eshell"))
;; Set the file for eshell history.
(setq eshell-history-file-name (dir-concat user-cache-directory "eshell-history"))
;; Set the file for eshell's last directory ring.
(setq eshell-last-dir-ring-file-name (dir-concat user-cache-directory "eshell-last-dir-ring"))
;; Set the directory for undo session files.
(setq undo-fu-session-directory (dir-concat user-cache-directory "undo-fu-session/"))
;; Set the file for perspective sessions.
(setq persp-state-default-file (dir-concat user-emacs-directory ".cache/sessions"))
;; Set the file for org-id locations.
(setq org-id-locations-file (concat user-emacs-directory ".cache/.org-id-locations"))
;; Set the file for org-roam database.
(setq org-roam-db-location "/mnt/Karna/Git/Project-K/Org/Roam/org-roam.db")
;; Set the preview tailor storage file.
(setq preview-tailor-storage-file (dir-concat user-cache-directory ".preview-tailor"))

(provide 'hooks-dirs)
;;; hooks-dirs.el ends here
