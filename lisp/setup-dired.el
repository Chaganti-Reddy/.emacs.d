;;; setup-dired.el --- File System Setup for emacs -*- lexical-binding :t -*-

;;; Commentary:

;; Using dired, dired-open and dired preview as an emacs file system 

;;; Code:

;; ----------------------------------------------------------------------------
;; DIRED CONFIGURATION (Minimal & Fast)
;; ----------------------------------------------------------------------------
(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x d" . dired)) ;; Open Dired quickly
  :custom
  (dired-listing-switches "-alh")  ;; Human-readable file sizes
  (dired-dwim-target t)           ;; Guess target directory
  (dired-auto-revert-buffer t)    ;; Auto refresh Dired buffer
  :config
  (require 'dired-x)) ;; Enable extra Dired functionality

;; ----------------------------------------------------------------------------
;; DIRED-OPEN (Open Files with External Apps)
;; ----------------------------------------------------------------------------
(use-package dired-open
  :after dired
  :defer t
  :custom
  (dired-open-extensions
   '(("gif"  . "sxiv")
     ("jpg"  . "sxiv")
     ("png"  . "sxiv")
     ("mkv"  . "mpv")
     ("mp4"  . "mpv")))) ;; Open files with designated apps

;; ----------------------------------------------------------------------------
;; DIRED-PREVIEW (File Previews in Dired)
;; ----------------------------------------------------------------------------
(use-package dired-preview
  :after dired
  :custom
  (dired-preview-delay 0.3)  ;; Adjust delay before previewing
  (dired-preview-max-size (expt 2 20)) ;; Limit preview size
  (dired-preview-ignored-extensions-regexp
   "\\.\\(gz\\|zst\\|tar\\|xz\\|rar\\|zip\\|iso\\|epub\\)") ;; Ignore certain formats
  :config
  (dired-preview-global-mode 1)) ;; Enable previews globally

;; ----------------------------------------------------------------------------
;; AUTO-CREATE MISSING DIRECTORIES
;; ----------------------------------------------------------------------------
(advice-add 'read-file-name :around
	    (lambda (orig-fun &rest args)
	      (let ((result (apply orig-fun args)))
		(when (and (stringp result)
			   (not (file-exists-p (file-name-directory result))))
		  (make-directory (file-name-directory result) t))
		result)))

;; ----------------------------------------------------------------------------
;; SUDO EDIT (Edit Files as Root)
;; ----------------------------------------------------------------------------
(use-package sudo-edit :defer t)


(provide 'setup-dired)
;;; setup-dired.el ends here
