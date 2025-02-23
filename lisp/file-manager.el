;;; file-manager.el --- File Manager Setup -*- lexical-binding: t; -*-

;; ----------------------------------------------------------------------------
;; TREEMACS
;; ----------------------------------------------------------------------------

(use-package treemacs
  :ensure t
  :defer t
  :bind
  (("C-c t" . treemacs))
  :config
  (setq treemacs-width 30))

(setq treemacs-persist-file (concat user-cache-directory "treemacs-persist"))

;; Mouse single-click expands nodes in Treemacs
(with-eval-after-load 'treemacs
  (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action))

;; ----------------------------------------------------------------------------
;; DIRED OPEN
;; ----------------------------------------------------------------------------

(use-package dired-open
  :ensure t
  :defer t
  :config
  ;; Define applications to open specific file types
  (setq dired-open-extensions '(("gif" . "sxiv")   ;; Open GIFs with sxiv
				("jpg" . "sxiv")   ;; Open JPEGs with sxiv
				("png" . "sxiv")   ;; Open PNGs with sxiv
				("mkv" . "mpv")    ;; Open MKVs with mpv
				("mp4" . "mpv")))) ;; Open MP4s with mpv

;; Auto Create Directory if it doesn't exist
(advice-add 'read-file-name :around
	    (lambda (orig-fun &rest args)
	      (let ((result (apply orig-fun args)))
		(when (and (stringp result)
			   (not (file-exists-p (file-name-directory result))))
		  (make-directory (file-name-directory result) t))
		result)))

;; ----------------------------------------------------------------------------
;; PEEP DIRED
;; ----------------------------------------------------------------------------

(use-package peep-dired
  :after dired
  :ensure t
  :hook (evil-normalize-keymaps . peep-dired-hook) ;; Ensure peep-dired works with Evil mode
  :config
  ;; Define keybindings for navigation in Dired mode
  (evil-define-key 'normal dired-mode-map (kbd "h") 'dired-up-directory)   ;; Go up one directory
  (evil-define-key 'normal dired-mode-map (kbd "l") 'dired-open-file)      ;; Open file (use `dired-find-file` if not using `dired-open`)

  ;; Keybindings for navigating files in peep-dired preview mode
  (evil-define-key 'normal peep-dired-mode-map (kbd "j") 'peep-dired-next-file)  ;; Next file preview
  (evil-define-key 'normal peep-dired-mode-map (kbd "k") 'peep-dired-prev-file)) ;; Previous file preview

;; ----------------------------------------------------------------------------
;; SUDO EDIT
;; ----------------------------------------------------------------------------

(use-package sudo-edit :ensure t :defer t)


(provide 'file-manager)
;; file-manager.el ends here
