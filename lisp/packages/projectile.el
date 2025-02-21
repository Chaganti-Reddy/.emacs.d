;;; packages/projectile.el --- Projectile Package -*- lexical-binding: t; -*-

(use-package projectile
  :ensure t
  :init
  ;; Set the search path for Projectile if the directory exists.
  (when (file-directory-p "/mnt/Karna/Git/Project-K")
    (setq projectile-project-search-path '("/mnt/Karna/Git/Project-K/")))
  ;; Define the action to take when switching projects.
  (setq projectile-switch-project-action #'projectile-dired)
  ;; :custom
  ;; Use Ivy for completion within Projectile.
  ;; (projectile-completion-system 'ivy)
  :config
  ;; Now that Projectile is loaded, append the extra directories to ignore.
  (setq projectile-globally-ignored-directories
	(append projectile-globally-ignored-directories
		'("node_modules" "dist" "build" "vendor" ".venv" "tmp" "cache" "log" "bower_components")))
  (projectile-mode 1))


(provide 'packages/projectile)
;;; packages/projectile.el ends here
