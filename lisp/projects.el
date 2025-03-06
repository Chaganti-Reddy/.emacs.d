;;; projects.el --- Project.el Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; My project.el configuration for project management

;;; Code:

(require 'project)

;; Default action when switching projects (open in Dired)
;; (setq project-switch-commands 'project-dired)

;; Ignore unnecessary directories
(setq project-vc-extra-root-markers '(".git" ".hg" ".svn")) ;; Ensure proper detection
(setq project-ignored-directories
      '("node_modules" "dist" "build" "vendor" ".venv" "tmp" "cache" "log" "bower_components"))

(setq project-list-file (expand-file-name ".cache/projects" user-emacs-directory))


;; The [[https://github.com/karthink/project-x][project-x]] package adds support
;; for "local" projects, as well as facilities for periodically saving
;; project-specific window configurations. It also adds commands to save and
;; restore project windows.


(use-package project-x
  :load-path "~/.emacs.d/plugins/project-x.el"
  :ensure nil
  ;; :ensure (:host github :protocol ssh
  ;;            :repo "karthink/project-x")
  :after project
  :config
  (setq project-x-local-identifier '(".project"))
  (setq project-x-window-list-file (dir-concat user-cache-directory "project-window-list")
        project-x-save-interval nil)
  (project-x-mode 1))

(add-hook 'project-find-functions 'project-x-try-local 90)
    

(provide 'projects)
;;; projects.el ends here
