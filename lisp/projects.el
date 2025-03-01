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

(provide 'projects)
;;; projects.el ends here
