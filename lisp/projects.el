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


(defun karna/clean-project-known-projects ()
  "Remove non-existing paths from the project list and persist changes."
  (interactive)
  (when (boundp 'project--list)
    (setq project--list
          (seq-filter (lambda (proj)
                        (file-directory-p (if (consp proj) (car proj) proj)))
                      project--list)))
  
  ;; Remove stale projects using project-forget-project
  (when (fboundp 'project-forget-project)
    (dolist (proj (copy-sequence (project-known-project-roots)))  ;; Avoid mutation issues
      (unless (file-directory-p proj)
        (project-forget-project proj))))  ;; Forget only if the directory doesn't exist

  ;; Persist the updated project list
  (when (and (boundp 'project-list-file) project-list-file)
    (with-temp-file project-list-file
      (insert (prin1-to-string project--list))))

  (message "Removed stale project paths and saved changes to %s" project-list-file))

(global-set-key (kbd "C-x p C") #'karna/clean-project-known-projects)

;; (add-hook 'emacs-startup-hook #'karna/clean-project-known-projects)
    

(defun karna/agenda-for-project-or-org-dir ()
  "Show agenda for the current project if in a project, otherwise use `org-directory`."
  (interactive)
  (let* ((project-root (when (project-current)
                         (project-root (project-current))))
         (org-agenda-files
          (if project-root
              (directory-files-recursively project-root "\\.org$")
            (directory-files-recursively org-directory "\\.org$"))))
    (org-agenda nil "t")))  ;; Show TODOs

(global-set-key (kbd "C-x p a") #'karna/agenda-for-project-or-org-dir)


(provide 'projects)
;;; projects.el ends here
