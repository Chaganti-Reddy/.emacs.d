;;; packages/conda.el --- Conda Integration -*- lexical-binding: t; -*-

(use-package conda
  :ensure t
  :defer t
  :init
  ;; Set base directories (this is lightweight and safe to run on startup)
  (setq conda-anaconda-home (expand-file-name "~/miniconda"))
  (setq conda-env-home-directory (expand-file-name "~/miniconda"))
  :config
  ;; Initialize shells and modeline updates only when Conda is loaded.
  (conda-env-initialize-interactive-shells)
  (conda-env-initialize-eshell)
  (conda-env-autoactivate-mode nil)   ;; Disable global autoactivation.
  (conda-mode-line-setup)              ;; Update modeline when Conda env changes.
  :hook ((python-mode . conda-env-autoactivate-mode)
	 (conda-postactivate-hook . restart-python-shell-with-conda)))


(defun restart-python-shell-with-conda ()
  "Restart Python shell using the currently activated Conda environment."
  (interactive)
  (when (bound-and-true-p conda-env-current-name)
    (let* ((conda-base-path (or (getenv "CONDA_PREFIX") "~/miniconda"))
	   (env-path (if (string= conda-env-current-name "base")
			 conda-base-path
		       (concat conda-base-path "/envs/" conda-env-current-name)))
	   (env-bin (concat env-path "/bin/python"))
	   (python-buffer (get-buffer "*Python*")))
      ;; Kill existing Python shell if running.
      (when (get-process "Python")
	(delete-process "Python"))
      (when python-buffer
	(kill-buffer python-buffer))
      (delete-other-windows)
      (if (file-executable-p env-bin)
	  (progn
	    (setq-local python-shell-interpreter env-bin)
	    (setq-local python-shell-interpreter-args "-i")
	    (setq-local pythonic-interpreter env-bin)  ;; If using pythonic.el.
	    (run-python (concat env-bin " -i") nil nil)
	    (message "Switched Python shell to Conda environment: %s"
		     conda-env-current-name))
	(message "Error: Could not find Python executable at %s" env-bin)))))


(defun open-python-right-side ()
  "Toggle a Python REPL in a vertical split on the right side."
  (interactive)
  (let ((python-buffer (get-buffer "*Python*"))
	(python-window (get-buffer-window "*Python*")))
    (if python-buffer
	(if python-window
	    (progn
	      (delete-window python-window)
	      (other-window 1))
	  (progn
	    (split-window-right)
	    (other-window 1)
	    (run-python)
	    (when (get-buffer "*Python*")
	      (switch-to-buffer "*Python*"))
	    (other-window 1)))
      (progn
	(split-window-right)
	(other-window 1)
	(run-python)
	(when (get-buffer "*Python*")
	  (switch-to-buffer "*Python*"))
	(other-window 1)))))

(setq display-buffer-alist
      '(("\\*compilation\\*"
	 (display-buffer-reuse-window display-buffer-at-bottom)
	 (window-height . 0.3))))


(provide 'packages/conda)
;; packages/conda.el ends here
