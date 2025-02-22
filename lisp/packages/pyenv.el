;;; packages/pyenv.el --- Pyenv Integration -*- lexical-binding: t; -*-

(use-package pyvenv
  :ensure t
  :defer t)

(defun pyvenv-autoload ()
  (require 'pyvenv)
  (require 'projectile)
  (interactive)
  "auto activate venv directory if exists"
  (f-traverse-upwards (lambda (path)
	(let ((venv-path (f-expand "env" path)))
	  (when (f-exists? venv-path)
	    (pyvenv-activate venv-path))))))
(add-hook 'python-mode 'pyvenv-autoload)


(provide 'packages/pyenv)
;; packages/pyenv.el ends here
