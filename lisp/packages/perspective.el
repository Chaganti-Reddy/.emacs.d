;;; packages/perspective.el --- Perspective Package -*- lexical-binding: t; -*-

(use-package perspective
  :ensure t
  :custom
  ;; NOTE! I have also set 'SCP =' to open the perspective menu.
  ;; I'm only setting the additional binding because setting it
  ;; helps suppress an annoying warning message.
  (persp-mode-prefix-key (kbd "C-c M-p"))
  :config
  (persp-mode 1)
  ;; Sets a file to write to when we save states
)

;; This will group buffers by persp-name in ibuffer.
(add-hook 'ibuffer-hook
	  (lambda ()
	    (persp-ibuffer-set-filter-groups)
	    (unless (eq ibuffer-sorting-mode 'alphabetic)
	      (ibuffer-do-sort-by-alphabetic))))

;; Automatically save perspective states to file when Emacs exits.
;; (add-hook 'kill-emacs-hook #'persp-state-save)

(global-set-key (kbd "C-S-s") #'persp-state-save)


(provide 'packages/perspective)
;;; packages/perspective.el ends here
