;;; -*- lexical-binding: t; -*-
(org-babel-load-file
 (expand-file-name
  "config.org"
  user-emacs-directory))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("de8f2d8b64627535871495d6fe65b7d0070c4a1eb51550ce258cd240ff9394b0" "e7820b899036ae7e966dcaaec29fd6b87aef253748b7de09e74fdc54407a7a02" "0325a6b5eea7e5febae709dab35ec8648908af12cf2d2b569bedc8da0a3a81c1" "2e7dc2838b7941ab9cabaa3b6793286e5134f583c04bde2fba2f4e20f2617cf7" "b41d0a9413fb0034cea34eb8c9f89f6e243bdd76bccecf8292eb1fefa42eaf0a" "cd5f8f91cc2560c017cc9ec24a9ab637451e36afd22e00a03e08d7b1b87c29ca" "a3a71b922fb6cbf9283884ac8a9109935e04550bcc5d2a05414a58c52a8ffc47" "8a379e7ac3a57e64de672dd744d4730b3bdb88ae328e8106f95cd81cbd44e0b6" "2035a16494e06636134de6d572ec47c30e26c3447eafeb6d3a9e8aee73732396" "fbf73690320aa26f8daffdd1210ef234ed1b0c59f3d001f342b9c0bbf49f531c" "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" "6945dadc749ac5cbd47012cad836f92aea9ebec9f504d32fe89a956260773ca4" "944d52450c57b7cbba08f9b3d08095eb7a5541b0ecfb3a0a9ecd4a18f3c28948" "636b135e4b7c86ac41375da39ade929e2bd6439de8901f53f88fde7dd5ac3561" "1f669e8abe4dc2855268c9a607b5e350e2811b3c5afd09af5939ff0c01a89c5a" default))
 '(package-selected-packages '(auctex))
 '(safe-local-variable-values '((TeX-master . t))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:inherit outline-1 :height 1.3))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.25))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.2))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.15))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.1))))
 '(org-level-6 ((t (:inherit outline-5 :height 1.05))))
 '(org-level-7 ((t (:inherit outline-5 :height 1.0)))))

;; Ensure the Emacs server is loaded so that `server-name` is defined.
(require 'server)

;; * DAEMON
;;;################################################################

;; Hack: When starting a server, silently load all the "heavy" libraries and
;; goodies I use. There are more elegant approaches, such as incremental
;; deferring, but this is good enough.
(when (daemonp)
  (defvar pulse-flag t)
  (add-hook
   'after-init-hook
   (defun my/load-packages-eagerly ()
     (add-hook 'server-visit-hook
	       (lambda ()
		 (when (and (equal default-directory temporary-file-directory)
			    (equal major-mode 'text-mode)
			    (fboundp 'markdown-mode))
		   (markdown-mode))))
     (run-at-time 1 nil
		  (lambda ()
		    (when (fboundp 'pdf-tools-install)
		      (pdf-tools-install t))
		    (load-library "pulse")
		    (when (and (boundp 'server-name)
			       (string-suffix-p "server" server-name))
		      (let ((after-init-time (current-time)))
			(dolist (lib '("org" "ob" "ox" "ol" "org-roam" "org-modern" "toc-org"
				       "org-capture" "org-agenda" 
				       "org-tempo" "ob-mermaid" "ox-latex" "calfw" "calfw-org"
					"latex" "reftex" "cdlatex"
				       "consult" "elisp-mode"
				       "simple" "expand-region"
				       "yasnippet"
				       "magit" "gcmh" "evil" "evil-collection" "which-key" "dashboard" "projectile" "perspective" "corfu" "cape" "gptel"
					"ef-themes"
				       ;; "ewal" "ewal-doom-themes" "doom-themes" "modus-themes"
				       "dired" "ibuffer" "pdf-tools"))
			  (with-demoted-errors "Error: %S" (load-library lib)))
			(with-temp-buffer (org-mode))
			(let ((elapsed (float-time (time-subtract (current-time)
								    after-init-time))))
			  (message "[Pre-loaded packages in %.3fs]" elapsed)))))))))
