;;; setup-consult.el --- Consult Setup -*- lexical-binding: t; -*-

;;; Commentary:
;; Consult Configuration

;;; Code:

;; ----------------------
;; CONSULT (Enhanced Navigation)
;; ----------------------
(use-package consult
  :ensure t
  :demand t
  :bind (("M-s M-g" . consult-grep)      ;; Recursive grep
	 ("M-s M-f" . consult-fd)        ;; Search for file names
	 ("M-s M-o" . consult-outline)   ;; Search file outline (headings)
	 ("M-s M-l" . consult-line)      ;; Search current buffer
	 ("M-s M-b" . consult-buffer)))  ;; Switch buffers, bookmarks, recent files

;; CONSULT-DIR (Quick Directory Switching)
(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir)
	 :map vertico-map
	 ("C-x C-d" . consult-dir)
	 ("C-x C-j" . consult-dir-jump-file)))

;; ----------------------
;; WGREP (Editable Grep Results)
;; ----------------------
(use-package wgrep
  :ensure t
  :bind (:map grep-mode-map
	 ("e" . wgrep-change-to-wgrep-mode)
	 ("C-x C-q" . wgrep-change-to-wgrep-mode)
	 ("C-c C-c" . wgrep-finish-edit)))

;; ----------------------
;; SEARCH ENHANCEMENTS
;; ----------------------
(setq isearch-lazy-count t
      lazy-count-prefix-format "(%s/%s) "
      lazy-count-suffix-format nil
      search-whitespace-regexp ".*?") ;; Treat spaces as wildcards in searches

(use-package consult-project-extra
  :ensure nil
  :load-path "~/.emacs.d/plugins/consult-project-extra.el"
  :after consult)
(require 'consult-yasnippet)


(provide 'setup-consult)
;;; setup-consult.el ends here
