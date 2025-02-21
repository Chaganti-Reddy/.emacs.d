;;; packages/consult.el --- Consult Package -*- lexical-binding: t; -*-

(use-package consult
  :ensure t
  :bind (;; A recursive grep
	 ("M-s M-g" . consult-grep)
	 ;; Search for files names recursively
	 ("M-s M-f" . consult-fd)
	 ;; Search through the outline (headings) of the file
	 ("M-s M-o" . consult-outline)
	 ;; Search the current buffer
	 ("M-s M-l" . consult-line)
	 ;; Switch to another buffer, or bookmarked file, or recently
	 ;; opened file.
	 ("M-s M-b" . consult-buffer)))

;;; CONSULT DIR

(use-package consult-dir
 :ensure t
 :defer t
 :bind (("C-x C-d" . consult-dir)
 :map vertico-map
 ("C-x C-d" . consult-dir)
 ("C-x C-j" . consult-dir-jump-file)))

;; The `wgrep' packages lets us edit the results of a grep search
;; while inside a `grep-mode' buffer.  All we need is to toggle the
;; editable mode, make the changes, and then type C-c C-c to confirm
;; or C-c C-k to abort.
;; Further reading: https://protesilaos.com/emacs/dotemacs#h:9a3581df-ab18-4266-815e-2edd7f7e4852

(use-package wgrep
  :ensure t
  :bind ( :map grep-mode-map
	  ("e" . wgrep-change-to-wgrep-mode)
	  ("C-x C-q" . wgrep-change-to-wgrep-mode)
	  ("C-c C-c" . wgrep-finish-edit)))


;; Display a counter showing the number of the current and the other
;; matches.  Place it before the prompt, though it can be after it.
(setq isearch-lazy-count t)
(setq lazy-count-prefix-format "(%s/%s) ")
(setq lazy-count-suffix-format nil)

;; Make regular Isearch interpret the empty space as a regular
;; expression that matches any character between the words you give
;; it.
(setq search-whitespace-regexp ".*?")

(use-package consult-projectile
  :ensure (consult-projectile :type git :host gitlab :repo "OlMon/consult-projectile" :branch "master"))


(provide 'packages/consult)
;; packages/consult.el ends here
