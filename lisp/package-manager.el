;;; package-manager.el --- Emacs Package Manager Setup -*- lexical-binding: t; -*-

;;; Commentary:
;; Optimized Emacs package manager setup with minimal overhead.

;;; Code:

;; Change ELPA directory to ~/.local/share/git/elpa
(setq package-user-dir (expand-file-name "~/.local/share/git/elpa"))

;; Disable automatic package initialization at startup (we do it manually)
(setq package-enable-at-startup nil)

;; Initialize package.el
(require 'package)

;; Set up package archives (default + MELPA)
(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/")))

;; Ensure `package` is initialized only when needed
(unless (bound-and-true-p package--initialized)
  (package-initialize))

;; Bootstrap `use-package` efficiently
(eval-when-compile
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package))

(setq use-package-always-ensure t)  ;; Ensure all packages are installed automatically

(provide 'package-manager)
;;; package-manager.el ends here
