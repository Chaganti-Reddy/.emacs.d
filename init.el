;;; init.el --- Emacs Init file -*- lexical-binding: t; -*-

;;; Commentary:
;; This is my init.el file which loads at startup of emacs.

;;; Code:

;; Add `lisp/` to the load path for modular configs
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; (require 'elpaca) ; Uncomment if you want to use ELPACA

(require 'package-manager) ; Uncomment if you want to use default package manager


(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Load core configurations
(load (expand-file-name "config.el" user-emacs-directory))
