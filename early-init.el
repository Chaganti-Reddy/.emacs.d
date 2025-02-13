;;; early-init.el -*- lexical-binding: t; -*-
(setq package-enable-at-startup nil)


(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache (expand-file-name "~/.cache/emacs/eln-cache")))
