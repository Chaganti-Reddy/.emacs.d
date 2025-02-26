;;; early-init.el -*- lexical-binding: t; -*-

;; Disable package.el since you're managing packages manually
(setq package-enable-at-startup nil)

;; Set native compilation cache directory (Emacs 30+)
(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache (expand-file-name "~/.cache/emacs/eln-cache")))

;; Optimize startup performance
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      inhibit-compacting-font-caches t)

;; Restore reasonable garbage collection after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 50 1000 1000)
                  gc-cons-percentage 0.1)))

;; Reduce frame flickering during startup
(setq frame-inhibit-implied-resize t)
