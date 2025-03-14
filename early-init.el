;;; early-init.el -*- lexical-binding: t; -*-

;; early-init.el is run before init.el,
;; - before package initialization, and
;; - before UI initialization

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'.
(setq package-enable-at-startup nil
      package-quickstart nil
      load-prefer-newer t)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq frame-inhibit-implied-resize t)

;; * Native Compilation 

(when (boundp 'native-comp-eln-load-path)
  (add-to-list 'native-comp-eln-load-path
               (concat "~/.cache/emacs/" "eln-cache/"))
  (setq native-comp-async-report-warnings-errors 'silent
        native-comp-deferred-compilation t))



;;;################################################################
