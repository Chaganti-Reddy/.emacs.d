;;; optimizations.el -*- Optimize Emacs Performance -*- lexical-binding: t; -*-

;;; Commentary:
;; Some known optimizations that can faster emacs startup and runtime as well.

;;; Code:

;;----------------------------------------------------------------------------
;; 🔹 1. Use gcmh for Smarter Garbage Collection
;;----------------------------------------------------------------------------
(use-package gcmh
  :ensure t
  :diminish
  :init
  (setq gcmh-high-cons-threshold (* 512 1024 1024)  ;; 512MB when active
        gcmh-low-cons-threshold  (* 64 1024 1024)    ;; 64MB when idle
        gcmh-idle-delay 10)                          ;; Reduce GC after 10s idle
  :config
  (gcmh-mode 1))

;;----------------------------------------------------------------------------
;; ⏳ 2. Display Startup Time & Garbage Collection Stats
;;----------------------------------------------------------------------------
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %.2f seconds with %d garbage collections."
                     (float-time (time-subtract after-init-time before-init-time))
                     gcs-done)))

;;----------------------------------------------------------------------------
;; 📌 3. Optimize Process Output Handling
;;----------------------------------------------------------------------------
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq read-process-output-max (* 1024 1024)))) ;; 1MB for better async performance

;;----------------------------------------------------------------------------
;; ⚡ 4. Native Compilation Optimizations
;;----------------------------------------------------------------------------
(setq native-comp-async-report-warnings-errors nil  ;; Silence warnings
      load-prefer-newer t)  ;; Prefer newer source files

;; Disable deferred native compilation during interactive use
(if (boundp 'comp-deferred-compilation)
    (setq comp-deferred-compilation nil)
  (setq native-comp-deferred-compilation nil))

;;----------------------------------------------------------------------------
;; 🔥 5. Auto Byte-Compile Config Files
;;----------------------------------------------------------------------------
(defun karna/auto-byte-recompile ()
  "Recompile `.el' files if they exist."
  (when (and buffer-file-name
             (eq major-mode 'emacs-lisp-mode)
             (not (string-match-p "init\\.el$" buffer-file-name))
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-recompile-file buffer-file-name)))

(defun karna/byte-recompile-init-file ()
  "Recompile the Emacs init file on exit."
  (when (and user-init-file (file-exists-p user-init-file))
    (byte-recompile-file user-init-file)))

(add-hook 'after-save-hook #'karna/auto-byte-recompile)
(add-hook 'kill-emacs-hook #'karna/byte-recompile-init-file)
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

(provide 'optimizations)
;; optimizations.el ends here
