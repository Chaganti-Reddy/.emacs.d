;; packages/eglott.el --- Eglot Package -*- lexical-binding: t; -*-

(use-package eglot
  :ensure nil
  :custom
  (eglot-autoshutdown t)     ;; Shut down LSP when the buffer is closed
  (eglot-sync-connect 1)     ;; Asynchronous connection
  (eglot-events-buffer-size 0) ;; Reduce spam in the messages buffer
  (eglot-extend-to-xref t)   ;; Jump to references in other buffers
  (eldoc-echo-area-use-multiline-p nil) ;; Keep documentation in one line

  :bind (:map eglot-mode-map
              ("C-c a" . eglot-code-actions)
              ("C-c f" . eglot-format-buffer)   ;; Auto-format buffer like VSCode
              ("C-c r" . eglot-rename)
              ("C-c i" . consult-imenu)        ;; Jump to function/class
              ("C-c o" . eglot-code-action-organize-imports) ;; Auto-imports
              ("C-c b" . imenu-list-smart-toggle))

  :hook ((python-ts-mode . eglot-ensure)
         (c-ts-mode . eglot-ensure)
         (c++-ts-mode . eglot-ensure)
         (go-ts-mode . eglot-ensure)
         (yaml-mode . eglot-ensure)
         (eglot-managed-mode . my/eglot-setup))

  :config
  ;; Associate modes with LSP servers
  (dolist (server '((c-ts-mode       . ("clangd"))
                    (python-ts-mode  . ("pyright-langserver" "--stdio"))
                    (c++-ts-mode     . ("clangd"))
                    (go-ts-mode      . ("gopls"))
                    (yaml-mode       . ("yaml-language-server" "--stdio"))))
    (add-to-list 'eglot-server-programs server)))

;; --------------------------------------------------------------
;; 🛠️ Custom Eglot Setup 
;; --------------------------------------------------------------

(defun my/eglot-setup ()
  "Custom configuration for eglot-managed buffers to behave like VSCode."
  (electric-indent-local-mode t)
  (setq-local completion-category-defaults nil) ;; Use default completion settings

  (cond
   ((derived-mode-p 'python-ts-mode)
    (setq-local indent-tabs-mode nil
                python-indent-offset 4
                python-indent-guess-indent-offset nil)
    (local-set-key (kbd "<f6>") #'eglot-format-buffer))

   ((derived-mode-p 'c-ts-mode 'c++-ts-mode)
    (setq-local c-default-style "linux"
                c-basic-offset 4)
    (local-set-key (kbd "<f6>") #'eglot-format-buffer))

   ((derived-mode-p 'go-ts-mode)
    (setq-local tab-width 4
                indent-tabs-mode t)  ;; Go conventionally uses tabs
    (local-set-key (kbd "<f6>") #'eglot-format-buffer))

   ((derived-mode-p 'yaml-mode)
    nil)))

;; --------------------------------------------------------------
;; 🗂️ Auto-mode association for C++ using Tree-Sitter
;; --------------------------------------------------------------
(add-to-list 'auto-mode-alist
             '("\\(\\.ii\\|\\.\\(CC?\\|HH?\\)\\|\\.[ch]\\(pp\\|xx\\|\\+\\+\\)\\|\\.\\(cc\\|hh\\)\\)\\'"
               . c++-ts-mode))


(provide 'packages/eglott)
;; packages/eglott.el ends here
