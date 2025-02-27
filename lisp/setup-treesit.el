;;; setup-treesit.el --- Optimized Tree-sitter configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This file configures Tree-sitter for Emacs, keeping it minimal, fast, and utilizing built-in features.

;;; Code:

;; ----------------------------------------------------------------------------
;; TREE-SITTER CORE CONFIGURATION
;; ----------------------------------------------------------------------------
(use-package treesit
  :defer
  :ensure nil ;; Built-in, no need to install
  :custom
  (treesit-language-source-alist
   '((bash       "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake      "https://github.com/uyha/tree-sitter-cmake")
     (css        "https://github.com/tree-sitter/tree-sitter-css")
     (elisp      "https://github.com/Wilfred/tree-sitter-elisp")
     (go         "https://github.com/tree-sitter/tree-sitter-go")
     (gomod      "https://github.com/camdencheek/tree-sitter-go-mod")
     (html       "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (cpp        "https://github.com/tree-sitter/tree-sitter-cpp")
     (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
     (json       "https://github.com/tree-sitter/tree-sitter-json")
     (make       "https://github.com/alemuller/tree-sitter-make")
     (markdown   "https://github.com/ikatyang/tree-sitter-markdown")
     (python     "https://github.com/tree-sitter/tree-sitter-python")
     (toml       "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx        "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml       "https://github.com/ikatyang/tree-sitter-yaml")
     (haskell    "https://github.com/tree-sitter/tree-sitter-haskell")
     (typst      "https://github.com/uben0/tree-sitter-typst")
     (java       "https://github.com/tree-sitter/tree-sitter-java")
     (ruby       "https://github.com/tree-sitter/tree-sitter-ruby")
     (rust       "https://github.com/tree-sitter/tree-sitter-rust"))))

;; ----------------------------------------------------------------------------
;; TREE-SITTER AUTO CONFIGURATION
;; ----------------------------------------------------------------------------
(use-package treesit-auto
  :defer
  :custom
  (treesit-auto-install 'prompt) ;; Ask before installing missing grammars
  (c-ts-mode-indent-offset 4)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(provide 'setup-treesit)
;;; setup-treesit.el ends here
