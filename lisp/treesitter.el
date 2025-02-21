;;; treesitter.el --- Treesitter Setup -*- lexical-binding: t; -*-

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  (c-ts-mode-indent-offset 4)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package treesit
  :ensure nil
  :config
  (setq major-mode-remap-alist
	'((python-mode . python-ts-mode)
	  (javascript-mode . javascript-ts-mode)
	  (typescript-mode . typescript-ts-mode)
	  (json-mode . json-ts-mode)
	  (go-mode . go-ts-mode)
	  (yaml-mode . yaml-ts-mode)
	  (css-mode . css-ts-mode)
	  (html-mode . html-ts-mode)
	  (dockerfile-mode . dockerfile-ts-mode)))

  (setq treesit-language-source-alist
	'((templ      "https://github.com/vrischmann/tree-sitter-templ")
	  (bash       "https://github.com/tree-sitter/tree-sitter-bash")
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


(provide 'treesitter)
;; treesitter.el ends here
