;;; ORG TEMPO

(require 'org-tempo)

;;; $EMACSDIR/config.el --- Emacs Setup -*- lexical-binding: t; -*-

(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'elpaca)  ;; The Elpaca Package Manager
(require 'buffer-move) ;; Buffer Moving Custom Functions
(require 'hooks-dirs) ;; Hooks and Directories
(require 'core-config) ;; Core Configurations
(require 'editor) ;; Editor Settings
(require 'packages/evil) ;; Evil Mode
(require 'ui-config) ;; UI Settings for Emacs
(require 'keybindings) ;; General Custom Keybindings
(require 'git) ;; Git
(require 'calcc) ;; Calc
(require 'file-manager) ;; File Manager
(require 'packages/wakatime) ;; Wakatime
(require 'packages/dashboard) ;; Dashboard
(require 'packages/doom-modeline) ;; Doom Modeline
(require 'packages/projectile) ;; Projectile
(require 'packages/perspective) ;; Perspective
(require 'packages/minibuffer) ;; Minibuffer
(require 'packages/consult) ;; Consult
(require 'packages/corfu) ;; Corfu
(require 'packages/cape) ;; Cape
(require 'packages/orderless) ;; Orderless
(require 'org-config) ;; Org Config
(require 'org-agenda-config) ;; Org Agenda
(require 'org-roam-config) ;; Org Roam
(require 'packages/tabnine) ;; TabNine
(require 'packages/yasnippet) ;; Yasnippet
(require 'packages/gptel) ;; GPTel
(require 'treesitter) ;; Treesitter
(require 'dev) ;; Development Stuff
(require 'packages/conda) ;; Conda
(require 'packages/pyenv) ;; PyEnv
(require 'packages/latex) ;; Latex
(require 'packages/org-latex) ;; Org Latex
(require 'packages/markdown) ;; Markdown
(require 'eshell-prompt) ;; Custom ESHELL Prompt
(require 'shells) ;; Shells
(require 'packages/calendar) ;; Calendar
(require 'consult-yasnippet) ;; Consult Yasnippet

(provide 'config)
;;; config.el ends here
