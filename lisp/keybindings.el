;;; keybindings.el --- Keybindings for Emacs Config -*- lexical-binding: t; -*-

(use-package evil-nerd-commenter
  :ensure t
  :defer t)

(use-package general
  :ensure t
  :config
  (general-evil-setup)

  ;; set up 'SPC' as the global leader key
  (general-create-definer karna/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC" ;; set leader
    :global-prefix "M-SPC") ;; access leader in insert mode

  (karna/leader-keys
    "SPC" '(counsel-M-x :wk "Counsel M-x")
    "." '(find-file :wk "Find file")
    "," '(scratch-buffer :wk "Scratch buffer")
    "=" '(perspective-map :wk "Perspective") ;; Lists all the perspective keybindings
    "/" '(evilnc-comment-or-uncomment-lines :wk "Toggle Comments")
    "TAB TAB" '(comment-line :wk "Comment lines")
    "u" '(universal-argument :wk "Universal argument"))

   (karna/leader-keys
    "a" '(:ignore t :wk "A.I.")
    "a a" '(ellama-ask-about :wk "Ask ellama about region")
    "a c" '(:prefix "c" :wk "Code")
    "a c a" '(ellama-code-add :wk "Ellama code add")
    "a c c" '(ellama-code-complete :wk "Ellama code complete")
    "a c e" '(ellama-code-edit :wk "Ellama code edit")
    "a c i" '(ellama-code-improve :wk "Ellama code improve")
    "a c r" '(ellama-code-review :wk "Ellama code review")
    "a e" '(:ignore t :wk "Ellama enhance")
    "a e g" '(ellama-improve-grammar :wk "Ellama enhance wording")
    "a e w" '(ellama-improve-wording :wk "Ellama enhance grammar")
    "a i" '(ellama-chat :wk "Ask ellama")
    "a p" '(ellama-provider-select :wk "Ellama provider select")
    "a s" '(ellama-summarize :wk "Ellama summarize region")
    "a t" '(ellama-translate :wk "Ellama translate region"))

  (karna/leader-keys
    "b" '(:ignore t :wk "Bookmarks/Buffers")
    "b b" '(switch-to-buffer :wk "Switch to buffer")
    "b c" '(clone-indirect-buffer :wk "Create indirect buffer copy in a split")
    "b C" '(clone-indirect-buffer-other-window :wk "Clone indirect buffer in new window")
    "b d" '(bookmark-delete :wk "Delete bookmark")
    "b I" '(consult-buffer :wk "Preview buffers")
    "b i" '(persp-switch-to-buffer* :wk "Persp Ibuffer")
    "b k" '(kill-current-buffer :wk "Kill current buffer")
    "b K" '(kill-some-buffers :wk "Kill multiple buffers")
    "b l" '(list-bookmarks :wk "List bookmarks")
    "b m" '(bookmark-set :wk "Set bookmark")
    "b n" '(next-buffer :wk "Next buffer")
    "b p" '(previous-buffer :wk "Previous buffer")
    "b r" '(revert-buffer :wk "Reload buffer")
    "b R" '(rename-buffer :wk "Rename buffer")
    "b s" '(basic-save-buffer :wk "Save buffer")
    "b S" '(save-some-buffers :wk "Save multiple buffers")
    "b w" '(bookmark-save :wk "Save current bookmarks to bookmark file"))

  ;; (karna/leader-keys
  ;;   "c" '(:ignore t :wk "Centaur Tabs")
  ;;   "c n" '(centaur-tabs-forward-tab :wk "Next Tab")
  ;;   "c p" '(centaur-tabs-backward-tab :wk "Previous Tab")
  ;;   "c c" '(centaur-tabs-close-tab :wk "Close Tab")
  ;;   "c r" '(centaur-tabs-rename-tab :wk "Rename Tab")
  ;;   "c l" '(centaur-tabs-list-tabs :wk "List Tabs")
  ;;   "c m" '(centaur-tabs-move-current-tab-to-left :wk "Move Tab Left")
  ;;   "c <left>" '(karna/scroll-year-calendar-backward :wk "Scroll year calendar backward")
  ;;   "c <right>" '(karna/scroll-year-calendar-forward :wk "Scroll year calendar forward")
  ;;   "c y" '(karna/year-calendar :wk "Show year calendar")
  ;;   "c t" '(centaur-tabs-move-current-tab-to-right :wk "Move Tab Right"))


  (karna/leader-keys
    "c"  '(:ignore t :wk "Consult")
    ;; Buffer-related commands
    "c b" '(consult-buffer         :wk "Switch Buffer")

    ;; File-related commands
    "c d" '(:prefix "d"             :wk "Consult Directory")
    "c d f" '(consult-dir           :wk "Find Directory")
    "c d j" '(consult-dir-jump-file :wk "Jump to a directory")
    "c f" '(consult-fd           :wk "Find File")
    "c r" '(consult-recent-file    :wk "Recent Files")

    ;; Search commands
    "c l" '(consult-line           :wk "Search Lines")
    "c g" '(consult-grep           :wk "Grep Search")
    "c p" '(consult-ripgrep        :wk "Ripgrep Search")

    ;; Navigation commands
    "c i" '(consult-imenu          :wk "Imenu")
    "c o" '(consult-outline        :wk "Outline")

    ;; Other commands
    "c m" '(consult-man            :wk "Man Pages")
    "c k" '(consult-bookmark       :wk "Bookmarks")
    "c y" '(karna/year-calendar    :wk "Show year calendar"))

    (karna/leader-keys
    "d" '(:ignore t :wk "Dired")
    "d d" '(dired :wk "Open dired")
    "d f" '(wdired-finish-edit :wk "Writable dired finish edit")
    "d j" '(dired-jump :wk "Dired jump to current")
    "d n" '(treemacs-find-file :wk "Open file in Treemacs")
    ;; "d n" '(neotree-dir :wk "Open directory in neotree")
    "d p" '(peep-dired :wk "Peep-dired")
    "d w" '(wdired-change-to-wdired-mode :wk "Writable dired")

    ;; New prefix for favorite directories
    "d o" '(:ignore t :wk "Favorite Directories")
    "d o p" `((lambda () (interactive) (let ((default-directory "/mnt/Karna/Git/portfolio/")) (execute-extended-command nil "find-file"))) :wk "Open Portfolio")
    "d o P" `((lambda () (interactive) (let ((default-directory "/mnt/Karna/Git/Project-K/")) (execute-extended-command nil "find-file"))) :wk "Open Project-K")
    "d o h" `((lambda () (interactive) (let ((default-directory "~/")) (execute-extended-command nil "find-file"))) :wk "Open Home")
    )

  (karna/leader-keys
    "e" '(:ignore t :wk "Ediff/Eshell/Eval/EWW")
    "e b" '(eval-buffer :wk "Evaluate elisp in buffer")
    "e d" '(eval-defun :wk "Evaluate defun containing or after point")
    "e e" '(eval-expression :wk "Evaluate and elisp expression")
    "e f" '(ediff-files :wk "Run ediff on a pair of files")
    "e F" '(ediff-files3 :wk "Run ediff on three files")
    "e h" '(counsel-esh-history :which-key "Eshell history")
    "e l" '(eval-last-sexp :wk "Evaluate elisp expression before point")
    "e n" '(eshell-new :wk "Create new eshell buffer")
    "e r" '(eval-region :wk "Evaluate elisp in region")
    "e R" '(eww-reload :which-key "Reload current page in EWW")
    "e s" '(eshell :which-key "Eshell")
    "e w" '(eww :which-key "EWW emacs web wowser"))

  (karna/leader-keys
    "f" '(:ignore t :wk "Files")
    "f c" '((lambda () (interactive)
	      (find-file "~/.emacs.d/config.org"))
	    :wk "Open emacs config.org")
    "f e" '((lambda () (interactive)
	      (dired "~/.emacs.d/"))
	    :wk "Open user-emacs-directory in dired")
    "f d" '(find-grep-dired :wk "Search for string in files in DIR")
    "f m" '(ian/format-code :wk "Format Buffer")
    "f g" '(consult-ripgrep :wk "Search for string current file")
    "f i" '((lambda () (interactive)
	      (find-file "~/dotfiles/install.sh"))
	    :wk "Open dotfiles install.sh")
    "f j" '(consult-dir-jump-file :wk "Jump to a file below current directory")
    "f l" '(consult-locate :wk "Locate a file")
    "f r" '(consult-recent-file :wk "Find recent files")
    "f u" '(sudo-edit-find-file :wk "Sudo find file")
    "f U" '(sudo-edit :wk "Sudo edit file"))

  (karna/leader-keys
    "g" '(:ignore t :wk "Git")
    "g /" '(magit-displatch :wk "Magit dispatch")
    "g ." '(magit-file-displatch :wk "Magit file dispatch")
    "g b" '(magit-branch-checkout :wk "Switch branch")
    "g c" '(:ignore t :wk "Create")
    "g c b" '(magit-branch-and-checkout :wk "Create branch and checkout")
    "g c c" '(magit-commit-create :wk "Create commit")
    "g c f" '(magit-commit-fixup :wk "Create fixup commit")
    "g C" '(magit-clone :wk "Clone repo")
    "g f" '(:ignore t :wk "Find")
    "g f c" '(magit-show-commit :wk "Show commit")
    "g f f" '(magit-find-file :wk "Magit find file")
    "g f g" '(magit-find-git-config-file :wk "Find gitconfig file")
    "g F" '(magit-fetch :wk "Git fetch")
    "g g" '(magit-status :wk "Magit status")
    "g i" '(magit-init :wk "Initialize git repo")
    "g l" '(magit-log-buffer-file :wk "Magit buffer log")
    "g r" '(vc-revert :wk "Git revert file")
    "g s" '(magit-stage-file :wk "Git stage file")
    "g t" '(git-timemachine :wk "Git time machine")
    "g u" '(magit-stage-file :wk "Git unstage file"))

 (karna/leader-keys
    "h" '(:ignore t :wk "Help")
    "h a" '(counsel-apropos :wk "Apropos")
    "h b" '(describe-bindings :wk "Describe bindings")
    "h c" '(describe-char :wk "Describe character under cursor")
    "h d" '(:ignore t :wk "Emacs documentation")
    "h d a" '(about-emacs :wk "About Emacs")
    "h d d" '(view-emacs-debugging :wk "View Emacs debugging")
    "h d f" '(view-emacs-FAQ :wk "View Emacs FAQ")
    "h d m" '(info-emacs-manual :wk "The Emacs manual")
    "h d n" '(view-emacs-news :wk "View Emacs news")
    "h d o" '(describe-distribution :wk "How to obtain Emacs")
    "h d p" '(view-emacs-problems :wk "View Emacs problems")
    "h d t" '(view-emacs-todo :wk "View Emacs todo")
    "h d w" '(describe-no-warranty :wk "Describe no warranty")
    "h e" '(view-echo-area-messages :wk "View echo area messages")
    "h f" '(describe-function :wk "Describe function")
    "h F" '(describe-face :wk "Describe face")
    "h g" '(describe-gnu-project :wk "Describe GNU Project")
    "h i" '(info :wk "Info")
    "h I" '(describe-input-method :wk "Describe input method")
    "h k" '(describe-key :wk "Describe key")
    "h l" '(view-lossage :wk "Display recent keystrokes and the commands run")
    "h L" '(describe-language-environment :wk "Describe language environment")
    "h m" '(describe-mode :wk "Describe mode")
    "h r" '(:ignore t :wk "Reload")
    "h r r" '((lambda () (interactive)
		(load-file "~/.emacs.d/init.el")
		(ignore (elpaca-process-queues)))
		 ;; (karna/org-colors-doom-one) ;; Reapply colors after reloading
	    :wk "Reload emacs config"r)
    "h t" '(consult-theme :wk "Load theme")
    "h v" '(describe-variable :wk "Describe variable")
    "h w" '(where-is :wk "Prints keybinding for command if set")
    "h x" '(describe-command :wk "Display full documentation for command"))

  (karna/leader-keys
    "m" '(:ignore t :wk "Org")
    "m a" '(org-agenda :wk "Org agenda")
    "m e" '(org-export-dispatch :wk "Org export dispatch")
    "m i" '(org-toggle-item :wk "Org toggle item")
    "m t" '(org-todo :wk "Org todo")
    "m B" '(org-babel-tangle :wk "Org babel tangle")
    "m T" '(org-todo-list :wk "Org todo list"))

  (karna/leader-keys
    "i" '(:ignore t :wk "Custom")
    "i a" '(karna/insert-auto-tangle-tag :wk "Insert auto-tangle tag"))

  (karna/leader-keys
    "q" '(:ignore t :wk "Quit")
    "q q" '(evil-quit :wk " Quit Emacs"))

  (karna/leader-keys
    "m b" '(:ignore t :wk "Tables")
    "m b -" '(org-table-insert-hline :wk "Insert hline in table"))

  (karna/leader-keys
    "m d" '(:ignore t :wk "Date/deadline")
    "m d t" '(org-time-stamp :wk "Org time stamp"))

  (karna/leader-keys
    "o" '(:ignore t :wk "Open")
    "o d" '(dashboard-open :wk "Dashboard")
    "o e" '(elfeed :wk "Elfeed RSS")
    "o f" '(make-frame :wk "Open buffer in new frame")
    "o p" '(open-python-right-side :wk "Open Python REPL")
    "o o" '(open-octave-right-side :wk "Open Octave REPL")
    "o F" '(select-frame-by-name :wk "Select frame by name"))

  ;; projectile-command-map already has a ton of bindings
  ;; set for us, so no need to specify each individually.
  (karna/leader-keys
    "p" '(projectile-command-map :wk "Projectile")
    "P a" '(projectile-add-known-project :wk "Add root to known projects"))

  (karna/leader-keys
    "P" '(:ignore t :wk "Custom Previews")
    "P m" '(markdown-preview-mode :wk "Preview Markdown Document"))

  (karna/leader-keys
    "r" '(:ignore t :wk "Org-roam")
    "r c" '(completion-at-point :wk "Completion at point")
    "r f" '(org-roam-node-find :wk "Find node")
    "r g" '(org-roam-graph :wk "Show graph")
    "r t" '(org-roam-dailies-goto-today :wk "Show today note")
    "r i" '(org-roam-node-insert :wk "Insert node")
    "r n" '(org-roam-capture :wk "Capture to node")
    "r d" '(:prefix "d" :wk "Dailies")
    "r d c" '(:prefix "c" :wk "Capture")
    "r d c c" '(org-roam-dailies-capture-today :wk "Capture Today")
    "r d c y" '(org-roam-dailies-capture-yesterday :wk "Capture Yesterday")
    "r d c t" '(org-roam-dailies-capture-tomorrow :wk "Capture Tomorrow")
    "r d c d" '(org-roam-dailies-capture-date :wk "Capture Specific Date")
    "r d g" '(:prefix "g" :wk "Go to")
    "r d g g" '(org-roam-dailies-goto-today :wk "Go to Today")
    "r d g y" '(org-roam-dailies-goto-yesterday :wk "Go to Yesterday")
    "r d g t" '(org-roam-dailies-goto-tomorrow :wk "Go to Tomorrow")
    "r d g d" '(org-roam-dailies-goto-date :wk "Go to Specific Date")
    "r d g n" '(org-roam-dailies-goto-next-note :wk "Go to Next Date")
    "r d g d" '(org-roam-dailies-goto-previous-note :wk "Go to Previous Date")
    "r s" '(org-id-get-create :wk "Create Small node inside buffer")
    "r a" '(org-roam-alias-add :wk "Create alias for a roam")
    "r r" '(org-roam-buffer-toggle :wk "Toggle roam buffer"))


  (karna/leader-keys
    "s" '(:ignore t :wk "Search")
    "s d" '(dictionary-search :wk "Search dictionary")
    "s m" '(man :wk "Man pages")
    "s s" '(occur :wk "Search buffer")
    "s p" '(pdf-occur :wk "Pdf search lines matching STRING")
    "s t" '(tldr :wk "Lookup TLDR docs for a command")
    "s r" '(query-replace :wk "Search and replace")
    "s w" '(woman :wk "Similar to man but doesn't require man"))

  (karna/leader-keys
    "t" '(:ignore t :wk "Toggle")
    "t c" '(company-mode :wk "Toggle Company Mode")
    "t e" '(eshell-toggle :wk "Toggle eshell")
    "t f" '(flycheck-mode :wk "Toggle flycheck")
    "t l" '(display-line-numbers-mode :wk "Toggle line numbers")
    "t h" '(my/toggle-ef-theme :wk "Toggle ef-themes")
    "t n" '(treemacs :wk "Toggle Treemacs")
    ;;"t n" '(neotree-toggle :wk "Toggle neotree file viewer")
    "t o" '(org-mode :wk "Toggle org mode")
    "t r" '(rainbow-mode :wk "Toggle rainbow mode")
    "t t" '(tabnine-mode :wk "Toggle Tabnine mode")
    "t T" '(visual-line-mode :wk "Toggle truncated lines")
    "t v" '(vterm-toggle :wk "Toggle vterm"))

  (karna/leader-keys
    "w" '(:ignore t :wk "Windows/Words")
    ;; Window splits
    "w c" '(evil-window-delete :wk "Close window")
    "w n" '(evil-window-new :wk "New window")
    "w s" '(evil-window-split :wk "Horizontal split window")
    "w v" '(evil-window-vsplit :wk "Vertical split window")
    ;; Window motions
    "w h" '(evil-window-left :wk "Window left")
    "w j" '(evil-window-down :wk "Window down")
    "w k" '(evil-window-up :wk "Window up")
    "w l" '(evil-window-right :wk "Window right")
    "w w" '(evil-window-next :wk "Goto next window")
    "w <left>" '(evil-window-left :wk "Window left")
    "w <down>" '(evil-window-down :wk "Window down")
    "w <up>" '(evil-window-up :wk "Window up")
    "w <right>" '(evil-window-right :wk "Window right")
    ;; Move Windows
    "w H" '(buf-move-left :wk "Buffer move left")
    "w J" '(buf-move-down :wk "Buffer move down")
    "w K" '(buf-move-up :wk "Buffer move up")
    "w L" '(buf-move-right :wk "Buffer move right")
    ;; Words
    "w d" '(downcase-word :wk "Downcase word")
    "w u" '(upcase-word :wk "Upcase word")
    "w =" '(count-words :wk "Count words/lines for buffer"))

  (karna/leader-keys
    "y" '(:ignore t :wk "YASnippet")
    "y i" '(consult-yasnippet :wk "Insert Consult yasnippet")
    "y I" '(yas-insert-snippet :wk "Insert snippet")
    "y n" '(yas-new-snippet :wk "New snippet")
    "y v" '(yas-visit-snippet-file :wk "Edit snippet")
    "y r" '(yas-reload-all :wk "Reload snippets")
    "y t" '(yas-describe-tables :wk "Show available snippets"))

)

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") nil)
(global-set-key (kbd "<C-wheel-down>") nil)
(global-set-key (kbd "C-a") 'mark-whole-buffer) ;; Selects whole buffer to copy/delete

;; Binds `C-s` to compile and view the latex preview document.
(add-hook 'LaTeX-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-s") #'TeX-command-run-all)))

;; Keybinding to manually insert snippets
(global-set-key (kbd "C-c y") #'consult-yasnippet)

(provide 'keybindings)
;; keybindings.el ends here
