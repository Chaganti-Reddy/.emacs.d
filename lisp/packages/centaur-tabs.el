;;; packages/centaur-tabs.el --- Centaur Tabs Package -*- lexical-binding: t; -*-

(use-package centaur-tabs
  :init
  (setq centaur-tabs-enable-key-bindings t)

:config
;; Appearance settings
(setq centaur-tabs-style "bar"
      centaur-tabs-height 25
      centaur-tabs-set-icons t
      centaur-tabs-show-new-tab-button t
      centaur-tabs-set-modified-marker t
      centaur-tabs-modified-marker "•"
      centaur-tabs-show-navigation-buttons t
      centaur-tabs-set-bar 'over
      centaur-tabs-show-count nil
      centaur-tabs-label-fixed-length 15
      centaur-tabs-gray-out-icons 'buffer
      x-underline-at-descent-line t
      centaur-tabs-left-edge-margin nil)

;; Hide specific buffers
(setq centaur-tab-buffer-local-list '(("\*scratch\*" :hide t)
				      ("\*Warnings\*" :hide t)
				      ("\*straight-process\*" :hide t)
				      ("\*Messages\*" :hide t)
				      ("Tasks.org" :hide t)))

;; Font and style adjustments
(centaur-tabs-change-fonts (face-attribute 'default :font) 110)
(centaur-tabs-headline-match)

;; Buffer naming behavior
(setq uniquify-separator "/"
      uniquify-buffer-name-style 'forward)

;; Custom buffer grouping
(defun my/centaur-tabs-buffer-groups ()
  "Define custom buffer groups for Centaur Tabs."
  (list
   (cond
    ((or (string-equal "*" (substring (buffer-name) 0 1))
	 (memq major-mode '(magit-process-mode
			    magit-status-mode
			    magit-diff-mode
			    magit-log-mode
			    magit-file-mode
			    magit-blob-mode
			    magit-blame-mode)))
     "Emacs")
    ((derived-mode-p 'prog-mode)
     "Editing")
    ((derived-mode-p 'dired-mode)
     "Dired")
    ((memq major-mode '(helpful-mode
			help-mode))
     "Help")
    ((memq major-mode '(org-mode
			org-agenda-clockreport-mode
			org-src-mode
			org-agenda-mode
			org-beamer-mode
			org-indent-mode
			org-bullets-mode
			org-cdlatex-mode
			org-agenda-log-mode
			diary-mode))
     "OrgMode")
    ;; ((member (buffer-name) '("*scratch*" "*Messages*" "*dashboard*" "*eww*")) "All")
    ((string-equal "newsrc-dribble" (buffer-name)) "Others")
    ((derived-mode-p 'gnus-mode) "All")
    ((eq major-mode 'message-mode) "All")
    ((string-match "org.*sidebar" (buffer-name)) "Others")
    ((string-match "<tree>" (buffer-name)) "Others")
    ((string-match "^TAGS.*" (buffer-name)) "Others")
    ((eq major-mode 'dired-mode) "Dired")
    (t "All"))))
(setq centaur-tabs-buffer-groups-function #'my/centaur-tabs-buffer-groups)

;; Enable centaur-tabs-mode
(centaur-tabs-mode t)

;; Keybindings
:bind
("C-<prior>" . centaur-tabs-backward)
("C-<next>" . centaur-tabs-forward)
("C-S-<prior>" . centaur-tabs-move-current-tab-to-left)
("C-S-<next>" . centaur-tabs-move-current-tab-to-right)
(:map evil-normal-state-map
      ("g t" . centaur-tabs-forward)
      ("g T" . centaur-tabs-backward))

;; Hooks
:hook
(dashboard-mode . centaur-tabs-local-mode)
(term-mode . centaur-tabs-local-mode)
(calendar-mode . centaur-tabs-local-mode)
(org-agenda-mode . centaur-tabs-local-mode))

;; Custom tab switching function
(defun my/switch-tabs (&optional direction cycle-group)
  "Change tabs in the given direction (left or right). Cycle within the same group unless cycle-group is non-nil."
  (interactive)
  (let* ((keys (mapcar #'event-basic-type (this-command-keys-vector)))
	 (direction (or direction (if (or (member 'left keys) (member 'home keys)) 'left 'right)))
	 (centaur-tabs-cycle-scope (if cycle-group 'groups (if (> (length keys) 1) 'groups 'tabs))))
    (if (eq window-system 'mac)
	(mac-start-animation (selected-window) :type 'swipe :direction direction))
    (centaur-tabs-cycle (eq direction 'left))))

;; Keybindings for tab switching
(keymap-global-set "C-M-<right>" #'my/switch-tabs)
(keymap-global-set "C-M-<left>" #'my/switch-tabs)


(provide 'packages/centaur-tabs)
;; packages/centaur-tabs.el ends here
