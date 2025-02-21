;;; wakatime.el --- Wakatime Extension -*- lexical-binding: t; -*-

(use-package wakatime-mode
  :ensure t
  :diminish
  :config
  (global-wakatime-mode)) ;; Requires wakatime-cli


(provide 'packages/wakatime)
;; packages/wakatime.el ends here
