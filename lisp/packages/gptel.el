;;; packages/gptel.el --- GPTel Package -*- lexical-binding: t; -*-

(use-package gptel
  :ensure t
  :defer t
  :config

  ;;; KAGI Backend
  ;(gptel-make-kagi "Kagi"
  ;  :key (lambda () (auth-source-pick-first-password :host "kagi.com" :login "apikey")))

  (setq gptel-default-mode 'org-mode)
  (setq gptel-expert-commands t)

  ;; Define and Set Ollama as Default Backend
  (setq gptel-backend
	(gptel-make-ollama "Ollama"
			   :host "localhost:11434"
			   :stream t
			   :models '("llama3:latest"
				     "deepseek-coder:6.7b"
				     "mistral"
				     "zephyr"
				     "llama3:8b-instruct-q6_K"
				     "deepseek-r1:8b"
				     "qwen2.5:3b"))))
  ;; Set up keybinding for sending messages
(define-key global-map (kbd "C-c RET") 'gptel-send)


(provide 'packages/gptel)
;; packages/gptel.el ends here
