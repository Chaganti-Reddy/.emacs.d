;;; setup-gptel --- GPTEL config -*- lexical-binding: t; -*-

;;; Commentary:

;; GPTEL is an AI integration system into emacs built by karthink.

;;; Code:

(use-package gptel
  :ensure t
  :defer t
  :custom
  (gptel-default-mode 'org-mode)   ;; Use Org mode for responses
  (gptel-expert-commands t)        ;; Enable advanced GPTel commands
  :config
  ;; ----------------------------------------------------------------------------
  ;; Ollama Backend - Local AI Model Serving
  ;; ----------------------------------------------------------------------------
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
				     "qwen2.5:3b")))

  ;; ----------------------------------------------------------------------------
  ;; Kagi Backend - Uncomment to Enable
  ;; ----------------------------------------------------------------------------
  ;; (gptel-make-kagi "Kagi"
  ;;   :key (lambda () (auth-source-pick-first-password :host "kagi.com" :login "apikey")))
)

(add-hook 'after-change-major-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c RET") 'gptel-send)))


(provide 'setup-gptel)
;;; setup-gptel.el ends here
