;;; packages/ellama.el --- Ellama Package -*- lexical-binding: t; -*-

(use-package ellama
  :init
  (setopt ellama-keymap-prefix "C-c e")  ;; keymap for all ellama functions
  (setopt ellama-language "English")     ;; language ellama should translate to
  (require 'llm-ollama)
  (setopt ellama-provider
    (make-llm-ollama
     ;; this model should be pulled to use it
     ;; value should be the same as you print in terminal during pull
     :chat-model "llama3.1"
     :embedding-model "nomic-embed-text"
     :default-chat-non-standard-params '(("num_ctx" . 8192))))
  ;; Predefined llm providers for interactive switching.
  (setopt ellama-providers
	'(("zephyr" . (make-llm-ollama
	   :chat-model "zephyr"
	   :embedding-model "zephyr"))
	  ("deepseek-r1:8b" . (make-llm-ollama
	   :chat-model "deepseek-r1:8b"
	   :embedding-model "deepseek-r1:8b"))
	  ("llama3" . (make-llm-ollama
	   :chat-model "llama3"
	   :embedding-model "llama3"))
	  ("mistral" . (make-llm-ollama
	    :chat-model "mistral"
	    :embedding-model "mistral"))))
  (setopt ellama-coding-provider
	(make-llm-ollama
	 ;; :chat-model "qwen2.5-coder:3b"
	 :chat-model "deepseek-coder:6.7b"
	 :embedding-model "deepseek-coder:6.7b"
	 ;; :default-chat-non-standard-params '(("num_ctx" . 32768))
))
  (setopt ellama-naming-scheme 'ellama-generate-name-by-llm)
  ;; Translation llm provider
  (setopt ellama-translation-provider (make-llm-ollama
	       :chat-model "qwen2.5:3b"
	       :embedding-model "nomic-embed-text"))
  ;; customize display buffer behaviour
  ;; see ~(info "(elisp) Buffer Display Action Functions")~
  (setopt ellama-chat-display-action-function #'display-buffer-full-frame)
  (setopt ellama-instant-display-action-function #'display-buffer-at-bottom)
  :config
  (setq ellama-sessions-directory "~/.cache/emacs/ellama-sessions/"
	ellama-sessions-auto-save t))


(provide 'packages/ellama)
;; packages/ellama.el ends here
