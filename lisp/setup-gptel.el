;;; setup-gptel --- GPTEL config -*- lexical-binding: t; -*-

;;; Commentary:

;; GPTEL is an AI integration system into emacs built by karthink.

;;; Code:

(use-package gptel
  :ensure t
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
				     "deepseek-r1:8b"
             (llava:7b :description "Llava 1.6: Vision capable model"
                   :capabilities (media)
                   :mime-types ("image/jpeg" "image/png")))))

  ;; ----------------------------------------------------------------------------
  ;; Kagi Backend - Uncomment to Enable
  ;; ----------------------------------------------------------------------------
  ;; (gptel-make-kagi "Kagi"
  ;;   :key (lambda () (auth-source-pick-first-password :host "kagi.com" :login "apikey")))

(defun my/gptel-code-infill ()
    "Fill in code at point based on buffer context.  Note: Sends the whole buffer."
    (let ((lang (gptel--strip-mode-suffix major-mode)))
      `(,(format "You are a %s programmer and assistant in a code buffer in a text editor.

Follow my instructions and generate %s code to be inserted at the cursor.
For context, I will provide you with the code BEFORE and AFTER the cursor.


Generate %s code and only code without any explanations or markdown/org code fences.  NO markdown and NO org code.
You may include code comments.

Do not repeat any of the BEFORE or AFTER code." lang lang lang)
       nil
       "What is the code AFTER the cursor?"
       ,(format "AFTER\n```\n%s\n```\n"
               (buffer-substring-no-properties
                (if (use-region-p) (max (point) (region-end)) (point))
                (point-max)))
       "And what is the code BEFORE the cursor?"
       ,(format "BEFORE\n```%s\n%s\n```\n" lang
               (buffer-substring-no-properties
                (point-min)
                (if (use-region-p) (min (point) (region-beginning)) (point))))
       ,@(when (use-region-p) "What should I insert at the cursor?"))))

  (setq gptel-directives
        `((default . "To assist:  Be terse.  Do not offer unprompted advice or clarifications.  Speak in specific,
 topic relevant terminology.  Do NOT hedge or qualify.  Speak directly and be willing to make creative guesses.

Explain your reasoning.  if you don’t know, say you don’t know.  Be willing to reference less reputable sources for
 ideas.

Do NOT summarize your answers.

If you use LaTex notation, enclose math in \\( and \\), or \\[ and \\] delimiters.

 Never apologize.  Ask questions when unsure.")
          (code-infill . ,#'my/gptel-code-infill)
          (programmer . "You are a careful programmer.  Provide code and only code as output without any additional text, prompt or note.  Do NOT use markdown backticks (```) to format your response.")
          (cliwhiz . "You are a command line helper.  Generate command line commands that do what is requested, without any additional description or explanation.  Generate ONLY the command, without any markdown code fences.")
          (emacser . "You are an Emacs maven.  Reply only with the most appropriate built-in Emacs command for the task I specify.  Do NOT generate any additional description or explanation.")
          (explain . "Explain what this code does to a novice programmer.")
          (tutor . "You are a tutor and domain expert in the domain of my questions.  You will lead me to discover the answer myself by providing hints.  Your instructions are as follows:
- If the question or notation is not clear to you, ask for clarifying details.
- At first your hints should be general and vague.
- If I fail to make progress, provide more explicit hints.
- Never provide the answer itself unless I explicitly ask you to.  If my answer is wrong, again provide only hints to correct it.
- If you use LaTeX notation, enclose math in \\( and \\) or \\[ and \\] delimiters.")))
  (setq gptel--system-message (alist-get 'default gptel-directives)
        gptel-default-mode 'org-mode)
  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "*Prompt*: "
        (alist-get 'org-mode gptel-response-prefix-alist) "*Response*:\n"
        (alist-get 'markdown-mode gptel-prompt-prefix-alist) "#### ")
  (with-eval-after-load 'gptel-org
    (setq-default gptel-org-branching-context t))
  
  (defun my/gptel-eshell-send (&optional arg)
    (interactive "P")
    (if (use-region-p)
        (gptel-send arg)
      (push-mark)
      (or (eshell-previous-prompt 0)
          (eshell-previous-prompt 1))
      (activate-mark)
      (gptel-send arg)
      (exchange-point-and-mark)
      (deactivate-mark)))
  (defun my/gptel-eshell-keys ()
    (define-key eshell-mode-map (kbd "C-c <return>")
                #'my/gptel-eshell-send))
;; Testing
  (defun gptel-rewrite-commit-message ()
    (when (and (string-match-p "COMMIT_EDITMSG" (buffer-name))
               (derived-mode-p 'text-mode))
      "You are a git expert.  What you write will be passed to git commit -m \"[message]\".
Rewrite the following message."))

  (add-to-list 'popper-reference-buffers "\\*gptel-log\\*")
  (setf (alist-get "\\*gptel-log\\*" display-buffer-alist nil nil #'equal)
        `((display-buffer-reuse-window display-buffer-in-side-window)
          (side . right)
          (window-width . 72)
          (slot . 20)
          (body-function . ,(lambda (win)
                              (select-window win)
                              (my/easy-page)))))

  (cl-pushnew '(:propertize
               (:eval
                (when (local-variable-p 'gptel--system-message)
                  (concat
                   "["
                   (if-let ((n (car-safe (rassoc gptel--system-message gptel-directives))))
                       (symbol-name n)
                     (gptel--describe-directive gptel--system-message 12))
                   "]")))
               'face 'gptel-rewrite-highlight-face)
              mode-line-misc-info)
  (add-to-list
   'mode-line-misc-info
   '(:eval
     (unless gptel-mode
      (when (and (local-variable-p 'gptel-model)
             (not (eq gptel-model (default-value 'gptel-model))))
       (concat "[" (gptel--model-name gptel-model) "]")))))
  (with-eval-after-load 'gptel-rewrite
    (add-hook 'gptel-rewrite-directives-hook #'gptel-rewrite-commit-message))
)

(add-hook 'after-change-major-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c RET") 'gptel-send)))

(use-package gptel-rewrite
  :ensure nil
  :after gptel
  :bind (:map gptel-rewrite-actions-map
         ("C-c C-i" . gptel--rewrite-inline-diff))
  :config
  (defun gptel--rewrite-inline-diff (&optional ovs)
    (interactive (list (gptel--rewrite-overlay-at)))
    (unless (require 'inline-diff nil t)
      (user-error "Inline diffs require the inline-diff package."))
    (when-let* ((ov-buf (overlay-buffer (or (car-safe ovs) ovs)))
                ((buffer-live-p ov-buf)))
      (with-current-buffer ov-buf
        (cl-loop for ov in (ensure-list ovs)
                 for ov-beg = (overlay-start ov)
                 for ov-end = (overlay-end ov)
                 for response = (overlay-get ov 'gptel-rewrite)
                 do (delete-overlay ov)
                 (inline-diff-words
                  ov-beg ov-end response)))))
  (when (boundp 'gptel--rewrite-dispatch-actions)
    (add-to-list
     'gptel--rewrite-dispatch-actions '(?i "inline-diff")
     'append)))

(use-package gptel-quick
  :ensure (:host github :protocol ssh
           :repo "karthink/gptel-quick")
  :bind (:map embark-general-map
         ("?" . gptel-quick)))

(use-package project
    :ensure nil
    :after (popper visual-fill-column)
    :bind (:map project-prefix-map
           ("C" . gptel-project))
    :config
    (setf (alist-get ".*Chat.org$" display-buffer-alist nil nil #'equal)
          `((display-buffer-below-selected)
            (window-height . 0.5)
            (body-function . ,#'select-window)))
    (defun gptel-project ()
      "Open the ChatGPT file for the current project."
      (interactive)
      (let ((default-directory (or (project-root (project-current))
                                   default-directory)))
        (find-file "Chat.org")
        (require 'gptel)
        (unless gptel-mode
          (gptel-mode 1))
        (unless visual-fill-column-mode
          (visual-fill-column-mode 1))
        (unless (equal popper-popup-status 'user-popup)
          (popper-toggle-type)))))

(use-package gptel-ask 
  :ensure nil
  :load-path "~/.emacs.d/plugins/gptel-ask.el"
  :after gptel)


(provide 'setup-gptel)
;;; setup-gptel.el ends here
