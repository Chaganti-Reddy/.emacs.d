;;; my-llm.el --- LLM chat / rewrite via gptel (keyless Copilot) -*- lexical-binding: t; no-byte-compile: t; -*-
;; NOTE: `no-byte-compile: t' is deliberate. While this module is DORMANT (not
;; loaded -- see the commented `(my/load 'my-llm)' in init.el), byte-compiling
;; it would macro-expand its `(use-package ... :ensure t)' forms and INSTALL
;; gptel/gptel-agent/mcp.

;; gptel = the Emacs LLM client. This setup is KEYLESS: it authenticates through
;; your GitHub Copilot subscription (OAuth device login, `M-x gptel-gh-login'
;; once), which exposes Claude + GPT + Gemini -- no API key pasted anywhere.
;; Copilot's token caches under `~/.emacs.d/.cache/copilot-chat/' (it does NOT
;; reuse copilot.el's apps.json). Optional local Ollama backend is also keyless.
;;
;; First-time setup:  uncomment (my/load 'my-llm) in init.el §6 + restart
;;                    (elpaca installs gptel), then M-x gptel-gh-login (once)
;;
;; Leader: C-c l   (l l chat · l s send · l m menu · l r rewrite · l a add
;;                  context · l f add-file · l k abort · l R resume saved chat)

;;; --- Chat persistence: auto-name + auto-save + resume -----------------------
(defconst my/gptel-chat-directory (my/var "gptel-chats/" t)
  "Where unnamed gptel chat buffers are saved.")

(defun my/gptel-assign-filename ()
  "Give an unsaved gptel chat a timestamped file under `my/gptel-chat-directory'.
Writes a prop line so reopening the file turns `gptel-mode' back on."
  (when (and (bound-and-true-p gptel-mode)
             (not (buffer-file-name))
             (not current-prefix-arg))
    (save-excursion
      (add-file-local-variable-prop-line
       'eval '(and (fboundp 'gptel-mode) (gptel-mode 1))))
    (let ((name (file-name-concat
                 my/gptel-chat-directory
                 (concat (format-time-string "%Y%m%d-%H%M%S-")
                         (replace-regexp-in-string "[ /]" "-" (buffer-name))
                         (if (derived-mode-p 'org-mode) ".org" ".md")))))
      (set-visited-file-name name t t))))

(defun my/gptel-chat-setup ()
  "Per-chat-buffer setup: enable prompt caching + auto-name on first save."
  (setq-local gptel-cache t)
  (add-hook 'before-save-hook #'my/gptel-assign-filename nil t))

(defun my/gptel-resume (chat)
  "Reopen a saved gptel CHAT from `my/gptel-chat-directory'."
  (interactive (list (read-file-name "Resume chat: "
                                     my/gptel-chat-directory nil t)))
  (find-file chat))

(use-package gptel
  :ensure t
  :commands (gptel gptel-send gptel-menu gptel-rewrite gptel-add gptel-add-file)
  :bind (("C-c l l"   . gptel)            ; open / switch to a chat buffer
         ("C-c l s"   . gptel-send)       ; send (or continue chat at point)
         ("C-c l m"   . gptel-menu)       ; transient: model/backend/tools/system
         ("C-c l r"   . gptel-rewrite)    ; refactor/rewrite the region in place
         ("C-c l a"   . gptel-add)        ; add region/buffer to context
         ("C-c l f"   . gptel-add-file)   ; add a file (incl. images) to context
         ("C-c l k"   . gptel-abort)      ; stop the running request
         ("C-c l R"   . my/gptel-resume)) ; reopen a saved chat
  :hook (gptel-mode . my/gptel-chat-setup)
  :config
  ;; ---- Core behaviour ------------------------------------------------------
  (setq gptel-default-mode 'org-mode      ; folding, links, src blocks in chats
        gptel-expert-commands t           ; expose advanced options in the menu
        gptel-track-media t)              ; send image/file context as media

  ;; ---- Backend: keyless GitHub Copilot (PRIMARY) ---------------------------
  ;; Business/Enterprise plans: add :host "api.business.githubcopilot.com" etc.
  (setq gptel-backend (gptel-make-gh-copilot "Copilot" :stream t)
        ;; Pick any model via C-c l m. gpt-4o is a safe default Copilot serves;
        ;; switch to claude-sonnet-4 / gpt-5 / gemini-* from the menu anytime.
        gptel-model 'gpt-4o)

  ;; ---- Backend: local Ollama (OPTIONAL, also keyless) ----------------------
  ;; Uncomment + adjust if you run Ollama locally:
  ;; (gptel-make-ollama "Ollama" :host "localhost:11434" :stream t
  ;;   :models '(qwen2.5-coder:14b llama3.1:8b))

  ;; ---- Directives (named system prompts; choose from the menu) -------------
  (setf (alist-get 'programmer gptel-directives)
        "You are a careful senior programmer. Output code only, no prose and no
markdown fences unless asked. Comment only where the intent is non-obvious."
        (alist-get 'explain gptel-directives)
        "Explain the following clearly and concisely for an expert reader. Be
terse. Do not hedge, apologize, or restate the question."
        (alist-get 'tutor gptel-directives)
        "You are a tutor. Lead me to the answer with hints and questions; give
the full solution only when I explicitly ask.")

  ;; ---- Presets (type @name at the start of a prompt to apply) --------------
  (when (fboundp 'gptel-make-preset)
    (gptel-make-preset 'prog
      :description "Code-only, uses buffer context" :system 'programmer)
    (gptel-make-preset 'explain
      :description "Explain the selection" :system 'explain))

  ;; ---- Tools (function calling). Enable per-buffer via the menu. -----------
  (when (fboundp 'gptel-make-tool)
    (gptel-make-tool
     :name "read_buffer" :category "emacs"
     :description "Return the full text of a live Emacs buffer."
     :args '((:name "buffer" :type string :description "Buffer name"))
     :function (lambda (buffer)
                 (if (buffer-live-p (get-buffer buffer))
                     (with-current-buffer buffer
                       (buffer-substring-no-properties (point-min) (point-max)))
                   (format "No live buffer named %s" buffer))))
    (gptel-make-tool
     :name "create_file" :category "filesystem" :confirm t
     :description "Create a file with the given content (asks before writing)."
     :args '((:name "path"     :type string :description "Directory")
             (:name "filename" :type string :description "File name")
             (:name "content"  :type string :description "Contents"))
     :function (lambda (path filename content)
                 (let ((full (expand-file-name filename path)))
                   (with-temp-buffer (insert content) (write-file full))
                   (format "Created %s" full)))))

  ;; ---- Org: each outline path is its own conversation thread ---------------
  (with-eval-after-load 'gptel-org
    (setq-default gptel-org-branching-context t))
  ;; ---- Rewrite: choose diff/ediff/merge/accept interactively ---------------
  (with-eval-after-load 'gptel-rewrite
    (setq gptel-rewrite-default-action nil)))

;;; --- gptel-quick: instant one-shot explain of word/region/symbol -----------
;; `C-c l q' anywhere, or `?' on any Embark target (symbol/region/thing).
(use-package gptel-quick
  :ensure nil
  :after gptel
  :commands (gptel-quick)
  :bind ("C-c l q" . gptel-quick)
  :init
  (with-eval-after-load 'embark
    (define-key embark-general-map (kbd "?") #'gptel-quick))
  :config
  (setq gptel-quick-word-count 20
        gptel-quick-timeout 12))

;;; --- llm-tool-collection: ready-made tools (read/edit/search/shell/...) -----
;; To expose per-request from the gptel menu (C-c l m -> Tools).
(use-package llm-tool-collection
  :ensure nil
  :after gptel
  :config
  (mapc (apply-partially #'apply #'gptel-make-tool)
        (llm-tool-collection-get-all)))

;;; --- gptel-agent: agentic loop (Read/Write/Edit/Bash/WebSearch...) ----------
;; "Claude Code in Emacs": the @gptel-agent preset gives a full tool-using agent,
;; @gptel-plan a read-only planner.
(use-package gptel-agent
  :ensure t
  :after gptel
  :config
  (when (fboundp 'gptel-agent-update) (ignore-errors (gptel-agent-update))))

;;; --- MCP: expose Model Context Protocol servers as gptel tools --------------
;; Needs the external server binaries (npx-based, etc.). Configure servers below, then:  M-x mcp-hub  (start) + M-x gptel-mcp-connect.
(use-package mcp
  :ensure t
  :defer t
  :commands (mcp-hub mcp-hub-start-all-server)
  :init (with-eval-after-load 'gptel (require 'gptel-integrations nil t))
  :custom
  ;; An MCP server is a tiny external process exposing TOOLS/RESOURCES over a
  ;; standard protocol (read files, search the web, query a DB, drive a browser).
  ;; mcp.el launches them; `gptel-mcp-connect' turns their tools into gptel tools.
  ;; Most are run via `npx' (Node) or `uvx' (Python's uv). On Windows, if a bare
  ;; "npx"/"uvx" :command isn't found, use "npx.cmd"/"uvx.exe".
  ;; Uncomment the ones you want AFTER installing Node/uv + any token.
  (mcp-hub-servers
   '(;; Files under a directory (read/write/list). Node, no token.
     ;; ("filesystem" :command "npx"
     ;;  :args ("-y" "@modelcontextprotocol/server-filesystem"
     ;;         "C:/Users/vchagant/projects/"))
     ;;
     ;; Fetch a URL -> markdown for the model. Python (uv).
     ;; ("fetch" :command "uvx" :args ("mcp-server-fetch"))
     ;;
     ;; Persistent knowledge graph / memory across chats. Node.
     ;; ("memory" :command "npx" :args ("-y" "@modelcontextprotocol/server-memory"))
     ;;
     ;; Structured step-by-step reasoning scratchpad. Node.
     ;; ("think" :command "npx"
     ;;  :args ("-y" "@modelcontextprotocol/server-sequential-thinking"))
     ;;
     ;; GitHub (issues/PRs/repos). GitHub's official server is now the Go binary
     ;; `github-mcp-server' (the old npm @modelcontextprotocol/server-github is
     ;; archived). Needs a Personal Access Token.
     ;; ("github" :command "github-mcp-server" :args ("stdio")
     ;;  :env (:GITHUB_PERSONAL_ACCESS_TOKEN "ghp_xxx"))
     ;;
     ;; Query a SQLite DB. Python (uv).
     ;; ("sqlite" :command "uvx"
     ;;  :args ("mcp-server-sqlite" "--db-path" "C:/path/to/db.sqlite"))
     ;;
     ;; Web search (needs a Brave API key). Node.
     ;; ("brave" :command "npx" :args ("-y" "@modelcontextprotocol/server-brave-search")
     ;;  :env (:BRAVE_API_KEY "xxx"))
     ;;
     ;; Drive a real browser (navigate/click/scrape). Node.
     ;; ("playwright" :command "npx" :args ("-y" "@playwright/mcp@latest"))
     )))

(provide 'my-llm)
;;; my-llm.el ends here
