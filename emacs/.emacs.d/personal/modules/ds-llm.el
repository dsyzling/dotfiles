;;; ds-llm.el --- Large language models and coding assistants

;;
;;
;; Large language models and coding assistants
;;

;; Author: Darren Syzling <dsyzling@gmail.com>
;; Keywords: 

;;;
;;; Code:
;; Use .authinfo for LLM passwords and keys
(require 'auth-source)

(defun get-llm-api-key (host)
  "Get the LLM API key for a specific HOST."
  (auth-source-pick-first-password :host host))

;;
;; https://github.com/karthink/gptel
;;
(straight-use-package 'gptel)

;; temp for now - Claude key
;; (gptel-make-anthropic "Claude"          ;Any name you want
;;   :stream t                             ;Streaming responses
;;   :key (get-llm-api-key "claude.anthropic.com"))

;; (setq
;;  gptel-model 'claude-3-sonnet-20240229 ;  "claude-3-opus-20240229" also available
;;  gptel-backend (gptel-make-anthropic "Claude"
;;                  :stream t :key claude-key))

;;
;; Gemini
;;

;; :key can be a function that returns the API key.
(gptel-make-gemini "Gemini" :key (get-llm-api-key "gemini.google.com") :stream t)

;; OPTIONAL configuration
(setq
 gptel-model 'gemini-exp-1206
 gptel-backend (gptel-make-gemini "Gemini"
                 :key (get-llm-api-key "gemini.google.com")
                 :stream t))

;;
;; Claude Sonnet
;; Claude web: https://claude.ai/new
;;
;; (gptel-make-anthropic "Claude"          ;Any name you want
;;   :stream t                             ;Streaming responses
;;   :key (get-llm-api-key "claude.anthropic.com"))

;; (gptel-make-anthropic "Claude-thinking" ;Any name you want
;;   :key "your-API-key"
;;   :stream t
;;   :models '(claude-3-7-sonnet-20250219)
;;   :header (lambda () (when-let* ((key (gptel--get-api-key)))
;;                   `(("x-api-key" . ,key)
;;                     ("anthropic-version" . "2023-06-01")
;;                     ("anthropic-beta" . "pdfs-2024-09-25")
;;                     ("anthropic-beta" . "output-128k-2025-02-19")
;;                     ("anthropic-beta" . "prompt-caching-2024-07-31"))))
;;   :request-params '(:thinking (:type "enabled" :budget_tokens 2048)
;;                     :max_tokens 4096))

;;
;; copilot chat 
;;
(use-package copilot-chat)

;;
;; chatgpt-shell
;; See the following post for ways of using this mode:
;;   https://lmno.lol/alvaro/chatgpt-shell-goes-multi-model
;; highlight function - chatgpt-shell-prompt-compose
;; enter a prompt and C-c C-c
;;
(use-package chatgpt-shell
  :ensure t
  :custom
  ((chatgpt-shell-google-key
    (lambda ()
      (get-llm-api-key "gemini.google.com")))
   
   )
  ;; key bindings - prompt compose - then C-c C-c to send prompt 
  :bind (("C-c C-p" . chatgpt-shell-prompt-compose))
  )

;; set default model
(setq chatgpt-shell-model-version "gemini-2.0-flash")


(provide 'ds-llm)
