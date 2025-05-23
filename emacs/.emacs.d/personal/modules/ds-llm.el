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
;; copilot chat 
;;
(use-package copilot-chat)


(provide 'ds-llm)
