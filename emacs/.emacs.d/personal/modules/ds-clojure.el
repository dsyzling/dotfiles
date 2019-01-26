;;; ds-clojure.el --- Personal Clojure/Clojurescript Configuration for Emacs
;;
;; Author: Darren Syzling <dsyzling@gmail.com>
;; Keywords: clojure, clojurescript, cider

;;; Commentary:
;;;
;;; Code:

;;
;; Cider/Clojure/Clojurescript customisation
;;
(require 'cider)

(prelude-require-packages '(paredit))

(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-mode-hook 'paredit-mode)

;; Bind keys in cider mode
(bind-key "C-c C-f" 'helm-projectile-find-file cider-mode-map)

;; these should be enabled by prelude by default - but might be useful
;; for other scheme and lisp modes.
;;(add-hook 'cider-repl-mode-hook 'subword-mode)
;;(add-hook 'cider-repl-mode-hook 'company-mode)
;;(add-hook 'cider-mode-hook 'company-mode)

(defun cider-repl-command (cmd)
  "Execute commands on the cider repl"
  (cider-switch-to-repl-buffer)
  (goto-char (point-max))
  (insert cmd)
  (cider-repl-return)
  (cider-switch-to-last-clojure-buffer))

(defun cider-repl-reset ()
  "Assumes reloaded + tools.namespace is used to reload everything"
  (interactive)
  (save-some-buffers)
  (cider-repl-command "(user/reset)"))

(defun cider-reset-test-run-tests ()
  (interactive)
  (cider-repl-reset)
  (cider-test-run-tests))

;; some repl tweaks
(setq cider-repl-use-clojure-font-lock t)
;; (add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)

;; (define-key cider-mode-map (kbd "C-c r") 'cider-repl-reset)
;; (define-key cider-mode-map (kbd "C-c .") 'cider-reset-test-run-tests)


(provide 'ds-clojure)
