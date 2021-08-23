;;; ds-lisp.el --- Emacs lisp configuration, editing defaults etc.
;;
;; Author: Darren Syzling <dsyzling@gmail.com>
;; Keywords: Emacs, editing

;;; Commentary:

;;; Customisation of Emacs Lisp defaults.

;;; Code:

(require 'rainbow-delimiters)
(require 'paredit)

;; Lisp configuration
(define-key read-expression-map (kbd "TAB") 'completion-at-point)

(defun ds-lisp-coding-defaults ()
  (smartparens-strict-mode +1)
  (rainbow-delimiters-mode +1)
  (eldoc-mode +1)
  (rainbow-mode +1)
  (enable-paredit-mode))

(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-defun)
(define-key emacs-lisp-mode-map (kbd "C-c C-b") 'eval-buffer)

(add-hook 'emacs-lisp-mode-hook 'ds-lisp-coding-defaults)

(provide 'ds-lisp)
