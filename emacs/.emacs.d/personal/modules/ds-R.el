;;; ds-R.el --- Personal R Configuration for Emacs
;;
;; Author: Darren Syzling <dsyzling@gmail.com>
;; Keywords: R, RStudio
;;
;; ESS can be found here: https://github.com/emacs-ess/ESS

(use-package ess
  :ensure t
  :config
  ;; Customise modules enabled for elpy - other behaviour will be
  ;; managed by lsp-mode.
  (setq ess-R-font-lock-keywords
   '((ess-R-fl-keyword:keywords . t)
     (ess-R-fl-keyword:constants . t)
     (ess-R-fl-keyword:modifiers . t)
     (ess-R-fl-keyword:fun-defs . t)
     (ess-R-fl-keyword:assign-ops . t)
     (ess-R-fl-keyword:%op% . t)
     (ess-fl-keyword:fun-calls . t)
     (ess-fl-keyword:numbers . t)
     (ess-fl-keyword:operators)
     (ess-fl-keyword:delimiters)
     (ess-fl-keyword:=)
     (ess-R-fl-keyword:F&T . t)
     (ess-eval-visibly 'nowait)
     (ess-use-company 'script-only)))
  
  :init
  (require 'ess-site))

(setq ess-use-flymake nil)

;;
;; Override keys to match our lsp-mode keys for other languages
;;
(define-key ess-mode-map (kbd "C-c C-d") 'ess-display-help-on-object)
;;
;; This temporarily skirts around an issue which is prompting for a working
;; but can't display a buffer prompt with helm active.
;; The original issue was logged here: https://github.com/emacs-ess/ESS/issues/1074
;; But closed and an issue related to all Display buffer nuances opened
;; https://github.com/emacs-ess/ESS/issues/1112.
;;
(setq ess-ask-for-ess-directory nil)

(setq display-buffer-alist
      '(("*R Dired"
	 (display-buffer-reuse-window display-buffer-at-bottom)
	 (window-width . 0.5)
	 (window-height . 0.25)
	 (reusable-frames . nil))
	("*R"
	 (display-buffer-reuse-window display-buffer-in-side-window)
	 (side . right)
	 (slot . -1)
	 (window-width . 0.5)
	 (reusable-frames . nil))
	("*Help"
	 (display-buffer-reuse-window display-buffer-in-side-window)
	 (side . right)
	 (slot . 1)
	 (window-width . 0.5)
	 (reusable-frames . nil))))

(provide 'ds-R)
