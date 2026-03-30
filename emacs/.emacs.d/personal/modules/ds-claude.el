;;; ds-claude.el --- Personal Python Configuration for Emacs
;;
;; Author: Darren Syzling <dsyzling@gmail.com>
;; Keywords: LLM, Claude Code

;;; Commentary:

;;; Personal configuration for running Claude Code within Emacs.

;;; Code:

;; 
;; Use C-c c
;; C-c c m - transient menu
;; toggle readonly mode to scroll through output C-c c m z
;;

;; See
;;   https://willschenk.com/howto/2025/claude_code_in_emacs/
;;   https://github.com/stevemolitor/claude-code.el
;;
;; to build for Ubuntu WSL
;; https://github.com/emacsmirror/vterm
;; sudo apt-get install libtool-bin cmake
;; git clone https://github.com/akermu/emacs-libvterm.git
;; cmake ./
;; make 
;;
(add-to-list 'load-path "/home/dsyzling/dev/emacs-libvterm")

;; vterm for running claude - we can use eat or vterm as the back end.
(use-package vterm
  :ensure t)

;; for eat terminal backend:
(use-package eat :ensure t)

;; install required inheritenv dependency:
(use-package inheritenv
  :vc (:url "https://github.com/purcell/inheritenv" :rev :newest))


;; Optional: IDE features like go-to-definition
(use-package monet
  :straight (:type git :host github :repo "stevemolitor/monet"))

;; Main Claude Code package 
(use-package claude-code :ensure t
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  :config
  ;; Use vterm instead of eat for better color rendering
  (setq claude-code-terminal-backend 'vterm)
  ;; optional IDE integration with Monet
  (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
  (monet-mode 1)

  (claude-code-mode)
  :bind-keymap ("C-c c" . claude-code-command-map)

  ;; Optionally define a repeat map so that "M" will cycle thru Claude auto-accept/plan/confirm modes after invoking claude-code-cycle-mode / C-c M.
  :bind
  (:repeat-map my-claude-code-map ("M" . claude-code-cycle-mode)))

(provide 'ds-claude)
