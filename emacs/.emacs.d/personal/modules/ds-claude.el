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

(use-package vterm
    :ensure t)

(use-package eat
  :straight (:type git
                   :host codeberg
                   :repo "akib/emacs-eat"
                   :files ("*.el" ("term" "term/*.el") "*.texi"
                           "*.ti" ("terminfo/e" "terminfo/e/*")
                           ("terminfo/65" "terminfo/65/*") 
                           ("integration" "integration/*")
                           (:exclude ".dir-locals.el" "*-tests.el")))
  
  :config
  ;; Make URLs clickable in eat buffers
  (add-hook 'eat-mode-hook #'goto-address-mode))

;; Force dark theme colors globally for eat terminal
;; eat-term-color-N are FACES - foreground for text, background for highlighting
;; Use contrasting pairs so inverse video (diffs, selections) remains readable
(with-eval-after-load 'eat
  ;; Standard colors (0-7): light foreground, dark background
  (set-face-attribute 'eat-term-color-0 nil :foreground "#b0b0b0" :background "#404040") ; black/gray
  (set-face-attribute 'eat-term-color-1 nil :foreground "#ff6b6b" :background "#5c1f1f") ; red
  (set-face-attribute 'eat-term-color-2 nil :foreground "#69ff94" :background "#1f5c2e") ; green
  (set-face-attribute 'eat-term-color-3 nil :foreground "#fffb7a" :background "#5c5c1f") ; yellow
  (set-face-attribute 'eat-term-color-4 nil :foreground "#6bb5ff" :background "#1f3d5c") ; blue
  (set-face-attribute 'eat-term-color-5 nil :foreground "#ff6bff" :background "#5c1f5c") ; magenta
  (set-face-attribute 'eat-term-color-6 nil :foreground "#6bffff" :background "#1f5c5c") ; cyan
  (set-face-attribute 'eat-term-color-7 nil :foreground "#e0e0e0" :background "#505050") ; white
  ;; Bright colors (8-15)
  (set-face-attribute 'eat-term-color-8 nil :foreground "#c0c0c0" :background "#505050") ; bright black
  (set-face-attribute 'eat-term-color-9 nil :foreground "#ff8a8a" :background "#6b2a2a") ; bright red
  (set-face-attribute 'eat-term-color-10 nil :foreground "#8affb0" :background "#2a6b3d") ; bright green
  (set-face-attribute 'eat-term-color-11 nil :foreground "#ffff8a" :background "#6b6b2a") ; bright yellow
  (set-face-attribute 'eat-term-color-12 nil :foreground "#8ac5ff" :background "#2a4a6b") ; bright blue
  (set-face-attribute 'eat-term-color-13 nil :foreground "#ff8aff" :background "#6b2a6b") ; bright magenta
  (set-face-attribute 'eat-term-color-14 nil :foreground "#8affff" :background "#2a6b6b") ; bright cyan
  (set-face-attribute 'eat-term-color-15 nil :foreground "#ffffff" :background "#606060") ; bright white
  ;; Fix faint/dim text - default is too hard to read on dark backgrounds
  (set-face-attribute 'eat-term-faint nil :foreground "#a0a0a0" :weight 'normal))

;; Force dark background in eat buffers
(defun my/eat-force-dark-background ()
  "Force dark background for eat buffers."
  (face-remap-add-relative 'default
                           :background "#1e1e1e"
                           :foreground "#e0e0e0"))
(add-hook 'eat-mode-hook #'my/eat-force-dark-background)
;; Notify process of window size changes
(add-hook 'eat-mode-hook
          (lambda ()
            (add-hook 'window-size-change-functions
                      (lambda (_frame)
                        (when (and (eq major-mode 'eat-mode)
                                   eat-terminal)
                          (eat-term-resize eat-terminal
                                           (window-max-chars-per-line)
                                           (window-text-height))))
                      nil t)))

;; Optional: IDE features like go-to-definition
(use-package monet
  :straight (:type git :host github :repo "stevemolitor/monet"))
;; Main Claude Code package
(use-package claude-code
  :straight (:type git :host github :repo "stevemolitor/claude-code.el")
  :bind-keymap ("C-c c" . claude-code-command-map)
  :config
  ;; Use vterm instead of eat for better color rendering
  (setq claude-code-terminal-backend 'vterm)
  ;; Enable IDE integration
  (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
  (monet-mode 1))

(provide 'ds-claude)
