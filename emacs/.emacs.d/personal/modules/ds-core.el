;;; ds-core.el --- Core Emacs configuration, editing defaults etc.
;;
;; Author: Darren Syzling <dsyzling@gmail.com>
;; Keywords: Emacs, editing

;;; Commentary:

;;; Customisation of Emacs defaults.

;;; Code:

;; When saving org files a temp buffer is created with the default encoding
;; this can cause issues under Windows and other platforms if the default
;; buffer encoding can't deal with the characters in the document -
;; even though the document has been specified as using utf-8 - the
;; temp buffer will be the Emacs default (in Windows - latin1-dos)
(set-language-environment "UTF-8")

(setq ns-use-srgb-colorspace t)

;; Modes typically use c-basic-offset to adjust personal indentation, but
;; set some global defaults.
(setq-default indent-tabs-mode nil             ;; don't use tabs to indent
              tab-width 4
              inhibit-startup-screen t         ; Disable start-up screen
              initial-scratch-message ";;\n;; Scratch Buffer\n;;\n"    
              ring-bell-function 'ignore       ; disable the annoying bell ring
              ;; improved scrolling
              scroll-margin 0
              scroll-conservatively 100000
              scroll-preserve-screen-position 1
              )   

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(menu-bar-mode -1)

;; blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)
;; don't show scroll bars
(scroll-bar-mode 0)
;; Disable ido we'll use ivy or helm
(ido-mode -1)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '("" invocation-name " " (:eval (if (buffer-file-name)
                                          (abbreviate-file-name (buffer-file-name))
                                        "%b"))))

;; Use electric-pair mode
(electric-pair-mode 1)

;; show available keybindings after you start typing
(require 'which-key)
(which-key-mode +1)

;; delete the selection with a keypress
(delete-selection-mode t)

;; Iterate through CamelCase words
(global-subword-mode 1)                 

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; smart tab behavior - indent or complete
;;(setq tab-always-indent 'complete)

;; smart parens for all
(require 'smartparens-config)
(setq sp-base-key-bindings 'paredit)
(setq sp-autoskip-closing-pair 'always)
(setq sp-hybrid-kill-entire-symbol nil)
(sp-use-paredit-bindings)
(show-smartparens-global-mode +1)

;; saveplace remembers your location in a file when saving files
(setq save-place-file (expand-file-name "saveplace" ds-savefile-dir))
;; activate it for all buffers
(save-place-mode 1)

;; savehist keeps track of some history
(require 'savehist)
(setq savehist-additional-variables
      ;; search entries
      '(search-ring regexp-search-ring)
      ;; save every minute
      savehist-autosave-interval 60
      ;; keep the home clean
      savehist-file (expand-file-name "savehist" ds-savefile-dir))
(savehist-mode +1)

;; xclip for console clipboard support.
(use-package xclip
  :ensure t
  :config
  (when (not (window-system))
    (xclip-mode 1)))

;; Save file when we switch buffers or switch focus away from editor.
;; Do not do this for gpg encrypted files.
(use-package super-save
  :ensure t
  :config
  (super-save-mode +1)
  (setq super-save-exclude '(".gpg"))
  (setq auto-save-default nil))

;; save recent files
(require 'recentf)
(setq recentf-save-file (expand-file-name "recentf" ds-savefile-dir)
      recentf-max-saved-items 500
      recentf-max-menu-items 15
      ;; disable recentf-cleanup on Emacs start, because it can cause
      ;; problems with remote files
      recentf-auto-cleanup 'never)
(recentf-mode +1)

;; bookmarks
(require 'bookmark)
(setq bookmark-default-file (expand-file-name "bookmarks" ds-savefile-dir)
      bookmark-save-flag 1)

;; projectile is a project management mode
(require 'projectile)
(setq projectile-cache-file (expand-file-name  "projectile.cache" ds-savefile-dir))
(projectile-mode t)

(require 'eshell)
(setq eshell-directory-name (expand-file-name "eshell" ds-savefile-dir))

;;
;; common programming defaults.
;;

;; enable on-the-fly syntax checking
(if (fboundp 'global-flycheck-mode)
    (global-flycheck-mode +1)
  (add-hook 'prog-mode-hook 'flycheck-mode))

;; font-lock annotations like TODO in source code
(require 'hl-todo)
(global-hl-todo-mode 1)

(defun ds-prog-mode-defaults ()
  "Default coding hook, useful with any programming language."
  (flyspell-prog-mode)
  (smartparens-mode +1))

(add-hook 'prog-mode-hook (lambda ()
                            (run-hooks 'ds-prog-mode-defaults)))

;;
;; Spell checking with UK dictionary.
;;
(require 'flyspell)
(flyspell-mode-on)
(ispell-change-dictionary "british")

;; Colour compilation buffers and shells based on terminal type.
;; https://github.com/atomontage/xterm-color
;;
;; Add this to .zshrc so that ls --color-tty will show filename/directory
;; colours and git colours etc.
;;
;; # Allow colour support within Emacs shell.
;; if [ "$TERM" = dumb ] && [ "$INSIDE_EMACS" ]; then
;;    export TERM=xterm-256color
;; fi
;;
(use-package xterm-color :ensure t)
(require 'xterm-color)

;; Compilation buffer colours.
(defun my/advice-compilation-filter (f proc string)
  (funcall f proc (xterm-color-filter string)))

(advice-add 'compilation-filter :around #'my/advice-compilation-filter)

;; Shell colours
(setq comint-output-filter-functions
      (remove 'ansi-color-process-output comint-output-filter-functions))

(defun my/shell-xterm-colour-hook ()
  ;; Disable font-locking in this buffer to improve performance
  (font-lock-mode -1)
  ;; Prevent font-locking from being re-enabled in this buffer
  (make-local-variable 'font-lock-function)
  (setq font-lock-function (lambda (_) nil))
  (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t))

(add-hook 'shell-mode-hook 'my/shell-xterm-colour-hook)

;; Fix shell window for lsp-mode - comint is used for some terminal windows -
;; e.g. to install servers, this particular one uses a compilation shell minor
;; mode - so apply our shell mode default colourise process as above.
(add-hook 'compilation-shell-minor-mode-hook 'my/shell-xterm-colour-hook)

;; eshell
;; Currently not working.
;; (require 'eshell)
;; (add-hook 'eshell-before-prompt-hook
;;           (lambda ()
;;             (setq xterm-color-preserve-properties t)))

;; (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
;; (setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions))

;; (require 'compile)
;; (setq compilation-ask-about-save nil  ; Just save before compiling
;;       compilation-always-kill t       ; Just kill old compile processes before
;;                                         ; starting the new one
;;       compilation-scroll-output 'first-error ; Automatically scroll to first
;;                                         ; error
;;       )


(provide 'ds-core)
