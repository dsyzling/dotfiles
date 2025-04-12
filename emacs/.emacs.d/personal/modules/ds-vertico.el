;;; ds-vertico.el --- Personal Vertico Configuration for Emacs
;;
;; Author: Darren Syzling <dsyzling@gmail.com>
;; Keywords: vertico, consult, marginalia, orderless
;; See:
;;   https://kristofferbalintona.me/posts/202202211546/
;;   https://protesilaos.com/codelog/2024-02-17-emacs-modern-minibuffer-packages/
;;
;; When using consult-lsp-symbols (M-.) in order to narrow symbols
;; use C-, then select the character of the type you want to narrow to
;; e.g. 'c' for class.
;;  see: consult-narrow-key
;;

;;; Commentary:

;;; Code:

;; Test vertico
;;
;; Enable vertico
(use-package vertico
  :ensure t
  
  :custom
  (vertico-count 13)                 ; Number of candidates to display
  (vertico-resize t)
  (vertico-cycle nil) ; Go from last to first candidate and first to last (cycle)?
  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  
  (:keymaps 'vertico-map
            ;; "<tab>" #'vertico-insert ; Insert selected candidate into text area
            "<escape>" #'minibuffer-keyboard-quit ; Close minibuffer
            "C-M-n" #'vertico-next-group
            "C-M-p" #'vertico-previous-group)  
  :init
  (vertico-mode))

;;
;; Allow us to remove path in completion area by typing ~/
;;
(add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

;;
;; Allow backspace/delete key to remove entire directory when using vertico
;; to complete filepaths.
;;
(require 'vertico-directory)
(keymap-set vertico-map "DEL" 'vertico-directory-delete-char)


;; The `consult' package provides lots of commands that are enhanced
;; variants of basic, built-in functionality.  One of the headline
;; features of `consult' is its preview facility, where it shows in
;; another Emacs window the context of what is currently matched in
;; the minibuffer.  Here I define key bindings for some commands you
;; may find useful.  The mnemonic for their prefix is "alternative
;; search" (as opposed to the basic C-s or C-r keys).
;;
;; Further reading: https://protesilaos.com/emacs/dotemacs#h:22e97b4c-d88d-4deb-9ab3-f80631f9ff1d
(use-package consult
  :ensure t
  
  :bind (;; A recursive grep
         ("M-s M-g" . consult-grep)
         ;; Search for files names recursively
         ("M-s M-f" . consult-find)
         ;; Search through the outline (headings) of the file
         ("M-s M-o" . consult-outline)
         ;; Search the current buffer
         ("C-s" . consult-line)
         ;; Switch to another buffer, or bookmarked file, or recently
         ;; opened file.
         ("C-x b" . consult-buffer)))

;; Optionally configure the narrowing key.
;; Both < and C-+ work reasonably well.
(setq consult-narrow-key "C-,")

;;
;; for consult-projectile-find-file etc.
;;
(use-package consult-projectile
  :ensure t)

;;
;; lsp workspace symbol etc.
;;   https://github.com/gagbo/consult-lsp?tab=readme-ov-file
;;
;; For type narrowing see consult-narrow-key and this response:
;;   https://github.com/gagbo/consult-lsp/issues/31
;;
(use-package consult-lsp
  :ensure t)

;;
;; Disable preview of file when using consult-buffer
;; This preents trying to view an encrypted file and prompting
;; for the password. It can also be expensive to view files as
;; we're selecting them in the minibuffer. Here we provide a
;; manual preview option.
;;  https://github.com/minad/consult#live-previews
;;
(consult-customize
 ;; consult-ripgrep consult-git-grep consult-grep
 ;; consult-bookmark consult-recent-file consult-xref
 ;; consult--source-bookmark consult--source-file-register
 ;; consult--source-recent-file consult--source-project-recent-file
 ;; my/command-wrapping-consult    ;; disable auto previews inside my command
 ;; :preview-key '(:debounce 0.4 any) ;; Option 1: Delay preview
 consult-buffer
 :preview-key "M-." ;; manual preview
 )

(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)

;; The `orderless' package lets the minibuffer use an out-of-order
;; pattern matching algorithm.  It matches space-separated words or
;; regular expressions in any order.  In its simplest form, something
;; like "ins pac" matches `package-menu-mark-install' as well as
;; `package-install'.  This is a powerful tool because we no longer
;; need to remember exactly how something is named.
;;
;; Note that Emacs has lots of "completion styles" (pattern matching
;; algorithms), but let us keep things simple.
;;
;; Further reading: https://protesilaos.com/emacs/dotemacs#h:7cc77fd0-8f98-4fc0-80be-48a758fcb6e2
(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic)))

;; Enable richer annotations using the Marginalia package
(use-package marginalia
  :ensure t
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  
  ;; The :init configuration is always executed (Not lazy!)
  :init
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(use-package all-the-icons-completion
  :ensure t
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))


;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))


;; The built-in `recentf-mode' keeps track of recently visited files.
;; You can then access those through the `consult-buffer' interface or
;; with `recentf-open'/`recentf-open-files'.
;;
(recentf-mode 1)

(global-set-key "\C-s" 'consult-line)


;;
;; Leaving this more complex consult block here so we can review options.
;; For now going with the simplest set of default options/customisations.
;;

;; Example configuration for Consult
;; (use-package consult
;;   ;; Replace bindings. Lazily loaded due by `use-package'.
;;   :bind (;; C-c bindings (mode-specific-map)
;;          ("C-c h" . consult-history)
;;          ("C-c m" . consult-mode-command)
;;          ("C-c k" . consult-kmacro)
;;          ;; C-x bindings (ctl-x-map)
;;          ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
;;          ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
;;          ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
;;          ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
;;          ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
;;          ;; Custom M-# bindings for fast register access
;;          ("M-#" . consult-register-load)
;;          ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
;;          ("C-M-#" . consult-register)
;;          ;; Other custom bindings
;;          ("M-y" . consult-yank-pop)                ;; orig. yank-pop
;;          ("<help> a" . consult-apropos)            ;; orig. apropos-command
;;          ;; M-g bindings (goto-map)
;;          ("M-g e" . consult-compile-error)
;;          ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
;;          ("M-g g" . consult-goto-line)             ;; orig. goto-line
;;          ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
;;          ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
;;          ("M-g m" . consult-mark)
;;          ("M-g k" . consult-global-mark)
;;          ("M-g i" . consult-imenu)
;;          ("M-g I" . consult-imenu-multi)
;;          ;; M-s bindings (search-map)
;;          ("M-s d" . consult-find)
;;          ("M-s D" . consult-locate)
;;          ("M-s g" . consult-grep)
;;          ("M-s G" . consult-git-grep)
;;          ("M-s r" . consult-ripgrep)
;;          ("M-s l" . consult-line)
;;          ("M-s L" . consult-line-multi)
;;          ("M-s m" . consult-multi-occur)
;;          ("M-s k" . consult-keep-lines)
;;          ("M-s u" . consult-focus-lines)
;;          ;; Isearch integration
;;          ("M-s e" . consult-isearch-history)
;;          :map isearch-mode-map
;;          ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
;;          ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
;;          ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
;;          ("M-s L" . consult-line-multi))           ;; needed by consult-line to detect isearch
  
;;   ;; Enable automatic preview at point in the *Completions* buffer. This is
;;   ;; relevant when you use the default completion UI. You may want to also
;;   ;; enable `consult-preview-at-point-mode` in Embark Collect buffers.
;;   :hook (completion-list-mode . consult-preview-at-point-mode)

;;   ;; The :init configuration is always executed (Not lazy)
;;   :init

;;   ;; Optionally configure the register formatting. This improves the register
;;   ;; preview for `consult-register', `consult-register-load',
;;   ;; `consult-register-store' and the Emacs built-ins.
;;   (setq register-preview-delay 0
;;         register-preview-function #'consult-register-format)

;;   ;; Optionally tweak the register preview window.
;;   ;; This adds thin lines, sorting and hides the mode line of the window.
;;   (advice-add #'register-preview :override #'consult-register-window)

;;   ;; Optionally replace `completing-read-multiple' with an enhanced version.
;;   (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

;;   ;; Use Consult to select xref locations with preview
;;   (setq xref-show-xrefs-function #'consult-xref
;;         xref-show-definitions-function #'consult-xref)

;;   ;; Configure other variables and modes in the :config section,
;;   ;; after lazily loading the package.
;;   :config

;;   ;; Optionally configure preview. The default value
;;   ;; is 'any, such that any key triggers the preview.
;;   ;; (setq consult-preview-key 'any)
;;   ;; (setq consult-preview-key (kbd "M-."))
;;   ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
;;   ;; For some commands and buffer sources it is useful to configure the
;;   ;; :preview-key on a per-command basis using the `consult-customize' macro.
;;   (consult-customize
;;    consult-theme
;;    :preview-key '(:debounce 0.2 any)
;;    consult-ripgrep consult-git-grep consult-grep
;;    consult-bookmark consult-recent-file consult-xref
;;    consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
;;    :preview-key (kbd "M-."))

;;   ;; Optionally configure the narrowing key.
;;   ;; Both < and C-+ work reasonably well.
;;   (setq consult-narrow-key "<") ;; (kbd "C-+")

;;   ;; Optionally make narrowing help available in the minibuffer.
;;   ;; You may want to use `embark-prefix-help-command' or which-key instead.
;;   ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

;;   ;; Optionally configure a function which returns the project root directory.
;;   ;; There are multiple reasonable alternatives to chose from.
;;   ;;;; 1. project.el (project-roots)
;;   (setq consult-project-root-function
;;         (lambda ()
;;           (when-let (project (project-current))
;;             (car (project-roots project)))))
;;   ;;;; 2. projectile.el (projectile-project-root)
;;   ;; (autoload 'projectile-project-root "projectile")
;;   ;; (setq consult-project-root-function #'projectile-project-root)
;;   ;;;; 3. vc.el (vc-root-dir)
;;   ;; (setq consult-project-root-function #'vc-root-dir)
;;   ;;;; 4. locate-dominating-file
;;   ;; (setq consult-project-root-function (lambda () (locate-dominating-file "." ".git")))
;; )


;; A few more useful configurations...
;; (use-package emacs
;;   :init
;;   ;; Add prompt indicator to `completing-read-multiple'.
;;   ;; Alternatively try `consult-completing-read-multiple'.
;;   (defun crm-indicator (args)
;;     (cons (concat "[CRM] " (car args)) (cdr args)))
;;   (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

;;   ;; Do not allow the cursor in the minibuffer prompt
;;   (setq minibuffer-prompt-properties
;;         '(read-only t cursor-intangible t face minibuffer-prompt))
;;   (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;;   ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
;;   ;; Vertico commands are hidden in normal buffers.
;;   ;; (setq read-extended-command-predicate
;;   ;;       #'command-completion-default-include-p)

;;   ;; Enable recursive minibuffers
;;   (setq enable-recursive-minibuffers t)
;;   )

(provide 'ds-vertico)

;;; ds-vertico.el ends here
