;;; init.el --- My customisations for Emacs
;;

;; Set the frame size according to pixels and not based on the size of the font.
(setq frame-resize-pixelwise t)

;; Inhibit resizing the frame when changing the font
(setq frame-inhibit-implied-resize t)

;; make sure that UTF-8 is used everywhere.
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-language-environment    'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system   'utf-8)
(prefer-coding-system        'utf-8)
(set-input-method nil)

;; Use this only on laptops...
(unless (string-match-p "^Power N/A" (battery))
  ;; it's nice to know how much power you have
  (display-battery-mode 1)
  )

;; personal laptop
(defvar my-laptop-p (equal (system-name) "dsarchie"))

;; Paths for our configuration modules.
(defvar ds-init-dir (file-name-directory load-file-name)
  "The root dir of my Emacs configuration files.")

(defvar ds-modules-dir (expand-file-name "modules" ds-init-dir)
  "My Custom Emacs modules.")

(defvar ds-savefile-dir (expand-file-name "savefile" ds-init-dir)
  "Store all automatically generated save/history-files.")

(defvar sync-home (pcase system-type
                    ('windows-nt "f:/Dropbox/Dropbox")
                    ('gnu/linux  "~/Sync")
                     ('darwin     "~/Sync"))
  "SyncThing directory syncing our todo items and notes.")

(defvar todo-home (expand-file-name "todo" sync-home)
    "Our task files for org-mode")

(defvar ds-org-archive (expand-file-name "archive/%s_archive::" sync-home)
    "Task archive location.")

;;
;; Some of our packages can only be installed with straight directly
;; from github
;;
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; config changes made through the customize UI will be stored here
(setq custom-file (expand-file-name "custom.el" ds-init-dir))

(add-to-list 'load-path ds-modules-dir)

;; When using nvm to manage node for pyright on nix platforms we
;; need to inherit the shell exec-path changes
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))


;; Avoid bad request and TLS issues when refreshing packages. 
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Emacs 29 native compilation build - disable showing warnings as default.
(setq warning-minimum-level :error)

;; Emacs 29 compilation buffer introduced a line length which then truncates
;; output with [..] - this seems to be triggered by python progress bars
; so I've disabled this option for now.
(setq compilation-max-output-line-length nil)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
;;(package-initialize)
(unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure 't)

;; Any passwords or secrets that we do not want to commit to github.
(require 'ds-secrets)

;; Core editing and Emacs defaults.
(require 'ds-core)

;; lisp editing defaults.
(require 'ds-lisp)

;;
;; org-mode customisation module.
;;
(require 'ds-org)

;;
;; Research papers with Quarto and Markdown
;;
(require 'ds-quarto)

;;
;; magit customisation
;; Turn on spell checking
;;
(use-package magit
  :hook ((git-commit-mode . flyspell-mode)))

;; Icons for git status page.
(setopt magit-format-file-function #'magit-format-file-all-the-icons) 

;;
;; Forge for working with PRs etc. on Github/Gitlab
;;
;; Setup for github requires access tokens
;;   https://magit.vc/manual/forge/Setup-for-Githubcom.html
(use-package forge
  :after magit)


;; ;; TODO - remove these prelude definitions.
;; ;; Disable undo-tree, slowing down saving large orgmode buffers and
;; ;; I've never really used it.
;; (global-undo-tree-mode 0)
;; ;; disable prelude guru mode
;; ;; disable whitespace higlighting - long lines etc. prelude turns this on in text mode by default
;; (setq prelude-guru nil)
;; (setq prelude-whitespace nil)

;; enable desktop save mode
;;(desktop-save-mode 1)

(when (eq system-type 'windows-nt)
  ;;(set-face-font 'default "-outline-Consolas-bold-r-normal-normal-15-112-96-96-c-*-iso8859-1")
  (set-face-attribute 'default
                      nil
                      :font "Consolas"
                      :height 110
                      :weight 'bold))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Linux customisations
(when (eq system-type 'gnu/linux)
  (set-face-attribute 'default
                      nil
                      :font "JetBrainsMono"
                      ;; :height 105
                      :height 115
                      :weight 'medium)

  ;; Use google-chrome-stable as our default browser
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "firefox"
        ;; browse-url-generic-program "google-chrome-stable"
        )

  ;; When using Emacs & WSL on my work machine update browser executable.
  (when (string= "DARRENS1" (system-name))
    (setq browse-url-browser-function 'browse-url-generic
          browse-url-generic-program
          "/mnt/c/Program Files/Google/Chrome/Application/chrome.exe")
    ;; Set default frame position and size on work machine.
    (setq default-frame-alist '((left . 100) (width . 250) (height . 125)))
    (set-frame-position (selected-frame) 100 60)
    )

  ;; (set-face-attribute 'default
  ;;                     nil
  ;;                     :font "Menlo"
  ;;                     :height 120
  ;;                     :weight 'bold)

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mac OS X customisations

(when (eq system-type 'darwin)
  ;; enable cmd key to be used as emacs meta key
  (setq mac-command-modifier 'meta)
  ;; As of Emacs 25 on mac this appears to be the only way of selecting consolas
  ;;  (set-face-attribute 'default nil :family "Consolas")
  (set-frame-font "Menlo 14")

  ;; Disable right alt/meta so that we can access # using M-3
  (setq ns-right-alternate-modifier (quote none)))

;; enable window numbering mode
(use-package window-numbering
  :ensure t
  :config
  (window-numbering-mode))


;; erc customisations - remove join, leave quit messages
(setq erc-hide-list '("JOIN" "PART" "QUIT"))

;; Racket/Geiser customisation

;; allow C-c C-c to eval current definition - same as M-C-x
(add-hook 'geiser-mode-hook
          '(lambda ()
             (define-key geiser-mode-map (kbd "C-c C-c") 'geiser-eval-definition)))

;; PHP mode - temporary for zu-uk site
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

;; GPG 2.2.x - now gpg uses pinentry with a different interface - this change allows
;; mini buffer entry of the passphrase
;;  https://colinxy.github.io/software-installation/2016/09/24/emacs25-easypg-issue.html
(setq epa-pinentry-mode 'loopback)

;; Gnus and news initialisation directory on our directory that is synced so we can
;; share amongst machines
(setq gnus-init-file (expand-file-name "gnus/.gnus.el" sync-home))
(setq gnus-startup-file (expand-file-name "gnus/.newsrc" sync-home))


;; Mood-lone mode - sort out by mode line
(use-package mood-line

  ;; Enable mood-line
  :config
  (mood-line-mode)

  ;; Use pretty Fira Code-compatible glyphs
  ;; :custom
  ;; (mood-line-glyph-alist mood-line-glyphs-fira-code)
  )

(set-face-background 'mode-line "#000000")
(set-face-foreground 'mode-line "#FEFEFE")

;;
;; use lsp-mode for scala, python etc.
;;
;; Enable nice rendering of diagnostics like compile errors.
(use-package flycheck
  :ensure t
  :config
  ;; (setq flycheck-check-syntax-automatically nil)
  ;; Override default column width for filename 6 -> 12
  (setq flycheck-error-list-format
      `[("File" 12)
        ("Line" 5 flycheck-error-list-entry-< :right-align t)
        ("Col" 3 nil :right-align t)
        ("Level" 8 flycheck-error-list-entry-level-<)
        ("ID" 35 flycheck-error-list-entry-level-<)
        (,(flycheck-error-list-make-last-column "Message" 'Checker) 0 t)])

  :init (global-flycheck-mode))

;; position flycheck messages buffer
(setq display-buffer-alist
      (append display-buffer-alist
              '(("*Flycheck errors*"
        (display-buffer-reuse-window display-buffer-at-bottom)
        (window-width . 1.0)
        (window-height . 0.25)
        (reusable-frames . nil)))))

;;
;; treesitter
;; auto configure and install treesitter language files
;;
;; Disabled for now - there are too many issues interfering with
;; common workflows. The latest was no HTML treesitter mode and then
;; not falling back to the standard html-mode.
;;
;; (use-package treesit-auto
;;   :ensure t
;;   :custom
;;   (treesit-auto-install 'prompt)
;;   :config
;;   (treesit-auto-add-to-auto-mode-alist 'all)
;;   (global-treesit-auto-mode))

(use-package lsp-mode
  :ensure t
  ;; :load-path "~/projects/emacs/lsp-mode/"
  ;;:hook (lsp-mode . lsp-lens-mode)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq completion-ignore-case t
        lsp-prefer-capf  t
        
        ;; auto enable lens mode
        lsp-lens-enable t
        ;; default is after-line however lsp-avy-lens assumes the position
        ;; is before-line and doesn't deal currently with after-line.
        lsp-lens-place-position 'before-line
        ;; Performance suggestions recommended for lsp-mode
        ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
        gc-cons-threshold 100000000
        gc-cons-percentage 0.5
        read-process-output-max (* 1024 1024) ;; 1mb
        
        ;; lsp-copilot - explicitly disable for now
        lsp-copilot-enabled nil
        )
  ;; the modeline may require icons from lsp-treemacs and will fallback to
  ;; all-the-icons.
  (set-face-attribute 'mode-line nil :font "Hack-9")
  :bind (:map lsp-mode-map
              ;; ("TAB" . company-indent-or-complete-common)
              ("C-c C-h" . lsp-ui-doc-show)
              ("M-RET" . lsp-execute-code-action)
              ))
;; Use C-Windows-l to bring up lsp-mode kep map
;; Keys defined here: https://emacs-lsp.github.io/lsp-mode/page/keybindings/
(with-eval-after-load 'lsp-mode
  (define-key lsp-mode-map (kbd "C-s-l") lsp-command-map))

;; Enable emoji fonts - so we can see modeline icons - in particular
;; the light bulb for code actions
(set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)
;;(set-fontset-font t 'symbol "Apple Color Emoji")
;;(set-fontset-font t 'symbol "Segoe UI Emoji" nil 'append)
;;(set-fontset-font t 'symbol "Symbola" nil 'append)

(use-package lsp-treemacs
  :ensure t
  :config
  (setq lsp-treemacs-error-list-current-project-only t)
  ;; temp for testing - normally requires autoload
  ;; (require 'lsp-metals-treeview)
  ;; (lsp-metals-treeview-enable nil)
  ;; (setq lsp-metals-treeview-show-when-views-received t)

  ;;:load-path "~/projects/emacs/lsp-treemacs/"
  )

(use-package treemacs-all-the-icons
  :ensure t
  :after treemacs)

(use-package lsp-ui
  :ensure t
  :diminish
  :hook (lsp-mode . lsp-ui-mode)
  :init
  (setq lsp-ui-peek-enable t
        lsp-ui-sideline-enable t
        lsp-ui-imenu-enable t
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-include-signature t
        ;; Disable hover doc when cursor is over a symbol.
        ;; You can show this with C-c C-h
        lsp-ui-doc-enable nil
        ;;(setq lsp-ui-doc-position 'top)
        )
  :config
  ;;(lsp-ui-mode t)
  )
;; When in console mode use arrow for breadcrumbs
(when (not (window-system))
  (setq lsp-headerline-arrow "=>"))


(use-package markdown-mode
  :ensure t)


;; dap for debugging via lsp servers
(use-package dap-mode
  :ensure t
  ;; Our custom dap mode to debug multi-processes with Python.
  ;; :load-path "~/dev/dap-mode/"
  :defer t
  :config
  ;; Pythn debugger can be debugpy or ptvsd (now deprecated)
  (setq dap-python-debugger 'debugpy)
  ;; Don't show breakpoints on initial display for debugger.
  (setq dap-auto-configure-features '(sessions locals expressions controls tooltip repl))
  (dap-auto-configure-mode)
   ;; Position dap-ui-repl at the bottom of the screen. These are defaults from dap-ui.
  (setq dap-ui-buffer-configurations
        `((,dap-ui--locals-buffer . ((side . right) (slot . 1) (window-width . 0.20)))
          (,dap-ui--expressions-buffer . ((side . right) (slot . 2) (window-width . 0.20)))
          (,dap-ui--sessions-buffer . ((side . right) (slot . 3) (window-width . 0.20)))
          (,dap-ui--breakpoints-buffer . ((side . left) (slot . 2) (window-width . ,treemacs-width)))
          (,dap-ui--debug-window-buffer . ((side . bottom) (slot . 3) (window-width . 0.20)))
          (,dap-ui--repl-buffer . ((side . bottom) (slot . 1) (window-height . 0.20))))))


;; TODO - temporarily removed dap-hydra - finding it difficult to
;; use this in conjunction with the dap-ui-repl - seems to want
;; to capture all key presses. For now we're defining a few common
;; debugging keys.
;; Show dap-hydra when breakpoint is hit
;; (add-hook 'dap-stopped-hook
;;           (lambda (arg) (call-interactively #'dap-hydra)))

;;
;; Snippets with yasnippet
;;
(use-package yasnippet
  :ensure t
  :init
  (require 'yasnippet)
  (yas-global-mode 1)
  ;; Bind `C-c y' to `yas-expand' ONLY.
  ;; Replace prefix for yasnippet with C-c s so as not to clash
  ;; with some orgmode keys.
  (define-key yas-minor-mode-map (kbd "C-c y")     #'yas-insert-snippet)
  ;; remove yasnippet prefix and add our own.
  (define-key yas-minor-mode-map (kbd "C-c &") nil)
  (define-key yas-minor-mode-map (kbd "C-c s\C-s") #'yas-insert-snippet)
  (define-key yas-minor-mode-map (kbd "C-c s\C-n") #'yas-new-snippet)
  (define-key yas-minor-mode-map (kbd "C-c s\C-v") #'yas-visit-snippet-file))

;;
;; https://github.com/AndreaCrotti/yasnippet-snippets
;;
(use-package yasnippet-snippets
  :ensure t)

;;
;; Themes
;; 
(require 'ds-theme)

;;
;; Clojure/Clojurescript development configuration
;;
;; (require 'ds-clojure)

;;
;; Scala development configuration
;;
(require 'ds-scala)

;; corfu for completion
;; (require 'ds-corfu)
(require 'ds-company)

;; Vertico completion customisation.
(require 'ds-vertico)

;;
;; Python development configuration, company-jedi for completion.
;;
(require 'ds-python)

;; If we have issues with lsp and consult we can use ivy.
;;
;; (use-package lsp-ivy :ensure t)

;; (setq
;;  lsp-pylance-langserver-command
;;  '("/home/dsyzling/.vscode/extensions/ms-python.vscode-pylance-2021.9.0/dist/server.bundle.js")
;;  lsp-pylance-use-library-code-for-types t
;;  ;; lsp-pyright-venv-path "/home/dsyzling/miniconda3/envs"
;;  )
;; (require 'ds-pylance)
;; ;; Test using pylance
;; (setf (lsp--client-priority (ht-get lsp-clients 'pyright)) 1)
;; (setf (lsp--client-priority (ht-get lsp-clients 'pylance)) 0)

;;
;; Java development configuration with lsp-mode
;;
(require 'ds-java)

;;
;; Customisation for an R environment.
;;
(require 'ds-R)

;;
;; Emacs based LLMs
;;
(require 'ds-llm)

;; pyls appears to have some terrible performance problems and issues
;; - goto definition not working reliably, so for now use the Microsoft
;; server by default.
;; (with-eval-after-load 'lsp-pyls
;;   (ds-python-use-mspyls))


;; Render pdfs in docview at a higher resolution to improve font quality.
;; changing this variable may require you to call docview-clear-cache
(setq doc-view-resolution 300)

;;
;; mastodon
;;
;; (setq mastodon-instance-url "https://fosstodon.org"
;;       mastodon-active-user "dsyzling")
;; (use-package mastodon
;;     :ensure t)
;;

;;
;; global keys
;;
;; reindent automatically when return is pressed
(define-key global-map (kbd "RET") 'newline-and-indent)
;; Font size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-x j") 'avy-goto-char)
(global-set-key (kbd "C-c a") 'org-agenda)

;;
;; We're using projectile with consult to find files in projects
;; so we will specify a list of directories and suffixes that should
;; be filtered.
;;
;; indexing must be hybrid or native to filter.
(setq projectile-indexing-method `hybrid)
;; Java build directory with class files
;; (add-to-list 'projectile-globally-ignored-directories "*build")
;; list of file suffixes to ignore - this list doesn't exist by default.
(setq projectile-globally-ignored-file-suffixes `(".class"))

;;
;; keys that I want to apply to all of our programming modes
;;
;; lsp symbols
;; Use C-, to narrow symbols and then c - classes, f - field etc.
;;
(define-key lsp-mode-map (kbd "C-M-.")   'consult-lsp-symbols)
;; If we have issues with consult-lsp we can use ivy instead - uncomment ivy inclusion above.
;; (define-key lsp-mode-map (kbd "C-M-.")   'lsp-ivy-workspace-symbol)

(define-key lsp-mode-map (kbd "C-c C-f") 'consult-projectile-find-file)
(define-key lsp-mode-map (kbd "C-c C-d") 'lsp-describe-thing-at-point)
(define-key lsp-mode-map (kbd "M-.")     'lsp-find-definition)
;; Apply code action - useful for automatic imports in mspyls
;; also see - lsp-ui-sideline-apply-code-actions
(define-key lsp-mode-map (kbd "M-RET")   'lsp-execute-code-action)
(define-key lsp-mode-map (kbd "C-c C-r") 'lsp-ui-peek-find-references)

;; define a set of basic keys to allow us simple debug navigation.
(with-eval-after-load 'dap-mode
  (define-key dap-mode-map (kbd "<f8>") 'dap-next)
  (define-key dap-mode-map (kbd "<f7>") 'dap-step-in)
  (define-key dap-mode-map (kbd "<f5>") 'dap-continue)
  (define-key dap-mode-map (kbd "<f9>") 'dap-breakpoint-toggle)
  (define-key dap-mode-map (kbd "<f6>") 'dap-disconnect))

;; Use helm for buffer switching
;; (global-set-key (kbd "C-x b") 'helm-mini)
;; (global-set-key (kbd "C-x C-f") 'helm-find-files)

;; Helm now uses vanilla Emacs left and right to move between groups
;; restore previous behaviour and use arrow keys
;; https://github.com/emacs-helm/helm/wiki/FAQ#arrow-keys-behavior-have-changed
;; (define-key helm-map (kbd "<left>") 'helm-previous-source)
;; (define-key helm-map (kbd "<right>") 'helm-next-source)

; direnv mode allows automatic loading of direnv variables
(use-package direnv
  :ensure t
  :config
  (direnv-mode))

;; credit: yorickvP on Github
;; Requires wl-clipboard - sudo apt install wl-clipboard on WSL
;;
;; Use under WSL only
(when (string-match "-[Mm]icrosoft" operating-system-release)
  ;; WSL: WSL1 has "-Microsoft", WSL2 has "-microsoft-standard"
  (setq wl-copy-process nil)
  (defun wl-copy (text)
    (setq wl-copy-process (make-process :name "wl-copy"
                                        :buffer nil
                                        :command '("wl-copy" "-f" "-n")
                                        :connection-type 'pipe))
    (process-send-string wl-copy-process text)
    (process-send-eof wl-copy-process))
  (defun wl-paste ()
    (if (and wl-copy-process (process-live-p wl-copy-process))
        nil ; should return nil if we're the current paste owner
      (shell-command-to-string "wl-paste -n | tr -d \r")))
  (setq interprogram-cut-function 'wl-copy)
  (setq interprogram-paste-function 'wl-paste))

;;
;; Smarter keyboard quit from
;;  https://emacsredux.com/blog/2025/06/01/let-s-make-keyboard-quit-smarter/
;;
(defun er-keyboard-quit ()
  "Smater version of the built-in `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it."
  (interactive)
  (if (> (minibuffer-depth) 0)
      (abort-recursive-edit)
    (keyboard-quit)))
    
(global-set-key [remap keyboard-quit] #'er-keyboard-quit)
