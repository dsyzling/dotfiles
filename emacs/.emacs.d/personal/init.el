;;; init.el --- My customisations for Emacs
;;

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

;; config changes made through the customize UI will be stored here
(setq custom-file (expand-file-name "custom.el" ds-init-dir))

(add-to-list 'load-path ds-modules-dir)

;; Avoid bad request and TLS issues when refreshing packages. 
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

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

;; Use counsel/swiper with ivy
(use-package counsel :ensure t)

;; Use counsel with projectile - define projectile prefix key.
(use-package counsel-projectile :ensure t
  :config
  (counsel-projectile-mode)
  (setq projectile-completion-system 'ivy)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package ivy :ensure t
  :diminish (ivy-mode . "")
  :bind
  (:map ivy-mode-map
        ("C-'" . ivy-avy)
        ("C-J" . ivy-im)
        )
  :config
  (ivy-mode 1)
  ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  ;; number of result lines to display
  (setq ivy-height 10)
  ;; does not count candidates
  (setq ivy-count-format "%d/%d")
  ;; no regexp by default
  (setq ivy-initial-inputs-alist nil)
  ;; configure regexp engine.
  (setq ivy-re-builders-alist
	;; allow input not in order
        '((t   . ivy--regex-ignore-order))
         ))

;; Use ivy-avy to trigger lens actions in lsp-mode
(use-package ivy-avy :ensure t)

;;
;; org-mode customisation module.
;;
(require 'ds-org)

;; Use helm mode everywhere
;;(helm-mode 1)

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
                      :height 105
                      :weight 'medium)

  ;; Use google-chrome-stable as our default browser
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "google-chrome-stable")

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

;; for now let's use the default theme with a different background colour
;;(set-background-color "#211e1e")

(use-package twilight-theme
  :ensure t
  :config
  (load-theme 'twilight t))

;; make comment italic and standard foreground text less 'bright'
;; for the twilight theme
(custom-theme-set-faces
 'twilight
 '(default ((t (:background "#141414" :foreground "#cacaca"))))
 '(font-lock-comment-face ((t (:italic t :foreground "#5F5A60"))))
 '(font-lock-warning-face ((t (:background "#141414" :foreground "red"))))
 )
(set-background-color "#1E1E1E")


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

;; GPG 2.2.x - now uses pinentry with a different interface - this change allows
;; mini buffer entry of the passphrase
;;  https://colinxy.github.io/software-installation/2016/09/24/emacs25-easypg-issue.html
(setq epa-pinentry-mode 'loopback)

;; Gnus and news initialisation directory on our directory that is synced so we can
;; share amongst machines
(setq gnus-init-file (expand-file-name "gnus/.gnus.el" sync-home))
(setq gnus-startup-file (expand-file-name "gnus/.newsrc" sync-home))

;;
;; Company mode customisation.
;;
(use-package company
  :ensure t
  :config
  ;; Reduce company idle delay for completion
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2
        company-tooltip-align-annotations t
        company-tooltip-minimum-width 50
        company-icon-size 20)
  :init
  (global-company-mode 1)
  ;; In case we want to override completion style -
  ;; Default is basic partial-completion
  ;;(setq completion-styles '(partial-completion))
  ;;(add-to-list 'company-backends '(company-capf :with company-yasnippet))
  ;; override idle delay for certain modes, otherwise it can be annoying
  ;; when typing.
  :hook
  (org-mode . (lambda ()
                (set (make-local-variable 'company-idle-delay) 0.4)))
  )

;; Company colour theme customisation
(require 'color)
(let ((bg (face-attribute 'default :background)))
  (custom-set-faces
   `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
   `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
   `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
   `(company-tooltip-selection ((t (:background ,(color-lighten-name bg 10)))))
   `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))

;; Mood-lone mode - sort out by mode line
(use-package mood-line
  :ensure t
  :init
  (mood-line-mode))

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

(use-package lsp-mode
  :ensure t
  ;; :load-path "~/projects/emacs/lsp-mode/"
  ;;:hook (lsp-mode . lsp-lens-mode)
  :init
  :config
  (setq lsp-prefer-capf  t
        completion-ignore-case t

        ;; auto enable lens mode
        lsp-lens-enable t
        ;; default is after-line however lsp-avy-lens assumes the position
        ;; is before-line and doesn't deal currently with after-line.
        lsp-lens-place-position 'before-line
        ;; Performance suggestions recommended for lsp-mode
        ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
        gc-cons-threshold 100000000
        read-process-output-max (* 1024 1024) ;; 1mb
        )
  ;; the modeline may require icons from lsp-treemacs and will fallback to
  ;; all-the-icons.
  (set-face-attribute 'mode-line nil :font "Hack-9")
  :bind (:map lsp-mode-map
              ("TAB" . company-indent-or-complete-common)
              ("C-c C-h" . lsp-ui-doc-show)
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

(use-package helm-lsp
  :ensure t)

(use-package lsp-ivy :ensure t)

(use-package markdown-mode
  :ensure t)


;; dap for debugging via lsp servers
(use-package dap-mode
  :ensure t
  :defer t
  :config
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
;; Clojure/Clojurescript development configuration
;;
(require 'ds-clojure)

;;
;; Scala development configuration
;;
(require 'ds-scala)

;;
;; Python development configuration, company-jedi for completion.
;;
(require 'ds-python)

;;
;; Java development configuration with lsp-mode
;;
(require 'ds-java)

;;
;; Customisation for an R environment.
;;
(require 'ds-R)

;; pyls appears to have some terrible performance problems and issues
;; - goto definition not working reliably, so for now use the Microsoft
;; server by default.
;; (with-eval-after-load 'lsp-pyls
;;   (ds-python-use-mspyls))


;; Render pdfs in docview at a higher resolution to improve font quality.
;; changing this variable may require you to call docview-clear-cache
(setq doc-view-resolution 300)

;;
;; global keys
;;
;; reindent automatically when return is pressed
(define-key global-map (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key "\C-s" 'swiper)
(global-set-key "\C-r" 'swiper-backward)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c a") 'org-agenda)
;; Font size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-x j") 'avy-goto-char)

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
