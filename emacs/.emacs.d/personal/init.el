;;; init.el --- My customisations for prelude
;;; Commentary:
;;; To merge upstream changes to prelude
;;;   git fetch upstream
;;;   git checkout master
;;;   git merge upstream/master

(prelude-require-packages '(use-package))

;; Disable prelude mode keymap, use our own keys.
(prelude-mode -1)

;; When saving org files a temp buffer is created with the default encoding
;; this can cause issues under Windows and other platforms if the default
;; buffer encoding can't deal with the characters in the document -
;; even though the document has been specified as using utf-8 - the
;; temp buffer will be the Emacs default (in Windows - latin1-dos)
(set-language-environment "UTF-8")

(setq ns-use-srgb-colorspace t)

;; Use helm mode everywhere
(helm-mode 1)

;; Disable undo-tree, slowing down saving large orgmode buffers and
;; I've never really used it.
(global-undo-tree-mode 0)

;; Use helm for buffer switching
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; Helm now uses vanilla Emacs left and right to move between groups
;; restore previous behaviour and use arrow keys
;; https://github.com/emacs-helm/helm/wiki/FAQ#arrow-keys-behavior-have-changed
(define-key helm-map (kbd "<left>") 'helm-previous-source)
(define-key helm-map (kbd "<right>") 'helm-next-source)

;; add melpa stable to our package archives
(add-to-list 'package-archives
             '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)

;;
;; Set dropbox home and orgmode-home on each platform/system.
;;
(setq dropbox-home
      (pcase system-type
        ('windows-nt "f:/Dropbox/Dropbox")
        ('gnu/linux  "~/Dropbox")
        ('darwin     "~/Dropbox")))

(setq orgmode-home (concat dropbox-home "/home/org"))

;; org files will use utf-8 by default
(modify-coding-system-alist 'file "\.org\'" 'utf-8)

;; disable prelude guru mode
(setq prelude-guru nil)

;; disable whitespace higlighting - long lines etc. prelude turns this on in text mode by default
(setq prelude-whitespace nil)

;; enable desktop save mode
                                        ;(desktop-save-mode 1)

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
  ;; (set-face-attribute 'default
  ;;                     nil
  ;;                     :font "Inconsolata"
  ;;                     :height 120
  ;;                     :weight 'bold)
  ;; Use google-chrome-stable as our default browser
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "google-chrome-stable")

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
  ;;(set-face-font 'default "-unknown-Consolas-bold-normal-normal-*-16-*-*-*-m-0-iso10646-1")
  ;;(set-face-font 'default "-unknown-Inconsolata-bold-normal-normal-*-17-*-*-*-m-0-iso10646-1")
  ;; improved font rendering in emacs 24.4 now makes this font style more pleasing on mac
  ;;(set-face-font 'default "-unknown-Inconsolata-normal-normal-normal-*-16-*-*-*-m-0-iso10646-1")
  ;; As of Emacs 25 on mac this appears to be the only way of selecting consolas
  ;;  (set-face-attribute 'default nil :family "Consolas")
  ;;  (set-face-attribute 'default nil :height 165)
  (set-frame-font "Menlo 15")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general editing preferences

;; reindent automatically when return is pressed
(define-key global-map (kbd "RET") 'newline-and-indent)

;; enable window numbering mode
(prelude-require-packages '(window-numbering))
(window-numbering-mode)

;; don't show scroll bars
(scroll-bar-mode 0)

;; for now let's use the default theme with a different background colour
(set-background-color "#211e1e")

(prelude-require-packages '(twilight-theme))
(disable-theme 'zenburn)
(load-theme 'twilight t)

;; make comment italic and standard foreground text less 'bright'
;; for the twilight theme
(custom-theme-set-faces
 'twilight
 '(default ((t (:background "#141414" :foreground "#cacaca"))))
 '(font-lock-comment-face ((t (:italic t :foreground "#5F5A60"))))
 '(font-lock-warning-face ((t (:background "#141414" :foreground "red"))))
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode config
(setq org-agenda-files (list (concat orgmode-home "/todo/todo.org")
                             (concat orgmode-home "/todo/garch.org")))

(setq org-agenda-custom-commands
      '(("p" "Projects" tags "PROJECT" nil)
        ("d" "Day's Agenda" agenda ""
         ((org-agenda-span 1)))
        ("w" "Waiting Tasks"  ((tags-todo "WAITING")))
        ("r" "Studying Tasks/Research"
         ((tags-todo "study")
          ))
        ("c" "Completed Tasks" todo "DONE" nil)
        ("v" "21 day view" agenda "" ((org-agenda-span 21)))
        ("u" "Unscheduled tasks" todo "TODO"
         ((org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline))))))

;; set org mode wrap at 80 cols
(add-hook 'org-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook
          '(lambda() (set-fill-column 80))
          ;;(set-face-foreground 'org-scheduled-today "#cacaca")
          )

(setq org-log-done 'time)
(custom-set-variables
 ;; hide stars and indent note items automatically
 '(org-archive-location (concat orgmode-home "/archive/%s_archive::"))
 '(org-startup-indented t)
 '(org-agenda-ndays 7)
 '(org-deadline-warning-days 14)
 ;;'(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-blank-before-new-entry '((heading . auto)
                                (plain-list-item . auto)))
 '(org-agenda-start-on-weekday nil)
 '(org-completion-use-ido t)
 '(org-agenda-window-setup 'current-window))

;; widen category field a little
(setq org-agenda-prefix-format "  %-17:c%?-12t% s")

;; return key follow hyperlink
(setq org-return-follows-link t)

(define-key global-map "\C-cr" 'org-remember)

;; refile targets
(setq org-directory orgmode-home)
(setq org-refile-targets (quote (("personal.org" :level . 1))))

;; mobile org
(setq org-mobile-directory "~/Dropbox/MobileOrg")
(setq org-mobile-files (cons  "labnotebook.org" org-agenda-files))
(setq org-mobile-inbox-for-pull "~/Dropbox/home/org/from-mobile.org")

(defun labnotebook-file () (concat org-directory "/labnotebook.org"))
(defun visit-journal-file ()
  "Open lab notebook and journal file"
  (interactive)
  (find-file (labnotebook-file)))
(global-set-key (kbd "C-c J") 'visit-journal-file)
(global-set-key (kbd "C-c j") 'org-capture)

(setq org-capture-templates
      `(
        ("j" "Journal" entry
         (file+datetree ,(concat orgmode-home "/labnotebook.org")) "** %^{Heading}\n%?")

        ("t" "ToDo" entry
         (file+headline ,(concat orgmode-home "/todo/inbox.org") "Inbox") "** %?\n" :prepend t)

        ("l" "Link" plain
         (file ,(concat orgmode-home "/labnotebook.org")) "- %?\n %x\n")

        ("n" "Note-someday" entry
         (file+headline ,(concat orgmode-home "/labnotebook.org") "Someday") "\n\n** %?\n")
        ))

;; our org-remember templates
;; we can set our default org notes file - but we'll specify a template for todo items
;;(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-remember-templates
      '(("Todo" ?t "* TODO %?\n  %i\n  %a" (concat orgmode-home "/newgtd.org") "Tasks")
        ("Articles" ?a "* %?\n  %i\n  %a" (concat orgmode-home "/newgtd.org") "Articles")
        ("Journal" ?j "\n* %^{topic} %T \n%i%?\n" (concat orgmode-home "/journal.org"))
        ("Someday" ?s "* %?\n  %i\n  %a" (concat orgmode-home "/newgtd.org") "Someday")))

;; use helm to access headlines in org file
(add-hook 'org-mode-hook
          (lambda () (local-set-key (kbd "s-h") 'helm-org-headlines)))

;; allow us to include source code into org mode docs and fontify
(setq  org-src-fontify-natively t)

;; erc customisations - remove join, leave quit messages
(setq erc-hide-list '("JOIN" "PART" "QUIT"))

;; our custom twlight colour theme
;;(require 'color-theme-twilight-ds)
;;(color-theme-twilight-ds)
;; For now we're just setting a different background colour - we will
;; be modifying our theme template in time.
;; Background matches default vscode template.
(set-background-color "#1E1E1E")


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

;; Gnus and news initialisation directory on Dropbox so we can
;; share amongst machines
(setq gnus-init-file (concat dropbox-home "/home/gnus/.gnus.el"))
(setq gnus-startup-file (concat dropbox-home "/gnus/.newsrc"))

;;
;; Company mode customisation
;; Company-box - icons for company mode
;;
(prelude-require-packages '(company-box))
(require 'company-box)
(add-hook 'company-mode-hook 'company-box-mode)

;; Reduce company idle delay for completion
(setq company-idle-delay 0.3)

;; Company colour customisation
(require 'color)
(let ((bg (face-attribute 'default :background)))
  (custom-set-faces
   `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
   `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
   `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
   `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
   `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))

;;
;; Deft for note taking and searching
;;
(use-package deft
  :ensure t
  :commands (deft)
  :config
  (setq deft-directory (concat dropbox-home "/home/org/deft"))
  (setq deft-default-extension "org")
  (setq deft-use-filename-as-title t)
  (setq deft-recursive nil)
  (setq deft-use-filter-string-for-filename t))

(use-package quelpa
  :ensure t)
(use-package quelpa-use-package
  :ensure t)

;;
;; org-roam
;; Not available on (m)elpa or gnu - use quelpa to install
;; from github.
;;
;;(quelpa 'org-roam :upgrade t)
(use-package org-roam
  :ensure t
  :hook
  (after-init . org-roam-mode)
  ;;:quelpa (org-roam :fetcher github :repo "jethrokuan/org-roam")
  ;;:straight (:host github :repo "jethrokuan/org-roam" :branch "develop")
  :config
  :custom
  (org-roam-directory (concat dropbox-home "/home/org/deft"))
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-graph-show))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))))

(use-package company-org-roam
  :ensure t
  :config
  (push 'company-org-roam company-backends))

;;
;; org-roam capture templates, specify default and by own custom one
;; which doesn't include the timestamp in the filename and uses showall
;; to expand all headings and sections in the doc by default.
;;
(setq org-roam-capture-templates
      '(("d" "default" plain (function org-roam--capture-get-point)
         "%?"
         :file-name "%<%Y%m%d%H%M%S>-${slug}"
         :head "#+TITLE: ${title}\n"
         :unnarrowed t)
        ;; My own custom template no date in file and add show all by default
        ("n" "DS Custom" plain (function org-roam--capture-get-point)
         "%?"
         :file-name "${slug}"
         :head "#+STARTUP: showall\n#+TITLE: ${title}\n\n"
         :unnarrowed t)))

;; Mood-lone mode - sort out by mode line
(use-package mood-line
  :ensure t
  :init
  (mood-line-mode))

(set-face-background 'mode-line "#000000")
(set-face-foreground 'mode-line "#FEFEFE")

;;
;;org-cliplink - copy links to orgmode docs
;;
(use-package org-cliplink
  :ensure t)

;;
;; org-download - download images from web browsers and file system,
;; drag and drop images into orgmode docs and copy files to local directory
;; near org doc.
;;
(use-package org-download
  :ensure t)

;;
;; use lsp-mode for scala, python etc.
;;
;; Enable nice rendering of diagnostics like compile errors.
(use-package flycheck
  :ensure t
;;  :config (setq flycheck-check-syntax-automatically nil)
  :init (global-flycheck-mode))


(use-package lsp-mode
  :ensure t
  ;; :load-path "~/projects/emacs/lsp-mode/"
  :init
  :config
  (setq lsp-prefer-capf  t
        completion-ignore-case t)
  :bind (:map lsp-mode-map
              ("TAB" . company-indent-or-complete-common)))

(use-package lsp-treemacs
  ;;:ensure t
  :config
  ;; temp for testing - normally requires autoload
  (require 'lsp-metals-treeview)
  (lsp-metals-treeview-enable nil)
  (setq lsp-metals-treeview-show-when-views-received t)

  ;;:load-path "~/projects/emacs/lsp-treemacs/"
  )

(use-package lsp-ui
  :ensure t
  :diminish
  :hook (lsp-mode . lsp-ui-mode)
  :init
  (setq lsp-ui-peek-enable t
        lsp-ui-sideline-enable t
        lsp-ui-imenu-enable t
        lsp-ui-doc-enable t
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-include-signature t)
  :config
  (lsp-ui-mode t)
  ;;:init (setq lsp-scala-server-command "~/utils/metals-emacs")
  )

(use-package markdown-mode
  :ensure t)

(use-package helm-lsp
  :ensure t)

;; dap for debugging via lsp servers
(use-package dap-mode
  :ensure t
  :defer t
  :custom
  (dap-mode 1)
  (dap-ui-mode 1)
  ;; enables mouse hover support
  (dap-tooltip-mode 1)
  ;; use tooltips for mouse hover
  ;; if it is not enabled `dap-mode' will use the minibuffer.
  (tooltip-mode 1))

;; TODO - temporarily removed dap-hydra - finding it difficult to
;; use this in conjunction with the dap-ui-repl - seems to want
;; to capture all key presses. For now we're defining a few common
;; debugging keys.
;; Show dap-hydra when breakpoint is hit
;; (add-hook 'dap-stopped-hook
;;           (lambda (arg) (call-interactively #'dap-hydra)))

;; define a set of basic keys to allow us simple debug navigation.
(define-key dap-mode-map (kbd "<f8>") 'dap-next)
(define-key dap-mode-map (kbd "<f7>") 'dap-step-in)
(define-key dap-mode-map (kbd "<f5>") 'dap-continue)
(define-key dap-mode-map (kbd "<f9>") 'dap-toggle-breakpoint)
(define-key dap-mode-map (kbd "<f6>") 'dap-disconnect)


;;
;; add our modules directory to loadpath
;;
(add-to-list 'load-path (concat(file-name-directory load-file-name) "modules"))

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
;; DS temporarily removed - testing lsp-mode
;; If lsp completion fails - consider using company-jedi:
;;  (prelude-require-packages '(elpy company-jedi))
;;
(require 'ds-python)

;; pyls appears to have some terrible performance problems and issues
;; - goto definition not working reliably, so for now use the Microsoft
;; server by default.
(with-eval-after-load 'lsp-pyls
  (ds-python-use-mspyls))


;; Render pdfs in docview at a higher resolution to improve font quality.
;; changing this variable may require you to call docview-clear-cache
(setq doc-view-resolution 300)

;;
;; Experimental
;; Configuring a different look for orgmode documents.
;;

;; (setq org-hide-emphasis-markers t)

;; (font-lock-add-keywords
;;  'org-mode
;;  '(("^ *\\([-]\\) "
;;     (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;; (use-package org-bullets
;;   :ensure t
;;   :config
;;   (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; (let* ((variable-tuple
;;         (cond ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
;;               ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
;;               ((x-list-fonts "Verdana")         '(:font "Verdana"))
;;               ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
;;               (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
;;        (base-font-color     (face-foreground 'default nil 'default))
;;        (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

;;   (custom-theme-set-faces
;;    'user
;;    `(org-level-8 ((t (,@headline ,@variable-tuple))))
;;    `(org-level-7 ((t (,@headline ,@variable-tuple))))
;;    `(org-level-6 ((t (,@headline ,@variable-tuple))))
;;    `(org-level-5 ((t (,@headline ,@variable-tuple))))
;;    `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
;;    `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.15))))
;;    `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.2))))
;;    `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.25))))
;;    `(org-document-title ((t (,@headline ,@variable-tuple :height 1.3 :underline nil))))))

;; (custom-theme-set-faces
;;  'user
;;  '(variable-pitch ((t (:family "Source Sans Pro" :height 180 :weight light))))
;;  '(fixed-pitch ((t ( :family "JetBrainsMono"
;;                              :slant normal :weight normal
;;                              :height 1.0 :width normal)))))
