;;; init.el --- My customisations for prelude
;;; Commentary:
;;; To merge upstream changes to prelude
;;;   git fetch upstream
;;;   git checkout master
;;;   git merge upstream/master

(setq ns-use-srgb-colorspace t)

;; add melpa stable to our package archives
(add-to-list 'package-archives
             '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)

;; disable prelude guru mode
(setq prelude-guru nil)

;; disable whitespace higlighting - long lines etc. prelude turns this on in text mode by default
(setq prelude-whitespace nil)

;; enable desktop save mode
;(desktop-save-mode 1)

(when (eq system-type 'windows-nt)
    (set-face-font 'default "-outline-Consolas-bold-r-normal-normal-15-112-96-96-c-*-iso8859-1")
  ;; use cygwin find on windows with grep and windows cmd console.
  ;;(setq grep-find-template "c:\\cygwin\\bin\\find . <X> -type f <F> -exec grep <C> -n <R> {} NUL \";\"")
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Linux customisations
(when (eq system-type 'gnu/linux)
  ;; (set-face-attribute 'default
  ;;                     nil
  ;;                     :font "Inconsolata"
  ;;                     :height 135
  ;;                     :weight 'bold)
  (set-face-attribute 'default
                      nil
                      :font "Menlo"
                      :height 120
                      :weight 'bold)
  
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
 '(font-lock-comment-face ((t (:italic t :foreground "#5F5A60")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode config

(setq org-agenda-files (list "~/Dropbox/home/org/todo/todo.org"
                             "~/Dropbox/home/org/todo/garch.org"
                             ))

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
 '(org-archive-location "~/Dropbox/home/org/archive/%s_archive::")
 '(org-startup-indented t)
 '(org-agenda-ndays 7)
 '(org-deadline-warning-days 14)
 ;;'(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-blank-before-new-entry nil)
 '(org-agenda-start-on-weekday nil)
 '(org-completion-use-ido t)
 '(org-agenda-window-setup 'current-window))

;; widen category field a little
(setq org-agenda-prefix-format "  %-17:c%?-12t% s")

;; return key follow hyperlink
(setq org-return-follows-link t)

(define-key global-map "\C-cr" 'org-remember)

;; refile targets
(setq org-directory "~/Dropbox/home/org")
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
      '(
        ("j" "Journal" entry (file+datetree
                              "~/Dropbox/home/org/labnotebook.org")
         "** %^{Heading}\n%?")
        
        ("t" "ToDo" entry (file+headline "~/Dropbox/home/org/todo/inbox.org" "Inbox") "** %?\n" :prepend t)
        
        ("l" "Link" plain (file "~/Dropbox/home/org/labnotebook.org")
         "- %?\n %x\n")

        ("n" "Note-someday" entry (file+headline "~/Dropbox/home/org/labnotebook.org" "Someday")
         "\n\n** %?\n")
        ))

;; our org-remember templates
;; we can set our default org notes file - but we'll specify a template for todo items
;;(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-remember-templates
      '(("Todo" ?t "* TODO %?\n  %i\n  %a" "~/Dropbox/home/org/newgtd.org" "Tasks")
	("Articles" ?a "* %?\n  %i\n  %a" "~/Dropbox/home/org/newgtd.org" "Articles")
        ("Journal" ?j "\n* %^{topic} %T \n%i%?\n" "~/Dropbox/home/org/journal.org")
	("Someday" ?s "* %?\n  %i\n  %a" "~/Dropbox/home/org/newgtd.org" "Someday")))

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


;; Cider/Clojure customisation


(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-mode-hook 'paredit-mode)

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
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)

;; clojure cheat sheet bind to meta-f1
(prelude-require-packages '(clojure-cheatsheet))
(add-hook 'clojure-mode-hook
          (lambda () (local-set-key [M-f1] 'clojure-cheatsheet)))

;; (define-key cider-mode-map (kbd "C-c r") 'cider-repl-reset)
;; (define-key cider-mode-map (kbd "C-c .") 'cider-reset-test-run-tests)


(prelude-require-packages '(use-package))

;; Scala/ensime
;; http://ensime.github.io/editors/emacs/install/
(use-package ensime
  :ensure t
  :pin melpa)

(use-package sbt-mode
  :pin melpa)

(use-package scala-mode
  :pin melpa)

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
(setq gnus-init-file "~/Dropbox/home/gnus/.gnus.el")
(setq gnus-startup-file "~/Dropbox/home/gnus/.newsrc")
