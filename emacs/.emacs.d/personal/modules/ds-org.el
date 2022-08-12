;;; ds-org.el --- Personal org-mode Configuration for Emacs
;;
;; Author: Darren Syzling <dsyzling@gmail.com>
;; Keywords: org-mode, org

;;; Commentary:

;;; Customisation of org-mode and org super agenda.

;;; Code:

;;
;; org-mode customisation.
;;
(require 'org)

;; org-mode defaults.
(add-hook 'org-mode-hook
          '(lambda ()
             ;; spell check org mode buffers.
             (flyspell-mode t)
             (turn-on-auto-fill)
             (set-fill-column 80)
             (prettify-symbols-mode)))

;; org files will use utf-8 by default
(modify-coding-system-alist 'file "\.org\'" 'utf-8)

;; configure org-babel to allow execution for certain languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (emacs-lisp . t)
   (python . t)
   (R . t)
   ))

;;
;; org-super-agenda
;;
(use-package org-super-agenda
  :config
  (setq org-super-agenda-mode 1)
  :ensure t)

;; Activate org-super-agenda-mode - must call this explicitly otherwise super
;; agenda will not appear in our custom agenda.
(org-super-agenda-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode config
(setq org-agenda-files (list (expand-file-name "todo.org" todo-home)
                             (expand-file-name "garch.org" todo-home)
                             (expand-file-name "reading-list.org" todo-home)
                             (expand-file-name "gcal.org" todo-home)
                             (expand-file-name "to-watch.org" todo-home)))

(setq org-agenda-custom-commands
      '(("p" "Projects" tags "PROJECT" nil)
        ("d" "Day's Agenda" agenda ""
         ;; Define Agenda using org-super-agenda.
         ;; Create TODO and Reading sections.
         (
          (org-super-agenda-groups
           '(
             (:name "Today:"
                    :time-grid t
                    :order 5)
             (:name "Tasks:"
                    :and (:todo ("TODO"))
                    :order 1)
             (:name "Reading:"
                    :and (:tag ("Reading"))
                    :order 2)
             (:name "To Watch:"
                    :and (:tag ("Watch"))
                    :order 3)
             (:name "Study:"
                    :and (:tag ("Study"))
                    :order 4)

             ))
          (org-agenda-remove-tags t)
          (org-agenda-skip-timestamp-if-done t)
          (org-agenda-skip-deadline-if-done t)
          (org-agenda-start-day "+0d")
          (org-agenda-span 1)
          ;;(org-agenda-overriding-header " Calendar")
          ;;(org-agenda-prefix-format "   %i %?-2 t%s")
          (org-agenda-todo-keyword-format "")
          (org-agenda-time)
          (org-agenda-current-time-string "ᐊ┈┈┈┈┈┈┈ Now")
          ;; remove the Scheduled text in our agenda.
          (org-agenda-scheduled-leaders '("" ""))
          (org-agenda-deadline-leaders '("Deadline: " "Deadline: "))
          (org-agenda-time-grid (quote ((today require-timed remove-match) (0900 2100) "      " "┈┈┈┈┈┈┈┈┈┈┈┈┈")))
          ))
        ("w" "Waiting Tasks"  ((tags "WAITING")))
        ("r" "Studying Tasks/Research"
         ((tags-todo "study")
          ))
        ("c" "Completed Tasks" todo "DONE" nil)
        ("v" "21 day view" agenda "" ((org-agenda-span 21)))
        ("u" "Unscheduled tasks" todo "TODO"
         ((org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled
          'deadline))))))

;; (setq org-agenda-custom-commands
;;       '(("p" "Projects" tags "PROJECT" nil)
;;         ("d" "Day's Agenda" agenda ""
;;          ;; Define Agenda using org-super-agenda.
;;          ;; Create TODO and Reading sections.
;;          (
;;           (org-super-agenda-groups
;;            '(
;;              (:name "Today:"
;;                     :time-grid t
;;                     :order 4)
;;              (:name "Tasks:"
;;                     :and (:todo ("TODO"))
;;                     :order 1)
;;              (:name "Reading:"
;;                     :and (:tag ("Reading"))
;;                     :order 2)
;;              (:name "To Watch:"
;;                     :and (:tag ("Watch"))
;;                     :order 3)
;;              ))
;;           (org-agenda-remove-tags t)
;;           (org-agenda-skip-timestamp-if-done t)
;;           (org-agenda-skip-deadline-if-done t)
;;           (org-agenda-start-day "+0d")
;;           (org-agenda-span 1)
;;           ;;(org-agenda-overriding-header " Calendar")
;;           ;;(org-agenda-prefix-format "   %i %?-2 t%s")
;;           (org-agenda-todo-keyword-format "")
;;           (org-agenda-time)
;;           (org-agenda-current-time-string "ᐊ┈┈┈┈┈┈┈ Now")
;;           ;; remove the Scheduled text in our agenda.
;;           (org-agenda-scheduled-leaders '("" ""))
;;           (org-agenda-deadline-leaders '("Deadline: " "Deadline: "))
;;           (org-agenda-time-grid (quote ((today require-timed remove-match) (0900 2100) "      " "┈┈┈┈┈┈┈┈┈┈┈┈┈")))
;;           ))
;;         ("w" "Waiting Tasks"  ((tags "WAITING")))
;;         ("r" "Studying Tasks/Research"
;;          ((tags-todo "study")
;;           ))
;;         ("c" "Completed Tasks" todo "DONE" nil)
;;         ("v" "21 day view" agenda "" ((org-agenda-span 21)))
;;         ("u" "Unscheduled tasks" todo "TODO"
;;          ((org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline))))))



(setq org-log-done 'time)
(custom-set-variables
 ;; hide stars and indent note items automatically
 '(org-archive-location ds-org-archive)
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
;;(setq org-directory orgmode-home)
;;(setq org-refile-targets (quote (("personal.org" :level . 1))))

(defun labnotebook-file () (expand-file-name "notes/labnotebook.org" sync-home))
(defun visit-journal-file ()
  "Open lab notebook and journal file"
  (interactive)
  (find-file (labnotebook-file)))
(global-set-key (kbd "C-c J") 'visit-journal-file)
(global-set-key (kbd "C-c j") 'org-capture)

;; TODO - Decide what to do with refile targets.
;; (setq org-capture-templates
;;       `(
;;         ("j" "Journal" entry
;;          (file+datetree ,(concat orgmode-home "/labnotebook.org")) "** %^{Heading}\n%?")

;;         ("t" "ToDo" entry
;;          (file+headline ,(concat todo-home "/inbox.org") "Inbox") "** %?\n" :prepend t)

;;         ("l" "Link" plain
;;          (file ,(concat orgmode-home "/labnotebook.org")) "- %?\n %x\n")

;;         ("n" "Note-someday" entry
;;          (file+headline ,(concat orgmode-home "/labnotebook.org") "Someday") "\n\n** %?\n")
;;         ))

;; our org-remember templates
;; we can set our default org notes file - but we'll specify a template for todo items
;;(setq org-default-notes-file (concat org-directory "/notes.org"))
;; (setq org-remember-templates
;;       '(("Todo" ?t "* TODO %?\n  %i\n  %a" (concat orgmode-home "/newgtd.org") "Tasks")
;;         ("Articles" ?a "* %?\n  %i\n  %a" (concat orgmode-home "/newgtd.org") "Articles")
;;         ("Journal" ?j "\n* %^{topic} %T \n%i%?\n" (concat orgmode-home "/journal.org"))
;;         ("Someday" ?s "* %?\n  %i\n  %a" (concat orgmode-home "/newgtd.org") "Someday")))

;; allow us to include source code into org mode docs and fontify
(setq  org-src-fontify-natively t)

;; require us to surround an super/sub script expression with {} if we want
;; to see this in the document.
;; use x_{sub} and x^{super}
(setq org-use-sub-superscripts nil)


;;
;; calfw for calendar - integrate google calendar and org-mode schedules.
;;
(use-package calfw-ical
  :config
  (require 'calfw-ical)
  :ensure t)

(use-package calfw-org
  :config
  (require 'calfw-org)
  :ensure t)

(use-package calfw
  :config
  (require 'calfw)
  (require 'calfw-org)
  (setq cfw:org-overwrite-default-keybinding t)
  (setq cfw:render-line-breaker-wordwrap 'cfw:render-line-breaker-wordwrap)
  (require 'calfw-ical)
  :ensure t)

;; Note org-gcal-client-id and org-gcal-client-secret defined within ds-secrets.el
(use-package org-gcal
  :after org
  :config
  (setq org-gcal-file-alist '(("dsyzling@gmail.com" . "~/Sync/todo/gcal.org"))
        org-gcal-auto-archive nil)
  :ensure t)

(defun ds-org-agenda-redo-with-gcal-refresh ()
  "Refresh org-agenda fully - fetch google calendar events and then refresh agenda."
  (interactive)
  (org-gcal-fetch)
  (org-agenda-redo-all))

(defun ds-refresh-google-calendar ()
  (interactive)
  (org-gcal-request-token))

;; Redefine 'r' key on org-agenda to fetch google calendar events and update agenda.
(define-key org-agenda-mode-map (kbd "r") 'ds-org-agenda-redo-with-gcal-refresh)

;;
;; Wrapper around C-c C-c function for orgmode -
;; Check if we're in a code block and a region is highlighted,
;; if this code block is python then send it to ipython.
;; Otherwise call the original orgmode function to execute the
;; code block.
;; This allows us to prototype and execute code within code blocks
;; and see results in ipython.
;;
(defun ds-org-ctrl-c-ctrl-c ()
  "Execute python code region highlighted in orgmode file (send to ipython)
or if no region highlighted delegate to org-ctrl-c-ctrl-c"
  (interactive)
  (if-let* ((info (org-babel-get-src-block-info)))
      (let* ((language (car info)))
        (if (and (equal language "python")
                 (use-region-p))
            (ds-ipython-shell-send-region
             (region-beginning) (region-end) current-prefix-arg t)
          (org-ctrl-c-ctrl-c)))
    (org-ctrl-c-ctrl-c)))

;;
;; Wrapper for (org-add-note) - which is bound to C-c C-z
;; Allow us to open ipython when we're within a python source
;; code block.
;;
(defun ds-org-add-note ()
  "Open IPython if we're within a python source code and orgmode buffer.
Otherwise delegate to the default org-add-note."
  (interactive)
  (if-let* ((info (org-babel-get-src-block-info)))
      (let* ((language (car info)))
        (if (equal language "python")
            (elpy-shell-switch-to-shell)
          (org-add-note)))
    (org-add-note)))

;; Redefine C-c C-c for orgmode to call our wrapper above.
(define-key org-mode-map (kbd "C-c C-c") 'ds-org-ctrl-c-ctrl-c)
(define-key org-mode-map (kbd "C-c C-z") 'ds-org-add-note)

(defun ds-open-calendar ()
  "Open calfw calendar and show google calendar events and scheduled org-mode tasks."
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-source "Green")  ; orgmode source
    ;;(cfw:cal-create-source "Orange") ; diary source
    ;; (cfw:ical-create-source my/ical-source "gcal" "IndianRed")
    )))

;; Unicode characters for calfw calendar - a neater look.
(setq cfw:fchar-junction ?╋
      cfw:fchar-vertical-line ?┃
      cfw:fchar-horizontal-line ?━
      cfw:fchar-left-junction ?┣
      cfw:fchar-right-junction ?┫
      cfw:fchar-top-junction ?┯
      cfw:fchar-top-left-corner ?┏
      cfw:fchar-top-right-corner ?┓)


;;
;; Experimental
;; Configuring a different look for orgmode documents.
;;

;; (font-lock-add-keywords
;;  'org-mode
;;  '(("^ *\\([-]\\) "
;;     (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(setq org-hide-emphasis-markers t
      org-fontify-done-headline t
      org-hide-leading-stars t
      org-pretty-entities t
      org-odd-levels-only t)

(use-package org-bullets
  :custom
  (org-bullets-bullet-list '("◉" "☯" "○" "☯" "✸" "☯" "✿" "☯" "✜" "☯" "◆" "☯" "▶"))
  (org-ellipsis "⤵")
  :hook (org-mode . org-bullets-mode))

;; Show bullets instead of dashed list items.
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([+]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "◦"))))))

(setq-default prettify-symbols-alist '(("#+BEGIN_SRC" . "†")
                                       ("#+END_SRC" . "†")
                                       ("#+begin_src" . "†")
                                       ("#+end_src" . "†")
                                       (">=" . "≥")
                                       ("=>" . "⇨")))
(setq prettify-symbols-unprettify-at-point 'right-edge)


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

;;
;; Deft and Roam for note taking and searching
;;
(use-package deft
  :ensure t
  :commands (deft)
  :config
  (setq deft-directory (expand-file-name "deft" sync-home ))
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
;; Confirm that we have migrated to org-roam v2.0
;;
(setq org-roam-v2-ack t)

(use-package org-roam
  :ensure t
  :config
  (org-roam-setup)
  :custom
  (org-roam-directory (expand-file-name "deft" sync-home ))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)))

;; for org-roam-buffer-toggle
;; Recommendation in the official manual
(add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-direction)
               (direction . right)
               (window-width . 0.33)
               (window-height . fit-window-to-buffer)))

;;
;; org-roam capture templates, specify default and by own custom one
;; which doesn't include the timestamp in the filename.
;;
(setq org-roam-capture-templates
      '(("d" "default" plain "%?"
         :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                            "#+title: ${title}\n")
         :unnarrowed t)
        ;; My own custom template no date in file
        ("n" "DS Default" plain "%?"
         :if-new (file+head "${slug}.org"
                            "#+title: ${title}\n")
         :unnarrowed t)))

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
;; Formatting support for code in org html export
;;
(use-package htmlize
  :ensure t
  :after ox
  :config
  (setq org-html-htmlize-output-type 'css))

;;
;; Research papers - orgmode, babel, latex 
;;

;; Use minted to highlight source code
(setq org-latex-listings 'minted
      org-latex-packages-alist '(("" "minted")))

;; Our minted options.
(setq org-latex-minted-options      
      '(("breaklines" "true")
        ("tabsize" "4")
        ("autogobble")
        ("linenos")
        ("numbersep" "0.5cm")
        ("xleftmargin" "1cm")
        ("frame" "single")))
        
;; -shell-escape required for minted.
(setq org-latex-pdf-process
      '("latexmk -f -pdf -%latex -shell-escape -interaction=nonstopmode
-output-directory=%o %f"))


(provide 'ds-org)
