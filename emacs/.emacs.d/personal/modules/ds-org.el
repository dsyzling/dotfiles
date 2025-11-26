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
(require 'all-the-icons)

;; Use to disable prompting to confirm running code block
;; Possible security issue - but I use this so much for research papers.
(setq org-confirm-babel-evaluate nil)

;; org-mode defaults.
(add-hook 'org-mode-hook
          '(lambda ()
             ;; spell check org mode buffers.
             (flyspell-mode t)
             (turn-on-auto-fill)
             (set-fill-column 80)
             (prettify-symbols-mode)
             ;; disable company mode for org mode - completions
             ;; in this mode can be annoying.
             (company-mode 0)))

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
(use-package calfw
  :config
  (require 'calfw)
  (require 'calfw-org)
  (setq cfw:org-overwrite-default-keybinding t)
  (setq cfw:render-line-breaker-wordwrap 'cfw:render-line-breaker-wordwrap)
  (require 'calfw-ical)
  :ensure t)

(use-package calfw-ical
  :config
  (require 'calfw-ical)
  :ensure t)

(use-package calfw-org
  :config
  (require 'calfw-org)
  :ensure t)


;; Note org-gcal-client-id and org-gcal-client-secret defined within ds-secrets.el
;; Currently generating an error - wrong number of arguments org-gcal/:catch
;; (use-package org-gcal
;;   :after org
;;   :config
;;   (setq org-gcal-file-alist '(("dsyzling@gmail.com" . "~/Sync/todo/gcal.org"))
;;         org-gcal-auto-archive nil)
;;   :ensure t)

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
;; org-modern for a modern look to org documents
;; 
(use-package org-modern
  :ensure t)
;; apply org-modern globally
(with-eval-after-load 'org (global-org-modern-mode))


(setq-default prettify-symbols-alist '(("#+BEGIN_SRC" . "†")
                                       ("#+END_SRC" . "†")
                                       ("#+begin_src" . "†")
                                       ("#+end_src" . "†")
                                       (">=" . "≥")
                                       ("=>" . "⇨")))
(setq prettify-symbols-unprettify-at-point 'right-edge)

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
  (org-roam-db-autosync-mode)
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

(require 'org)
(require 'ox-html)
(require 'base64)

;;
;; use ox-gfm so that we can export to markdown with
;; with github syntax for code blocks.
;;
(use-package ox-gfm)
(eval-after-load "org"
  '(require 'ox-gfm nil t))

;; citeproc required to allow us to use csl styles
;; with orgmode html export.
(use-package citeproc
  :ensure t)

;; part of orgmode define what processor to use for exporting citations.
(setq org-cite-export-processors
 '((md . (csl "chicago-fullnote-bibliography.csl"))       ; Footnote reliant
   (latex biblatex)                                       ; For pdf
   (odt . (csl "chicago-fullnote-bibliography.csl"))      ; Footnote reliant
   (t . (csl "chicago-fullnote-bibliography.csl"))))      ; Fallback

;;
;; Citations via completing read options.
;; C-c b to prompt for citations
;; add zotero export to ~/bib/references.bib
;;
(use-package citar
  :no-require
  :custom
  (org-cite-global-bibliography '("~/bib/references.bib"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)
  ;; optional: org-cite-insert is also bound to C-c C-x C-@
  :bind
  (:map org-mode-map :package org ("C-c b" . #'org-cite-insert)))

(setq citar-templates
      '((main . "${author editor:30%sn}     ${date year issued:4}     ${title:48}")
        (suffix . "          ${=key= id:15}    ${=type=:12}    ${tags keywords:*}")
        (preview . "${author editor:%etal} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
        (note . "Notes on ${author editor:%etal}, ${title}")))

;;
;; Indicators using all-the-icons
;;
(defvar citar-indicator-files-icons
  (citar-indicator-create
   :symbol (all-the-icons-faicon
            "file-o"
            :face 'all-the-icons-green
            :v-adjust -0.1)
   :function #'citar-has-files
   :padding "  " ; need this because the default padding is too low for these icons
   :tag "has:files"))

(defvar citar-indicator-links-icons
  (citar-indicator-create
   :symbol (all-the-icons-octicon
            "link"
            :face 'all-the-icons-orange
            :v-adjust 0.01)
   :function #'citar-has-links
   :padding "  "
   :tag "has:links"))

(defvar citar-indicator-notes-icons
  (citar-indicator-create
   :symbol (all-the-icons-material
            "speaker_notes"
            :face 'all-the-icons-blue
            :v-adjust -0.3)
   :function #'citar-has-notes
   :padding "  "
   :tag "has:notes"))

(defvar citar-indicator-cited-icons
  (citar-indicator-create
   :symbol (all-the-icons-faicon
            "circle-o"
            :face 'all-the-icon-green)
   :function #'citar-is-cited
   :padding "  "
   :tag "is:cited"))

;;
;; if we want to use nerd icons with citations for indicators - we can use the following:
;;
;; (defvar citar-indicator-notes-icons
;;   (citar-indicator-create
;;    :symbol (nerd-icons-mdicon
;;             "nf-md-notebook"
;;             :face 'nerd-icons-blue
;;             :v-adjust -0.3)
;;    :function #'citar-has-notes
;;    :padding "  "
;;    :tag "has:notes"))

;; (defvar citar-indicator-links-icons
;;   (citar-indicator-create
;;    :symbol (nerd-icons-octicon
;;             "nf-oct-link"
;;             :face 'nerd-icons-orange
;;             :v-adjust -0.1)
;;    :function #'citar-has-links
;;    :padding "  "
;;    :tag "has:links"))

;; (defvar citar-indicator-files-icons
;;   (citar-indicator-create
;;    :symbol (nerd-icons-faicon
;;             "nf-fa-file"
;;             :face 'nerd-icons-green
;;             :v-adjust -0.1)
;;    :function #'citar-has-files
;;    :padding "  "
;;    :tag "has:files"))

(setq citar-indicators
  (list citar-indicator-files-icons
        citar-indicator-notes-icons
        citar-indicator-links-icons))

;; not sure where zotero styles are but you can download
;; styles from the following repos
;; https://github.com/citation-style-language/styles
;; then refer to this directory or add the style file
;; to the same directory as the org file
;; #+CITE_EXPORT: csl chicago-author-date.csl
;; (org-cite-csl-styles-dir
;;  (expand-file-name "~/Documents/Zotero/styles/"))

;; Define our html premble to show author, date, title etc.
;; latex will automatically include these with various templates
;; when configired.
;; We should be ale to use %a for author name here and define
;; it within the org document. But for this to work we need
;; to update the arxiv.sty file to specify company name, title
;; and email address - possibly via affil
;;
(setq org-html-preamble-format
      '(("en" "<div style=\"margin: auto;text-align: center;\">
<em>Darren Syzling</em><br />
Senior Research and Development Engineer <br />
Wimmer Horizon LLP <br />
%e<br />
<p>
%d
</p>
</div>")))

;;
;; Register our own template function for exporting to html
;; for academic papers. This allows us to reorganise the preamble
;; after the title - so we can display name, company, email and
;; date after template. The default is to display this above the
;; the title and there's no way to modify the template or reorder.
;; ds-org-academic-paper-html-template
;;
;; C-c C-e a h
;; export 'academic papers' then h for html file.
;;
(defun ds/org-academic-paper-html-template (contents info)
  "Copied from ox-html.el - org-html-template, we've modified
the order of preamable to include it before after title and
before the body's contents.
Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   (when (and (not (org-html-html5-p info)) (org-html-xhtml-p info))
     (let* ((xml-declaration (plist-get info :html-xml-declaration))
	    (decl (or (and (stringp xml-declaration) xml-declaration)
		      (cdr (assoc (plist-get info :html-extension)
				  xml-declaration))
		      (cdr (assoc "html" xml-declaration))
		      "")))
       (when (not (or (not decl) (string= "" decl)))
	 (format "%s\n"
		 (format decl
			 (or (and org-html-coding-system
				  (fboundp 'coding-system-get)
				  (coding-system-get org-html-coding-system 'mime-charset))
			     "iso-8859-1"))))))
   (org-html-doctype info)
   "\n"
   (concat "<html"
	   (cond ((org-html-xhtml-p info)
		  (format
		   " xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"%s\" xml:lang=\"%s\""
		   (plist-get info :language) (plist-get info :language)))
		 ((org-html-html5-p info)
		  (format " lang=\"%s\"" (plist-get info :language))))
	   ">\n")
   "<head>\n"
   (org-html--build-meta-info info)
   (org-html--build-head info)
   (org-html--build-mathjax-config info)
   "</head>\n"
   "<body>\n"
   (let ((link-up (org-trim (plist-get info :html-link-up)))
	 (link-home (org-trim (plist-get info :html-link-home))))
     (unless (and (string= link-up "") (string= link-home ""))
       (format (plist-get info :html-home/up-format)
	       (or link-up link-home)
	       (or link-home link-up))))
   ;; Document contents.
   (let ((div (assq 'content (plist-get info :html-divs))))
     (format "<%s id=\"%s\" class=\"%s\">\n"
             (nth 1 div)
             (nth 2 div)
             (plist-get info :html-content-class)))
   ;; Document title.
   (when (plist-get info :with-title)
     (let ((title (and (plist-get info :with-title)
		       (plist-get info :title)))
	   (subtitle (plist-get info :subtitle))
	   (html5-fancy (org-html--html5-fancy-p info)))
       (when title
	 (format
	  (if html5-fancy
	      "<header>\n<h1 class=\"title\">%s</h1>\n%s</header>"
	    "<h1 class=\"title\">%s%s</h1>\n")
	  (org-export-data title info)
	  (if subtitle
	      (format
	       (if html5-fancy
		   "<p class=\"subtitle\" role=\"doc-subtitle\">%s</p>\n"
		 (concat "\n" (org-html-close-tag "br" nil info) "\n"
			 "<span class=\"subtitle\">%s</span>\n"))
	       (org-export-data subtitle info))
	    "")))))
   ;; Preamble.
   (org-html--build-pre/postamble 'preamble info)
   contents
   (format "</%s>\n" (nth 1 (assq 'content (plist-get info :html-divs))))
   ;; Postamble.
   (org-html--build-pre/postamble 'postamble info)
   ;; Possibly use the Klipse library live code blocks.
   (when (plist-get info :html-klipsify-src)
     (concat "<script>" (plist-get info :html-klipse-selection-script)
	     "</script><script src=\""
	     org-html-klipse-js
	     "\"></script><link rel=\"stylesheet\" type=\"text/css\" href=\""
	     org-html-klipse-css "\"/>"))
   ;; Closing document.
   "</body>\n</html>"))

(defun ds/org-academic-paper-html-export-to-html
    (&optional async subtreep visible-only body-only ext-plist)
  "Export our academic paper format to html - i.e.
call our custom html template function above by specifying
the type we're exporting - 'academic-paper-html which is
registered above"
  (interactive)
  (let* ((extension (concat
		     (when (> (length org-html-extension) 0) ".")
		     (or (plist-get ext-plist :html-extension)
			 org-html-extension
			 "html")))
	 (file (org-export-output-file-name extension subtreep))
	 (org-export-coding-system org-html-coding-system))
    (org-export-to-file 'academic-paper-html file
      async subtreep visible-only body-only ext-plist)))

;;
;; Register a new org export backend for html (derived from html) and include
;; a new menu option for exporting academic papers.
;; 
(org-export-define-derived-backend 'academic-paper-html 'html
  :translate-alist '((template . ds/org-academic-paper-html-template))
  :menu-entry
    '(?a "Export to Academic HTML Paper"
         ((?h "As HTML file" ds/org-academic-paper-html-export-to-html))))


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
      '("latexmk -f -pdf -%latex -shell-escape -interaction=nonstopmode -output-directory=%o %f"))


;;
;; Generate inline html images for exporting orgmode docs to html.
;; from:
;; https://emacs.stackexchange.com/questions/28781/export-to-single-html-file-like-reveal-single-filet-in-regular-html-export
;; TODO - update so that we can enable/disable this with a function.
;; default will be disabled.
;;

(defcustom org-html-image-base64-max-size #x40000
  "Export embedded base64 encoded images up to this size."
  :type 'number
  :group 'org-export-html)

(defun file-to-base64-string (file &optional image prefix postfix)
  "Transform binary file FILE into a base64-string prepending PREFIX and appending POSTFIX.
Puts \"data:image/%s;base64,\" with %s replaced by the image type before the actual image data if IMAGE is non-nil."
  (concat prefix
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents file nil nil nil t)
        (base64-encode-region (point-min) (point-max) 'no-line-break)
        (when image
          (goto-char (point-min))
          (insert (format "data:image/%s;base64," (image-type-from-file-name file))))
        (buffer-string))
      postfix))

(defun orgTZA-html-base64-encode-p (file)
  "Check whether FILE should be exported base64-encoded.
The return value is actually FILE with \"file://\" removed if it is a prefix of FILE."
  (when (and (stringp file)
             (string-match "\\`file://" file))
    (setq file (substring file (match-end 0))))
  (and
   (file-readable-p file)
   (let ((size (nth 7 (file-attributes file))))
     (<= size org-html-image-base64-max-size))
   file))

(defun orgTZA-html--format-image (source attributes info)
  "Return \"img\" tag with given SOURCE and ATTRIBUTES.
SOURCE is a string specifying the location of the image.
ATTRIBUTES is a plist, as returned by
`org-export-read-attribute'.  INFO is a plist used as
a communication channel."
  (if (string= "svg" (file-name-extension source))
      (org-html--svg-image source attributes info)
    (let* ((file (orgTZA-html-base64-encode-p source))
           (data (if file (file-to-base64-string file t)
                   source)))
      (org-html-close-tag
       "img"
       (org-html--make-attribute-string
        (org-combine-plists
         (list :src data
               :alt (if (string-match-p "^ltxpng/" source)
                        (org-html-encode-plain-text
                         (org-find-text-property-in-string 'org-latex-src source))
                      (file-name-nondirectory source)))
         attributes))
       info))))

(advice-add 'org-html--format-image :override #'orgTZA-html--format-image)

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
;; there are issues with these notification methods
;; they cause issues with org-agenda when hitting the tab key
;; marker-p nil periodically.
;;
;;
;; org-mode notification
;;
;; (use-package org-notifications
;;   :ensure t)
;; (setq alert-default-style 'libnotify
;;       alert-fade-time 60000)
;; (org-notifications-start)

;; (use-package org-alert
;;   :ensure t
;;   :config
;;   (setq alert-default-style 'libnotify))
;; (org-alert-enable)

(provide 'ds-org)
