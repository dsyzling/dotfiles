;;; ds-scala.el --- Personal Scala Configuration for Emacs
;;
;; Author: Darren Syzling <dsyzling@gmail.com>
;; Keywords: scala, ensime

;;; Commentary:
;;; Open a scala file within a project and execute
;;;  M-x ensime
;;;
;;; In order for auto compile on save to work run
;;;  M-x ensime-sbt
;;;
;;; To configure the files displayed with helm-projectile-find-file
;;; use the .gitignore file - so add the following to your .gitignore
;;;   .ensime
;;;   .ensime_cache
;;;   target
;;;   .idea
;;;
;;; Then initialise a git repository with git init.
;;
;;; The fix ds/ensime-sbt-ansi-color-workaround may be removed in future
;;; it was added to avoid ansi codes being displayed inside sbt mode.
;;;
;;; Code:


;; (use-package sbt-mode
;;   :pin melpa)

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

;; Override default indenting rules allow parameters to indented/aligned
;; See the following for further options:
;;  http://ensime.github.io/editors/emacs/scala-mode/
;;
;; We may also want to consider aligning forms:
;; (setq scala-indent:align-forms t)
(use-package scala-mode
  :config
  (setq scala-indent:align-parameters nil)
  ;; :pin melpa
  )

;; When in comment blocks - return should automatically add an
;; asterisk and indent.
(defun scala-mode-newline-comments ()
  "Custom newline appropriate for `scala-mode'."
  ;; shouldn't this be in a post-insert hook?
  (interactive)
  (newline-and-indent)
  (scala-indent:insert-asterisk-on-multiline-comment))

(bind-key "RET" 'scala-mode-newline-comments scala-mode-map)

;;
;; Scala dev with Metals/lsp
;;

;;
;; Use bloop and bloop server with metals, for now we're using a
;; lisp package from, which we'll replace in time.
;;   https://github.com/tues/emacs-bloop
;;
(load "bloop.el")

;; Enable nice rendering of diagnostics like compile errors.
(use-package flycheck
  :init (global-flycheck-mode))

(use-package lsp-mode
  :init (setq lsp-prefer-flymake nil))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  ;;:init (setq lsp-scala-server-command "~/utils/metals-emacs")
  )

(use-package lsp-scala
  :after scala-mode
  :demand t
  ;; Optional - enable lsp-scala automatically in scala files
  :hook (scala-mode . lsp)
  :init
  ;; Bloop tries to compile the .#temp files used for interlock
  ;; so we disable interlocking - unfortunately this is global.
  (setq create-lockfiles nil)
  :bind (:map scala-mode-map
              ("C-c C-f" . helm-projectile-find-file)
              ("C-c C-t" . bloop-test-only)
              ("C-M-."   . helm-lsp-workspace-symbol)
              ("C-c C-c" . bloop-compile)
              ("C-c C-z" . ensime-inf-switch)
              ("C-c C-c" . ensime-inf-eval-region)

              ;; find a key for this
              ;;("M-?"     . lsp-ui-peek-find-references)
              )
  )

;;
;; Use ensime-inf for providing an inferior process for the Scala repl
;;
(require 'ensime-inf)

(defun ensime-top-level-class-closest-to-point ()
  "Return the name of first class, trait or object enclosing the point,
or (if the point isn't inside a class definition) the class that follows
the point. Return nil if no class can be found."
  ;; TODO use an RPC call instead of this cheesy search
  (cl-labels
      ((inside-string? () (nth 3 (syntax-ppss)))
       (pos-of-top-level-class (&optional last-try)
         (save-excursion
           (save-restriction
             (widen)
             (while (inside-string?)
               (goto-char (1- (point))))
             (let ((top-level-sexp (point)))
               ;; Try to go up a sexp until we get an error
               (condition-case nil
                   (while t
                     (setq top-level-sexp (point))
                     (backward-up-list))
                 (error nil))
               (goto-char top-level-sexp)

               (re-search-backward "}\\|\\<object\\>\\|\\<class\\>\\|\\<trait\\>" nil t)
               (let ((class-re
                      (concat "\\<\\(object\\|class\\|trait\\)[ \t\n]+\\("
                              scala-syntax:id-re
                              "\\)")))
                 (if (re-search-forward class-re nil t)
                     (match-beginning 2)
                   (unless last-try
                     (pos-of-top-level-class t)))))))))
    (let ((pos (pos-of-top-level-class)))
      (when pos
        (save-excursion
          (goto-char pos)
          (replace-regexp-in-string
           "\\$$" ""
           (plist-get (ensime-rpc-get-type-at-point) :full-name)))))))

(defun bloop-run (mainClass)
  (let* ((root (bloop-find-root (buffer-file-name)))
         (project (bloop-current-project root))
         (project-name (car project)))
    (bloop-exec nil root "run" "--reporter" bloop-reporter "--main " mainClass project-name)))

(defun ensime-runMain-current-buffer ()
  (interactive)
  (let* ((impl-class
          (or (ensime-top-level-class-closest-to-point)
              (return (message "Could not find top-level class"))))
         (cleaned-class (replace-regexp-in-string "<empty>\\." "" impl-class)))
    (bloop-run cleaned-class)))

;;
;; Ensime for Scala - use this or metals/lsp
;;

;; Scala/ensime
;; http://ensime.github.io/editors/emacs/install/
;; setup our own keys for common operations to match python.
;; (use-package ensime
;;   :ensure t
;;   :config
;;   ;;(setq ensime-sbt-perform-on-save "test:compile")
;;   :bind (:map ensime-mode-map
;;               ("C-c C-t" . ensime-sbt-do-test-only-dwim)
;;               ("C-c C-d" . ensime-show-doc-for-symbol-at-point)
;;               ("C-c C-f" . helm-projectile-find-file)
;;               ("C-c C-z" . ensime-inf-switch)
;;               ("C-c C-c" . ensime-inf-eval-region)
;;               ("C-u C-c C-c" . ensime-sbt-do-run)
;;               )
;;   :pin melpa)

;; (require 'ensime-sbt)

;;
;; When activating an ensime project for the first time -
;; run M-x ensime-sbt. This will activate sbt and setup
;; the auto save hooks. If we run a unit test (which starts sbt)
;; the auto save hooks do not seem to be setup properly.
;;

;; (defun ds/ensime-sbt-ansi-color-workaround (&rest args)
;;   "https://github.com/ensime/emacs-sbt-mode/issues/150"
;;   (with-current-buffer (sbt:buffer-name)
;;     (remove-hook 'comint-output-filter-functions 'ensime-inf-postoutput-filter)
;;     (add-hook 'comint-output-filter-functions 'ensime-inf-postoutput-filter t)))

;; (advice-add 'ensime-sbt :after #'ds/ensime-sbt-ansi-color-workaround)

;;
;; Run the command App extended class within the current buffer
;; using sbt runMain.
;;

(defun ensime-sbt-runMain-current ()
  "Execute the sbt `runMain' command for the project and current
object extending App within the current source test file."
  (interactive)
  (let* ((impl-class
          (or (ensime-top-level-class-closest-to-point)
              (return (message "Could not find top-level class"))))
         (cleaned-class (replace-regexp-in-string "<empty>\\." "" impl-class))
         (command (concat "runMain" " " cleaned-class)))
    (sbt:command command))
  )

(provide 'ds-scala)
