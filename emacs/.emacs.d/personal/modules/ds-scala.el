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

;; Scala/ensime
;; http://ensime.github.io/editors/emacs/install/
;; setup our own keys for common operations to match python.
(use-package ensime
  :ensure t
  :config
  (setq ensime-sbt-perform-on-save "test:compile")
  :bind (:map ensime-mode-map
              ("C-c C-t" . ensime-sbt-do-test-only-dwim)
              ("C-c C-d" . ensime-show-doc-for-symbol-at-point)
              ("C-c C-f" . helm-projectile-find-file)
              ("C-c C-z" . ensime-inf-switch)
              ("C-c C-c" . ensime-inf-eval-region)
              ("C-u C-c C-c" . ensime-sbt-do-run)
              )
  :pin melpa)

(use-package sbt-mode
  :pin melpa)

(use-package scala-mode
  :pin melpa)

(require 'ensime-sbt)

;;
;; When activating an ensime project for the first time -
;; run M-x ensime-sbt. This will activate sbt and setup
;; the auto save hooks. If we run a unit test (which starts sbt)
;; the auto save hooks do not seem to be setup properly.
;;

(defun ds/ensime-sbt-ansi-color-workaround (&rest args)
  "https://github.com/ensime/emacs-sbt-mode/issues/150"
  (with-current-buffer (sbt:buffer-name)
    (remove-hook 'comint-output-filter-functions 'ensime-inf-postoutput-filter)
    (add-hook 'comint-output-filter-functions 'ensime-inf-postoutput-filter t)))

(advice-add 'ensime-sbt :after #'ds/ensime-sbt-ansi-color-workaround)


(provide 'ds-scala)
