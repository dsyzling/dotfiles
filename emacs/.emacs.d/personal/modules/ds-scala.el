;;; ds-scala.el --- Personal Scala Configuration for Emacs

;;
;;
;; Scala dev with Metals/lsp
;;

;; Author: Darren Syzling <dsyzling@gmail.com>
;; Keywords: scala, lsp, metals

;;;
;;; Code:

;;
;; Use bloop and bloop server with metals, for now we're using a
;; lisp package from, which we'll replace in time.
;;   https://github.com/tues/emacs-bloop
;;
(load "bloop.el")

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  ;; (setq sbt:program-options '("-Dsbt.supershell=false"))
  )

(use-package lsp-metals
  :ensure t
  :custom
  ;; You might set metals server options via -J arguments. This might not always work, for instance when
  ;; metals is installed using nix. In this case you can use JAVA_TOOL_OPTIONS environment variable.
  (lsp-metals-server-args '(;; Metals claims to support range formatting by default but it supports range
                            ;; formatting of multiline strings only. You might want to disable it so that
                            ;; emacs can use indentation provided by scala-mode.
                            "-J-Dmetals.allow-multiline-string-formatting=off"
                            ;; Enable unicode icons. But be warned that emacs might not render unicode
                            ;; correctly in all cases.
                            "-J-Dmetals.icons=unicode"))
  ;; In case you want semantic highlighting. This also has to be enabled in lsp-mode using
  ;; `lsp-semantic-tokens-enable' variable. Also you might want to disable highlighting of modifiers
  ;; setting `lsp-semantic-tokens-apply-modifiers' to `nil' because metals sends `abstract' modifier
  ;; which is mapped to `keyword' face.
  (lsp-metals-enable-semantic-highlighting t)
  :init
  :hook ((scala-mode scala-ts-mode) . lsp))


(defun ds-scala-run-tests-current-file ()
  "Run unit Scala tests in current Scala file."
  (interactive)
  (bloop-test-only))

(defun ds-scala-run-all-tests ()
  "Run all Scala unit tests in project."
  (interactive)
  (bloop-test))

;; We may also want to consider aligning forms:
;; (setq scala-indent:align-forms t)

;; https://github.com/hvesalai/emacs-scala-mode
(use-package scala-mode
  :interpreter
  ("scala" . scala-mode)
  :bind (:map scala-mode-map
               ("C-c C-t" . ds-scala-run-tests-current-file)))

;; (use-package scala-mode
;;   :ensure t
;;   :hook (scala-mode . lsp)
;;   ;; note for bloop-cli integrtion
;;   ;; :hook ((scala-mode . lsp)
;;   ;;        (scala-mode . bloop-cli-init))
;;   :config
;;   (setq scala-indent:align-parameters nil)
;;   :init
;;   ;; Bloop tries to compile the .#temp files used for interlock
;;   ;; so we disable interlocking - unfortunately this is global.
;;   (setq create-lockfiles nil)
;;   (when (eq system-type 'windows-nt)
;;     (setq lsp-scala--server-command "metals-emacs.bat"))
;;   :bind (:map scala-mode-map
;;               ("C-c C-t" . bloop-test-only)
;;               ("C-c C-c" . bloop-compile)
;;               ("C-c C-z" . ensime-inf-switch)
;;               ("C-c C-c" . ensime-inf-eval-region)
;;               ("RET"     . 'scala-mode-newline-comments)
;;               ))

;; When in comment blocks - return should automatically add an
;; asterisk and indent.
;; (defun scala-mode-newline-comments ()
;;   "Custom newline appropriate for `scala-mode'."
;;   ;; shouldn't this be in a post-insert hook?
;;   (interactive)
;;   (newline-and-indent)
;;   (scala-indent:insert-asterisk-on-multiline-comment))




;;(bind-key "RET" 'scala-mode-newline-comments scala-mode-map)


(provide 'ds-scala)
