;;; ds-scala.el --- Personal Scala Configuration for Emacs

;;
;;
;; Scala dev with Metals/lsp
;;

;; Author: Darren Syzling <dsyzling@gmail.com>
;; Keywords: scala, lsp, metals

;;;
;;; Code:
(require 'seq)
(require 'lsp-mode)

;;
;; Use bloop and bloop server with metals, for now we're using a
;; lisp package from, which we'll replace in time.
;;   https://github.com/tues/emacs-bloop
;;
(load "bloop.el")

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false")))

;; Override default indenting rules allow parameters to indented/aligned
;; See the following for further options:
;;  http://ensime.github.io/editors/emacs/scala-mode/
;;
;; We may also want to consider aligning forms:
;; (setq scala-indent:align-forms t)
(use-package scala-mode
  :ensure t
  :hook (scala-mode . lsp)
  ;; note for bloop-cli integrtion
  ;; :hook ((scala-mode . lsp)
  ;;        (scala-mode . bloop-cli-init))
  :config
  (setq scala-indent:align-parameters nil)
  :init
  ;; Bloop tries to compile the .#temp files used for interlock
  ;; so we disable interlocking - unfortunately this is global.
  (setq create-lockfiles nil)
  (when (eq system-type 'windows-nt)
    (setq lsp-scala--server-command "metals-emacs.bat"))
  :bind (:map scala-mode-map
              ;;("C-c C-f" . helm-projectile-find-file)
              ("C-c C-f" . counsel-projectile-find-file)
              ("C-c C-t" . bloop-test-only)
              ;;("C-M-."   . helm-lsp-workspace-symbol)
              ("C-M-."   . lsp-ivy-workspace-symbol)
              ("C-c C-c" . bloop-compile)
              ("C-c C-z" . ensime-inf-switch)
              ("C-c C-c" . ensime-inf-eval-region)
              ("C-c C-d" . lsp-describe-thing-at-point)
              ("RET"     . 'scala-mode-newline-comments)
              ("M-RET"   . 'helm-lsp-code-actions)
              ("C-c C-r" . lsp-ui-peek-find-references)
              ))

;; Add metals backend for lsp-mode
(use-package lsp-metals
  :ensure t)

;; For Testing new metals versions and lsp-mode
;; For some reason I can't override the command line options using arguments and reset
;; So we just download a new version with the required command line options and remove
;; metals.client=emacs.
;;(setq lsp-metals-server-command "/home/dsyzling/utils/metals-emacs")
;;(setq lsp-metals-server-args '("-Dmetals.client=vscode" "-Dmetals.execute-client-command=on"))

;; When in comment blocks - return should automatically add an
;; asterisk and indent.
(defun scala-mode-newline-comments ()
  "Custom newline appropriate for `scala-mode'."
  ;; shouldn't this be in a post-insert hook?
  (interactive)
  (newline-and-indent)
  (scala-indent:insert-asterisk-on-multiline-comment))


;; Temp - leaving previous config here for reference until our new
;; lsp metals and scala config has been tested.

;; (use-package lsp-mode
;;   :demand t
;;   ;; Optional - enable lsp-scala automatically in scala files
;;   :hook (scala-mode . lsp)
;;   ;; note for bloop-cli integrtion
;;   ;; :hook ((scala-mode . lsp)
;;   ;;        (scala-mode . bloop-cli-init))
;;   :init
;;   ;; Bloop tries to compile the .#temp files used for interlock
;;   ;; so we disable interlocking - unfortunately this is global.
;;   (setq create-lockfiles nil)
;;   (when (eq system-type 'windows-nt)
;;     (setq lsp-scala--server-command "metals-emacs.bat"))
;;   :bind (:map scala-mode-map
;;               ("C-c C-f" . helm-projectile-find-file)
;;               ("C-c C-t" . bloop-test-only)
;;               ("C-M-."   . helm-lsp-workspace-symbol)
;;               ("C-c C-c" . bloop-compile)
;;               ("C-c C-z" . ensime-inf-switch)
;;               ("C-c C-c" . ensime-inf-eval-region)
;;               ("RET"     . 'scala-mode-newline-comments)

;;               ;; find a key for this
;;               ;;("M-?"     . lsp-ui-peek-find-references)
;;               )
;;   )

;;(bind-key "RET" 'scala-mode-newline-comments scala-mode-map)

;; (use-package lsp-scala
;;   :after scala-mode
;;   :demand t
;;   ;; Optional - enable lsp-scala automatically in scala files
;;   :hook (scala-mode . lsp)
;;   ;; note for bloop-cli integrtion
;;   ;; :hook ((scala-mode . lsp)
;;   ;;        (scala-mode . bloop-cli-init))
;;   :init
;;   ;; Bloop tries to compile the .#temp files used for interlock
;;   ;; so we disable interlocking - unfortunately this is global.
;;   (setq create-lockfiles nil)
;;   (when (eq system-type 'windows-nt)
;;     (setq lsp-scala--server-command "metals-emacs.bat"))
;;   :bind (:map scala-mode-map
;;               ("C-c C-f" . helm-projectile-find-file)
;;               ("C-c C-t" . bloop-test-only)
;;               ("C-M-."   . helm-lsp-workspace-symbol)
;;               ("C-c C-c" . bloop-compile)
;;               ("C-c C-z" . ensime-inf-switch)
;;               ("C-c C-c" . ensime-inf-eval-region)

;;               ;; find a key for this
;;               ;;("M-?"     . lsp-ui-peek-find-references)
;;               )
;;   )

;;
;; Use ensime-inf for providing an inferior process for the Scala repl
;;
(require 'ensime-inf)

;; Work in progress - query type at point

(defun lsp--scala-symbol-info-to-type (symbol)
  (let* ((location (gethash "location" symbol))
         (uri (gethash "uri" location))
         (container-name (gethash "containerName" symbol))
         (name (gethash "name" symbol))
         )
    ;; return plist with details
    `(name ,name namespace ,container-name path ,(lsp--uri-to-path uri))))


(defun lsp-scala-query-fully-qualified-name (type-name)
  "Return the fully qualified type at the current cursor location."
  (let* ((type-query type-name)
         (current-file (buffer-file-name))
         (symbol-results (lsp-request "workspace/symbol" `(:query ,type-query)))
         (symbol-list (seq-map #'lsp--scala-symbol-info-to-type symbol-results))
         )
    ;; filter results to match filename of current buffer
    (seq-filter (lambda (symbol-info)
                  (string= (plist-get symbol-info 'path) current-file))
                symbol-list)))


;; Testing
;; (progn
;;   (set-buffer "TestMonadTransformers.scala")
;;   ;;(seq-map #'lsp--scala-symbol-info-to-type tempr)
;;   (let ((fqdn-seq (lsp-scala-query-fully-qualified-name "TestMonadTransformers")))
;;     (let ((fqdn-name (car fqdn-seq)))
;;       (concat (plist-get fqdn-name 'namespace) (plist-get fqdn-name 'name))))
;;   ;;(lsp-scala-query-fully-qualified-name "TestMonadTransformers")
;;   ;;(lsp-request "workspace/symbol" `(:query "TestMonadTransformers"))
;;   )

(defun ensime-top-level-class-closest-to-point ()
  "Return the name of first class, trait or object enclosing the point,
or (if the point isn't inside a class definition) the class that follows
the point. Return nil if no class can be found."
  ;; TODO use an RPC call instead of this cheesy search
  (interactive)
  (cl-labels
      ((inside-string? () (nth 3 (syntax-ppss)))
       (name-of-top-level-class (&optional last-try)
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
                     (match-string 2)
                   (unless last-try
                     (name-of-top-level-class t)))))))))
    (let ((name (name-of-top-level-class)))
      (when name
        (let ((fqdn-seq (lsp-scala-query-fully-qualified-name name)))
          (let ((fqdn-name (car fqdn-seq)))
            (concat (plist-get fqdn-name 'namespace) (plist-get fqdn-name 'name))))
        ))))

(defun bloop-runMain ()
  "Run the main class defined within the current buffer - bloop run will be used
to perform the run action."
  (interactive)
  (let ((mainClass
         (ensime-top-level-class-closest-to-point)))
    (if mainClass
        (bloop--run-current-buffer mainClass)
      (cl-return "Could not find top-level class"))))

;;
;; You can explicitly run main classes in scala projects using the follow form:
;; arguments project-root project-name fully-qualified-main-class arguments
;;(bloop-run "~/projects/scala/language" "language-test"
;;           "language.TestMain" "test1 test2")


(provide 'ds-scala)
