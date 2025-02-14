;;; ds-java.el --- Personal Java Configuration for Emacs

;;
;;
;; java dev with lsp-java
;;

;; Author: Darren Syzling <dsyzling@gmail.com>
;; Keywords: Java, lsp

;;;
;;; Code:

(defun ds-java-editing-defaults()
  "Set defaults for java mode."
  ;; (c-toggle-auto-newline 1)
  (setq c-default-style "k&r"
        c-basic-offset 4
        tab-width 4
        indent-tabs-mode nil
        ;; c-tab-always-indent t
        )
  ;; enable lens mode for running tests.
  ;;(lsp-jt-lens-mode 1)
  ;; (c-set-offset 'defun-block-intro '+)
  ;; (c-set-offset 'topmost-intro '+)
  ;; (c-set-offset 'substatement-open 0)
  
  )

;;
;; use lsp-java-debug-test-method to debug unit tests.
;;
(use-package lsp-java
  :ensure t
  :init
  (add-hook 'java-mode-hook 'lsp)
  (add-hook 'java-mode-hook 'ds-java-editing-defaults)
  :bind (:map java-mode-map
              ;; will run tests for the method selected or class
              ("C-c C-t" . dap-java-run-test-method)
              ("RET"     . 'newline-and-indent)
              ))

;; LSP slowness could be caused by slow JDT server, especially on large JAVA
;; projects. Bump up the heap size maybe a good idea. 
;; current VSCode defaults
(setq lsp-java-vmargs '("-XX:+UseParallelGC" "-XX:GCTimeRatio=4"
			"-XX:AdaptiveSizePolicyWeight=90"
			"-Dsun.zip.disableMemoryMapping=true"
			"-Xmx2G" "-Xms100m"))

;; I've experienced some issues with the Eclipse server - Bad Location and
;; Index out of bounds exceptions when performing simple editing. Completion
;; stops and throws IndexOutOfBounds - this may be related to formatting or
;; possibly running vscode in the background with the same project - and the
;; two may have been conflicting.
;;(setq lsp-java-format-enabled nil)

;;
;; Simple functions to run tests via Maven.
;; If the following invocations of maven cause problems add -B
;; to run in batch mode.
;; Use C-x <backtick> to jump to the next unit test error.
;; Consider adding running an individual unit test:
;;  mvn -q -Dtest=TestPerson#testPerson test
;;
;; projectile can also be used to run tests
;;  projectile has C-c p P    - projectile-test-project
;;
(defun ds-java-run-tests-current-buffer ()
  "Run unit tests in the current buffer using maven."
  (interactive)
  (let* ((root (lsp-workspace-root))
         (current-class (file-name-sans-extension (file-name-nondirectory
                                                   (buffer-file-name))))
         (cmd (format "mvn -Dtest=%s test" current-class))
         (saved-dir default-directory))
    (cd root)
    (compile cmd)
    (cd saved-dir)))

(defun ds-java-run-all-tests ()
  "Run all unit tests within the project workspace."
  (interactive)
  (let* ((root (lsp-workspace-root))
         (saved-dir default-directory))
    (cd root)
    (compile "mvn test")
    (cd saved-dir)))

(defun ds-java--exec-cmd (cmd)
  "Run the given command in a compile with comint buffer."
  (let* ((root (lsp-workspace-root))
         (saved-dir default-directory))
    (cd root)
    (compile cmd t)
    (cd saved-dir)))
;;
;; Uses Maven to execute a main entry point.
;; Uses the following plugin:
;;   http://www.mojohaus.org/exec-maven-plugin/usage.html
;;
(defun ds-java--exec-main-class (main-class exec-args)
  "Exec the main java class using maven with exec plugin."
  (ds-java--exec-cmd
   (format "mvn -q exec:java -Dexec.mainClass=\"%s\" -Dexec.args=\"%s\""
           main-class exec-args)))

;;
;; A test run config.
;;
(defun ds-java-run-test()
  "Execution test - work in progress"
  (interactive)
  (ds-java--exec-main-class "com.dsyzling.app.App" "Darren"))

;;
;; Run template without debug
;; Use via dap-debug and select configuration.
;;
(dap-register-debug-template "Java Run No Debug"
                             (list :type "java"
                                   :request "launch"
                                   :args ""
                                   :cwd nil
                                   :noDebug t
                                   :stopOnEntry :json-false
                                   :host "localhost"
                                   :request "launch"
                                   :modulePaths (vector)
                                   :classPaths nil
                                   :projectName nil
                                   :mainClass nil))

(defun ds-java-run ()
  "Run the current main method class without debugging enabled."
  (interactive)
  (dap-debug (list :type "java"
                   :request "launch"
                   :args ""
                   :cwd nil
                   :noDebug t
                   :stopOnEntry :json-false
                   :host "localhost"
                   :request "launch"
                   :modulePaths (vector)
                   :classPaths nil
                   :projectName nil
                   :mainClass nil)))


(provide 'ds-java)

;;; ds-java.el ends here
