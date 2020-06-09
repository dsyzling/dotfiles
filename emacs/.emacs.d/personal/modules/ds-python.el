;;; ds-python.el --- Personal Python Configuration for Emacs
;;
;; Author: Darren Syzling <dsyzling@gmail.com>
;; Keywords: python

;;; Commentary:

;;; Personal customised Python environment and modes.
;;; Based on prelude-python.el - by Bozhidar Batsov, however I do not want
;;; to use Anaconda mode and need to configure my environment for lsp and the
;;; the MS Python Language Server.

;;; Code:

(require 'electric)
(require 'prelude-programming)

;;
;; Eventually pyls (Palentir) will have its own mypy plugin internally.
;; In the meantime we use the pyls-mypy third party plugin (pip install pyls-mypy)
;; and control the activation with these variables.
;;
(defcustom lsp-pyls-plugins-mypy-enabled t
  "Enable or disable the plugin."
  :type 'boolean
  :group 'lsp-pyls
  :package-version '(lsp-mode . "6.3"))

(defcustom lsp-pyls-plugins-mypy-live-mode-enabled nil
  "Enable or disable the mypy live-mode. live_mode provides type checking as you type,
but have some limitation, 1) Imports cannot be followed correctly.
2) Stub files are not validated correctly. Turning off live_mode means you must save
your changes for mypy diagnostics to update correctly."
  :type 'boolean
  :group 'lsp-pyls
  :package-version '(lsp-mode . "6.3"))

(defcustom ds-python-use-conda t
  "Are we using conda to manage environments?"
  :type 'boolean)

;; For python and other languages let's remove trailing whitespace before we save.
(defun ds/python-mode-before-save-hook ()
  (when (eq major-mode 'python-mode)
    (delete-trailing-whitespace)))

(add-hook 'before-save-hook #'ds/python-mode-before-save-hook)

;; Copy pasted from ruby-mode.el
(defun prelude-python--encoding-comment-required-p ()
  (re-search-forward "[^\0-\177]" nil t))

(defun prelude-python--detect-encoding ()
  (let ((coding-system
         (or save-buffer-coding-system
             buffer-file-coding-system)))
    (if coding-system
        (symbol-name
         (or (coding-system-get coding-system 'mime-charset)
             (coding-system-change-eol-conversion coding-system nil)))
      "ascii-8bit")))

(defun prelude-python--insert-coding-comment (encoding)
  (let ((newlines (if (looking-at "^\\s *$") "\n" "\n\n")))
    (insert (format "# coding: %s" encoding) newlines)))

(defun prelude-python-mode-set-encoding ()
  "Insert a magic comment header with the proper encoding if necessary."
  (save-excursion
    (widen)
    (goto-char (point-min))
    (when (prelude-python--encoding-comment-required-p)
      (goto-char (point-min))
      (let ((coding-system (prelude-python--detect-encoding)))
        (when coding-system
          (if (looking-at "^#!") (beginning-of-line 2))
          (cond ((looking-at "\\s *#\\s *.*\\(en\\)?coding\\s *:\\s *\\([-a-z0-9_]*\\)")
                 ;; update existing encoding comment if necessary
                 (unless (string= (match-string 2) coding-system)
                   (goto-char (match-beginning 2))
                   (delete-region (point) (match-end 2))
                   (insert coding-system)))
                ((looking-at "\\s *#.*coding\\s *[:=]"))
                (t (prelude-python--insert-coding-comment coding-system)))
          (when (buffer-modified-p)
            (basic-save-buffer-1)))))))

(when (fboundp 'exec-path-from-shell-copy-env)
  (exec-path-from-shell-copy-env "PYTHONPATH"))

(defun ds-python-mode-defaults ()
  "Defaults for Python programming."
  (subword-mode +1)
  ;; (eldoc-mode 1)
  (setq-local electric-layout-rules
              '((?: . (lambda ()
                        (and (zerop (first (syntax-ppss)))
                             (python-info-statement-starts-block-p)
                             'after)))))
  (when (fboundp #'python-imenu-create-flat-index)
    (setq-local imenu-create-index-function
                #'python-imenu-create-flat-index))
  (add-hook 'post-self-insert-hook
            #'electric-layout-post-self-insert-function nil 'local)
  (add-hook 'after-save-hook 'prelude-python-mode-set-encoding nil 'local))

(setq ds-python-mode-hook 'ds-python-mode-defaults)

(add-hook 'python-mode-hook (lambda ()
                              (run-hooks 'ds-python-mode-hook)))

;; Using elpy with lsp-mode for our python
(use-package elpy
  :ensure t
  :config
  ;; Customise modules enabled for elpy - other behaviour will be
  ;; managed by lsp-mode.
  (setq elpy-modules '(elpy-module-sane-defaults
                       ;;elpy-module-company
                       ;;elpy-module-eldoc
                       ;;elpy-module-flymake
                       elpy-module-highlight-indentation
                       elpy-module-pyvenv
                       ;;elpy-module-yasnippet
                       ;;elpy-module-django
                       )
        ;; Use pytest by default
        elpy-test-runner 'elpy-test-pytest-runner)
  :init
  (elpy-enable))

;; Enable pylint and flake8, disable pyflakes.
(setq lsp-pyls-plugins-pylint-enabled nil
      lsp-pyls-plugins-flake8-enabled t
      lsp-pyls-plugins-pyflakes-enabled nil)

;;
;; Register our third party mypy plugin for pyls.
;;
(with-eval-after-load 'lsp-mode
  (lsp-register-custom-settings
   '(("pyls.plugins.pyls_mypy.enabled" lsp-pyls-plugins-mypy-enabled t)
     ("pyls.plugins.pyls_mypy.live_mode" lsp-pyls-plugins-mypy-live-mode-enabled nil))))

;;
;; When using Conda we'll update our environment variable so that pyvenv used by elpy
;; can find our virtual environments.
;;
(setenv "WORKON_HOME"
      (pcase system-type
        ('windows-nt "f:/util/miniconda3/envs")
        ('gnu/linux  "~/miniconda3/envs")
        ('darwin     "~/miniconda3/envs")))

;; use dap for debugging - this will make sure the dap run configurations
;; are available for python - pytest and main. 
(require 'dap-python)

;;
;; An example debug template showing how to invoke a script and pass arguments.
;;
;; (dap-register-debug-template
;;  "Python :: mypy"
;;  (list :type "python"
;;        :args "/home/dsyzling/projects/python/pandastest/tests/test_mypy.py"
;;        :cwd nil
;;        :program nil
;;        ;; :module "pytest"
;;        :target-module "/home/dsyzling/projects/python/mypy/mypy/__main__.py"
;;        :request "launch"
;;        :name "Python :: Run Configuration"))

;; Importmagic for automatic imports
(use-package importmagic
  :ensure t
  :config
  ;; Originally C-c C-l was mapped to importmagic-fix-imports, however this doesn't
  ;; support type hints very well. So for now use fix-symbol-at-point.
  (define-key importmagic-mode-map (kbd "C-c C-l") 'importmagic-fix-symbol-at-point)
  (add-hook 'python-mode-hook 'importmagic-mode))

;;
;; This is my older implementation of using the MS Python Server.
;;
;;(add-hook 'python-mode-hook 'lsp)
;; Linux laptop location for MS Python Server
;; (when (eq system-type 'gnu/linux)
;;   (setq ms-pyls-dir
;;         (expand-file-name "~/projects/python/python-language-server/output/bin/Release/")))

;;
;; Register Microsoft Python Language server for lsp
;;
;; (load "~/.emacs.d/personal/modules/lsp-ms-python.el")
;; (require 'lsp-ms-python)

;;
;; lsp-python-ms - implementation currently linked from the main lsp-mode project.
;;
;; Compiling the Microsoft Python language Server
;;  cd python-language-server/src/LanguageServer/Impl
;;  dotnet build -c Release
;;
(when (eq system-type 'gnu/linux)
  (setq lsp-python-ms-executable
        (expand-file-name "~/projects/python/python-language-server/output/bin/Release/Microsoft.Python.LanguageServer")))

(use-package lsp-python-ms
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp))))  ; or lsp-deferred

(defun ds-python-elpy-shell-send-region-or-buffer-and-step (&optional arg)
  "Send selected region or buffer to python interpreter and then remove/
deactive current selection. This is a wrapper around the function -
elpy-shell-send-region-or-buffer-and-step."
  (interactive "P")
  (elpy-shell-send-region-or-buffer-and-step arg)
  (deactivate-mark))

;;
;; Define key map for python mode with lsp
;;
(define-key elpy-mode-map (kbd "C-c C-f") 'helm-projectile-find-file)
(define-key elpy-mode-map (kbd "C-M-.")   'helm-lsp-workspace-symbol)
(define-key elpy-mode-map (kbd "C-c C-d") 'lsp-describe-thing-at-point)
(define-key elpy-mode-map (kbd "M-.")     'lsp-find-definition)
;; Apply code action - useful for automatic imports in mspyls
;; also see - lsp-ui-sideline-apply-code-actions
(define-key elpy-mode-map (kbd "M-RET")   'lsp-execute-code-action)
;; Override default elpy key binding to send selection or buffer to python interpreter.
;; I want to remove the selection after executing.
(define-key elpy-mode-map (kbd "C-c C-c") 'ds-python-elpy-shell-send-region-or-buffer-and-step)

;; Customise flycheck.
;; Conda on Windows has already moved to using python rather than python3
;; within environments, flycheck defaults to using python3 so we need to
;; customise linters.
(setq-default flycheck-python-flake8-executable "python")
(setq-default flycheck-python-pylint-executable "python")

;;
;; switch servers to use either Palentir (pyls) or the Microsoft Python
;; language server mspyls by updating the registered client priority.
;;

(defvar ds/lsp-flycheck-checkers (ht))

(defun ds-python-use-pyls ()
  "Override priority of registered python servers and use Palentir pyls by default."
  (interactive)
  (setf (lsp--client-priority (ht-get lsp-clients 'pyls)) 0)
  (setf (lsp--client-priority (ht-get lsp-clients 'mspyls)) -2)
  
  (ht-remove ds/lsp-flycheck-checkers 'python-mode))


(defun ds-python-use-mspyls ()
  "Override priority of registered python servers and use MS pyls by default."
  (interactive)
  (setf (lsp--client-priority (ht-get lsp-clients 'pyls)) -1)
  (setf (lsp--client-priority (ht-get lsp-clients 'mspyls)) 0)

  (ht-set ds/lsp-flycheck-checkers 'python-mode 'python-flake8)
  
  ;; setup chain of checkers - flake8 -> mypy -> lsp
  (setf (flycheck-checker-get 'python-flake8 'next-checkers) nil)
  (setf (flycheck-checker-get 'lsp 'next-checkers) nil)
  (flycheck-add-next-checker 'python-flake8 'lsp)
  (flycheck-add-next-checker 'python-flake8 'python-mypy)
  )

;;
;; Provide our own lsp-flycheck-enable function which allows us to
;; support the MS Python Language Server along with flake8 and mypy
;; running controlled by flycheck.
;; Lsp curretly assumes that the language server provides all linting and syntax
;; diagnostics. Pyls does this by adding/enabling plugins for flake8, pylint
;; etc. and then generating errors/warnings through lsp diagnostic messages.
;; mspyls doesn't do this expecting the client to add further linting.
;; We support this by modifying the flycheck checker chain and chaining our
;; linters first finishing with lsp to provide MS diagnostic messages.
;;
;; Switch to use mspyls with M-x ds-python-use-mspyls.
;; This will setup a flycheck chain:
;;  python-flake8 -> lsp -> python-mypy
;;
(with-eval-after-load 'lsp-mode
  (defun lsp-flycheck-enable (&rest _)
    "Enable flycheck integration for the current buffer."
    (flycheck-mode 1)
    (lsp-flycheck-add-mode major-mode)
    (add-to-list 'flycheck-checkers 'lsp)
    (setq-local flycheck-checker (ht-get ds/lsp-flycheck-checkers major-mode 'lsp))
    (add-hook 'lsp-after-diagnostics-hook #'lsp--flycheck-report nil t)))

;; TODO Testing a new version, we can remove this version when I know this works
;; reliably.
;;
;; (defun ds-python-run-script ()
;;   "Run the python script in the current buffer, output will be written to a compilation buffer."
;;   (interactive)
;;   (let* ((root (lsp-workspace-root))
;;          (current-file (expand-file-name (buffer-file-name)))
;;          (cmd (format "python -c \"import sys;import runpy;sys.path.append('%s');runpy.run_path('%s', run_name='__main__')\""
;;                       root current-file)))
;;     (compile cmd)))

(defun ds/quoteStr (v)
  (format "'%s'" v))

(defun ds/python-list (args)
  (mapconcat 'ds/quoteStr args ","))

(defun ds-python-run-script (script-file &rest args)
  "Run the given python script, output will be written to a compilation  
buffer. SCRIPT-FILE contains the python file name and optional ARGS which
will be passed on the command line."
  (let* ((root (lsp-workspace-root))
         (sysv-args (if args
                        (format ";sys.argv=[sys.argv[0], %s];" (ds/python-list args))
                      ";"))
         (cmd (format "python -c \"import sys;import runpy;sys.path.append('%s') %s runpy.run_path('%s', run_name='__main__')\""
                      root sysv-args script-file)))
    (compile cmd)))

(defun ds-python-run-current-buffer ()
  "Run the python script in the current buffer, output will be written to
a compilation buffer. The python script is executed with the fully qualified
path and the current directory is set to the workspace root directory and
restored after.  have encountered issues where the current directory
can contain modules with the same name as site-packages (mypy/types)."
  (interactive)
  (let* ((root (lsp-workspace-root))
         (current-file (expand-file-name (buffer-file-name)))
         (saved-dir default-directory))
    (cd root)
    (ds-python-run-script current-file)
    (cd saved-dir)))

;;
;; When using conda as an environment manager on Windows the directory
;; paths are not adjusted correctly by pyvenv (part of elpy).
;; Here we define the list of miniconda directories on Windows and
;; add them to the path if we're using conda.
;; Note: not sure if we need the following directories on Windows
;; in the path:
;;  Library/bin
;;  Library/mingw-w64
;;
(with-eval-after-load 'pyvenv
  (when (eq system-type 'windows-nt)
    (defun ds/miniconda--windows-directories (directory)
      "Creates a list of miniconda directories to be added to PATH
on Windows.  DIRECTORY is the top level miniconda environment directory."
      (let ((miniconda-dirs '("./"
                              "Library\\bin"
                              "Scripts")))
        (mapcar (lambda (path) (format "%s\\%s" directory path))
                miniconda-dirs)))

    (defun pyvenv-activate (directory)
      "Activate the virtual environment in DIRECTORY."
      (interactive (list (read-directory-name "Activate venv: " nil nil nil
                                              pyvenv-default-virtual-env-name)))
      (setq directory (expand-file-name directory))
      (pyvenv-deactivate)
      (setq pyvenv-virtual-env (file-name-as-directory directory)
            pyvenv-virtual-env-name (file-name-nondirectory
                                     (directory-file-name directory))
            python-shell-virtualenv-path directory
            python-shell-virtualenv-root directory)
      ;; Set venv name as parent directory for generic directories or for
      ;; the user's default venv name
      (when (or (member pyvenv-virtual-env-name '("venv" ".venv" "env" ".env"))
                (and pyvenv-default-virtual-env-name
                     (string= pyvenv-default-virtual-env-name
                              pyvenv-virtual-env-name)))
        (setq pyvenv-virtual-env-name
              (file-name-nondirectory
               (directory-file-name
                (file-name-directory
                 (directory-file-name directory))))))
      ;; Preserve variables from being overwritten.
      (let ((old-exec-path exec-path)
            (old-eshell-path eshell-path-env)
            (old-process-environment process-environment))
        (unwind-protect
            (pyvenv-run-virtualenvwrapper-hook "pre_activate" pyvenv-virtual-env)
          (setq exec-path old-exec-path
                eshell-path-env old-eshell-path
                process-environment old-process-environment)))
      (run-hooks 'pyvenv-pre-activate-hooks)
      (let ((new-directories (append
                              ;; Unix
                              (when (file-exists-p (format "%s/bin" directory))
                                (list (format "%s/bin" directory)))
                              ;; Windows
                              (when (file-exists-p (format "%s/Scripts" directory))
                                ;; For miniconda/anaconda add all required directories.
                                (if ds-python-use-conda
                                    (ds/miniconda--windows-directories directory)
                                  (list (format "%s/Scripts" directory)
                                        ;; Apparently, some virtualenv
                                        ;; versions on windows put the
                                        ;; python.exe in the virtualenv root
                                        ;; for some reason?
                                        directory))))))
        (setq pyvenv-old-exec-path exec-path
              pyvenv-old-eshell-path eshell-path-env
              pyvenv-old-process-environment process-environment
              ;; For some reason, Emacs adds some directories to `exec-path'
              ;; but not to `process-environment'?
              exec-path (append new-directories exec-path)
              ;; set eshell path to same as exec-path
              eshell-path-env (mapconcat 'identity exec-path ":")
              process-environment (append
                                   (list
                                    (format "VIRTUAL_ENV=%s" directory)
                                    (format "PATH=%s"
                                            (mapconcat 'identity
                                                       (append new-directories
                                                               (split-string (getenv "PATH")
                                                                             path-separator))
                                                       path-separator))
                                    ;; No "=" means to unset
                                    "PYTHONHOME")
                                   process-environment)
              ))
      (pyvenv-run-virtualenvwrapper-hook "post_activate")
      (run-hooks 'pyvenv-post-activate-hooks))))

;; eglot for Language server integration - alternative to lsp-mode
;; eglot probably functions better with the features of elpy.
;; (prelude-require-packages '(eglot))
;; (add-hook 'python-mode-hook 'eglot-ensure)
;; (eval-after-load 'eglot
;;   '(define-key eglot-mode-map (kbd "C-c C-d") 'eglot-help-at-point))

;; (add-hook 'lsp-mode-hook 'lsp-ui-mode)
;; (add-hook 'python-mode-hook 'flycheck-mode)

(provide 'ds-python)

;;; ds-python.el ends here
