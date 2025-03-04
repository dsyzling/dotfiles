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

;;
;; Eventually pyls (Palentir) will have its own mypy plugin internally.
;; In the meantime we use the pyls-mypy third party plugin (pip install pyls-mypy)
;; and control the activation with these variables.
;;
(require 'flycheck)

;; use Ruff for linting within emacs - use the lsp-ruff server.
(require 'lsp-ruff)

;; (setq-default lsp-ruff-show-notifications "always")


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

;; ;; Copy pasted from ruby-mode.el
;; (defun prelude-python--encoding-comment-required-p ()
;;   (re-search-forward "[^\0-\177]" nil t))

;; (defun prelude-python--detect-encoding ()
;;   (let ((coding-system
;;          (or save-buffer-coding-system
;;              buffer-file-coding-system)))
;;     (if coding-system
;;         (symbol-name
;;          (or (coding-system-get coding-system 'mime-charset)
;;              (coding-system-change-eol-conversion coding-system nil)))
;;       "ascii-8bit")))

;; (defun prelude-python--insert-coding-comment (encoding)
;;   (let ((newlines (if (looking-at "^\\s *$") "\n" "\n\n")))
;;     (insert (format "# coding: %s" encoding) newlines)))

;; (defun prelude-python-mode-set-encoding ()
;;   "Insert a magic comment header with the proper encoding if necessary."
;;   (save-excursion
;;     (widen)
;;     (goto-char (point-min))
;;     (when (prelude-python--encoding-comment-required-p)
;;       (goto-char (point-min))
;;       (let ((coding-system (prelude-python--detect-encoding)))
;;         (when coding-system
;;           (if (looking-at "^#!") (beginning-of-line 2))
;;           (cond ((looking-at "\\s *#\\s *.*\\(en\\)?coding\\s *:\\s *\\([-a-z0-9_]*\\)")
;;                  ;; update existing encoding comment if necessary
;;                  (unless (string= (match-string 2) coding-system)
;;                    (goto-char (match-beginning 2))
;;                    (delete-region (point) (match-end 2))
;;                    (insert coding-system)))
;;                 ((looking-at "\\s *#.*coding\\s *[:=]"))
;;                 (t (prelude-python--insert-coding-comment coding-system)))
;;           (when (buffer-modified-p)
;;             (basic-save-buffer-1)))))))

;; (when (fboundp 'exec-path-from-shell-copy-env)
;;   (exec-path-from-shell-copy-env "PYTHONPATH"))

;; (defun ds-python-mode-defaults ()
;;   "Defaults for Python programming."
;;   (subword-mode +1)
;;   ;; (eldoc-mode 1)
;;   (setq-local electric-layout-rules
;;               '((?: . (lambda ()
;;                         (and (zerop (first (syntax-ppss)))
;;                              (python-info-statement-starts-block-p)
;;                              'after)))))
;;   (when (fboundp #'python-imenu-create-flat-index)
;;     (setq-local imenu-create-index-function
;;                 #'python-imenu-create-flat-index))
;;   (add-hook 'post-self-insert-hook
;;             #'electric-layout-post-self-insert-function nil 'local)
;;   (add-hook 'after-save-hook 'prelude-python-mode-set-encoding nil 'local))

;; (setq ds-python-mode-hook 'ds-python-mode-defaults)

;; (add-hook 'python-mode-hook (lambda ()
;;                               (run-hooks 'ds-python-mode-hook)))

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
                       ;; elpy-module-highlight-indentation
                       elpy-module-pyvenv
                       ;;elpy-module-yasnippet
                       ;;elpy-module-django
                       )
        ;; Use pytest by default
        elpy-test-runner 'elpy-test-pytest-runner)
  :init
  (elpy-enable)
  ;; activate for treesitter python mode
  (add-hook 'python-ts-mode-hook 'elpy-mode))

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
        ;; ('gnu/linux  "~/miniconda3/envs)
        ('gnu/linux  "/home/dsyzling/.pyenv/versions")
        ('darwin     "~/miniconda3/envs")))


;; use dap for debugging - this will make sure the dap run configurations
;; are available for python - pytest and main. 
(require 'dap-python)

;;
;; Register our own debug provider and template to automatically add
;; workspace root to pythonpath before invoking the debugger.
;;
(defun dap-python-run-buffer-with-pythonpath (conf)
  "Define PYTHONPATH to use workspace root so package imports work
when debugging. Setup CONF to use standard python debugging and
override environment variables with our new PYTHONPATH."
  (dap-python--populate-start-file-args conf)
  (plist-put conf :environment-variables `(("PYTHONPATH" . ,(lsp-workspace-root))))
  conf)

(defun dap-python-run-buffer-as-module (conf)
  "Run the current file buffer as a python module for dap-debug
so that we can import modules from the current project."
  ;; populate conf
  (dap-python--populate-start-file-args conf)
  ;; extract file buffer script, convert to a file relative to
  ;; workspace root. Convert this file to a module - drop .py extension
  ;; and change path separators to '.'
  (let ((program (plist-get conf :program)))
    (plist-put conf :module
               (replace-regexp-in-string "/" "."
                                         (file-name-sans-extension
                                          (file-relative-name program (lsp-workspace-root)))))
    (plist-put conf :program nil)
    (plist-put conf :cwd (lsp-workspace-root))
    (plist-put conf :environment-variables
               `(("PYTHONPATH" . ,(lsp-workspace-root)))))
  ;; process our new module entry so debugger will be called correctly.
  (dap-python--populate-start-file-args conf)
  conf)

(dap-register-debug-provider "python-run-with-python-path" 'dap-python-run-buffer-as-module)
(dap-register-debug-template "DS Python :: Run file (buffer)"
                             (list :type "python-run-with-python-path"
                                   :args ""
                                   :cwd nil
                                   :module nil
                                   :program nil
                                   :request "launch"
                                   :name "DS Python :: Run file (buffer)"))
;;
;; Override dap-python.el definitions for python-test at point -
;; these are incorrectly defined type should be specified as
;; :type "python-test-at-point"
;;
(dap-register-debug-provider "python-test-at-point" 'dap-python--populate-test-at-point)
(dap-register-debug-template "Python :: Run pytest (at point)"
                             (list :type "python-test-at-point"
                                   :args ""
                                   :program nil
                                   :module "pytest"
                                   :request "launch"
                                   :name "Python :: Run pytest (at point)"))

;; Temporary fix to align output buffer for our debug template above.
;; Position the output window at the bottom of our screen.
(add-to-list 'display-buffer-alist
             `("DS Python"
               (display-buffer-reuse-window display-buffer-at-bottom)
               (window-width . 0.5)
               (window-height . 0.25)
               (reusable-frames)))

;; (with-eval-after-load 'lsp-mode
;;   (setq display-buffer-alist 
;;         (append display-buffer-alist 
;;                 `(("*DS Python*"
;;                    (display-buffer-reuse-window display-buffer-at-bottom)
;;                    (window-width . 0.5)
;;                    (window-height . 0.25)
;;                    (reusable-frames))))))

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
;; (use-package importmagic
;;   :ensure t
;;   :config
;;   ;; Originally C-c C-l was mapped to importmagic-fix-imports, however this doesn't
;;   ;; support type hints very well. So for now use fix-symbol-at-point.
;;   (define-key importmagic-mode-map (kbd "C-c C-l") 'importmagic-fix-symbol-at-point)
;;   (add-hook 'python-mode-hook 'importmagic-mode))

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
;; (when (eq system-type 'gnu/linux)
;;   (setq lsp-python-ms-executable
;;         (expand-file-name "~/projects/python/python-language-server/output/bin/Release/Microsoft.Python.LanguageServer")))

;; (use-package lsp-python-ms
;;   :ensure t
;;   :hook (python-mode . (lambda ()
;;                          (require 'lsp-python-ms)
;;                          (lsp))))  ; or lsp-deferred

;;
;; Use pyright - replacement for Microsoft Python Language Server.
;;
(use-package lsp-pyright
  :ensure t
  :config
  :custom (lsp-pyright-langserver-command "pyright") ;; pyright or basedpyright
  (setq lsp-pyright-disable-language-services nil
        lsp-pyright-disable-organize-imports nil
        lsp-pyright-auto-import-completions t
        ;; This is required to get completion/docs in libraries 
        lsp-pyright-use-library-code-for-types t
        lsp-pyright-multi-root nil
        ;; off basic or strict - basic is the default.
        ;; lsp-pyright-typechecking-mode "strict"
        )
  ;; Ensure if we're running mypy that generated files do not cause change
  ;; notifications otherwise this leads to recurring diagnostic checks.
  (push "[/\\\\]\\.mypy_cache\\'" lsp-file-watch-ignored-directories)
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred)))
  :hook (python-ts-mode . (lambda ()
                            (require 'lsp-pyright)
                            (lsp-deferred)))
  )

;;
;; Emacs interface for Python poetry.
;;
(use-package poetry
  :ensure t)

;;
;; isort for sorting imports
;; use py-isort-buffer
;; or (add-hook 'before-save-hook 'py-isort-before-save)
;;
(use-package py-isort
  :ensure t)

;;
;; Support for black for formattting.
;;
;; Requires black macchiato python package for
;; region formatting.

(use-package python-black
  :demand t
  :after python
  ;; :hook (python-mode . python-black-on-save-mode-enable-dwim)
  )

;;
;; numpydoc mode - Python doc strings using numpy/google formatting.
;; https://github.com/douglasdavis/numpydoc.el
;; https://google.github.io/styleguide/pyguide.html#doc-function-raises
;; 
(use-package numpydoc
  :ensure t
  :bind (:map python-mode-map
              ("C-c M-d" . numpydoc-generate)))

;;
;; uv mode for auto activating virtual envs with .venv directories.
;;   https://github.com/z80dev/uv-mode
;;
;; Currently we have deactivated uv-mode (or not activated it)
;; since it makes changes to the keymap which are not reversed
;; when using another mode - i.e. orgmode-map
;;
(use-package uv-mode
  :hook (python-mode . ds-uv-mode-auto-activate-hook))
;; Apply to treesitter python mode.
(use-package uv-mode
  :hook (python-ts-mode . ds-uv-mode-auto-activate-hook))

(require 'uv-mode)
(defun ds-uv-mode-auto-activate-hook ()
  "Automatically activate UV mode and the project's virtualenv if available.
This is my custom version using uv-mode, we extend this to optionally 
execute a project.el file in the project root directory to set 
environment variables etc."
  (when (or (derived-mode-p 'python-mode) (derived-mode-p 'python-ts-mode))
    (let ((project-root (uv-mode-root)))
      (when project-root
        ;; uv-mode setting env with pythonic doesn't appear to
        ;; work for me and current lsp-mode setup.
        (pyvenv-activate (concat project-root ".venv"))
        ;; For now do not activate uv-mode since this conflicts with
        ;; orgmode-map keys - C-c C-s - schedule task.
        ;; (uv-mode 1)                     ; Enable uv-mode
        ;; (uv-mode-set)
        ;; if there's a custom Emacs project file load it. 
        (let* ((proj-file (concat project-root ".project.el")))
          (if (file-exists-p proj-file)
              (load proj-file)))))))

;;
;; Sphinx-doc mode.
;; bound to C-c M-d
;;
;; (use-package sphinx-doc
;;   :demand t
;;   :after python
;;   :init (add-hook 'python-mode-hook #'sphinx-doc-mode)
;;   (add-hook 'python-ts-mode-hook #'sphinx-doc-mode)
;;   :config
;;   (setq sphinx-doc-include-types t))

;; Consider py-vterm in future requires
;; emacs-libvterm to be compiled and installed
;;   https://github.com/akermu/emacs-libvterm
;; (use-package py-vterm-interaction
;;   :hook (python-mode . py-vterm-interaction-mode)
;;   :config
;;   ;;; Suggested:
;;   (setq-default py-vterm-interaction-repl-program "ipython")
;;   ;; (setq-default py-vterm-interaction-silent-cells t)
;;   )

(defun ds-python-elpy-shell-send-region-or-buffer-and-step (&optional arg)
  "Send selected region or buffer to python interpreter and then remove/
deactive current selection. This is a wrapper around the function -
elpy-shell-send-region-or-buffer-and-step."
  (interactive "P")
  (elpy-shell-send-region-or-buffer-and-step arg)
  (deactivate-mark))

;;
;; pretty print objects in python repl.
;; Requires adding a definition of ppretty to ipython_config.py file.
;; c.InteractiveShellApp.exec_lines = [
;;     "%autoreload 2",
;;     """
;;     from ppretty import ppretty as ppretty_temp
;;     def ppretty(obj):
;;         print(ppretty_temp(obj, seq_length=99, show_properties=True, depth=3), end='')
;;     """
;; ] 
;;
(defun ds/print-python-object-fields-in-repl ()
  "Sends symbol at point to IPython REPL with the `ppretty' function defined in ipython_config.
Lists the object's non-method fields and their respective current values."
  (interactive)
  (let ((sym (symbol-at-point)))
    (python-shell-send-string
     (format "print(); print('=> %s'); ppretty(%s)" sym sym))))

;;
;; Define key map for python mode with lsp - we need to override some keys
;; that are currently mapped by elpy.
;;
(define-key elpy-mode-map (kbd "C-c C-d") 'lsp-describe-thing-at-point)
(define-key elpy-mode-map (kbd "C-c C-r") 'lsp-ui-peek-find-references)

;; Override default elpy key binding to send selection or buffer to python interpreter.
;; I want to remove the selection after executing.
(define-key elpy-mode-map (kbd "C-c C-c") 'ds-python-elpy-shell-send-region-or-buffer-and-step)
(define-key elpy-mode-map (kbd "C-c C-o") 'ds/print-python-object-fields-in-repl)

;; Also note the following keys to eval python for repl driven development
;;
;; | C-c C-z   | open or go to oppen repl                                             |
;; | C-c C-c   | eval buffer or region selection                                      |
;; | C-c C-y e | eval current statement - useful to eval current line or function     |
;; | C-c C-y s | eval top statement or enclosing scope - useful for function or class |

;;
;; Using python-ruff after lsp diagnostic for flycheck.
;;

;; Customise flycheck.
;; Conda on Windows has already moved to using python rather than python3
;; within environments, flycheck defaults to using python3 so we need to
;; customise linters.
;; (setq-default flycheck-python-flake8-executable "python")
;; (setq-default flycheck-python-pylint-executable "python")

;; Customise flycheck checkers used within specific language modes.
;; For python when using pyright we'll chain flake8 and mypy.
;;TODO - move flycheck-local-cache and functions to a common area so
;; leaving these functions examples in case we need them in future.
;; (add-to-list 'flycheck-disabled-checkers 'python-pylint)
;; (add-to-list 'flycheck-disabled-checkers 'python-pycompile)
;;(add-to-list 'flycheck-disabled-checkers 'python-mypy)
;; (setq my/flycheck-local-cache
;;       '((python-pylint . ((next-checkers . ())))))
;; (setf (flycheck-checker-get 'python-flake8 'next-checkers) nil)
;; (setq my/flycheck-local-cache
;;       '((lsp . ((next-checkers . (python-flake8 python-mypy))))))
;; (setf (flycheck-checker-get 'python-flake8 'next-checkers) '(python-mypy))
;; (setf (flycheck-checker-get 'python-flake8 'next-checkers) nil)
;; we can use this approach in multiple language modes.
;; from https://github.com/flycheck/flycheck/issues/1762
(defvar-local my/flycheck-local-cache nil)

(defun my/flycheck-checker-get (fn checker property)
  (or (alist-get property (alist-get checker my/flycheck-local-cache))
      (funcall fn checker property))) 

(advice-add 'flycheck-checker-get :around 'my/flycheck-checker-get)

(add-hook 'lsp-managed-mode-hook
          (lambda ()
            (when (or  (derived-mode-p 'python-ts-mode) (derived-mode-p 'python-mode))
              (setq-local flycheck-python-mypy-config "pyproject.toml")
              (setq my/flycheck-local-cache
                    '((lsp . ((next-checkers . (python-ruff))))))
              )))


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
  (setf (lsp--client-priority (ht-get lsp-clients 'mspyls)) 0))

;;
;; Provide our own lsp-flycheck-add-mode function which allows us to
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
;; (with-eval-after-load 'lsp-mode
;;   (defun lsp-flycheck-add-mode (mode)
;;     "Register flycheck support for MODE."
;;     (unless (flycheck-checker-supports-major-mode-p 'lsp mode)
;;       (flycheck-add-mode 'lsp mode))
;;     (when (eq mode 'python-mode)
;;       ;; setup chain of checkers - flake8 -> mypy -> lsp
;;       (setf (flycheck-checker-get 'python-flake8 'next-checkers) nil)
;;       (setf (flycheck-checker-get 'lsp 'next-checkers) nil)
;;       (flycheck-add-next-checker 'python-flake8 'lsp)
;;       (flycheck-add-next-checker 'python-flake8 'python-mypy)
;;       (setq-local flycheck-checker 'python-flake8))
;; ))

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
                        (format ";sys.argv=['%s', %s];" script-file (ds/python-list args))
                      ";"))
         (cmd (format "python -c \"import sys;import runpy;sys.path.append('%s') %s runpy.run_path('%s', run_name='__main__')\""
                      root sysv-args script-file)))
    (compile cmd)))

(defun ds-python-run-current-buffer ()
  "Run the python script in the current buffer, output will be written to
a compilation buffer. The python script is executed with the fully qualified
path and the current directory is set to the workspace root directory and
restored after.  have encountered issues where the current directory
can contain modules with the same name as site-packages (mypy/types).
Name the buffer based on the file name of the script being run."
  (interactive)
  (let* ((root (lsp-workspace-root))
         (current-path (expand-file-name (buffer-file-name)))
         (current-file (file-name-nondirectory current-path))
         (new-buffer-name (format "*%s*" current-file))
         (saved-dir default-directory))
    (cd root)
    (when (get-buffer new-buffer-name)
      (kill-buffer new-buffer-name))
    (ds-python-run-script current-path)
    (with-current-buffer "*compilation*"
      (rename-buffer new-buffer-name))
    (cd saved-dir)))

(defun ds-python-profile-script (script-file &rest args)
  "Profile the given python script, output will be written to a compilation  
buffer. SCRIPT-FILE contains the python file name and optional ARGS which
will be passed on the command line."
  (let* ((root (lsp-workspace-root))
         (cmd (format "python -m cProfile -s cumtime %s" script-file)))
    (ds-python-run-command cmd "./" (format "*profile %s*" script-file))))

(defun ds-python-profile-current-buffer ()
  "Profile the python script in the current buffer, output will be written to
a compilation buffer. The python script is executed with the fully qualified
path and the current directory is set to the workspace root directory and
restored after.  have encountered issues where the current directory
can contain modules with the same name as site-packages (mypy/types).
Name the buffer based on the file name of the script being run."
  (interactive)
  (let* ((root (lsp-workspace-root))
         (current-path (expand-file-name (buffer-file-name)))
         (current-file (file-name-nondirectory current-path))
         (new-buffer-name (format "*%s*" current-file))
         (saved-dir default-directory))
    (cd root)
    (when (get-buffer new-buffer-name)
      (kill-buffer new-buffer-name))
    (ds-python-profile-script current-path)
    (with-current-buffer "*compilation*"
      (rename-buffer new-buffer-name))
    (cd saved-dir)))

(defun ds-python-line-profile-script (script-file &rest args)
  "Profile the given python script, output will be written to a compilation  
buffer. SCRIPT-FILE contains the python file name and optional ARGS which
will be passed on the command line."
  (let* ((root (lsp-workspace-root))
         (cmd (format "kernprof -l %s; python -m line_profiler %s.lprof"
                      script-file
                      (file-name-nondirectory script-file))))
    (ds-python-run-command cmd "./" (format "*profile %s*" script-file))))

(defun ds-python-line-profile-current-buffer ()
  "Use line_profiler to profile the script in the current buffer, output will be written to
a compilation buffer. The python script is executed with the fully qualified
path and the current directory is set to the workspace root directory and
restored after.  have encountered issues where the current directory
can contain modules with the same name as site-packages (mypy/types).
Name the buffer based on the file name of the script being run."
  (interactive)
  (let* ((root (lsp-workspace-root))
         (current-path (expand-file-name (buffer-file-name)))
         (current-file (file-name-nondirectory current-path))
         (new-buffer-name (format "*%s*" current-file))
         (saved-dir default-directory))
    (cd root)
    (when (get-buffer new-buffer-name)
      (kill-buffer new-buffer-name))
    (ds-python-line-profile-script current-path)
    (with-current-buffer "*compilation*"
      (rename-buffer new-buffer-name))
    (cd saved-dir)))

(defun ds-python-run-command (cmd working-dir new-buffer-name)
  "Run the command CMD in the given WORKING-DIR relative to the top level
workspace directory. This function ensures PYTHONPATH is updated with
the top level project directory so modules can be imported. Rename the
buffer to NEW_BUFFER-NAME after the process has started"
  (interactive)
  (let* ((root (lsp-workspace-root))
         (saved-dir default-directory)
         (run-dir (concat (file-name-as-directory root) working-dir))
         (run-cmd (format "export PYTHONPATH=$PYTHONPATH:%s; %s" root cmd)))
    (cd run-dir)
    (when (get-buffer new-buffer-name)
      (kill-buffer new-buffer-name))
    (compile run-cmd)
    (with-current-buffer "*compilation*"
      (rename-buffer new-buffer-name))
    (cd saved-dir)))

(defun pytrader-stats ()
  "Launch Pytrader Bokeh stats server for experiments.
If pytrader-stats is running close the buffer/process and restart."
  (interactive)
  (when (get-buffer "*pytrader-stats*")
    (let ((kill-buffer-query-functions nil))
      (kill-buffer "*pytrader-stats*")))
  (ds-python-run-command
   "python -m bokeh serve experiments stats markets" "pytrader/plot" "*pytrader-stats*"))

(defun pytrader-stats-server ()
  "Launch Pytrader Bokeh stats server for experiments.
If pytrader-stats is running close the buffer/process and restart."
  (interactive)
  (when (get-buffer "*pytrader-stats-server*")
    (let ((kill-buffer-query-functions nil))
      (kill-buffer "*pytrader-stats-server*")))
  (ds-python-run-command
   "pytrader_stats_server" "./" "*pytrader-stats-server*"))

(defun debug-pytrader-stats-server ()
  "Debug Pytrader Bokeh stats server for experiments.
If pytrader-stats-server is running close the buffer/process and restart."
  (interactive)
  (when (get-buffer "*debug-pytrader-stats-server*")
    (let ((kill-buffer-query-functions nil))
      (kill-buffer "*debug-pytrader-stats-server*")))
  ;; must be run from the root workspace - has issues
  ;; finding types imported from ./experiments.
  (let* ((root (lsp-workspace-root))
         (saved-dir default-directory))
    (cd root)
    (ds-python-run-script "pytrader/plot/server.py" "--debug")
    ;; Can't get this to work using debugpy wrapper module - causes
    ;; issues trying to dynamically load experiments.trend_follower module.
    ;; (ds-python-run-command
    ;;  "python -m debugpy --listen localhost:5678 --wait-for-client pytrader/plot/server.py" "./" "*debug-pytrader-stats-server*")
    (dap-debug (list :name "Python Attach"
                     :type "python"
                     :request "attach"
                     :port 5678
                     :host "localhost"))
    (cd saved-dir)))


(defun ds-uv-project (directory)
  "Switch and activate a uv managed project. Prompts for a project directory
and runs pyvenv-activate on the .venv directory within the directory. 
Optionally you can add a .project.el to set environment variables for 
the project."
  (interactive (list (read-directory-name "Project directory")))
  (let* ((proj-file (concat directory ".project.el")))
    (pyvenv-activate (concat directory ".venv"))
    (if (file-exists-p proj-file)
        (load proj-file))))

;; (defun debug-pytrader-stats ()
;;   "Debug launch Pytrader Bokeh stats server for experiments.
;; if pytrader-stats is running close the buffer/process and restart"
;;   (interactive)
;;   (when (get-buffer "*pytrader-stats*")
;;     (let ((kill-buffer-query-functions nil))
;;     (kill-buffer "*pytrader-stats*")))
;;   (ds-python-run-command
;;    "python -m debugpy --listen localhost:5678 --wait-for-client -m bokeh serve experiments stats markets" "pytrader/plot" "*debug-pytrader-stats*")
;;   (dap-debug (list :name "Python Attach"
;;                    :type "python"
;;                    :request "attach"
;;                    :port 5678
;;                    :host "localhost")))

(defun ds-python-package-root ()
  "Return the top level package root directory of the current file buffer.
Uses the workspace root to identify the top level directory and then parses
the file buffer's path to obtain the first directory after the workspace root.
Crude replacement of paths for this to work with Windows."
  (let* ((buffer-fullpath (file-truename (file-name-directory buffer-file-name)))
         (root (lsp-workspace-root))
         (package-string
          (s-replace "\\" "/" (substring buffer-fullpath (length root)))))
    (car (split-string package-string "/" t))))

;; TODO - add this to my config.
;; we can use ./ in place of pytrader and combine with
;; flycheck mypy config file - i.e. pyproject.toml
;; (setq-local flycheck-python-mypy-config "pyproject.toml")
(defun ds-mypy-project ()
  "Run mypy type checker over project."
  (interactive)
  (let* ((package-root (ds-python-package-root))
         (cmd (concat "mypy --config-file pyproject.toml " package-root)))
   (ds-python-run-command cmd "./" "*mypy*")))

(defun ds-pylint-project ()
  "Run pylint over project. Runs from the top level package of the
current buffer's root package. This may be the library/app's top level
package or 'test'."
  (interactive)
  (let* ((package-root (ds-python-package-root))
         (cmd (concat "pylint --rcfile=pyproject.toml " package-root)))
   (ds-python-run-command cmd "./" "*pylint*")))

;;
;; ipython support for interactive python, data analysis and notebooks.
;;
(use-package ipython-shell-send
  :ensure t)

(defun ds-ipython-shell-send-region (start end &optional send-main msg)
  "Send selected region or buffer to ipython interpreter and then remove/
deactive current selection. This is a wrapper around the function -
ipython-shell-send-region"
  (interactive
   (list (region-beginning) (region-end) current-prefix-arg t))
  (ipython-shell-send-region start end send-main msg)
  (deactivate-mark))

(defun ds-switch-to-ipython ()
  "Switch interpreter to ipython."
  (interactive)
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i --simple-prompt --no-autoindent --InteractiveShell.display_page=True --matplotlib")
  (define-key elpy-mode-map
    (kbd "C-c C-c")
    'ds-ipython-shell-send-region))

(defun ds-switch-to-python ()
  "switch interpreter to standard python."
  (interactive)
  (setq python-shell-interpreter "python"
        python-shell-interpreter-args "-i")
  (define-key elpy-mode-map
    (kbd "C-c C-c")
    'ds-python-elpy-shell-send-region-or-buffer-and-step))

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

;; By default use IPython
(ds-switch-to-ipython)


(provide 'ds-python)

;;; ds-python.el ends here
