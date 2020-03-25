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

;; lsp-mode for Language server integration.
(prelude-require-packages '(lsp-mode company-lsp markdown-mode))
(require 'lsp)

;; Enable pylint and flake8, disable pyflakes.
(setq lsp-pyls-plugins-pylint-enabled t
      lsp-pyls-plugins-flake8-enabled t
      lsp-pyls-plugins-pyflakes-enabled nil)

;;
;; Register our third party mypy plugin for pyls.
;;
(with-eval-after-load 'lsp-mode
  (lsp-register-custom-settings
   '(("pyls.plugins.pyls_mypy.enabled" lsp-pyls-plugins-mypy-enabled t)
     ("pyls.plugins.pyls_mypy.live_mode" lsp-pyls-plugins-mypy-live-mode-enabled nil))))

;; For python add flake8 as the next checker after lsp
;;(setq lsp-flycheck-live-reporting t)
;;(flycheck-add-next-checker 'lsp 'python-flake8)
;;(flycheck-add-next-checker 'python-flake8 'lsp)
;;(flycheck-select-checker 'python-flake8)
;;(setq flycheck-check-syntax-automatically '(save idle-change new-line mode-enabled))

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
(when (eq system-type 'gnu/linux)
  (setq lsp-python-ms-executable
        (expand-file-name "~/projects/python/python-language-server/output/bin/Release/Microsoft.Python.LanguageServer")))

(use-package lsp-python-ms
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp))))  ; or lsp-deferred

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

;;
;; switch servers to use either Palentir (pyls) or the Microsoft Python
;; language server mspyls by updating the registered client priority.
;;
(defun ds-python-use-pyls ()
  "Override priority of registered python servers and use Palentir pyls by default."
  (interactive)
  (setf (lsp--client-priority (ht-get lsp-clients 'pyls)) 0)
  (setf (lsp--client-priority (ht-get lsp-clients 'mspyls)) -2))

(defun ds-python-use-mspyls ()
  "Override priority of registered python servers and use MS pyls by default."
  (interactive)
  (setf (lsp--client-priority (ht-get lsp-clients 'pyls)) -1)
  (setf (lsp--client-priority (ht-get lsp-clients 'mspyls)) 0)
  
  ;; Setup flycheck checkers for mypy and flake8 since the Microsoft
  ;; server doesn't have plugins for these and generate diagnostic messages.
  ;; First clear down next checkers for mypy otherwise we'll continue add
  ;; checkers.
  (setf (flycheck-checker-get 'python-mypy 'next-checkers) nil)
  ;; lsp will be end of the flycheck chain since its next-checker is nil.
  (flycheck-add-next-checker 'python-mypy 'lsp)
  (flycheck-add-next-checker 'python-mypy 'python-flake8))

;;
;; Provide our own lsp-flycheck-enable function which allows us to
;; support the MS Python Language Server along with flake8 and mypy
;; running controlled by flycheck.
;; Lsp assumes that the language server provides all linting and syntax
;; diagnostics. Pyls does this by adding/enabling plugins for flake8, pylint
;; etc. and then generating errors/warnings through lsp diagnostic messages.
;; mspyls doesn't do this expecting the client to add further linting.
;; We support this by modifying the flycheck checker chain and chaining our
;; linters first finishing with lsp to provide MS diagnostic messages.
;;
;; Switch to use mspyls with M-x ds-python-use-mspyls.
;; This will setup a flycheck chain:
;;  python-mypy -> python-flake8 -> lsp
;;
;; We then set flycheck-check-syntax-automatically to perform linting/errors
;; on save. and set the initial checker to be python-mypy.
;; 
;;
(with-eval-after-load 'lsp-mode
  (defun lsp-flycheck-enable (&rest _)
    "Enable flycheck integration for the current buffer."
    (flycheck-mode 1)
    ;; Apply our behaviour to python mode only when we're using
    ;; the ms python server.
    (if (and (eq major-mode 'python-mode)
             (eq 'mspyls (lsp--client-server-id (car (lsp--find-clients)))))
        (progn
          (when lsp-flycheck-live-reporting
            (setq-local flycheck-check-syntax-automatically
                        '(save idle-change new-line mode-enabled)))
          (setq-local flycheck-checker 'python-mypy))
      (progn
        (when lsp-flycheck-live-reporting
          (setq-local flycheck-check-syntax-automatically nil))
        (setq-local flycheck-checker 'lsp) ))
    
    (lsp-flycheck-add-mode major-mode)
    (add-to-list 'flycheck-checkers 'lsp)
    (add-hook 'lsp-after-diagnostics-hook #'lsp--flycheck-report nil t)))

(defun ds-python-run-script ()
  "Run the python script in the current buffer, output will be written to a compilation buffer."
  (interactive)
  (let* ((root (lsp-workspace-root))
         (current-file (expand-file-name (buffer-file-name)))
         (cmd (format "python -c \"import sys;import runpy;sys.path.append('%s');runpy.run_path('%s', run_name='__main__')\""
                      root current-file)))
    (compile cmd)))

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
