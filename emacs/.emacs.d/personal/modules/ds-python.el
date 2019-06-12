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
(prelude-require-packages '(elpy))
(elpy-enable)

;; Use pytest as our default runner.
(setq elpy-test-runner 'elpy-test-pytest-runner)

;; lsp-mode for Language server integration.
(prelude-require-packages '(lsp-mode company-lsp markdown-mode))
(require 'lsp)
(add-hook 'python-mode-hook 'lsp)

;; Linux laptop location for MS Python Server
(when (eq system-type 'gnu/linux)
  (setq ms-pyls-dir
        (expand-file-name "~/projects/python/python-language-server/output/bin/Release/")))

;;
;; Register Microsoft Python Language server for lsp
;;
(load "~/.emacs.d/personal/modules/lsp-ms-python.el")
(require 'lsp-ms-python)

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
