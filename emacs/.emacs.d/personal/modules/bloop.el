;;; bloop.el --- Run Bloop from Emacs   -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (c) 2018 Paweł Bartkiewicz

;; Author: Paweł Bartkiewicz <tuuresairon+emacs.bloop@gmail.com>
;; URL: https://github.com/tues/emacs-bloop
;; Keywords: scala bloop compilation tools convenience
;; Version: 0.1

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; TODO

;;; Code:

;;; Customization
(defgroup bloop nil
  "Run Bloop from Emacs."
  :group 'tools
  :group 'convenience
  :link '(url-link :tag "Github" "https://github.com/tues/emacs-bloop"))

(defcustom bloop-program-name "bloop"
  "Program invoked by the `bloop-exec' command."
  :type 'string
  :group 'bloop)

(defcustom bloop-reporter "scalac"
  "Either bloop or scalac. The main difference is that bloop shows errors in reverse order. Emacs generally assumes the first error in the output is the most relavent so the scalac reporter will most likely be preferred. This is used for test and compile."
  :type 'string
  :group 'bloop)

(defun bloop-buffer-name (root command)
  (concat "*bloop-" command "*"))

(defun bloop-directory (root)
  (file-name-as-directory (concat root ".bloop")))

(defun bloop-find-root (file)
  (file-name-as-directory (or (locate-dominating-file file ".bloop")
                              (error (concat "Can't find `.bloop' directory. "
                                             "Have you generated bloop config for this project? "
                                             "https://scalacenter.github.io/bloop/docs/installation/")))))

(defun bloop-project-files (bloop-dir)
  (directory-files bloop-dir t "\\.json$"))

(defun bloop-read-project-file (project-file)
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
         (json (json-read-file project-file))
         (project (gethash "project" json))
         (name (gethash "name" project))
         (dirs (gethash "sources" project)))
    (cons name (mapcar 'file-name-as-directory dirs))))

(defun bloop-longest-string (func strings)
  (let ((sorted (sort strings (lambda (x y) (> (length (func x)) (length (func y)))))))
    (car sorted)))

(defun bloop-project-match-file (project file)
  (let* ((name (car project))
         (sources (cdr project))
         (filtered (seq-filter (lambda (path) (string-prefix-p path file)) sources)))
    (cons name (bloop-longest-string 'identity filtered))))

(defun bloop-find-project (root file)
  (let* ((project-files (bloop-project-files (bloop-directory root)))
         (projects (mapcar 'bloop-read-project-file project-files))
         (sources (mapcar (lambda (project) (bloop-project-match-file project file)) projects))
         (filtered (seq-filter (lambda (x) (cdr x)) sources))
         (project (bloop-longest-string 'cdr filtered)))
    project))

(defun bloop-current-project (root)
  (bloop-find-project root (buffer-file-name)))

(defun bloop-exec (comint root command &rest args)
  (unless command (error "Missing argument `command'."))

  (let* ((buffer-name (bloop-buffer-name root command))
         (raw-command (cons bloop-program-name (cons command args)))
         (full-command (string-join (mapcar 'shell-quote-argument raw-command) " "))
         (inhibit-read-only 1))

    (when (not (executable-find bloop-program-name))
      (error (concat "`%s' not found. Is bloop installed and on PATH? "
                     "See `bloop-program-name' variable.")
             bloop-program-name))

    (if comint
        (with-current-buffer (get-buffer-create buffer-name)
          (pop-to-buffer-same-window (current-buffer))
          ;; (read-only-mode)
          (buffer-disable-undo)
          (if (comint-check-proc (current-buffer))
              (error "A bloop command is still running!")
            ;; TODO: Maybe save buffers?
            (cd root)
            (erase-buffer)
            (insert (concat root "$ " full-command))
            (newline 2)
            (comint-mode)
            ;; (compilation-shell-minor-mode)
            (comint-exec (current-buffer) buffer-name bloop-program-name nil (cons command args))
            (current-buffer)))
      (let ((compilation-buffer-name-function (lambda (mode) buffer-name)))
        (cd root)
        (compile full-command)))))


(defun bloop-compile ()
  (interactive)
  (let* ((root (bloop-find-root (buffer-file-name)))
         (project (bloop-current-project root))
         (project-name (car project)))
    (bloop-exec nil root "compile" "--reporter" bloop-reporter project-name)))

(defun bloop-test ()
  (interactive)
  (let* ((root (bloop-find-root (buffer-file-name)))
         (project (bloop-current-project root))
         (project-name (car project)))
    (bloop-exec nil root "test" "--reporter" bloop-reporter project-name)))

(defun bloop-test-only ()
  (interactive)
  (let* ((root (bloop-find-root (buffer-file-name)))
         (project (bloop-current-project root))
         (project-name (car project))
         (target-test (concat "*" (replace-regexp-in-string ".scala" "" (car (last (split-string (buffer-file-name) "/")))))))
    (bloop-exec nil root "test" "--reporter" bloop-reporter "--only" target-test project-name)))

(defun bloop-show-current-project ()
  (interactive)
  (let* ((root (bloop-find-root (buffer-file-name))))
    (message (format "%S %S" root (bloop-current-project root)))))

;;
;; DS - remove these key bindings as they do not work with my
;; prelude configuration - no prefix key.
;;
;; (global-set-key (kbd "C-c b c") 'bloop-compile)
;; (global-set-key (kbd "C-c b q") 'bloop-show-current-project)
