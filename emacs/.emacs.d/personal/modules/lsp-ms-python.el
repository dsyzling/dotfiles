;;; lsp-ms-python.el --- lsp client for Microsoft Python Language server

;; Author: Darren Syzling
;; Keywords: lsp, python, microsoft
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Currently work in progress...
;;
;; The Microsoft Python Server can be built from source and required the dotnet
;; core sdk.
;; 
;; For Arch Linux
;;  pacman -S dotnet-sdk
;;  (https://www.archlinux.org/packages/community/x86_64/dotnet-sdk/)
;;
;; Clone the python language server repos:
;;  git clone https://github.com/Microsoft/python-language-server.git
;; and then: 
;;  cd src/LanguageServer/Impl 
;;  dotnet build -c Release
;;
;; builds to:
;; ./output/bin/Release

;; Based on ideas discussed in the following article online:
;; https://vxlabs.com/2018/11/19/configuring-emacs-lsp-mode-and-microsofts-visual-studio-code-python-language-server/
;;

;;; Code:
(require 'cl)
(require 'lsp)
(require 'python)
(require 'projectile)

;; To debug lsp - enable io - lsp protocol messages will be written to the messages buffer.
;;(setq lsp-print-io 1)

;; The user must supply the directory location of the Microsoft Python Language Server.
(defcustom ms-pyls-dir ""
  "Directory where the Microsoft Python Language server - Microsoft.Python.LanguageServer.dll is installed."
  :group 'lsp-ms-python
  :type 'string)

;; it's crucial that we send the correct Python version to MS PYLS, else it returns no docs
;; in many cases furthermore, we send the current Python's (can be virtualenv) sys.path as
;; searchPaths.
(defun get-python-ver-and-syspath (workspace-root)
  "return list with pyver-string and json-encoded list of python search paths."
  (let ((python (executable-find python-shell-interpreter))
        (init "from __future__ import print_function; import sys; import json;")
        (ver "print(\"%s.%s\" % (sys.version_info[0], sys.version_info[1]));")
        (sp (concat "sys.path.insert(0, '" workspace-root "'); print(json.dumps(sys.path))")))
    (with-temp-buffer
      (call-process python nil t nil "-c" (concat init ver sp))
      (subseq (split-string (buffer-string) "\n") 0 2))))


;; This is the bare minimum initialisation required for the ms python language server.
;; (defun ms-pyls-extra-init-params (workspace)
;;   `(:interpreter
;;     (:properties
;;      (:UseDefaultDatabase 1))))


;; I based most of this on the vs.code implementation:
;; https://github.com/Microsoft/vscode-python/blob/master/src/client/activation/languageServer/languageServer.ts#L219
;; (it still took quite a while to get right, but here we are!)
(defun ms-pyls-extra-init-params (workspace)
  (destructuring-bind (pyver pysyspath) (get-python-ver-and-syspath workspace)
    `(:interpreter (
                    :properties (
                                 :InterpreterPath ,(executable-find python-shell-interpreter)
                                 ;; this database dir will be created if required
                                 :DatabasePath ,(expand-file-name (concat ms-pyls-dir "db/"))
                                 :Version ,pyver))
                   ;; preferredFormat "markdown" or "plaintext"
                   ;; experiment to find what works best -- over here mostly plaintext
                   :displayOptions (:preferredFormat "plaintext"
                                    :trimDocumentationLines :json-false
                                    :maxDocumentationLineLength 0
                                    :trimDocumentationText :json-false
                                    :maxDocumentationTextLength 0)
                   :searchPaths ,(json-read-from-string pysyspath))))


(defcustom lsp-clients-ms-python-library-directories '("/usr/")
  "List of directories which will be considered to be libraries."
  :group 'lsp-python
  :risky t
  :type '(repeat string))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection
                   `("dotnet" ,(concat ms-pyls-dir "Microsoft.Python.LanguageServer.dll")))
  :major-modes '(python-mode)
  :initialization-options (lambda ()
                            (progn (message "executing initialisation options")
                                   (ms-pyls-extra-init-params (projectile-project-root))))
  :server-id 'lsp-ms-python
  ;; :library-folders-fn (lambda (_workspace)
  ;;                       `("/home/dsyzling/.virtualenvs/python-test/lib/python37.zip"
  ;;                         "/home/dsyzling/.virtualenvs/python-test/lib/python3.7"
  ;;                         "/home/dsyzling/.virtualenvs/python-test/lib/python3.7/lib-dynload"
  ;;                         "/usr/lib64/python3.7" 
  ;;                         "/usr/lib/python3.7"
  ;;                         "/home/dsyzling/.virtualenvs/python-test/lib/python3.7/site-packages"))
  ))

;; The MS Python server sends doc strings with &nbsp; characters even when
;; asking for plain text. We add a filter to process these characters and replace
;; with a space.
(defun ms-python-ui-doc-filter (str)
  (replace-regexp-in-string "\&nbsp;" " " str))


;; lsp-ui-doc--extract is called and the result passed to
;; lsp-ui-sideline--format-info - we modify he doc string before being
;; passed on.
;; Docs would suggest that markdown mode is used when a language id is not found
;; in lsp-render-string but I can't see this code.
;; Also we don't want to be changing the lsp-language-id-configuration
;; this is used elsewhere not just rendering doc strings.
;; (python-mode . "python")
;; See lsp--render-string (str language)
(advice-add 'lsp-ui-doc--extract
            :filter-return #'ms-python-ui-doc-filter)

;; Company-lsp queries the language server for trigger characters automatically.
(require 'company-lsp)
(push 'company-lsp company-backends)

(provide 'lsp-ms-python)
;;; lsp-ms-python ends here
