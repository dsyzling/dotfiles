;;; bsp-mode.el --- Build Server Protocol mode               -*- lexical-binding: t; -*-

;;
;; Author: Darren Syzling <dsyzling@gmail.com>
;; Keywords: scala, bsp, languages

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
;;;
;;; 
;;;
;;; Code:

(require 'cl-lib)
;; TODO temporary - required for lsp-workspace-root - replace with a customisable function.
(require 'lsp-mode)
(require 'json)
(require 'pcase)
(require 'seq)
(require 'subr-x)
(require 'dash)
(require 'dash-functional)
(require 'url-parse)
(require 'url-util)
(require 'scala-mode)  ;; currently for definition of bsp-language-id-configuration.
(require 'ht)
(require 'ansi-color)

;;
;; Customisation parameters for bsp-mode
;; 
(defcustom bsp-print-io t
  "If non-nil, print all messages to and from the language server to *bsp-log*."
  :group 'bsp-mode
  :type 'boolean)

(defcustom bsp-log-max message-log-max
  "Maximum number of lines to keep in the log buffer.
If nil, disable message logging.  If t, log messages but don’t truncate
the buffer when it becomes large."
  :group 'bsp-mode
  :type '(choice (const :tag "Disable" nil)
                 (integer :tag "lines")
                 (const :tag "Unlimited" t)))

(defconst bsp-log-buffer "*bsp-log*"
  "Buffer name for bsp log messages."
  )

(defcustom bsp-use-native-json t
  "If non-nil, use native json parsing if available."
  :group 'bsp-mode
  :type 'boolean)

(defvar-local bsp--cur-workspace nil)

(defvar bsp-last-id 0
  "Last request id.")

(defvar bsp--session nil
  "Contain the `bsp-session' for the current Emacs instance.")

;;
;; Scala mode and language-id is not defined within lsp - we wrap the lsp
;; list adding scala mode and id.
;; Also language-id (which should be a function) is not defined within
;; a scala lsp workspace.
;;
(defvar bsp-language-id-configuration (append '((scala-mode . "scala"))
                                              lsp-language-id-configuration))

;; bsp session definition, one session per Emacs instance which then
;; manages multiple workspaces.
(cl-defstruct bsp-session
  (workspace-root->servers (make-hash-table :test `equal))
    )

(cl-defstruct bsp--client
  ;; A function that instantiates a build server protocol process.
  ;; Returns a cons (command-process . communication-process).
  ;; New connection is called with 2 arguments - filter and sentinel.
  ;; Filter should be used a process filter for communication-process and
  ;; sentinel should be used as a process sentinel for command-process
  (new-connection nil :read-only t)
  
  ;; ‘response-handlers’ is a hash table mapping integral JSON-RPC request
  ;; identifiers for pending asynchronous requests to functions handling the
  ;; respective responses.  Upon receiving a response from the language server,
  ;; ‘lsp-mode’ will call the associated response handler function with a
  ;; single argument, the deserialized response parameters.
  (response-handlers (make-hash-table :test 'eql) :read-only t)

  (server-id nil)

  ;; bsp server connection details, will be initialised once the server has
  ;; been started and connected.
  (bsp-connection nil)

  ;; build targets for this bsp client.
  (build-targets nil)

  ;; Selected build target to use with compile or test.
  (selected-build-target nil)
  
  (initialised nil))

;; parser data for handling messages sent from bsp server.
(cl-defstruct bsp--parser
  (headers '()) ;; alist of headers
  (body nil) ;; message body
  (reading-body nil) ;; If non-nil, reading body
  (body-length nil) ;; length of current message body
  (body-received 0) ;; amount of current message body currently stored in 'body'
  (leftovers nil) ;; Leftover data from previous chunk; to be processed
  (workspace nil))

(cl-defstruct bsp--workspace
  ;; Currently we only allow one bsp server per workspace - need to extend in future.
  (client nil :readonly t)
  
  ;; root directory of workspace.
  (root nil :readonly t)
  
  ;; parser state data for parsing bsp messages.
  (parser nil :readonly t)
  
  ;; ‘proc’ is a process object; it may represent a regular process, a pipe, or
  ;; a network connection.  ‘bsp-mode’ communicates with ‘proc’ using the
  ;;  build server protocol.  ‘proc’ corresponds to the COMMUNICATION-PROCESS
  ;; element of the return value of the client’s ‘get-root’ field.
  (proc nil)

  ;; ‘proc’ is a process object; it must represent a regular process, not a
  ;; pipe or network process.  It represents the actual server process that
  ;; corresponds to this workspace.  ‘cmd-proc’ corresponds to the
  ;; COMMAND-PROCESS element of the return value of the client’s ‘get-root’
  ;; field, which see.
  (cmd-proc nil)

  ;; Keep track of bsp-running-tasks - which can be compile, test, run tasks.
  ;; currently we're going to look them up by type since originId is not supported.
  (running-tasks '())
  
  ;; BSP Build server connections configured for the workspace - these are bsp-connection structs,
  ;; and describe how to connect to a build server to compile/test the projects.
  ;; A number of build servers may be available select the one based on language
  ;; support.
  (bsp-connections nil))

;;
;; Build targets details returned from the build server.
;;
(cl-defstruct bsp-build-target
  ;; opaque id of build target which should be used to invoke compile and test
  (id nil :read-only t)
  
  ;; display-name of build target
  (display-name nil :read-only t)
  
  ;; uri base directory of target
  (base-directory nil :read-only t)

  ;; list of string language ids supported by this target
  (language-ids nil :read-only t))

;; TODO - do we need this??
(defconst bsp-task-type
  '(("compile-task" . 'compile-task)))

;;
;; Track running tasks - compile, test or run tasks.
;; Once the request has been sent the bsp server will send us
;; taskStart and taskFinish combined with progress and diagnostic
;; notifications.
;;
(cl-defstruct bsp-running-task
  ;; build target - initialise on invocation of our compile/test etc.
  (build-target :read-only t)
  
  ;; task type - one of 'compile-task, 'test-task, 'run-task
  (task-type :read-only t)
  
  ;; task id - assigned by taskStart
  (task-id)
  
  ;; parent task id of task
  (parent-task-id)
  
  ;; name of buffer to update with results/progress
  (buffer-name)
  
  ;; we can set an origin-id and the bsp server is supposed to populate this field
  ;; in all responses - not sure if bloop is doing this yet?
  (origin-id))

(defvar bsp--uri-file-prefix (pcase system-type
                               (`windows-nt "file:///")
                               (_ "file://"))
  "Prefix for a file-uri.")

(defun bsp--path-to-uri (path)
  "Convert PATH to a uri."
  (concat bsp--uri-file-prefix
          (url-hexify-string (expand-file-name (or (file-remote-p path 'localname t) path))
                             url-path-allowed-chars)))


(defun bsp-workspace-root (&optional path)
    "Find the root directory for the current buffer path or path given.
For now we delegate to lsp-workspace-root but will make this configurable
in future"
    (lsp-workspace-root path))

(defun bsp-session ()
  "Get the bsp session associated with the current buffer."
  (or bsp--session (setq bsp--session (make-bsp-session))))


;;
;; bsp process support
;;
(defun bsp-server-present? (final-command)
  "Check whether FINAL-COMMAND is present."
  ;; executable-find only gained support for remote checks after 26.1 release
  (or (and (cond
            ((not (file-remote-p default-directory))
             (executable-find (cl-first final-command)))
            ((not (version<= emacs-version "26.2"))
             (with-no-warnings (executable-find (cl-first final-command) (file-remote-p default-directory))))
            (t))
           (prog1 t
             (bsp-log "Command \"%s\" is present on the path." (s-join " " final-command))))
      (ignore (bsp-log "Command \"%s\" is not present on the path." (s-join " " final-command)))))

(defun bsp-resolve-final-function (command)
  "Resolve final function COMMAND."
  (bsp-log "resolving final-function")
  (-let [command (if (functionp command) (funcall command) command)]
    (if (consp command) command (list command))))

(defun bsp-stdio-connection (command)
  "Create BSP stdio connection named name.
  COMMAND is either list of strings, string or function which
  returns the command to execute."
  (list :connect (lambda (filter sentinel name)
                   (bsp-log "starting bsp process")
                   (let ((final-command (bsp-resolve-final-function command))
                         (process-name (generate-new-buffer-name name)))
                     (bsp-log "command %s process name %s" final-command process-name)
                     (let ((proc (make-process
                                  :name process-name
                                  :connection-type 'pipe
                                  :buffer (format "*%s*" process-name)
                                  :coding 'no-conversion
                                  :command final-command
                                  :filter filter
                                  :sentinel sentinel
                                  :stderr (format "*%s::stderr*" process-name)
                                  :noquery t)))
                       (set-process-query-on-exit-flag proc nil)
                       (cons proc proc))))
        :test? (lambda () (-> command bsp-resolve-final-function bsp-server-present?))))

(defun bsp--read-json (str)
  "Read json string STR."
  (let* ((use-native-json (and bsp-use-native-json (fboundp 'json-parse-string)))
         (json-array-type (if lsp-json-use-lists 'list 'vector))
         (json-object-type 'hash-table)
         (json-false nil))
    (if use-native-json
        (with-no-warnings
          (with-temp-buffer
            (json-parse-string str
                               :object-type 'hash-table
                               :null-object nil
                               :false-object nil)))
      (json-read-from-string str))))

(defun bsp--parser-on-message (parser msg)
  (let* ((workspace (bsp--parser-workspace parser))
         (client (bsp--workspace-client workspace))
         (received-time (current-time))
         (server-id (bsp--client-server-id client))
         (json-data (bsp--read-json msg))
         (id (--when-let (gethash "id" json-data)
               (if (stringp it) (string-to-number it) it)))
         (data (gethash "result" json-data))
         )
    (pcase (bsp--get-message-type json-data)
      ('response
       ;; must have id
       (cl-assert id)
       (-let [(callback _ method start-time before-send) (gethash id (bsp--client-response-handlers client))]
         
         (when callback
           (when bsp-print-io
             (bsp-log "%s" (if callback "callback present" "no callback")))
           (funcall callback (gethash "result" json-data))
           (remhash id (bsp--client-response-handlers client))))
       )
      ('notification
       (bsp--on-notification workspace json-data))
      ;; TODO - add bsp--on-request
      )
    )
  ;; TODO add  common message response mapping for bsp and notifications
  )

(defconst bsp--notification-handlers
  (ht ("build/taskStart" #'bsp--on-task-start)
      ("build/taskFinish" #'bsp--on-task-finish)
      ("build/publishDiagnostics" #'bsp--on-publish-diagnostics)
      ("build/logMessage" #'bsp--on-log-message)))

(defun bsp--on-notification (workspace json-data)
  (-let (((&hash "params" "method" "result") json-data)
         (client (bsp--workspace-client workspace)))
    (if-let (handler (gethash method bsp--notification-handlers))
        (funcall handler workspace params result)
       (bsp-log "Unknown method: %s" method))))

(defun bsp--equal-build-target-id (id1 id2)
  "Check equality of build target ids, these are converted to hashtables
so we have to compare the number of items and then their key/values."
  (if (= (hash-table-count id1)
         (hash-table-count id2))
      (let ((sorted-keys (cl-sort (hash-table-keys id1) 'string-lessp)))
        ;; return true if none of the values are nil - i.e. key/values are equal.
        (null (memq nil (-map (lambda (key) (string= (gethash key id1)
                                                     (gethash key id2)))
                              sorted-keys))))
    nil))

;; (defun bsp--find-running-task (build-target-id)
;;   (seq-find (lambda (running-task)
;;               (bsp--equal-build-target-id build-target-id
;;                                           (bsp-build-target-id
;;                                            (bsp-running-task-build-target running-task))))
;;             bsp-running-tasks nil))
;;
;; (defun bsp--remove-running-task (build-target-id)
;;   (setq bsp-running-tasks
;;         (cl-remove-if (lambda (running-task)
;;                      (bsp--equal-build-target-id build-target-id
;;                                                  (bsp-build-target-id
;;                                                   (bsp-running-task-build-target running-task))))
;;                    bsp-running-tasks)))

(defun bsp--find-running-task (workspace task-type)
  "Find running task with the same task type in the workspace"
  (seq-find (lambda (running-task)
              (equal task-type (bsp-running-task-task-type running-task)))
            (bsp--workspace-running-tasks workspace) nil))

(defun bsp--remove-running-task (workspace task-type)
  (setf (bsp--workspace-running-tasks workspace)
        (cl-remove-if (lambda (running-task)
                     (equal task-type (bsp-running-task-task-type running-task)))
                   (bsp--workspace-running-tasks workspace))))


(defun bsp--create-task-buffer (buffer-name)
  (let ((buffer (generate-new-buffer buffer-name)))
    (with-current-buffer buffer
      (pop-to-buffer buffer)
      (buffer-disable-undo)
      ;; (if (comint-check-proc (current-buffer))
      ;;     (error "A command is still running!")

      (read-only-mode)
      
      (let ((inhibit-read-only t))
        (erase-buffer)
        (comint-mode)
        (compilation-shell-minor-mode)
        buffer))))

(defun bsp--get-task-buffer (buffer-name)
  (or (get-buffer buffer-name) (bsp--create-task-buffer buffer-name)))

(defun bsp--append-to-task-buffer (buffer-name message)
  (let ((buffer (bsp--get-task-buffer buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (pop-to-buffer buffer)
        (let ((p (point)))
          (insert message)
          (ansi-color-apply-on-region p (point)))))))

(defun bsp--append-message-to-task-buffer (running-task message)
  (let ((buffer-name (bsp-running-task-buffer-name running-task)))
    (bsp--append-to-task-buffer buffer-name message)))

(defun bsp--clear-task-buffer (buffer-name)
  (let ((buffer (bsp--get-task-buffer buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)))))

;; (defun bsp--create-task-buffer (buffer-name message)
;;   (with-current-buffer (get-buffer-create buffer-name)
;;     (pop-to-buffer-same-window (current-buffer))
;;     ;;(display-buffer-reuse-window (current-buffer))
;;     ;; (read-only-mode)
;;     (buffer-disable-undo)
;;     (if (comint-check-proc (current-buffer))
;;         (error "A command is still running!")

;;       (read-only-mode)
;;       (let ((inhibit-read-only t))
;;         (erase-buffer)
;;         (insert message)
;;         (newline 2)
;;         (comint-mode)
;;         (compilation-shell-minor-mode)
;;         (current-buffer)))))

(defun bsp--create-task-buffer-name (workspace task-type)
  "Format task buffer as *ServerId-task-type* - e.g.
*Bloop-compilation*."
  (let ((type-name
         (pcase task-type
           (`compile-task "compilation")
           (`test-task "log")
           (`log "log"))))
    (format "*%s-%s*"
            (bsp--client-server-id (bsp--workspace-client workspace))
            type-name)))

(defun bsp--task-type-from-data-kind (data-kind)
  (pcase data-kind
    (`"compile-task" 'compile-task)
    (`"compile-report" 'compile-task)
    (`"test-task" 'test-task)))

(defun bsp--on-task-start (workspace params result)
  (let* (
         (task-id   (gethash "taskId" params))
         (message   (gethash "message" params))
         (data      (gethash "data" params))
         (data-kind (gethash "dataKind" params))
         (target-id (gethash "target" data)))
    ;; find running task with build target id and task-type
    (if-let ((running-task (bsp--find-running-task workspace
                                                   (bsp--task-type-from-data-kind data-kind))))
        (let ((buffer-name
               (bsp--create-task-buffer-name workspace
                                             (bsp--task-type-from-data-kind data-kind))))
          (when (not (bsp-running-task-buffer-name running-task))
            ;; add message to compilation task buffer
            ;;(bsp--create-task-buffer buffer-name message))
            ;;(bsp--append-message-to-task-buffer running-task message)
            (bsp--append-to-task-buffer buffer-name (concat message "\n\n"))
            
          ;; update running task with notification data sent by bsp server.
            (setf (bsp-running-task-buffer-name running-task) buffer-name
                  (bsp-running-task-task-id running-task) task-id)))
      (error "Unable to find running task"))))


(defun bsp--compile-diagnostic-to-string (file-path diagnostic)
  "Format compiler errors/warning to string. Display file path with line
and column number of error along with the error message. Copy the line
in error from the source file and position a caret underneath the column
position of the reported error."
  (let* (
         (message       (gethash "message" diagnostic))
         (range         (gethash "range" diagnostic))
         (start         (gethash "start" range))
         (end           (gethash "end" range))
         (error-pos     `(
                          ,(1+ (gethash "line" start))
                          ,(gethash "character" start)
                          ,(1+ (gethash "line" end))
                          ,(gethash "character" end)
                          )))
    
    ;; Append line with error and position caret below column position of error.
    (let ((source-error-text
           (with-temp-buffer
             (insert-file-contents file-path)
             (forward-line (1- (elt error-pos 0)))
             (buffer-substring (line-beginning-position) (line-end-position)))))
      (format "%s:%d:%d: %s\n%s\n%s\n\n"
              file-path
              (elt error-pos 0)
              (elt error-pos 1)
              message
              source-error-text
              (format (concat "%" (number-to-string(1+ (elt error-pos 1))) "s") "^")))))

(defun bsp--remove-file-uri (file-uri)
  "Remove a file uri prefix from a file/directory path if it exists."
  (when (string-prefix-p bsp--uri-file-prefix file-uri)
    (replace-regexp-in-string bsp--uri-file-prefix "" file-uri)))

(defun bsp--on-publish-diagnostics (workspace params result)
  (let* ((text-document (gethash "textDocument" params))
         (file-path     (bsp--remove-file-uri (gethash "uri" text-document)))
         (target-id     (gethash "buildTarget" params))
         ;; diagnostics is a list of errors/warnings - each in their own hashmap
         (diagnostics   (gethash "diagnostics" params)))
    ;; find running task with build target id and task-type
    (if-let ((running-task (bsp--find-running-task workspace 'compile-task)))
        (progn
          (let ((error-string (string-join
                               (-map (lambda (diagnostic)
                                       (bsp--compile-diagnostic-to-string file-path diagnostic))
                                     diagnostics)
                               "\n")))
            (bsp--append-message-to-task-buffer running-task error-string)))
      (error "Unable to find running task"))))

(defun bsp--on-log-message (workspace params result)
  (let* ((message    (gethash "message" params))
         (origin-id  (gethash "originId" params)))
    ;; log messages can be written at any time - even during a compilation
    ;; failure - we assume this is going to the log buffer.
    ;; In future we may be able to tie this to an originId.
    (bsp--append-to-task-buffer (bsp--create-task-buffer-name workspace 'log)
                                (concat message "\n"))))

(defun bsp--on-task-finish (workspace params result)
  (let* (
         (data-kind (gethash "dataKind" params))
         ;;(task-id   (gethash "taskId" params))
         (message   (gethash "message" params))
         (data      (gethash "data" params))
         (target-id (gethash "target" data))
         (errors    (gethash "errors" data))
         (warnings  (gethash "warnings" data)))
    
    ;; find running task with build target id and task-type
    (if-let ((running-task (bsp--find-running-task workspace
                                                   (bsp--task-type-from-data-kind data-kind))))
        (progn
          (bsp--append-message-to-task-buffer
           running-task
           (format "%s\nErrors:   %s\nWarnings: %s\n" message errors warnings)))
      (error "Unable to find running task"))))

(defun bsp--create-filter (parser)
  "make process filter for our bsp parser to process json responses
from the bsp server."
  #'(lambda (_proc output)
      ;;(bsp-log "<<<< %s" output)
      
      (-when-let (messages (condition-case err
                               (bsp--parser-read parser output)
                             (error
                              (let ((chunk (concat (bsp--parser-leftovers parser) output)))
                                (bsp--parser-reset parser)
                                (ignore (lsp-warn "Failed to parse the following chunk:\n'''\n%s\n'''\nwith message %s" chunk err))))))
        (dolist (m messages)
          (when bsp-print-io
            (bsp-log "Response received %s\n" (bsp--json-pretty-print m)))
          (bsp--parser-on-message parser m)))))


(defun bsp--create-sentinel (workspace)
  "Create sentinel handler for workspace - for now just log process exit."
  (lambda (process exit-str)
    (bsp-log "bsp process exited")))

;;
;; bsp message parsing
;;
(defun bsp--parse-header (s)
  "Parse string S as a BSP (KEY . VAL) header."
  (let ((pos (string-match "\:" s))
        key val)
    (unless pos
      (signal 'bsp-invalid-header-name (list s)))
    (setq key (substring s 0 pos)
          val (substring s (+ 2 pos)))
    (when (string-equal key "Content-Length")
      (cl-assert (cl-loop for c across val
                          when (or (> c ?9) (< c ?0)) return nil
                          finally return t)
                 nil (format "Invalid Content-Length value: %s" val)))
    (cons key val)))

(defun bsp--parser-reset (p)
  "Reset parser P."
  (setf
   (bsp--parser-leftovers p) ""
   (bsp--parser-body-length p) nil
   (bsp--parser-body-received p) nil
   (bsp--parser-headers p) '()
   (bsp--parser-body p) nil
   (bsp--parser-reading-body p) nil))

(defun bsp--parser-read (p output)
  "Handle OUTPUT using parser P."
  (let* ((messages '())
         (output (with-no-warnings (string-as-unibyte output)))
         (chunk (concat (bsp--parser-leftovers p) output)))
    (while (not (string-empty-p chunk))
      (if (not (bsp--parser-reading-body p))
          ;; Read headers
          (let* ((body-sep-pos (string-match-p "\r\n\r\n" chunk)))
            (if body-sep-pos
                ;; We've got all the headers, handle them all at once:
                (let* ((header-raw (substring chunk 0 body-sep-pos))
                       (content (substring chunk (+ body-sep-pos 4)))
                       (headers
                        (mapcar 'bsp--parse-header
                                (split-string header-raw "\r\n")))
                       (body-length (lsp--get-body-length headers)))
                  (setf
                   (bsp--parser-headers p) headers
                   (bsp--parser-reading-body p) t
                   (bsp--parser-body-length p) body-length
                   (bsp--parser-body-received p) 0
                   (bsp--parser-body p) (make-string body-length ?\0)
                   (bsp--parser-leftovers p) nil)
                  (setq chunk content))

              ;; Haven't found the end of the headers yet. Save everything
              ;; for when the next chunk arrives and await further input.
              (setf (bsp--parser-leftovers p) chunk)
              (setq chunk "")))

        ;; Read body
        (let* ((total-body-length (bsp--parser-body-length p))
               (received-body-length (bsp--parser-body-received p))
               (chunk-length (string-bytes chunk))
               (left-to-receive (- total-body-length received-body-length))
               (this-body
                (substring chunk 0 (min left-to-receive chunk-length)))
               (leftovers (substring chunk (string-bytes this-body))))
          (store-substring (bsp--parser-body p) received-body-length this-body)
          (setf (bsp--parser-body-received p) (+ (bsp--parser-body-received p)
                                                 (string-bytes this-body)))
          (when (>= chunk-length left-to-receive)
            ;; TODO: keep track of the Content-Type header, if
            ;; present, and use its value instead of just defaulting
            ;; to utf-8
            (push (decode-coding-string (bsp--parser-body p) 'utf-8) messages)
            (bsp--parser-reset p))

          (setq chunk leftovers))))
    (nreverse messages)))

(defun bsp--get-message-type (json-data)
  "Get the message type from JSON-DATA."
  (when (not (string= (gethash "jsonrpc" json-data "") "2.0"))
    (signal 'bsp-unknown-json-rpc-version (list (gethash "jsonrpc" json-data))))
  (if (gethash "id" json-data nil)
      (if (gethash "error" json-data nil)
          'response-error
        (if (gethash "method" json-data nil)
            'request
          'response))
    (if (gethash "method" json-data nil)
        'notification
      (signal 'bsp-unknown-message-type (list json-data)))))

(defun bsp--make-workspace (client root)
  "Make workspace for the CLIENT and ROOT."
  (let* ((parser (make-bsp--parser))
         (workspace (make-bsp--workspace
                     :parser parser
                     :root root
                     :client client)))
    (setf (bsp--parser-workspace parser) workspace)
    workspace))

;;
;; TODO - move server-id into client definition
;;
(defun bsp--start-workspace (session client root server-id)
  "Early implementation of function to create a new workspace and
connect to the bsp server."
  (bsp-log "running start workspace")
  (-let* ((workspace (bsp--make-workspace client root))
          ((proc . cmd-proc) (funcall
                              (or (plist-get (bsp--client-new-connection client) :connect)
                                  (user-error "Client %s is configured incorrectly" client))
                              (bsp--create-filter (bsp--workspace-parser workspace))
                              (bsp--create-sentinel workspace)
                              (format "%s" server-id)))
         ;; we should be adding the proc and cmd proc to the workspace
         ;; we should be calling initialise here.
         )
    (setf (bsp--workspace-proc workspace) proc
          (bsp--workspace-cmd-proc workspace) cmd-proc)
    ;; register workspace with session.
    (puthash root workspace (bsp-session-workspace-root->servers (bsp-session)))
    workspace))

(defun bsp-shutdown-workspace (&optional workspace)
  (interactive)
  (if-let ((workspace (or workspace (bsp--get-current-workspace))))
    (bsp-request-async workspace
                       "build/shutdown"
                       nil
                       (lambda (response)
                         (bsp-log "response from shutdown request %s" response)
                         (bsp-notify workspace "build/exit" nil)
                         
                         ;; close Emacs process associated with workspace
                         (let ((proc (bsp--workspace-cmd-proc workspace)))
                           (when (process-live-p proc)
                             (kill-process proc)))
                         
                         ;; remove workspace from our session.
                         (remhash (bsp--workspace-root workspace)
                                  (bsp-session-workspace-root->servers (bsp-session)))
                         (message "bsp server %s shutdown..."
                                  (bsp-connection-name
                                   (bsp--client-bsp-connection
                                    (bsp--workspace-client workspace))))))))

(defun build--target-from-display-name (display-name build-targets)
  (car
   (seq-filter (lambda (target)
                 (string= (bsp-build-target-display-name target)
                          display-name))
               build-targets)))

(defun bsp-select-build-target()
  "Selected a new build target for compile/test in the current workspace.
Sets the selected-build-target field within the client attached to the
current workspace and returns the selected target."
  (interactive)
  ;; query current workspace and client.
  ;; prompt with a list of targets and ask user to select build target
  ;; store in client.
  (let* ((workspace (bsp--get-current-workspace))
         (client (bsp--workspace-client workspace))
         (build-targets (bsp--client-build-targets client))
         ;; show display names of build targets and ask user to select one.
         (selected-build-display-name (completing-read
                                       (format "Selected default built target: ")
                                       (-map (lambda (target)
                                               (bsp-build-target-display-name target))
                                             build-targets)
                                       nil
                                       t))
         (selected-target (build--target-from-display-name
                           selected-build-display-name build-targets)))
    
    ;; update our selected build target id for the current client.
    (setf (bsp--client-selected-build-target client) selected-target)
    selected-target))


(defun bsp--client-capabilities ()
  "Currently just return a list of language ids we support"
  (list :languageIds
        (list "scala")))

(defun bsp--intialise (workspace)
  "temporary testing function to send initialise to bsp server."
  ;; method params callback
  (bsp-log "bsp--initialise")
  (bsp-request-async workspace
                     "build/initialize"
                     (list :displayName "bsp-mode"
                           :version "0.1"
                           :bspVersion "2.0.0"
                           :rootUri (bsp--path-to-uri (bsp--workspace-root workspace))
                           :capabilities (bsp--client-capabilities))
                     (lambda (response)
                       ;; Inform bsp server that we're initialised.
                       (bsp-notify workspace "build/initialized" nil)
                       
                       ;; update client to indicate that we are initialised.
                       (let* ((client (bsp--workspace-client workspace))
                              (build-targets (bsp-query-build-targets workspace)))
                         ;; query build targets and store in our client.
                         (setf (bsp--client-build-targets client) build-targets)
                         (setf (bsp--client-initialised client) t)))
                     :mode 'detached))

(defun create--build-target (target)
  "Create bsp-build-target struct from individual target from workspace/buildTargets"
  (let* ((display-name    (gethash "displayName" target))
         (id              (gethash "id" target))
         (base-directory  (gethash "baseDirectory" target))
         (language-ids    (gethash "languageIds" target)))
    (message "build target %s" display-name)
    (make-bsp-build-target :id id
                           :display-name display-name
                           :base-directory base-directory
                           :language-ids language-ids)))

(defun process--build-targets (response)
  "Create a list of bsp-build-target structs from response from workspace/buildTargets"
  (let* ((targets (gethash "targets" (cdr (car response)))))
    (-map 'create--build-target targets)))

;;  https://github.com/scalacenter/bsp/blob/master/docs/bsp.md#scala-build-target
(defun bsp-query-build-targets (workspace)
  "Synchronous call to query build targets with the build server for the given workspace.
Returns a sequence of bsp-build-target structs."
  (let ((response (bsp-request workspace "workspace/buildTargets" nil)))
    (process--build-targets response)))


(defun bsp--add-running-task (workspace task)
  (setf (bsp--workspace-running-tasks workspace)
        (cons task (bsp--workspace-running-tasks workspace))))

(defun bsp-compile-target (workspace build-target)
  "Ask the bsp server to compile the build target described by the bsp-build-target struct
for the given workspace."
  (let* ((target-id (bsp-build-target-id build-target)))
    ;; keep track of our running tasks.
    (bsp--add-running-task workspace (make-bsp-running-task
                                      :build-target build-target
                                      :task-type 'compile-task))
    
    ;; clear buffer for compilation results
    (bsp--clear-task-buffer (bsp--create-task-buffer-name workspace 'compile-task))
    
    (bsp-request-async workspace
                       "buildTarget/compile"
                       (list :targets (list target-id)
                             :originId "12")
                       (lambda (response)
                         ;; (bsp-log "callback for buildTarget/compile response: %s"
                         ;;          response)
                         
                         ;; remove the running task from the list of running tasks
                         (bsp--remove-running-task workspace 'compile-task))
                       :mode 'detached)))

(defun bsp-run-tests-for-target (workspace build-target)
  "Ask the bsp server to run tests for the build target described by the
bsp-build-target struct and the given workspace."
  (let* ((target-id (bsp-build-target-id build-target)))
    ;; running tests will always invoke compile - so add
    ;; both a compile and test test task.
    (bsp--add-running-task workspace (make-bsp-running-task
                                      :build-target build-target
                                      :task-type 'compile-task))
    
    (bsp--add-running-task workspace (make-bsp-running-task
                                      :build-target build-target
                                      :task-type 'test-task))

    ;; clear buffer for compilation results
    (bsp--clear-task-buffer (bsp--create-task-buffer-name workspace 'compile-task))
    
    ;; clear buffer for our test results.
    (bsp--clear-task-buffer (bsp--create-task-buffer-name workspace 'test-task))
    
    (bsp-request-async workspace
                       "buildTarget/test"
                       (list :targets (list target-id)
                             :originId "12"
                             :data (list :testClasses
                                         (list :target target-id
                                               :classes (list "example.HelloSpec2"))))
                       (lambda (response)
                         ;; (bsp-log "callback for buildTarget/test response: %s"
                         ;;          response)
                         
                         ;; remove the running test and compile tasks from
                         ;; the list of running tasks in this workspace.
                         (bsp--remove-running-task workspace 'test-task)
                         (bsp--remove-running-task workspace 'compile-task))
                       
                       :mode 'detached)))

(defun ensure-selected-build-target (workspace)
  "Ensure the workspace has a selected build target, prompt the user to select one
if not and then return the build target selected."
  (let ((client (bsp--workspace-client workspace)))
    (or (bsp--client-selected-build-target client)
        (bsp-select-build-target))))

(defun bsp-run-tests ()
  "Run tests for the selected build target in the current workspace. If a build target
hasn't been selected it will prompt the user to select a target."
  (interactive)
  (if-let* ((workspace (bsp--get-current-workspace))
            (build-target (ensure-selected-build-target workspace)))
      (bsp-run-tests-for-target workspace build-target)
    (error "Current buffer is not part of a bsp workspace")))

(defun bsp-compile ()
  "Compile the selected build target in the current workspace. If a build target
hasn't been selected it will prompt the user to select a target to compile."
  (interactive)
  (if-let* ((workspace (bsp--get-current-workspace))
            (build-target (ensure-selected-build-target workspace)))
      (bsp-compile-target workspace build-target)
    (error "Current buffer is not part of a bsp workspace")))


(cl-defun bsp-request (workspace method params &key no-wait)
  "Send request METHOD with PARAMS.
If NO-WAIT is non-nil send the request as notification."
  (if no-wait
      (bsp-notify workspace method params)
    (let* ((send-time (time-to-seconds (current-time)))
           ;; max time by which we must get a response
           (expected-time (+ send-time lsp-response-timeout))
           resp-result resp-error)
      (bsp-request-async workspace
                         method params (lambda (res) (setf resp-result (or res :finished)))
                         :error-handler (lambda (err) (setf resp-error err))
                         :mode 'detached)

      (while (not (or resp-error resp-result))
        (accept-process-output nil 0.01)
        (when (< expected-time (time-to-seconds (current-time)))
          (error "Timeout while waiting for response. Method: %s." method)))

      (cond
       ((eq resp-result :finished) nil)
       (resp-result resp-result)
       ((ht? resp-error) (error (gethash "message" resp-error)))
       (t (error (gethash "message" (cl-first resp-error))))))))

(cl-defun bsp-request-async (workspace method params callback &key mode error-handler no-merge)
  "Send request METHOD with PARAMS."
  (bsp--send-request-async workspace `(:jsonrpc "2.0" :method ,method :params ,params) callback mode error-handler no-merge))


(defun bsp--warn (format &rest args)
  "Display warn message with FORMAT with ARGS."
  (message "%s :: %s" (propertize "BSP" 'face 'warning) (apply #'format format args)))

(defun bsp--create-default-error-handler (method)
  "Default error handler.
METHOD is the executed method."
  (lambda (error)
    (bsp--warn "%s" (or (gethash "message" error)
                        (format "%s Request has failed" method)))))

(defun bsp--create-async-callback (count callback mode method no-merge)
  "Create async handler expecting COUNT results, merge them and call CALLBACK.
MODE determines when the callback will be called depending on the
condition of the original buffer. METHOD is the invoked method.
If NO-MERGE is non-nil, don't merge the results but return alist workspace->result."
  (let ((buf (current-buffer))
        results)
    (cl-labels ((handle-result () (funcall
                                   callback
                                   results)))
      (pcase mode
        ('detached (lambda (result)
                     (push (cons bsp--cur-workspace result) results)

                     (when (and (eq (length results) count))
                       (handle-result))))
        
        (_ (lambda (result)
             (push (cons bsp--cur-workspace result) results)
             (if (and (eq (length results) count)
                      (eq buf (current-buffer)))
                 (handle-result)
               (bsp-log "Buffer switched - ignoring reponse. Method %s" method))))))))

(defun bsp--make-message (params)
  "Create a BSP message from PARAMS, after encoding it to a JSON string."
  (let* ((json-encoding-pretty-print bsp-print-io)
         (json-false :json-false)
         (body (if (and bsp-use-native-json
                        (fboundp 'json-serialize))
                   (with-no-warnings
                     (json-serialize params
                                     :null-object nil
                                     :false-object :json-false))
                 (json-encode params))))
    (concat "Content-Length: "
            (number-to-string (1+ (string-bytes body)))
            "\r\n\r\n"
            body
            "\n")))

;; TODO - remove workspace as argument - need to query which workspaces can deal with
;; with messages like this.
(defun bsp--send-request-async (workspace body callback &optional mode error-callback no-merge)
  (let* ((start-time (current-time))
         (method (plist-get body :method))
         (workspaces-count 1)
         (async-callback (bsp--create-async-callback workspaces-count
                                                     callback
                                                     mode
                                                     method
                                                     no-merge))
         (error-async-callback (bsp--create-async-callback workspaces-count
                                                           (or error-callback
                                                               (bsp--create-default-error-handler method))
                                                           mode
                                                           method
                                                           no-merge))
         (id (cl-incf bsp-last-id))
         (body (plist-put body :id id)))
    (let ((message (bsp--make-message body)))
      (puthash id
               (list async-callback error-async-callback method start-time (current-time))
               (-> workspace
                   bsp--workspace-client
                   bsp--client-response-handlers))
      (bsp--send-no-wait message (bsp--workspace-proc workspace)))
    body))

(define-inline bsp--make-notification (method &optional params)
  "Create notification body for method METHOD and parameters PARAMS."
  (inline-quote
   (progn (cl-check-type ,method string)
          (list :jsonrpc "2.0" :method ,method :params ,params))))

(defun bsp--send-notification (workspace body)
  "Send BODY as a notification to the language server."
  (bsp--send-no-wait (bsp--make-message body)
                     (bsp--workspace-proc workspace)))

(defalias 'bsp-send-notification 'bsp--send-notification)

(defun bsp-notify (workspace method params)
  "Send notification METHOD with PARAMS."
  (bsp--send-notification workspace (bsp--make-notification method params)))


;;
;; Logging and debugging
;;

(defun bsp--json-pretty-print (msg)
  "Convert json MSG string to pretty printed json string."
  (let ((json-encoding-pretty-print t))
    (json-encode (json-read-from-string msg))))

(defun bsp-log (format &rest args)
  "Log message to the ’*bsp-log*’ buffer.

FORMAT and ARGS i the same as for `message'."
  (when bsp-log-max
    (let ((log-buffer (get-buffer bsp-log-buffer))
          (inhibit-read-only t))
      (unless log-buffer
        (setq log-buffer (get-buffer-create bsp-log-buffer))
        (with-current-buffer log-buffer
          (view-mode 1)
          (set (make-local-variable 'bsp--log-lines) 0)))
      (with-current-buffer log-buffer
        (save-excursion
          (let* ((message (apply 'format format args))
                 ;; Count newlines in message.
                 (newlines (1+ (cl-loop with start = 0
                                        for count from 0
                                        while (string-match "\n" message start)
                                        do (setq start (match-end 0))
                                        finally return count))))
            (goto-char (point-max))

            ;; in case the buffer is not empty insert before last \n to preserve
            ;; the point position(in case it is in the end)
            (if (eq (point) (point-min))
                (progn
                  (insert "\n")
                  (backward-char))
              (backward-char)
              (insert "\n"))
            (insert message)

            (setq bsp--log-lines (+ bsp--log-lines newlines))

            (when (and (integerp bsp-log-max) (> bsp--log-lines bsp-log-max))
              (let ((to-delete (- bsp--log-lines bsp-log-max)))
                (goto-char (point-min))
                (forward-line to-delete)
                (delete-region (point-min) (point))
                (setq bsp--log-lines bsp-log-max)))))))))

;;
;; json rpc communication code for bsp - based on bsp-mode
;;
;; For now remove the call to bsp--workspace-print - see lsp-mode if we
;; need to add this in future.
(defun bsp--send-no-wait (message proc)
  "Send MESSAGE to PROC without waiting for further output."
  ;; note message will be pretty printed already if bsp-print-io is set via
  ;; our make-message function.
  (when bsp-print-io
    (bsp-log "bsp--send-no-wait (async)\n%s" message))
  (when (memq (process-status proc) '(stop exit closed failed nil))
    (error "%s: Cannot communicate with the process (%s)" (process-name proc)
           (process-status proc)))
  (process-send-string proc message))

;;
;; bsp connection read from the workspace .bsp directory or machine
;; ~/.bsp
;;
(cl-defstruct bsp-connection
  ;; name of build server.
  (name nil :read-only t)
  
  ;; version of this build server.
  (version nil :read-only t)
  
  ;; bsp version supported by build server.
  (bsp-version nil :read-only t)
  
  ;; List of strings containing language ids - "scala", "java" etc
  (languages nil :read-only t)

  ;; executable path and arguments - list of strings.
  (argv nil :read-only t))

(defun workspace--for-root (root)
  "Return the workspace for the given root directory - via the bsp-session"
  (gethash root (bsp-session-workspace-root->servers (bsp-session))))

(defun bsp--get-current-workspace ()
  "Return the current workspace for the current buffer or nil"
  (workspace--for-root (bsp-workspace-root)))

(defun read-bsp-connection-file (bsp-connection-file)
  "Read the bsp connection and construct a bsp-connection struct."
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
         (json (json-read-file bsp-connection-file))
         (name (gethash "name" json))
         (version (gethash "version" json))
         (bsp-version (gethash "bspVersion" json))
         (languages (gethash "languages" json))
         (argv (gethash "argv" json)))
    (make-bsp-connection :name name
                         :version version
                         :bsp-version bsp-version
                         :languages languages
                         :argv argv)))

(defun read-workspace-bsp-connection-files (root)
  "Read bsp connection files within the workspace return a list of
bsp-connection structs for each build server defined."
  (let* ((bsp-dir (concat (file-name-as-directory root) ".bsp"))
         (bsp-connection-files (if (file-directory-p bsp-dir)
                                   (directory-files bsp-dir t "\\.bsp$")
                                 '())))
    (-map 'read-bsp-connection-file bsp-connection-files)))

(defun language-id-from-major-mode (lang-mode)
  "Return the bsp/lsp language id from the major mode."
  (cdr (seq-find (lambda (lang-config)
                   (equal (car lang-config) lang-mode))
                 bsp-language-id-configuration
                 '())))

(defun find-bsp-server (bsp-connections lang-mode)
  "Find bsp server that supports the language-id associated with the major mode.
bsp-language-id-configuration contains a list of major-mode/language-id pairs.
We search for the language id associated with the major mode and see if we can
match this against a bsp server providing support for this id."
  (let* ((language-id (language-id-from-major-mode lang-mode))
         ;; filter bsp-servers by extension
         (selected-connections (seq-filter
                                (lambda (bsp-conn)
                                  (seq-contains (bsp-connection-languages bsp-conn) language-id))
                                bsp-connections)))
    (when (not (and selected-connections (= 1 (length selected-connections))))
        ;; TODO - if multiple matches prompt the user to select connection.
        (error "No bsp-server found to match current buffer or more than one server matches"))
    (car selected-connections)))

(defun bsp--init-workspace ()
  "Initialise the bsp workspace, read bsp connection files, attempt to
start the bsp server associated with this workspace and then initialise
the build server."
  (let* ((root (bsp-workspace-root))
         (bsp-connections (read-workspace-bsp-connection-files root)))

    (when (not bsp-connections)
      (error "Could not find .bsp directory or any bsp connection files."))
    
    ;; extract file extension for current buffer
    (let ((bsp-connection (find-bsp-server bsp-connections major-mode)))
      (when bsp-connection
        (let* ((client (make-bsp--client
                        :new-connection (bsp-stdio-connection (bsp-connection-argv bsp-connection))
                        :server-id (bsp-connection-name bsp-connection)))
               (workspace (bsp--start-workspace (bsp-session)
                                                client
                                                root
                                                (bsp-connection-name bsp-connection))))
          ;; store bsp-connection in client.
          (setf (bsp--client-bsp-connection client) bsp-connection)
          ;;store bsp-servers in workspace.
          (setf (bsp--workspace-bsp-connections workspace) bsp-connections)
          
          ;; Async call to initialise bsp server and workspace.
          (bsp--intialise workspace)
          
          (message "bsp server %s started..." (bsp-connection-name bsp-connection)))))))

(defun bsp ()
  "Initialisation of bsp-mode use this with the java/scala-mode hook to
read any bsp connection files in the workspace/machine and allow
it to start the build server for the project."
  (interactive)
  (if (not (bsp--get-current-workspace))
      (bsp--init-workspace)
    (message "bsp workspace already initialised.")))

(defun bsp--report-current-workspace ()
  (interactive)
  (let ((workspace (workspace--for-root (bsp-workspace-root))))
    (when workspace
      (let* ((root (bsp--workspace-root workspace))
             (client (bsp--workspace-client workspace))
             (server-id (bsp--client-server-id client))
             (initialised (bsp--client-initialised client))
             (build-targets (-map (lambda (target) (bsp-build-target-display-name target))
                                  (bsp--client-build-targets client)))
             (selected-build-target (bsp--client-selected-build-target client)))
        
        (message "Workspace root: %s\nserver-id: %s\ninitialised: %s\nbuild targets: %s\nselected build target: %s"
                 root server-id initialised (string-join build-targets ",")
                 (if selected-build-target
                     (bsp-build-target-display-name selected-build-target)
                   "none"))))))


;;
;; Test code
;;

;; (if (eq system-type `windows-nt)
;;     (progn
;;       (setq bloop-launcher-command '("f:/utils/bin/bloop-launcher.bat" "1.2.5"))
;;       (setq root "f:/dev/scalatest"))
;;   (progn
;;     (setq bloop-launcher-command '("~/utils/bloop-launcher" "1.2.5"))
;;     (setq root "/home/dsyzling/projects/scala/language")))

;;(setq build-servers (read-workspace-bsp-connection-files root))

;; (setq bsp--session nil)
;; (setq session (bsp-session))
;; (setq server-id "bloop")
;; (setq client (make-bsp--client :new-connection (bsp-stdio-connection bloop-launcher-command)
;;                                :server-id server-id))
;; (setq workspace (bsp--start-workspace session client root server-id))


;; (bsp--intialise workspace)
;; (setq our-targets (bsp-query-build-targets workspace))

;; (bsp-compile-target workspace (car our-targets))

;; (bsp-shutdown-workspace workspace)

;;(setq bloop-launcher-command "~/utils/bloop-launcher 1.2.5")

;; windows version - find test workspace
;; (setq workspace (gethash "f:/dev/scalatest/" (bsp-session-workspace-root->servers(bsp-session))))
;; (setq client (bsp--workspace-client workspace))

(provide 'bsp-mode)

;;; bsp-mode.el ends here
