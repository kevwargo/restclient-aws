;;; restclient-aws.el --- -*- lexical-binding: t -*-

;; Package-Requires: ((emacs "28") (restclient "1.0"))

;;; Code:

(require 'restclient)

;;;###autoload
(defun restclient-aws-http-do (send-fn method url headers body &rest handle-args)
  (let* ((vars (restclient-find-vars-before-point))
         (profile (cdr (assoc "aws-profile" vars)))
         (region (cdr (assoc "aws-region" vars)))
         (service (cdr (assoc "aws-service" vars))))
    (if (and profile region)
        (restclient-aws--sign profile
                              region
                              service
                              (list :method method
                                    :url url
                                    :headers headers
                                    :body body
                                    :send-fn send-fn
                                    :handle-args handle-args))
      (apply send-fn method url headers body handle-args))))

;;;###autoload
(advice-add 'restclient-http-do :around 'restclient-aws-http-do)

(defvar restclient-aws--signer-proc nil
  "Signer server process")

(defvar restclient-aws--cache-dir
  (expand-file-name (locate-user-emacs-file ".cache/restclient-aws"))
  "A directory which contains signer executable and socket")

(defvar restclient-aws--signer-socket
  (expand-file-name "signer.sock" restclient-aws--cache-dir))

(defvar restclient-aws--signer-exec
  (expand-file-name "signer" restclient-aws--cache-dir))

(defvar restclient-aws--signer-src-dir nil)
(if load-file-name
    (setq restclient-aws--signer-src-dir
          (expand-file-name
           "signer"
           (file-name-directory (file-truename (file-name-with-extension
                                                (file-name-sans-extension load-file-name)
                                                "el"))))))

(defun restclient-aws--sign (profile region service request)
  (let ((init-client-fn (lambda (client)
                   (process-put client :request request)
                   (process-send-string client
                                        (json-serialize
                                         `((profile . ,profile)
                                           (region . ,region)
                                           (service . ,service)
                                           (method . ,(plist-get request :method))
                                           (url . ,(plist-get request :url))
                                           (headers . ,(mapcar (lambda (h)
                                                                 (cons (intern (car h)) (cdr h)))
                                                               (plist-get request :headers)))
                                           (body . ,(plist-get request :body)))
                                         :null-object nil))))
        (client (restclient-aws--connect)))
    (if client
        (funcall init-client-fn client)
      (restclient-aws--reconnect init-client-fn))))

(defun restclient-aws--connect (&optional keep-unbound-socket)
  (restclient-aws--log "Connecting to %s ..." restclient-aws--signer-socket)
  (condition-case err
      (make-network-process
       :name "aws-signer-client"
       :family 'local
       :service restclient-aws--signer-socket
       :sentinel 'restclient-aws--client-sentinel
       :filter 'restclient-aws--client-filter)
    (file-missing
     (restclient-aws--log "Socket %s does not exist" restclient-aws--signer-socket))
    (file-error
     (if (equal (nth 2 err) "Connection refused")
         (progn
           (restclient-aws--log "Connection to %s refused" restclient-aws--signer-socket)
           (if (file-exists-p restclient-aws--signer-socket)
               (if keep-unbound-socket
                   (restclient-aws--log "Keeping unbound %s" restclient-aws--signer-socket)
                 (restclient-aws--log "Deleting unbound %s" restclient-aws--signer-socket)
                 (delete-file restclient-aws--signer-socket))))
       (signal (car err) (cdr err))))))

(defun restclient-aws--reconnect (init-client-fn)
  (restclient-aws--maybe-start-signer
   (lambda ()
     (let ((client nil)
           (ts (current-time)))
       (while (and (not client)
                   (process-live-p restclient-aws--signer-proc)
                   (time-less-p (time-since ts) 1))
         (sleep-for 0.1)
         (setq client (restclient-aws--connect t)))
       (if client
           (funcall init-client-fn client)
         (error "Could not reconnect to %s" restclient-aws--signer-socket))))))

(defun restclient-aws--maybe-start-signer (after-signer-start)
  (if (process-live-p restclient-aws--signer-proc)
      (funcall after-signer-start)
    (if (file-executable-p restclient-aws--signer-exec)
        (progn
          (restclient-aws--start-signer)
          (funcall after-signer-start))
      (and (y-or-n-p "Request signer for restclient-aws does not seem to be installed. Try to build from source? ")
           (or (not (file-exists-p restclient-aws--signer-exec))
               (y-or-n-p (format "File %s already exists. Overwrite? "
                                 restclient-aws--signer-exec)))
           (let ((default-directory restclient-aws--signer-src-dir))
             (make-process
              :name "restclient-aws-signer-build"
              :command `("go" "build" "-o" ,restclient-aws--signer-exec "main.go")
              :buffer (with-current-buffer
                          (generate-new-buffer "*restclient-aws-signer-build*")
                        (keymap-local-set "q" 'kill-current-buffer)
                        (keymap-local-set "k" 'kill-current-buffer)
                        (current-buffer))
              :sentinel (lambda (proc event)
                          (if (equal event "finished\n")
                              (progn
                                (kill-buffer (process-buffer proc))
                                (restclient-aws--start-signer)
                                (funcall after-signer-start))
                            (with-current-buffer (process-buffer proc)
                              (goto-char (point-max))
                              (insert (propertize event 'face 'font-lock-warning-face))
                              (pop-to-buffer (current-buffer)))))))))))

(defun restclient-aws--start-signer ()
  (setq restclient-aws--signer-proc
        (make-process
         :name "aws-signer"
         :command (list restclient-aws--signer-exec restclient-aws--signer-socket)
         :buffer "*aws-signer*"))
  (restclient-aws--log "Started %s with PID %d"
                       restclient-aws--signer-exec
                       (process-id restclient-aws--signer-proc)))

(defun restclient-aws--client-sentinel (p event)
  (pcase event
    ((or "connection broken by remote peer\n" "finished\n" "deleted\n")
     (restclient-aws--log "%S: %s" p (substring event 0 (1- (length event))))
     (if (process-buffer p)
         (kill-buffer (process-buffer p))))
    ((rx line-start "open"))
    (_
     (restclient-aws--log "%S: unexpected event %S" p event)
     (delete-process p))))

(defun restclient-aws--client-filter (p input)
  (let (responses)
    (with-current-buffer
        (or (process-buffer p)
            (set-process-buffer p (generate-new-buffer "*aws-signer-client*")))
      (goto-char (point-max))
      (insert input)
      (goto-char (or (process-get p :json-point) (point-min)))
      (while (condition-case err
                 (progn
                   (push (json-parse-buffer :object-type 'alist) responses)
                   (process-put p :json-point (point)))
               (json-end-of-file)
               (error
                (restclient-aws--log "%S: parsing json response: %S. Buffer contents: %s"
                                     p err
                                     (buffer-substring-no-properties
                                      (point-min) (point-max)))))))
    (mapc (apply-partially 'restclient-aws--handle-server-resp p)
          (nreverse responses))))

(defun restclient-aws--handle-server-resp (proc resp)
  (pcase (alist-get 'type resp)
    ("mfa-prompt"
     (process-send-string proc (restclient-aws--read-mfa resp)))
    ("signed"
     (restclient-aws--complete-request (plist-put (process-get proc :request)
                                                  :headers
                                                  (alist-get 'headers resp))))
    (_
     (let* ((err (alist-get 'err-msg resp))
            (err (if err
                     (format "ERROR: %s" err)
                   (format "unexpected response %S" resp))))
       (restclient-aws--log "%S: ERROR: %s" proc err)
       (message "%S: ERROR: %s" proc err)))))

(defun restclient-aws--read-mfa (resp)
  (let* ((profile (format "profile %s" (alist-get 'profile resp)))
         (role (alist-get 'role-arn resp))
         (role (if role (format ", role %s" role) ""))
         (serial (alist-get 'serial-number resp))
         (serial (if serial (format ", serial %s" serial) "")))
    (concat (read-from-minibuffer
             (format "Enter MFA code for %s%s%s: " profile role serial))
            "\n")))

(defun restclient-aws--complete-request (request)
  (apply (plist-get request :send-fn)
         (plist-get request :method)
         (plist-get request :url)
         (mapcar (lambda (h)
                   (cons (symbol-name (car h)) (cdr h)))
                 (plist-get request :headers))
         (plist-get request :body)
         (plist-get request :handle-args)))

(defun restclient-aws--log (fmt &rest args)
  (let ((msg (apply 'format
                    (concat "[%s] " fmt)
                    (format-time-string "%Y%m%d-%H%M%S.%3N")
                    args)))
    (with-current-buffer (get-buffer-create "*restclient-aws-log*")
      (setq display-line-numbers t)
      (goto-char (point-max))
      (insert msg "\n")))
  nil)

(provide 'restclient-aws)

;;; restclient-aws.el ends here
