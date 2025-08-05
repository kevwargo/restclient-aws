;;; restclient-aws.el --- -*- lexical-binding: t -*-

;; Package-Requires: ((emacs "28") (restclient "1.0"))

;;; Code:

(require 'restclient)

;;;###autoload
(defun restclient-aws-http-do (send-fn method url headers body &rest handle-args)
  (let* ((vars (restclient-find-vars-before-point))
         (profile (alist-get "aws-profile" vars))
         (region (alist-get "aws-region" vars)))
    )
  (apply send-fn method url headers body handle-args))

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

(defun restclient-aws--connect ()
  (condition-case err
      (make-network-process
       :name "aws-signer-client"
       :family 'local
       :service restclient-aws--signer-socket
       :sentinel 'restclient-aws--client-sentinel
       :filter 'restclient-aws--client-filter)
    (file-missing)
    (file-error (if (and (equal (nth 2 err) "Connection refused")
                         (file-exists-p restclient-aws--signer-socket))
                    'socket-unbound
                  (signal (car err) (cdr err))))))

(defun restclient-aws--client-sentinel (p event)
  (pcase event
    ((or "connection broken by remote peer\n" "finished\n" "deleted\n")
     (restclient-aws--log "%S is done: %s" p (s-trim event))
     (if (process-buffer p) (kill-buffer (process-buffer p))))
    ((rx line-start "open")
     (restclient-aws--log "opened aws-signer-client proc %S" p))
    (_ (restclient-aws--log "unexpected proc %S event %S"
                            p event))))

(defun restclient-aws--client-filter (p input)
  (let (resp received)
    (with-current-buffer (or (process-buffer p)
                             (set-process-buffer p (generate-new-buffer "*aws-signer-client*")))
      (goto-char (point-max))
      (insert input)
      (goto-char (or (process-get p :json-point) (point-min)))
      (condition-case e
          (json-parse-buffer :object-type 'alist)
        (json-end-of-file)
        (error (restclient-aws--log "parsing json response in %S: %S"
                                    (current-buffer) e))
        (:success
         (setq resp e received t)
         (process-put p :json-point (point)))))
    (if received
        (restclient-aws--handle-server-resp p resp))))

(defun restclient-aws--handle-server-resp (proc resp)
  (pcase (alist-get 'type resp)
    ("mfa-prompt" (process-send-string proc
                                       (concat (read-from-minibuffer
                                                (format "%S: "
                                                        (cl-remove-if (lambda (f)
                                                                        (eq (car f) 'type))
                                                                      resp)))
                                               "\n")))
    (_ (restclient-aws--log "%S: %S" proc resp))))

(defun restclient-aws--sign (profile)
  (interactive "MAWS profile: ")
  (let ((client (restclient-aws--connect)))
    (unless (processp client)
      (restclient-aws--log "first attempt to connect to signer server failed, trying again")
      (when (eq client 'socket-unbound)
        (delete-file restclient-aws--signer-socket)
        (restclient-aws--log "removed unbound socket %s" restclient-aws--signer-socket))
      (unless (process-live-p restclient-aws--signer-proc)
        (setq restclient-aws--signer-proc
              (make-process
               :name "aws-signer"
               :command (list restclient-aws--signer-exec restclient-aws--signer-socket)
               :buffer "*aws-signer*"))
        (restclient-aws--log "started %s with PID %d"
                             restclient-aws--signer-exec
                             (process-id restclient-aws--signer-proc)))
      (let ((ts (current-time)))
        (while (and (time-less-p (time-since ts) 1)
                    (not (processp client)))
          (setq client (restclient-aws--connect))))
      (unless (processp client)
        (user-error "Cannot connect to %s" restclient-aws--signer-socket)))
    (restclient-aws--log "%S connected to %s" client restclient-aws--signer-socket)
    (process-send-string client (json-serialize `((profile . ,profile))))))

(defun restclient-aws--log (fmt &rest args)
  (let ((msg (apply 'format fmt args)))
    (with-current-buffer (get-buffer-create "*restclient-aws-log*")
      (goto-char (point-max))
      (insert msg "\n"))))

(provide 'restclient-aws)

;;; restclient-aws.el ends here
