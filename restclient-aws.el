;;; restclient-aws.el --- -*- lexical-binding: t -*-

;; Package-Requires: ((emacs "28") (restclient "1.0"))

;;; Code:

(require 'restclient)

;;;###autoload
(defun restclient-aws-http-do (send-fn method url headers entity &rest handle-args)
  (let* ((vars (restclient-find-vars-before-point))
         (profile (alist-get "aws-profile" vars))
         (region (alist-get "aws-region" vars)))
    )
  (apply send-fn method url headers entity handle-args))

;;;###autoload
(advice-add 'restclient-http-do :around 'restclient-aws-http-do)

(provide 'restclient-aws)

;;; restclient-aws.el ends here
