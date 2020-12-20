;;; bole.el --- The minimal hyperbole without hyper -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'eieio)
(require 'cl-lib)

(defgroup bole nil
  "The minimal hyperbole without hyper."
  :prefix "bole-"
  :group 'tools
  :link '(url-link "https://github.com/condy0919/bole"))

(defcustom bole-action-key-default-function #'bole-action-key-error
  "Function to run by the Action Key in an unspecified context.
Set it to `ignore' if you want to silence errors."
  :type 'function
  :group 'bole)

(defcustom bole-assist-key-default-function #'bole-assist-key-error
  "Function to run by the Assist Key in an unspecified context.
Set it to `ignore' if you want to silence errors."
  :type 'function
  :group 'bole)

(defcustom bole-key-event-list (list
                                (bole-key-event :pred (lambda () t)
                                                :action (lambda ()
                                                          (message "run")))
                                )
  "A list of `bole-key-event'."
  :type '(repeat bole-key-event)
  :group 'bole)

(defconst bole-help-buffer-name "*Bole-Help*"
  "The bole help buffer name.")

(defun bole-action-key-error ()
  "The default function to be runned when no action is available."
  (error "(Bole Action Key): No action defined for this context; try another location"))

(defun bole-assist-key-error ()
  "The default function to be runned when no assist is available."
  (error "(Bole Assist Key): No action defined for this context; try another location"))

(defun bole-key-execute (assist)
  "Evaluate Action Key form (or Assist Key form with ASSIST non-nil)."
  (cl-loop for ev in bole-key-event-list
           when (funcall (bole-key-event-pred ev))
           return (funcall (bole-key-event-action ev) assist)))

(defun bole-key-execute-with-callback (cb)
  "Call CB with the active `bole-key-event'."
  (cl-loop for ev in bole-key-event-list
           when (funcall (bole-key-event-pred ev))
           return (funcall cb ev)))

(defclass bole-key-event ()
  ((pred :initarg :pred
         :type function
         :reader bole-key-event-pred
         :documentation "The predicator of the key-event.")
   (action :initarg :action
           :type function
           :reader bole-key-event-action
           :documentation "The action ot the key-event."))
  "A class for modularizing the event of a key.")

;;;###autoload
(defun bole-action-key ()
  "Use one key to perform functions that vary by context.
If no matching context is found, the default function set with
the `bole-action-key-default-function' variable is run."
  (interactive)
  (unless (bole-key-execute nil)
    (funcall bole-action-key-default-function)))

;;;###autoload
(defun bole-assist-key ()
  "Use one key to perform functions tat vary by context.
If no matching context is found, the default function set with
the `bole-assist-key-default-function' variable is run."
  (interactive)
  (unless (bole-key-execute t)
    (funcall bole-assist-key-default-function)))

;;;###autoload
(defun bole-key-either (arg)
  "Execute `bole-action-key' or with non-nil ARG execute `bole-assist-key'."
  (interactive "P")
  (if arg
      (bole-assist-key)
    (bole-action-key)))

;;;###autoload
(defun bole-key-help ()
  "Display help for the Action/Assist Key command in current context."
  (interactive)
  (bole-key-execute-with-callback
   (lambda (ev)
     (with-output-to-temp-buffer bole-help-buffer-name
       (princ "WHEN ")
       (princ (bole-key-event-pred ev))
       (terpri)
       (princ "WILL ")
       (princ (bole-key-event-action ev))))))

(provide 'bole)
;;; bole.el ends here
