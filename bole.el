;;; bole.el --- The minimal hyperbole without hyper -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

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

(defun bole-action-key-error ()
  "The default function to be runned when no action is available."
  (error "(Bole Action Key): No action defined for this context; try another location"))

(defun bole-assist-key-error ()
  "The default function to be runned when no assist is available."
  (error "(Bole Assist Key): No action defined for this context; try another location"))

(provide 'bole)
;;; bole.el ends here
