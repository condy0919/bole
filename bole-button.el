;;; bole-button.el --- Bole button constructs -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'cl-lib)

;;; ========================================================================
;;; Bole Buttons Abstraction
;;; ========================================================================

;; https://emacs.stackexchange.com/q/14269
(defun bole-button-in-comments-p ()
  "Return t if within a programming language buffer and prior regexp match in in a comment, else nil."
  (and (derived-mode-p 'prog-mode)
       (nth 4 (syntax-ppss))))

(defun bole-button-map (func start-delim end-delim)
  "Apply FUNC to bole buttons in the visible part of the current buffer.
The set of buttons are those whose labels are delimited by
START-DELIM and END-DELIM.

FUNC must take precisely three arguments: the button label, the
start position of the delimited button label and its end
position."
  (let* ((end-sym (regexp-quote (substring end-delim -1)))
         (delim-regexp (concat (regexp-quote start-delim)
                               (format "\\([^%s\"][^%s]*\\)" end-sym end-sym)
                               (regexp-quote end-delim)))
         (results))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward delim-regexp nil t)
        (let ((start (match-beginning 1))
              (end (match-end 1))
              (label (match-string 1)))
          ;; If within a programming language buffer, ignore matches outside
          ;; comments.
          (when (bole-button-in-comments-p)
            (cl-pushnew (funcall func label start end) results)))))
    (nreverse results)))

;;; ========================================================================
;;; Explicit Bole Buttons
;;; ========================================================================

(defconst bole-explicit-button-start "<("
  "String matching the start of a bole explicit button.")

(defconst bole-explicit-button-end ")>"
  "String matching the end of a bole explicit button.")

;;; ========================================================================
;;; Implicit Bole Buttons
;;; ========================================================================

(defconst bole-implicit-button-label-start "<["
  "String matching the start of a bole implicit button label.")

(defconst bole-implicit-button-label-end "]>"
  "String matching the end of a bole implicit button label.")

(defun bole-implicit-button-map (func)
  "Apply FUNC to the labeled implicit buttons.
The set of implicit buttons are those labels are delimited by
START-DELIM and END-DELIM.

FUNC must take precisely three arguments: the button label, the
start position of the delimited button label and its end
position."
  (bole-button-map func bole-implicit-button-label-start bole-implicit-button-label-end))

(defun bole-implicit-button-activate (label)
  "Activate bole implicit button with <[LABEL]> from the current buffer."
  (interactive (list (completing-read "Activate implicit button labeled: "
                                      (bole-implicit-button-list)
                                      nil
                                      t
                                      nil)))
  (message "%S" label)
  nil)

(defun bole-implicit-button-at-point (&optional key-only)
  "Return symbol for bole implicit button at point, else nil.
Point may be on the implicit button text or its optional
preceding label. With optional KEY-ONLY, return the label key for
button only.

Any labeled implicit button must contain at least two characters,
excluding delimiters, not just one."


  nil )

;; ibut:list
(defun bole-implicit-button-list (&optional buf)
  "Return list of labeled implicit buttons in BUF or current buffer."
  (with-current-buffer (or buf (current-buffer))
    (bole-implicit-button-map (lambda (label _start _end)
                                (substring-no-properties label)))))

;; <[My Emacs Conf]>: "~/.emacs.d/"
;; <[TCP]>: rfc-793

(provide 'bole-button)
;;; bole-button.el ends here
