;;; sh-utils.el --- Lisp wrappers for some standart programs

;; Author:  Alexander Zhuckov <zuav@int.spb.ru>
;; Created: Mon Mar 26 19:42:40 MSD 2001
;; Keywords: shell utils
;; $Id: sh-utils.el,v 1.7 2003/01/15 13:29:26 zuav Exp $

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;

;; tail FILENAME OUTPUT-CODING
;; who

;;; Code:

(require 'net-utils)

(defvar tail-program "tail"
  "Program to show tails of files.")

(defvar  tail-program-options (list "-f")
  "tail program options.")

(defvar who-program "who"
  "Program to show who is logged on.")

(defvar  who-program-options (list "--heading")
  "who program options.")

(defun tail (filename decoding)
  "Tail FILENAME and show results in separate buffer.
Optional argument DECODING specifies output encoding to decode tail output."
  (interactive "fTail file: \nZOutput encoding: ")
  (let* ((fn (expand-file-name filename))
         (options (append tail-program-options (list fn)))
         (encoding (cdr default-process-coding-system))
         (buffer nil))
    (save-excursion
        (if (not decoding) (setq decoding (car default-process-coding-system)))
        (setq buffer (net-utils-run-program
                      (concat "Tail" " " (file-name-nondirectory fn))
                      ""
                      tail-program options))
        (set-buffer buffer)
        (set-buffer-process-coding-system decoding encoding))))


(defun who ()
  "Call 'who' and show results in separate buffer."
  (interactive)
  (net-utils-run-program "Who" "" who-program who-program-options))


(provide 'sh-utils)

;;; sh-utils.el ends here
