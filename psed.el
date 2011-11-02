;;; psed.el --- Unix ps command in emacs buffer

;; Copyright (C) 1999, 2000, 2001, 2002 Alexander Zhuckov.

;; Author: Alexander Zhuckov <zuav@int.spb.ru>
;; Keywords: convenience major-mode processes

;; This file may be part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Major mode for GNU Emacs to work with list of running processes.
;; 

;; Что надо сделать:
;;   найти или создать буфер с соответствующим именем
;;   определить keymap для работы с процессами
;;   запустить асинхронный процесс `ps'
;;   отформатировать результат работы `ps' и вывести в буфер
;;   режим автообновления a la top
;;   возможность отмечать процесы как файлы в Dired
;;   
;; Команды:
;;
;;   up, down    -- вверх/вниз на одну строку, курсор в определенной колонке;
;;   pgup/pgdown -- вверх/вниз на одну страницу, курсор в определенной колонке;
;;   K           -- пристрелить отмеченные процессы, или тот процесс, на котором курсор;
;;   k           -- пристрелить отмеченные процессы, если отмеченных процессов нет,
;;                  то ничего не делать;
;;   C-k         -- пристрелить  отмеченные процессы, или тот процесс, на котором курсор,
;;                  через sudo, запросив пароль;
;;   M-K         -- послать процессу, на котором курсор, или отмеченным процессам, если
;;                  такие есть, сигнал; сигнал запросить через минибуфер;
;;   M-k         -- послать отмеченным процессам сигнал; сигнал запросить через минибуфер;
;;                  если отмеченных процессов нет, то ничего не делать;
;;   C-M-K       -- послать процессу, на котором курсор, или отмеченным процессам, если
;;                  такие есть, сигнал c помощью sudo; сигнал запросить через минибуфер;
;;                  пароль также запросить через минибуфер;
;;   C-M-k       -- послать отмеченным процессам сигнал; сигнал и пароль
;;                  запросить через минибуфер; если отмеченных процессов нет, то
;;                  ничего не делать;
;;   g           -- обновить список процессов;
;;   s           -- sort by user name;

;;
;; Используемые структуры:
;;

;;
;; $Id: psed.el,v 1.3 2002/06/08 15:43:17 zuav Exp $
;;

;;; Code:

;;
;; User-option variables
;;
(defvar psed-quit-by-kill nil
  "*Kill or bury psed buffer on exit mode.")

(defvar psed-initial-format 'user
  "*Format symbol to print process line.
The value of the variable is used on enter to psed mode.
Premissible values are long, user, jobs, signal, vm, memory.
Any other value will be treated as user.")

;;
;;
;;
(provide 'psed)

;;
;; Global variables
;;
(defvar psed-buffer-name "*ps*")        ; todo: description
(defvar psed-tempbuf-name "*ps-temp*")  ; todo: description
(defvar psed-ps-program-name "/bin/ps") ; todo: description

(defvar psed-current-format nil
  "Current format to print process line.
This variable is set by format switch commands and is used
in psed-revert-buffer to update buffer using correct format.")

;;
;; Psed-process-line: it's methods and variables
;;
;; Every psed-process-line is a vector whith the following elements:
;;   [USER PID %CPU %MEM SIZE RSS TTY STAT START TIME COMMAND]
;;
;;   [%CPU %MEM BLOCKED CATCHED COMMAND DRS DSIZ DT FLAGS IGNORED LIB LIM MAJFLT
;;    MINFLT NI PAGEIN PGID PID PPID PRI RSS SHRD SID SIGNAL SIZE STA START STAT
;;    SWAP TIME TPGID TRS TSIZ TTY UID USER WCHAN]
;;
(defconst psed-process-line-fldnum-cpu      0 "")
(defconst psed-process-line-fldnum-mem      1 "")
(defconst psed-process-line-fldnum-blocked  2 "")
(defconst psed-process-line-fldnum-catched  3 "")
(defconst psed-process-line-fldnum-command  4 "")
(defconst psed-process-line-fldnum-drs      5 "")
(defconst psed-process-line-fldnum-dsiz     6 "")
(defconst psed-process-line-fldnum-dt       7 "")
(defconst psed-process-line-fldnum-flags    8 "")
(defconst psed-process-line-fldnum-ignored  9 "")
(defconst psed-process-line-fldnum-lib     10 "")
(defconst psed-process-line-fldnum-lim     11 "")
(defconst psed-process-line-fldnum-majflt  12 "")
(defconst psed-process-line-fldnum-minflt  13 "")
(defconst psed-process-line-fldnum-ni      14 "")
(defconst psed-process-line-fldnum-pagein  15 "")
(defconst psed-process-line-fldnum-pgid    16 "")
(defconst psed-process-line-fldnum-pid     17 "")
(defconst psed-process-line-fldnum-ppid    18 "")
(defconst psed-process-line-fldnum-pri     19 "")
(defconst psed-process-line-fldnum-rss     20 "")
(defconst psed-process-line-fldnum-shrd    21 "")
(defconst psed-process-line-fldnum-sid     22 "")
(defconst psed-process-line-fldnum-signal  23 "")
(defconst psed-process-line-fldnum-size    24 "")
(defconst psed-process-line-fldnum-sta     25 "")
(defconst psed-process-line-fldnum-start   26 "")
(defconst psed-process-line-fldnum-stat    27 "")
(defconst psed-process-line-fldnum-swap    28 "")
(defconst psed-process-line-fldnum-time    29 "")
(defconst psed-process-line-fldnum-tpgid   30 "")
(defconst psed-process-line-fldnum-trs     31 "")
(defconst psed-process-line-fldnum-tsiz    32 "")
(defconst psed-process-line-fldnum-tty     33 "")
(defconst psed-process-line-fldnum-uid     34 "")
(defconst psed-process-line-fldnum-user    35 "")
(defconst psed-process-line-fldnum-wchan   36 "")

(defconst psed-process-line-size (1+ psed-process-line-fldnum-wchan)
  "The number of fields in psed process line.")

(defsubst psed-process-line-field (psline fldnum)
  "Return field FLDNUM value of the process-line PSLINE."
  (aref psline fldnum))

(defsubst psed-process-line-set-field (psline fldnum value)
  "Set value of the field FLDNUM of process-line PSLINE to VALUE."
  (aset psline fldnum value))

(defun psed-process-line-list-fields (line fldnums)
  "Return list of fields values from process line LINE with numbers FLDNUMS."
  (cond ((null fldnums) nil)
        (t (cons (psed-process-line-field line (car fldnums))
                 (psed-process-line-list-fields line (cdr fldnums))))))

(defun psed-process-line-print (fmt fldnums pline)
  "Print fields FLDNUMS from process line PLINE using format string FMT."
  (insert (apply 'format fmt 
                 (psed-process-line-list-fields pline fldnums))))

;;
;; Psed-service-array: structure and it's methods
;; (makunbound 'psed-service-array)
(defvar psed-service-array
  (let ((vec (vector
              [4  0 t psed-skip-non-blanks "%CPU"]      ; u
              [4  0 t psed-skip-non-blanks "%MEM"]      ; u       v
              [10 0 t psed-skip-non-blanks "BLOCKED"]   ;       s
              [10 0 t psed-skip-non-blanks "CATCHED"]   ;       s
              [1  0 t psed-skip-to-eol     "COMMAND"]   ; u l j s v m
              [3  0 t psed-skip-non-blanks "DRS"]       ;           m
              [4  0 t psed-skip-non-blanks "DSIZ"]      ;         v
              [2  0 t psed-skip-non-blanks "DT"]        ;           m
              [5  0 t psed-skip-non-blanks "FLAGS"]     ;   l
              [10 0 t psed-skip-non-blanks "IGNORED"]   ;       s
              [3  0 t psed-skip-non-blanks "LIB"]       ;           m
              [3  0 t psed-skip-non-blanks "LIM"]       ;         v
              [6  0 t psed-skip-non-blanks "MAJFLT"]    ;           m
              [6  0 t psed-skip-non-blanks "MINFLT"]    ;           m
              [2  0 t psed-skip-non-blanks "NI"]        ;   l
              [6  0 t psed-skip-non-blanks "PAGEIN"]    ;         v
              [10 0 t psed-skip-non-blanks "PGID"]      ;     j
              [5  0 t psed-skip-non-blanks "PID"]       ; u l j s v m
              [5  0 t psed-skip-non-blanks "PPID"]      ;   l j
              [3  0 t psed-skip-non-blanks "PRI"]       ;   l
              [10 0 t psed-skip-non-blanks "RSS"]       ; u l     v m
              [4  0 t psed-skip-non-blanks "SHRD"]      ;           m
              [3  0 t psed-skip-non-blanks "SID"]       ;     j
              [10 0 t psed-skip-non-blanks "SIGNAL"]    ;       s
              [10 0 t psed-skip-non-blanks "SIZE"]      ; u l       m
              [3  0 t psed-skip-non-blanks "STA"]       ;   l
              [4  0 t psed-skip-non-blanks "START"]     ; u
; todo           [4  0 t psed-skip-start      "START"]     ; u
              [5  0 t psed-skip-non-blanks "STAT"]      ; u   j s v
              [4  0 t psed-skip-non-blanks "SWAP"]      ;           m
              [5  0 t psed-skip-non-blanks "TIME"]      ; u l j s v
              [10 0 t psed-skip-non-blanks "TPGID"]     ;     j
              [3  0 t psed-skip-non-blanks "TRS"]       ;           m
              [4  0 t psed-skip-non-blanks "TSIZ"]      ;         v
              [3  0 t psed-skip-non-blanks "TTY"]       ; u l j s v m
              [10 0 t psed-skip-non-blanks "UID"]       ;   l j s
              [9  0 t psed-skip-non-blanks "USER"]      ; u
              [5  0 t psed-skip-non-blanks "WCHAN"])))  ;   l
    vec)
  "This vector contains service info for every field in the ps output.
Each element of the vector is a vector of the following structure:
[DEF-MAXLEN MAXLEN VISIBILITY SKIP-FUNC TITLE], where

  DEF-MAXLEN    -- default max length of the respective field;
  MAXLEN        -- real max length of fields counted during parsing;
  VISIBILITY    -- toggles visibility of the respective field in the mode
                   buffer;
  SKIP-FUNC     -- function to skip to the end of the field; this function must
                   return number of skipped chars;
  TITLE         -- title string for the field.")

(defun psed-service-array-init ()
  "Initialize psed service array.
Sets maxlen value for every field equal to title length.
Must be run before parsing."
  (let ((i 0))
    (while (< i psed-process-line-size)
      (psed-service-array-set-maxlen i (length (psed-service-array-title i)))
      (setq i (1+ i)))))

(defsubst psed-service-array-def-maxlen (fldnum)
  "Return default max len value for field FLDNUM."
  (aref (aref psed-service-array fldnum) 0))

(defsubst psed-service-array-maxlen (fldnum)
  "Return counted max len value for field FLDNUM."
  (aref (aref psed-service-array fldnum) 1))

(defsubst psed-service-array-set-maxlen (fldnum value)
  "Set counted max len value for field FLDNUM to value VALUE."
  (aset (aref psed-service-array fldnum) 1 value))

(defsubst psed-service-array-visible-p (fldnum)
  "Return t if field FLDNUM i visible, nil otherwise."
  (aref (aref psed-service-array fldnum) 2))

(defsubst psed-service-array-skip-func (fldnum)
  "Return skip function for the field."
  (aref (aref psed-service-array fldnum) 3))

(defsubst psed-service-array-title (fldnum)
  "Return title for the field."
  (aref (aref psed-service-array fldnum) 4))

;;
;; Psed-process-list: structure and it's methods
;;
(defvar psed-process-list nil
  "Main psed mode structure -- list of vectors of psed-process-line type.
This list contains info about all process in the system. To work with
structure one should use only the psed-process-list-*** methods.")

(defsubst psed-process-list-clear ()
  "Clear psed-process-list."
  (setq psed-process-list nil))

(defsubst psed-process-list-add (psline)
  "Add PSLINE to the end of psed-process-line."
  (setq psed-process-list (append psed-process-list (list psline))))

(defvar psed-header-line nil
  "")

(defun psed-process-list-print ()
  "Output process list into current buffer line by line."
  (let* ((fh (psed-format-fmt-string-headers))
         (fmt (car fh))
         (headers (cadr fh))
         (fldnums (psed-format-fldnums psed-current-format)))
    ;; print header
    (insert (apply 'format fmt headers))
    ;; print process lines
    (psed-process-list-print-internal fmt fldnums psed-process-list)
    ;;
    (goto-char (point-min))
    (let* ((header-end (save-excursion (forward-line 1) (1- (point))))
           (header-beg (point)))
      (set (make-local-variable 'psed-header-line)
           (concat " " (buffer-substring header-beg header-end)))
      (setq header-line-format 'psed-header-line)
      (narrow-to-region (1+ header-end) (point-max))
)))

(defun psed-process-list-print-internal (fmt fldnums plist)
  "Output process list PLIST into current buffer line by line."
  (cond ((null plist) nil)
        (t (psed-process-line-print fmt fldnums (car plist))
           (psed-process-list-print-internal fmt fldnums (cdr plist)))))

;;
;; Formats
;;
;; Format is a three element list: (ARGS FLDNUMS CURSPOS), where
;;    ARGS     --  string with ps program options;
;;    FLDNUMS  --  an array, containing numbers of respective fields from
;;                 process line;
;;    CURSPOS  --  cursor position on the current line for a given format;
;;                 usually this is the first char of the command;
;;
;; Here's supported ps ouput formats:
;;   long format   (ps awxl): FLAGS UID PID PPID PRI NI SIZE RSS WCHAN STA TTY TIME COMMAND
;;   user format   (ps awxu): USER PID %CPU %MEM SIZE RSS TTY STAT START TIME COMMAND
;;   jobs format   (ps awxj): PPID PID PGID SID TTY TPGID STAT UID TIME COMMAND
;;   signal format (ps awxs): UID PID SIGNAL BLOCKED IGNORED CATCHED STAT TTY TIME COMMAND
;;   vm format     (ps awxv): PID TTY STAT TIME PAGEIN TSIZ DSIZ RSS LIM %MEM COMMAND
;;   memory info   (ps awxm): PID TTY MAJFLT MINFLT TRS DRS SIZE SWAP RSS SHRD LIB DT COMMAND
;;
(defconst psed-format-long (list "awxhl"
                               (list psed-process-line-fldnum-flags
                                     psed-process-line-fldnum-uid
                                     psed-process-line-fldnum-pid
                                     psed-process-line-fldnum-ppid
                                     psed-process-line-fldnum-pri
                                     psed-process-line-fldnum-ni
                                     psed-process-line-fldnum-size
                                     psed-process-line-fldnum-rss
                                     psed-process-line-fldnum-wchan
                                     psed-process-line-fldnum-sta
                                     psed-process-line-fldnum-tty
                                     psed-process-line-fldnum-time
                                     psed-process-line-fldnum-command) 73)
  "Long ps format.")

(defconst psed-format-user (list "awxhu"
                                 (list psed-process-line-fldnum-user
                                       psed-process-line-fldnum-pid
                                       psed-process-line-fldnum-cpu
                                       psed-process-line-fldnum-mem
                                       psed-process-line-fldnum-size
                                       psed-process-line-fldnum-rss
                                       psed-process-line-fldnum-tty
                                       psed-process-line-fldnum-stat
                                       psed-process-line-fldnum-start
                                       psed-process-line-fldnum-time
                                       psed-process-line-fldnum-command) 66)
  "User ps format.")

(defconst psed-format-jobs (list "awxhj" 
                                 (list psed-process-line-fldnum-ppid
                                       psed-process-line-fldnum-pid
                                       psed-process-line-fldnum-pgid
                                       psed-process-line-fldnum-sid
                                       psed-process-line-fldnum-tty
                                       psed-process-line-fldnum-tpgid
                                       psed-process-line-fldnum-stat
                                       psed-process-line-fldnum-uid
                                       psed-process-line-fldnum-time
                                       psed-process-line-fldnum-command) 57)
  "Jobs ps format.")

(defconst psed-format-signal (list "awxhs"
                                   (list psed-process-line-fldnum-uid
                                         psed-process-line-fldnum-pid
                                         psed-process-line-fldnum-signal
                                         psed-process-line-fldnum-blocked
                                         psed-process-line-fldnum-ignored
                                         psed-process-line-fldnum-catched
                                         psed-process-line-fldnum-stat
                                         psed-process-line-fldnum-tty
                                         psed-process-line-fldnum-time
                                         psed-process-line-fldnum-command) 1)
  "Signal ps format.")

(defconst psed-format-vm (list "awxhv"
                               (list psed-process-line-fldnum-pid
                                     psed-process-line-fldnum-tty
                                     psed-process-line-fldnum-stat
                                     psed-process-line-fldnum-time
                                     psed-process-line-fldnum-pagein
                                     psed-process-line-fldnum-tsiz
                                     psed-process-line-fldnum-dsiz
                                     psed-process-line-fldnum-rss
                                     psed-process-line-fldnum-lim
                                     psed-process-line-fldnum-mem
                                     psed-process-line-fldnum-command) 1)
  "Psed vm format.")

(defconst psed-format-memory (list "awxhm"
                                   (list psed-process-line-fldnum-pid
                                         psed-process-line-fldnum-tty
                                         psed-process-line-fldnum-majflt
                                         psed-process-line-fldnum-minflt
                                         psed-process-line-fldnum-trs
                                         psed-process-line-fldnum-drs
                                         psed-process-line-fldnum-size
                                         psed-process-line-fldnum-swap
                                         psed-process-line-fldnum-rss
                                         psed-process-line-fldnum-shrd
                                         psed-process-line-fldnum-lib
                                         psed-process-line-fldnum-dt
                                         psed-process-line-fldnum-command) 1)
  "Memory info ps format.")

(defsubst psed-format-args (format)
  "Return format arguments for ps program for format FORMAT."
  (car format))

(defsubst psed-format-fldnums (format)
  "Return fields numbers for format FORMAT."
  (cadr format))

(defsubst psed-format-curspos (format)
  "Return cursor position for format FORMAT."
  (nth 2 format))

(defun psed-format-select ()
  "Return format based on the value of psed-initial-format."
  (cond ((eq psed-initial-format 'long) psed-format-long)
        ((eq psed-initial-format 'user) psed-format-user)
        ((eq psed-initial-format 'jobs) psed-format-jobs)
        ((eq psed-initial-format 'signal) psed-format-signal)
        ((eq psed-initial-format 'vm) psed-format-vm)
        ((eq psed-initial-format 'memory) psed-format-memory)
        (t psed-format-user)))

(defun psed-format-fmt-string-headers ()
  "Build and return format string and headers list.
Get format value from global variable psed-current-format.
Return value is two element list, where first element -- format string,
and second element -- list of fields headers."
  (let* ((headers nil)
         (fmt " ")
         (fldnums (psed-format-fldnums psed-current-format)))
    (while fldnums
      (setq fmt (concat fmt "%-"
                        (number-to-string (psed-service-array-maxlen (car fldnums)))
                        "s "))
      (setq headers (cons (psed-service-array-title (car fldnums)) headers))
      (setq fldnums (cdr fldnums)))
    (setq fmt (concat fmt "\n"))
    (list fmt (reverse headers))))

;;
;;
;;
(defun psed ()
  "Short mode description.
Long description."
  (interactive)
  (let ((buffer (get-buffer-create psed-buffer-name)))
    (set-buffer buffer)
    (setq psed-current-format (psed-format-select))
    (psed-update-buffer)
    (psed-mode)
    (revert-buffer)
    (switch-to-buffer buffer)))

(defun psed-update-buffer ()
  "Run ps program and update current buffer with new info using format FORMAT."
  (let ((tempbuf (psed-run-ps (psed-format-args psed-current-format))))
    (psed-parse-ps-output tempbuf (psed-format-fldnums psed-current-format))
    (kill-buffer tempbuf)
    (setq buffer-read-only nil)
    (erase-buffer)
    (psed-process-list-print)
    (setq buffer-read-only t)
    (goto-char (point-min))
    (psed-move-to-command)))

(defun psed-run-ps (args)
  "Run ps program with args ARGS and return buffer with results."
  (let ((buffer (generate-new-buffer psed-tempbuf-name)))
    (call-process psed-ps-program-name nil buffer nil args)
    buffer))

(defun psed-parse-ps-output (buffer fldnums)
  "Convert raw ps output into internal representation."
  (save-excursion
    (set-buffer buffer)
    (goto-char (point-min))
    (psed-service-array-init)
    (psed-process-list-clear)
    ;; here we relay on the empty line at the end of the ps output
    (psed-parse-raw-process-info 1 (1- (count-lines (point-min) (point-max))) fldnums)))

(defun psed-parse-raw-process-info (line numlines fldnums)
  "Parses current buffer line by line recursively."
  (cond ((> line numlines) nil)
        (t (skip-chars-forward " \t")
           (psed-process-list-add
            (psed-parse-raw-line fldnums (make-vector psed-process-line-size
                                                      "N/A")))
           (forward-line)
           (psed-parse-raw-process-info (1+ line) numlines fldnums))))

(defun psed-parse-raw-line (fldnums psline)
  "Parse current line of the current raw ps output buffer into PSLINE,
starting from the field number FLDNUM. Return filled PSLINE."
  (cond ((eolp) psline)
        (t (let* ((wb (point))
                  (fldnum (car fldnums))
                  (len (funcall (psed-service-array-skip-func fldnum)))
                  (we (point)))
             ;; set field value
             (psed-process-line-set-field psline fldnum (buffer-substring wb we))
             ;; count new max length value for all fields except command
             ;; command is a last field in all formats and I do not like trailing ws
             (if (/= psed-process-line-fldnum-command fldnum)
                 (if (> len (psed-service-array-maxlen fldnum))
                     (psed-service-array-set-maxlen fldnum len)))
             ;; move point to the begining of the next field
             (skip-chars-forward " \t")
             ;; parse next field
             (psed-parse-raw-line (cdr fldnums) psline)))))

;;
;; Major mode conventions
;;

;; Psed mode is suitable only for specially formatted data.
(put 'psed-mode 'mode-class 'special)

(defvar psed-mode-map nil "Local keymap for psed-mode buffers.")
(if psed-mode-map
    nil
  (let ((map (make-keymap)))
    (suppress-keymap map)
    ;; commands
    (define-key map "g"      'revert-buffer)
    (define-key map "j"      'psed-update-jobs-format)
    (define-key map "l"      'psed-update-long-format)
    (define-key map "m"      'psed-update-memory-format) ; broken
    (define-key map "n"      'psed-next-line)
    (define-key map "p"      'psed-previous-line)
    (define-key map "q"      'psed-quit)
    (define-key map "s"      'psed-update-signal-format)
    (define-key map "u"      'psed-update-user-format)
    (define-key map "v"      'psed-update-vm-format)     ; broken
    ;; moving
    (define-key map " "      'psed-next-line)
    (define-key map "\C-n"   'psed-next-line)
    (define-key map "\C-p"   'psed-previous-line)
    (define-key map [down]   'psed-next-line)
    (define-key map [up]     'psed-previous-line)
    (define-key map [next]   'psed-next-page)
    (define-key map [prior]  'psed-previous-page)
    (define-key map [C-home] 'psed-first-line)
    (define-key map [C-end]  'psed-last-line)
    ;; formats
  (setq psed-mode-map map)))

(defvar psed-mode-hook nil
  "Run at the very end of psed-mode.")

(defun psed-mode ()
  "Psed mode description -- todo."
  (kill-all-local-variables)
  (use-local-map psed-mode-map)
  (setq mode-name "Psed")
  (setq major-mode 'psed-mode)
  (set (make-local-variable 'revert-buffer-function) (function psed-revert))
  (run-hooks 'psed-mode-hook))

;;
;; Commands
;;
(defun psed-revert (&optional arg noconfirm)
  "Reread the psed buffer.
Preserves old cursor, marks/flags."
  (widen)				; just in case user narrowed
  (let ((opoint (point))
        (oprocess (psed-get-process)))
    ;; save marks/flags -- todo
    (psed-update-buffer)
    ;; restore cursor position
    (or (and oprocess (psed-goto-process oprocess))
	(goto-char opoint))
    ;; restore marks/flags -- todo
    ))

(defun psed-quit ()
  "Put psed buffer at the end of the list of all buffers."
  (interactive)
  (if psed-quit-by-kill
      (kill-buffer nil)
    (bury-buffer)))

(defun psed-update-jobs-format ()
  "Update psed buffer using jobs format."
  (interactive)
  (setq psed-current-format psed-format-jobs)
  (psed-update-buffer))

(defun psed-update-long-format ()
  "Update psed buffer using long format."
  (interactive)
  (setq psed-current-format psed-format-long)
  (psed-update-buffer))

(defun psed-update-memory-format ()
  "Update psed buffer using memory format."
  (interactive)
  (setq psed-current-format psed-format-memory)
  (psed-update-buffer))

(defun psed-update-signal-format ()
  "Update psed buffer using signal format."
  (interactive)
  (setq psed-current-format psed-format-signal)
  (psed-update-buffer))

(defun psed-update-user-format ()
  "Update psed buffer using user format."
  (interactive)
  (setq psed-current-format psed-format-user)
  (psed-update-buffer))

(defun psed-update-vm-format ()
  "Update psed buffer using vm format."
  (interactive)
  (setq psed-current-format psed-format-vm)
  (psed-update-buffer))

;;
;;
;;
;; todo
(defun psed-get-process () nil)
;; todo
(defun psed-goto-process () nil)

;;
;;
;;
(defun psed-skip-non-blanks ()
  "Skip non blank chars and return number of skipped chars.
Leave point in the new position."
  (skip-chars-forward "^ \t"))

(defun psed-skip-to-eol ()
  "Skip up to the end of line and return nuber of skipped chars.
Leave point in the new position."
  (let ((begin (point))
        (end (progn (end-of-line) (point))))
    (- end begin)))

(defun psed-skip-start ()
  "Skip data in start field."
  )

(defun psed-next-line ()
  "Move down lines then position at psed-cursor-pos.
Optional prefix ARG says how many lines to move; default is one line."
  (interactive)
  (forward-line 1)
  (psed-move-to-command))

(defun psed-previous-line ()
  "Move up lines then position at psed-cursor-pos.
Optional prefix ARG says how many lines to move; default is one line."
  (interactive)
  (forward-line -1)
  (psed-move-to-command))

(defun psed-next-page ()
  "Move one page down then position at psed-cursor-pos."
  (interactive)
  (scroll-up nil)
  (psed-move-to-command))

(defun psed-previous-page ()
  "Move one page up then position at psed-cursor-pos."
  (interactive)
  (scroll-down nil)
  (psed-move-to-command))

(defun psed-first-line ()
  "Move to the first line then position at psed-cursor-pos."
  (interactive)
  (goto-char (point-min))
  (psed-move-to-command))

(defun psed-last-line ()
  "Move to the last line then position at psed-cursor-pos."
  (interactive)
  (goto-char (point-max))
  (forward-line -1)
  (psed-move-to-command))

(defun psed-move-to-command ()
  "Move to first char of the command on the current line."
  (beginning-of-line)
  (goto-char (+ (point) (psed-format-curspos psed-current-format))))

;;; psed.el ends here
