;;; bittorrent.el --- BitTorrent Client

;; Copyright (C) 2007  Alexander Zhuckov

;; Author: Alexander Zhuckov <zuav@mail.spbnit.ru>
;; Keywords: comm

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; 

;;; Code:
(defvar bittorrent-buffer-name "*bittorrent*")        ; todo: description

(defun bittorrent ()
  "Short mode description.
Long description."
  (interactive)
  (let ((buffer (get-buffer-create bittorrent-buffer-name)))
    (set-buffer buffer)
    (switch-to-buffer buffer)))


(provide 'bittorrent)
;;; bittorrent.el ends here

(/ 35.0 27.0)
(* 1.3 9900)
