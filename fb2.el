;;; fb2.el --- FB2 mode

;; Copyright (C) 2012  Alexander Zhukov

;; Author: Alexander Zhukov <zuav@yandex.ru>
;; Keywords: languages, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 



;;; Code:
(defun fb2-region-to-one-paragraph (point mark)
  "Produce fb2 paragraph from lines in the region.
Like this:

  <p>Par1     Par1-1</p>
  <p>Par2</p>
  <p>Par3</p>

will be converted to 

  <p>Par1 Par1-1 Par2 Par3</p>

.All consecutive space chars are replaced with one blank char."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region point mark)
      (beginning-of-buffer)
      (while (search-forward "</p>\n<p>" nil t)
        (replace-match " " nil t))
      (beginning-of-buffer)
      (while (search-forward-regexp "[[:space:]]\\{2,\\}" nil t)
        (replace-match " " nil t)))))


(defun fb2-remove-emphasis-in-region (point mark)
  "Remove all <emphasis> and </emphasis> in the region."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region point mark)
      (beginning-of-buffer)
      (while (search-forward "<emphasis>" nil t)
        (replace-match "" nil t))
      (beginning-of-buffer)
      (while (search-forward "</emphasis>" nil t)
        (replace-match "" nil t)))))

(provide 'fb2)
;;; fb2.el ends here
