;;; git-messenger.el --- Port of gitmessenger.vim

;; Copyright (C) 2013 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-git-messenger
;; Version: 0.02
;; Package-Requires: ((popup "0.5.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a function called git-messenger:popup-message
;; that when called will pop-up the last git commit message for the
;; current line. This uses the git-blame tool internally.
;;
;; Example usage:
;;   (require 'git-messenger)
;;   (global-set-key (kbd "C-x v p") 'git-messenger:popup-message)
;;

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'popup)

(defgroup git-messenger nil
  "git messenger"
  :group 'git-messenger)

(defun git-messenger:blame-command (file line)
  (format "git --no-pager blame -L %d,+1 --porcelain %s"
          line (shell-quote-argument file)))

(defun git-messenger:cat-file-command (commit-id)
  (format "git --no-pager cat-file commit %s" commit-id))

(defun git-messenger:commit-id-at-line (file line)
  (with-temp-buffer
    (let ((cmd (git-messenger:blame-command file line)))
      (unless (zerop (call-process-shell-command cmd nil t))
        (error "Failed: %s" cmd))
      (goto-char (point-min))
      (let ((line (buffer-substring-no-properties
                   (line-beginning-position) (line-end-position))))
        (car (split-string line))))))

(defun git-messenger:commit-message (commit-id)
  (with-temp-buffer
    (if (string-match "\\`0+\\'" commit-id)
        (format "* not yet committed *")
      (let ((cmd (git-messenger:cat-file-command commit-id)))
        (unless (zerop (call-process-shell-command cmd nil t))
          (error "Failed: %s" cmd))
        (goto-char (point-min))
        (forward-paragraph)
        (buffer-substring-no-properties (point) (point-max))))))

;;;###autoload
(defun git-messenger:popup-message ()
  "Pop up a message showing the last commit message for the current line."
  (interactive)
  (let* ((file (buffer-file-name))
         (line (line-number-at-pos))
         (commit-id (git-messenger:commit-id-at-line file line)))
    (if (string-match "^0+$" commit-id)
        (message "No history for current line")
      (popup-tip (git-messenger:commit-message commit-id)))))

(provide 'git-messenger)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; git-messenger.el ends here
