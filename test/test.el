;;; test.el --- test of git-messenger

;; Copyright (C) 2016 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>

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

;;; Code:

(require 'ert)
(require 'git-messenger)

(ert-deftest find-vcs ()
  ""
  (let* ((tmp-dir (file-name-as-directory
                   (concat default-directory (make-temp-name "git-messenger"))))
         (git-dir (concat tmp-dir ".git"))
         (hg-dir (concat tmp-dir "foo/" ".hg"))
         (test-dir (concat tmp-dir "foo/bar/")))
    (unwind-protect
        (progn
          (make-directory git-dir t)
          (make-directory hg-dir t)
          (make-directory test-dir t)
          (let ((default-directory test-dir))
            (should (eq (git-messenger:find-vcs) 'hg))))
      (delete-directory tmp-dir t))))

;;; test.el ends here
