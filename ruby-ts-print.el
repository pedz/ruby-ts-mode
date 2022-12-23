;;; ruby-ts-navigation.el --- navigation tree-sitter support for Ruby  -*- lexical-binding: t; -*-

;; This is currently a work in progress.  My intent is to release it
;; with whatever copyright notice Free Software Foundation,
;; Inc. wants.

;; Author     : Perry Smith <pedz@easesoftware.com>
;; Created    : December 2022
;; Keywords   : ruby languages tree-sitter

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

;; Routines that "pretty print" nodes to help me debug things.

;;; Code:

(defun rtsn-node-to-string (node &optional point)
  "Return a pretty string describing NODE.
Optional POINT defaults to `point'."
  (setq point (or point (point)))
  (let* ((type (treesit-node-type node))
         (start (treesit-node-start node))
         (end (treesit-node-end node)))
    (format (concat "(%s "
                    (cond
                     ((= point start) ".%d-%d)")
                     ((= point end) "%d-%d.)")
                     ((and (< start point) (> end point)) "%d-.-%d)")
                     ("%d-%d)")))
            type start end)))

(defun rtsn-node-list-to-string (list &optional point)
  "Return a pretty string describing a LIST of nodes.
Optional POINT defaults to `point'."
  (setq point (or point (point)))
  (message "point %S" point)
  (let* ((past-point nil))
    (mapconcat (lambda (node)
                 (let* ((sep (if (and (not past-point)
                                     (< point (treesit-node-start node)))
                                " . "
                              " "))
                        (result (concat sep (rtsn-node-to-string node point))))
                   (setq past-point (< point (treesit-node-end node)))
                   result))
               list)))

(defun rtsn-print (cnt)
  "Move CNT times up the tree and then print node and its list of children.
CNT is prefix arg and defaults to 0."
  (interactive "P")
  (setq cnt (or cnt 0))
  (let* ((node (treesit-node-at (point)))
         (type (treesit-node-type node)))
    (while (and node (>= (setq cnt (1- cnt)) 0))
      (setq node (treesit-node-parent node)
            type (treesit-node-type node)))
    (if (not node)
        (message "Ran out of parents")
      (message "(%s (%s))" type
               (rtsn-node-list-to-string (treesit-node-children node))))))

(provide 'ruby-ts-print)

;;; ruby-ts-print.el ends here
