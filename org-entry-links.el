;;; org-entry-links.el --- Pick a link from an entry in Org mode -*- lexical-binding: t -*-

;; Copyright (C) 2019 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (ivy "0.12"))
;; Keywords: outlines convenience
;; URL: https://github.com/akirak/org-entry-links

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This lets you pick a link from an entry in `org-mode'.
;; It supports both `org-mode' and `org-agenda-mode'.

;; At present, `org-entry-links-ivy' is the entry point to this
;; package.

;;; Code:

(require 'dash)

(defun org-entry-links--next-link (&optional bound)
  "Go to the next link before BOUND in `org-mode'."
  (let ((pos (text-property-not-all (point) (or bound (point-max))
                                    'htmlize-link nil)))
    (when pos
      (goto-char pos)
      pos)))

(defun org-entry-links--get-links (&optional no-subtrees)
  "Get a list of links in the current entry.

Unless NO-SUBTREES is non-nil, this includes results from subtrees."
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in org-mode"))
  (when (org-before-first-heading-p)
    (user-error "Before the first heading"))
  (save-restriction
    (save-excursion
      (org-back-to-heading t)
      (org-show-entry)
      (let (result
            (subtree-end (save-excursion
                           (if no-subtrees
                               (progn
                                 (beginning-of-line 2)
                                 (re-search-forward (rx bol (+ "*") (+ space))
                                                    nil t))
                             (org-end-of-subtree)))))
        (while (org-entry-links--next-link subtree-end)
          (let* ((link (org-element-link-parser))
                 (cbegin (org-element-property :contents-begin link))
                 (cend (org-element-property :contents-end link))
                 (label (when cbegin (buffer-substring-no-properties cbegin cend))))
            (push (list :label label
                        :element link
                        :marker (point-marker)
                        :raw-string (buffer-substring-no-properties
                                     (org-element-property :begin link)
                                     (org-element-property :end link)))
                  result)
            (goto-char (1+ (org-element-property :end link)))))
        (nreverse result)))))

(defun org-entry-links--ivy-candidates (&optional arg)
  "Return a list of links as needed by `org-entry-links-ivy'.

ARG is passed to `org-entry-links--get-links'."
  (->> (org-entry-links--get-links arg)
       (-map (lambda (plist)
               (propertize
                (or (plist-get plist :label)
                    (org-element-property
                     :raw-link
                     (plist-get plist :element)))
                'org-link-element (plist-get plist :element)
                'org-raw-link (plist-get plist :raw-string))))))

;;;###autoload (autoload 'org-entry-links-ivy "ivy")
(with-eval-after-load 'ivy
  (defun org-entry-links-ivy (&optional arg)
    "Select a link in the current entry via Ivy.

When ARG is non-nil, exclude the results from subtrees."
    (interactive "P")
    (let ((candidates (cond
                       ((derived-mode-p 'org-mode)
                        (org-entry-links--ivy-candidates arg))
                       ((derived-mode-p 'org-agenda-mode)
                        (let ((marker (or (org-get-at-bol 'org-hd-marker)
                                          (org-agenda-error))))
                          (with-current-buffer (marker-buffer marker)
                            (org-with-wide-buffer
                             (goto-char (marker-position marker))
                             (org-entry-links--ivy-candidates arg))))))))
      (cl-case (length candidates)
        (0 (message "No links in the entry"))
        (1 (let ((link (get-char-property 0 'org-raw-link (car candidates))))
             (message "Opening the only link %s" link)
             (org-link-open-from-string link)))
        (otherwise (ivy-read "Link: " candidates
                             :action
                             (lambda (cand)
                               (org-link-open-from-string
                                (get-char-property 0 'org-raw-link cand))))))))

  (defun org-entry-links-ivy-display-transformer (cand)
    "Decorate CAND for display in the Ivy interface."
    (let* ((element (get-char-property 0 'org-link-element cand))
           (raw-link (org-element-property :raw-link element)))
      (format "%s  %s"
              cand
              (propertize raw-link 'face 'font-lock-comment-face))))

  (ivy-set-display-transformer 'org-entry-links-ivy
                               'org-entry-links-ivy-display-transformer))

(provide 'org-entry-links)
;;; org-entry-links.el ends here
