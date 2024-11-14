;;; git-commit-jira.el --- Automatically insert JIRA tickets in git commits -*- lexical-binding: t -*-

;; Copyright (C) 2023  Vitor Leal

;; Author: Vitor Leal <hellofromvitor@gmail.com>
;; URL: https://github.com/nvimtor/git-commit-jira.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; The git-commit-jira package automatically inserts JIRA ticket identifiers
;; into git commit messages based on the current branch name.
;;

;;; Code:

(eval-when-compile
  (require 'vc-git))

(defun git-commit-jira-split-words (string)
  "Split STRING into a list of words, using whitespace as the delimiter."
  (split-string string "\\s-+"))

(defgroup git-commit-jira nil
  "Customization group for the git-commit-jira package."
  :group 'tools
  :prefix "git-commit-jira-")

(defcustom git-commit-jira-ticket-regex-brackets "\\[[A-Za-z]+-[0-9]+\\]"
  "Regex pattern used to match JIRA ticket identifiers that are wrapped in brackets, e.g., [ABC-123]."
  :type 'regexp
  :group 'git-commit-jira)

(defcustom git-commit-jira-ticket-regex-nobrackets "\\([A-Za-z]+-[0-9]+\\)"
  "Regex pattern used to match JIRA ticket identifiers that are not wrapped in brackets, e.g., ABC-123."
  :type 'regexp
  :group 'git-commit-jira)

(defun git-commit-jira-vc-current-branch ()
  "Get the current git branch using VC."
  (car (vc-git-branches)))

(defcustom git-commit-jira-get-current-branch-function #'git-commit-jira-vc-current-branch
  "Function to retrieve the current branch name."
  :type 'function
  :group 'git-commit-jira)

(defun git-commit-jira-insert-ticket ()
"Insert a JIRA ticket identifier from the branch name into the commit message if not already present."
    (let ((branch-name (funcall git-commit-jira-get-current-branch-function))
          (commit-message (buffer-string)))
      (unless (string-match git-commit-jira-ticket-regex-brackets commit-message)
        (when (string-match git-commit-jira-ticket-regex-nobrackets branch-name)
          (let ((words (git-commit-jira-split-words branch-name)))
            (insert (format "[%s-%s] " (car words) (car (cdr words)))))))))

(define-minor-mode git-commit-jira-mode
  "Minor mode to automatically insert JIRA ticket in git commit messages."
  :global t
  :group 'git-commit-jira
  (if git-commit-jira-mode
      (add-hook 'git-commit-setup-hook #'git-commit-jira-insert-ticket)
    (remove-hook 'git-commit-setup-hook #'git-commit-jira-insert-ticket)))

(provide 'git-commit-jira)

;;; git-commit-jira.el ends here
