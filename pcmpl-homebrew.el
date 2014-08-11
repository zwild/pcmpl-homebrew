;;; pcmpl-homebrew.el --- pcomplete for homebrew

;; Copyright (C) 2014 Wei Zhao
;; Author: Wei Zhao <kaihaosw@gmail.com>
;; Git: https://github.com/kaihaosw/pcmpl-homebrew.git
;; Version: 0.1
;; Created: 2014-08-11
;; Keywords: pcomplete, homebrew, tools

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Pcomplete for homebrew.

;;; Code:
(require 'pcomplete)

(defconst pcmpl-homebrew-commands
  '("info" "home" "options" "install" "uninstall" "search" "list"
    "update" "upgrade" "pin/unpin" "doctor" "--env" "config" "create"
    "edit")
  "List of `homebrew' commands.")

(defconst pcmpl-homebrew-FORMULAs
  '("info" "home" "options" "uninstall" "list" "upgrade" "pin/unpin")
  "List of commands that need a formula.")

(defun pcmpl-homebrew-installed-FORMULAs ()
  "List of the installed formulas."
  (split-string (shell-command-to-string "brew list")))

(defun pcmpl-homebrew-searched-FORMULAs ()
  "List of all the formulas."
  (split-string (shell-command-to-string "brew search")))

;;;###autoload
(defun pcomplete/brew ()
  (pcomplete-here* pcmpl-homebrew-commands)
  (cond
   ((pcomplete-match (regexp-opt pcmpl-homebrew-FORMULAs 1))
    (pcomplete-here (pcmpl-homebrew-installed-FORMULAs)))
   ((pcomplete-match (regexp-opt '("install" "search")) 1)
    (pcomplete-here (pcmpl-homebrew-searched-FORMULAs)))))

(provide 'pcmpl-homebrew)

;;; pcmpl-homebrew.el ends here

