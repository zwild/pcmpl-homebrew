;;; pcmpl-homebrew.el --- pcomplete for homebrew

;; Copyright (C) 2014 Wei Zhao
;; Author: Wei Zhao <kaihaosw@gmail.com>
;; Git: https://github.com/kaihaosw/pcmpl-homebrew.git
;; Version: 0.6
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

(defun pcmpl-homebrew-commands ()
  "Homebrew commands from `brew commands'"
  (with-temp-buffer
    (call-process-shell-command "brew" nil (current-buffer) nil "commands")
    (goto-char 0)
    (search-forward "Built-in commands")
    (let (commands stop)
      (while (and (re-search-forward
                   "^\\([[:word:]-.]+\\)"
                   nil t) (not stop))
        (if (string= (match-string 1) "External")
            (setq stop 1)
          (push (match-string 1) commands)))
      commands)))

(defconst pcmpl-homebrew-commands
  (sort (append '("--version" "remove" "rm") (pcmpl-homebrew-commands)) 'string<)
  "List of homebrew commands.")

(defconst pcmpl-homebrew-local-formulas-commands
  '("cleanup" "ln" "link" "list" "pin" "rm" "remove" "unlink" "unpin" "uninstall"
    "upgrade" "test" "--prefix")
  "List of commands that use local formulas.")

(defconst pcmpl-homebrew-global-formulas-commands
  '("audit" "cat" "deps" "edit" "fetch" "home" "info" "install" "log"
    "missing" "reinstall" "search" "uses")
  "List of commands that use global formulas.")

(defun pcmpl-homebrew-installed-formulas ()
  "List of the installed formulas."
  (split-string (shell-command-to-string "brew list")))

(defun pcmpl-homebrew-all-formulas ()
  "List of all the formulas."
  (split-string (shell-command-to-string "brew search")))

(defconst pcmpl-homebrew-all-formulas-count
  (length (pcmpl-homebrew-all-formulas))
  "Count of homebrew formulas.")

;; homebrew command options
(defconst pcmpl-homebrew-cleanup-options
  '("--force" "-n" "-s" "-ns"))

(defconst pcmpl-homebrew-deps-options
  '("--1" "-n" "--union" "--tree" "--all" "--installed"))

(defconst pcmpl-homebrew-fetch-options
  '("--force" "-v" "--devel" "--HEAD" "--deps"
   "--build-from-source" "--force-bottle"))

(defconst pcmpl-homebrew-info-options
  '("--github" "--json=" "--all" "--installed"))

(defconst pcmpl-homebrew-install-options
  '("--debug" "--env=" "--ignore-dependencies" "--only-dependencies"
   "--cc=" "--build-from-source" "--devel" "--HEAD"
   "--interactive" "--git"))

(defconst pcmpl-homebrew-link-options
  '("--overwrite" "--dry-run" "--force"))

(defconst pcmpl-homebrew-linkapps-options
  '("--local"))

(defconst pcmpl-homebrew-list-options
  '("--unbrewed" "--versions" "--multiple" "--pinned"))

(defconst pcmpl-homebrew-options-options
  '("--compact" "--all" "--installed"))

(defconst pcmpl-homebrew-outdated-options
  '("--quiet"))

(defconst pcmpl-homebrew-uninstall-options
  '("--force"))

(defconst pcmpl-homebrew-search-options
  '("--debian" "--fedora" "--fink" "--macports" "--opensuse" "--ubuntu"))

(defconst pcmpl-homebrew-sh-options
  '("--env=std"))

(defconst pcmpl-homebrew-tap-options
  '("--repair"))

(defconst pcmpl-homebrew-test-options
  '("--devel" "--HEAD"))

(defconst pcmpl-homebrew-unlinkapps-options
  '("--local"))

(defconst pcmpl-homebrew-unpack-options
  '("--git" "--patch" "--destdir="))

(defconst pcmpl-homebrew-update-options
  '("--rebase"))

(defconst pcmpl-homebrew-upgrade-options
  pcmpl-homebrew-install-options)

(defconst pcmpl-homebrew-uses-options
  '("--installed" "--recursive" "--devel" "--HEAD"))

;; TODO
(defvar pcmpl-homebrew-show-p nil)

;;;###autoload
(defun pcomplete/brew ()
  (pcomplete-here* pcmpl-homebrew-commands)
  (cond
   ((pcomplete-match (regexp-opt '("linkapps" "unlinkapps") 1))
    nil)
   ((pcomplete-match (regexp-opt pcmpl-homebrew-local-formulas-commands) 1)
    (while (pcomplete-here (pcmpl-homebrew-installed-formulas))))
   ((pcomplete-match (regexp-opt pcmpl-homebrew-global-formulas-commands) 1)
    (progn
      (when (string= (car (cddr pcomplete-args)) "")
        (setq pcmpl-homebrew-show-p
              (yes-or-no-p
               (format "Do you wish to see all %d possibilities?"
                       pcmpl-homebrew-all-formulas-count))))
      (when pcmpl-homebrew-show-p
        (while (pcomplete-here (pcmpl-homebrew-all-formulas))))))))

(provide 'pcmpl-homebrew)

;;; pcmpl-homebrew.el ends here

