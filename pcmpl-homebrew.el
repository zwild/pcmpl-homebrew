;;; pcmpl-homebrew.el --- pcomplete for homebrew

;; Copyright (C) 2014, 2015, 2016 hiddenlotus
;; Author: hiddenlotus <kaihaosw@gmail.com>
;; Git: https://github.com/hiddenlotus/pcmpl-homebrew.git
;; Version: 0.92
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

(defcustom homebrew-command (executable-find "brew")
  "Homebrew command.")

(defun pcmpl-homebrew-commands ()
  (interactive)
  (with-temp-buffer
    (call-process-shell-command homebrew-command nil (current-buffer) nil "commands")
    (goto-char 0)
    (search-forward "Built-in commands")
    (let (commands)
      (while (re-search-forward "^\\([[:word:]-.]+\\)" nil t)
        (push (match-string 1) commands))
      (remove "External" commands))))

(defconst pcmpl-homebrew-commands
  (sort (append '("--version" "ln" "remove" "rm") (pcmpl-homebrew-commands)) 'string<)
  "List of homebrew commands.")

;;
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

;;
(defconst pcmpl-homebrew-options-hash-table
  (let (options-hash)
    (setq options-hash (make-hash-table :test 'equal))
    (puthash "cleanup" '("--force" "-n" "-s" "-ns") options-hash)
    (puthash "deps" '("--1" "-n" "--union" "--tree" "--all" "--installed") options-hash)
    (puthash "fetch" '("--force" "-v" "--devel" "--HEAD" "--deps"
                       "--build-from-source" "--force-bottle") options-hash)
    (puthash "info" '("--github" "--json=" "--all" "--installed") options-hash)
    (puthash "install" '("--debug" "--env=" "--ignore-dependencies" "--only-dependencies"
                         "--cc=" "--build-from-source" "--devel" "--HEAD"
                         "--interactive" "--git") options-hash)
    (puthash "link" '("--overwrite" "--dry-run" "--force") options-hash)
    (puthash "linkapps" '("--local") options-hash)
    (puthash "ln" '("--overwrite" "--dry-run" "--force") options-hash)
    (puthash "list" '("--unbrewed" "--versions" "--multiple" "--pinned") options-hash)
    (puthash "options" '("--compact" "--all" "--installed") options-hash)
    (puthash "outdated" '("--quiet") options-hash)
    (puthash "uninstall" '("--force") options-hash)
    (puthash "rm" '("--force") options-hash)
    (puthash "search" '("--debian" "--fedora" "--fink" "--macports"
                        "--opensuse" "--ubuntu") options-hash)
    (puthash "remove" '("--force") options-hash)
    (puthash "sh" '("--env=std") options-hash)
    (puthash "tap" '("--repair") options-hash)
    (puthash "test" '("--devel" "--HEAD") options-hash)
    (puthash "unlinkapps" '("--local") options-hash)
    (puthash "unpack" '("--git" "--patch" "--destdir=") options-hash)
    (puthash "update" '("--rebase") options-hash)
    (puthash "upgrade" '("--debug" "--env=" "--ignore-dependencies" "--only-dependencies"
                         "--cc=" "--build-from-source" "--devel" "--HEAD"
                         "--interactive" "--git" "--all") options-hash)

    (puthash "uses" '("--installed" "--recursive" "--devel" "--HEAD") options-hash)
    options-hash))

(defun pcmpl-homebrew-get-command-options (command)
  (gethash command pcmpl-homebrew-options-hash-table))


;;;###autoload
(defun pcomplete/brew ()
  (let ((command (nth 1 pcomplete-args)))
    (pcomplete-here* pcmpl-homebrew-commands)
    (when (member command pcmpl-homebrew-commands)
      (while
          (cond
           ((pcomplete-match "^-" 0)
            (pcomplete-here (pcmpl-homebrew-get-command-options command)))
           ((member command pcmpl-homebrew-local-formulas-commands)
            (pcomplete-here (pcmpl-homebrew-installed-formulas)))
           ((member command pcmpl-homebrew-global-formulas-commands)
            (pcomplete-here (pcmpl-homebrew-all-formulas)))
           ((string= command "services")
            (pcomplete-here '("cleanup" "list" "restart" "start" "stop"))))))))

(provide 'pcmpl-homebrew)

;;; pcmpl-homebrew.el ends here
