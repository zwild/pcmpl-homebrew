;;; pcmpl-homebrew.el --- pcomplete for homebrew

;; Copyright (C) 2014, 2015, 2016 hiddenlotus
;; Author: hiddenlotus <kaihaosw@gmail.com>
;; Git: https://github.com/hiddenlotus/pcmpl-homebrew.git
;; Version: 0.95
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

(defun pcmpl-homebrew-get-commands (executable-command shell-command search regex)
  (when (executable-find executable-command)
    (with-temp-buffer
      (call-process-shell-command shell-command nil (current-buffer) nil)
      (goto-char 0)
      (search-forward search)
      (let (commands)
        (while (re-search-forward regex nil t)
          (add-to-list 'commands (match-string 1) t))
        commands))))

(defun pcmpl-homebrew-get-formulas (&rest args)
  (with-temp-buffer
    (apply 'process-file "brew" nil (list t nil) nil args)
    (split-string (buffer-string) nil t)))

(defconst pcmpl-homebrew-commands
  (remove "External"
          (pcmpl-homebrew-get-commands "brew" "brew commands"
                                       "Built-in commands" "^\\([[:word:]-.]+\\)"))
  "List of homebrew commands.")

(defconst pcmpl-homebrew-local-formulas-commands
  '("cleanup" "ln" "link" "list" "pin" "rm" "remove" "unlink" "unpin" "uninstall"
    "upgrade" "test" "--prefix")
  "List of commands that use local formulas.")

(defconst pcmpl-homebrew-global-formulas-commands
  '("audit" "cat" "deps" "edit" "fetch" "home" "info" "install" "log"
    "missing" "reinstall" "search" "uses")
  "List of commands that use global formulas.")

(defconst pcmpl-homebrew-installed-formulas
  (pcmpl-homebrew-get-formulas "list")
  "List of the installed formulas.")

(defconst pcmpl-homebrew-all-formulas
  (pcmpl-homebrew-get-formulas "search")
  "List of all the formulas.")

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

;; external commands
;; brew services
(defconst pcmpl-homebrew-services-commands
  '("cleanup" "list" "restart" "start" "stop")
  "List of homebrew services commands.")

(defun pcmpl-homebrew-cask-installed? ()
  (null
   (string-match "Not installed"
                 (shell-command-to-string "brew tap-info caskroom/cask"))))

(when (pcmpl-homebrew-cask-installed?)
  (defconst pcmpl-homebrew-cask-commands
    '("audit" "cat" "cleanup" "create" "doctor" "edit" "fetch" "home" "info" "install"
      "list" "search" "style" "uninstall" "update" "zap")
    "List of homebrew cask commands.")

  (defconst pcmpl-homebrew-cask-all-casks
    (pcmpl-homebrew-get-formulas "cask" "search")
    "List of all casks.")

  (defconst pcmpl-homebrew-cask-local-casks
    (pcmpl-homebrew-get-formulas "cask" "list")
    "List of local casks."))


;;;###autoload
(defun pcomplete/brew ()
  (let ((command (nth 1 pcomplete-args)))
    (pcomplete-here* pcmpl-homebrew-commands)
    (while
        (cond
         ((pcomplete-match "^-" 0)
          (pcomplete-here (pcmpl-homebrew-get-command-options command)))
         ((member command pcmpl-homebrew-local-formulas-commands)
          (pcomplete-here pcmpl-homebrew-installed-formulas))
         ((member command pcmpl-homebrew-global-formulas-commands)
          (pcomplete-here pcmpl-homebrew-all-formulas))
         ((string= command "services")
          (pcomplete-here pcmpl-homebrew-services-commands))
         ((string= command "cask")
          (when (pcmpl-homebrew-cask-installed?)
            (let ((subcommand (nth 2 pcomplete-args)))
              (pcomplete-here pcmpl-homebrew-cask-commands)
              (cond ((member subcommand '("fetch" "home" "install" "info"))
                     (pcomplete-here pcmpl-homebrew-cask-all-casks))
                    ((string= subcommand "uninstall")
                     (pcomplete-here pcmpl-homebrew-cask-local-casks))))))))))

(provide 'pcmpl-homebrew)

;;; pcmpl-homebrew.el ends here
