;;; pcmpl-homebrew.el --- pcomplete for homebrew

;; Copyright (C) 2014-2019 Wei Zhao
;; Author: zwild <judezhao@outlook.com>
;; Git: https://github.com/zwild/pcmpl-homebrew.git
;; Version: 0.97.4
;; Created: 2014-08-11
;; Keywords: pcomplete, homebrew, tools, cask, services

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
;; Completion for homebrew when using shell or eshell in emacs.

;;; Code:
(require 'pcomplete)
(require 'seq)

(defun pcmpl-homebrew-get-commands (executable-command shell-command regex)
  (when (executable-find executable-command)
    (with-temp-buffer
      (call-process-shell-command shell-command nil (current-buffer) nil)
      (goto-char 0)
      (let (commands)
        (while (re-search-forward regex nil t)
          (add-to-list 'commands (match-string 1) t))
        commands))))

(defun pcmpl-homebrew-get-formulas (&rest args)
  (with-temp-buffer
    (apply 'process-file "brew" nil (list t nil) nil args)
    (split-string (buffer-string) nil t)))

(defconst pcmpl-homebrew-commands
  (pcmpl-homebrew-get-commands "brew" "brew commands" "^\\([[:word:]-.]+\\)")
  "List of homebrew commands.")

(defconst pcmpl-homebrew-local-formulas-commands
  '("cleanup" "link" "list" "pin" "unlink" "unpin" "uninstall" "test" "--prefix")
  "List of commands that use local formulas.")

(defconst pcmpl-homebrew-global-formulas-commands
  '("audit" "cat" "deps" "edit" "fetch" "home" "info" "install" "log" "missing" "search" "uses")
  "List of commands that use global formulas.")

(defmacro pcmpl-homebrew-set-formulas (var command)
  `(progn
     (when (null ,var)
       (setq ,var (funcall ,command)))
     ,var))

(defvar pcmpl-homebrew-installed-formulas '()
  "List of the installed formulas.")

(defun pcmpl-homebrew-installed-formulas ()
  (pcmpl-homebrew-set-formulas
   pcmpl-homebrew-installed-formulas
   (lambda ()
     (pcmpl-homebrew-get-formulas "list"))))

(defvar pcmpl-homebrew-all-formulas '()
  "List of all the formulas.")

(defun pcmpl-homebrew-all-formulas ()
  (pcmpl-homebrew-set-formulas
   pcmpl-homebrew-all-formulas
   (lambda ()
     (pcmpl-homebrew-get-formulas "search"))))

(defconst pcmpl-homebrew-options-hash-table
  (let (options-hash)
    (setq options-hash (make-hash-table :test 'equal))
    (puthash "--cache" '("--formula" "--cask") options-hash)
    (puthash "cleanup" '("--dry-run" "-n" "-s" "-ns") options-hash)
    (puthash "deps" '("--1" "-n" "--union" "--tree" "--all" "--installed") options-hash)
    (puthash "desc" '("-s" "--search" "-n" "--name" "-d" "--description") options-hash)
    (puthash "doctor" '("--list-checks" "--verbose") options-hash)
    (puthash "diy" '("--name" "--version") options-hash)
    (puthash "fetch" '("--force" "-v" "--devel" "--HEAD" "--deps"
                       "--build-from-source" "--force-bottle") options-hash)
    (puthash "info" '("--analytics" "--days" "--category"
                      "--github" "--json" "--all" "--installed") options-hash)
    (puthash "install" '("--debug" "--env" "--ignore-dependencies" "--only-dependencies"
                         "--cc" "--build-from-source" "--force-bottle" "--include-test"
                         "--HEAD" "--keep-tmp" "--build-bottle" "--bottle-arch" "--force" "-f"
                         "--verbose" "-v" "--display-times" "--interactive" "--git") options-hash)
    (puthash "link" '("--overwrite" "--dry-run" "--force") options-hash)
    (puthash "linkapps" '("--local") options-hash)
    (puthash "list" '("--full-name" "--unbrewed" "--versions" "--multiple" "--pinned"
                      "--formula" "--formulae" "--cask" "--casks" "-1" "-l" "-r" "-t") options-hash)
    (puthash "options" '("--compact" "--all" "--installed" "--command") options-hash)
    (puthash "outdated" '("--quiet" "--verbose" "--formula" "--cask" "--json" "--greedy") options-hash)
    (puthash "uninstall" '("--force" "--ignore-dependencies" "-f") options-hash)
    (puthash "reinstall" '("--display-times" "--build-from-source"
                           "--interactive" "--keep-tmp" "--force" "--verbose") options-hash)
    (puthash "search" '("--formula" "--formulae" "--cask" "--casks" "--desc" "--debian"
                        "--fedora" "--fink" "--macports" "--opensuse" "--ubuntu") options-hash)
    (puthash "sh" '("--env=std" "--cmd") options-hash)
    (puthash "tap" '("--repair" "--full" "--shallow" "--force-auto-update" "--list-pinned") options-hash)
    (puthash "test" '("--devel" "--HEAD" "--keep-tmp" "--retry") options-hash)
    (puthash "unlink" '("--dry-run") options-hash)
    (puthash "unpack" '("--git" "--patch" "--destdir=" "--force") options-hash)
    (puthash "update" '("--merge" "--preinstall" "--force") options-hash)
    (puthash "upgrade" '("--debug" "--formula" "--cask" "--build-from-source" "--interactive"
                         "--force-bottle" "--fetch-HEAD" "--ignore-pinned" "--keep-tmp" "--verbose"
                         "--display-times" "--dry-run" "--greedy") options-hash)
    (puthash "uses" '("--installed" "--recursive" "--include-build"
                      "--include-test" "--include-optional" "--skip-recommended") options-hash)
    options-hash))

(defun pcmpl-homebrew-get-command-options (command)
  (gethash command pcmpl-homebrew-options-hash-table))

(defun pcmpl-contains-options (option)
  (seq-find (lambda (s) (string= s option)) pcomplete-args))

;; external commands
;; homebrew/services
(defun pcmpl-external-commands-installed? (tap-name)
  (null (string-match
         "Not installed"
         (shell-command-to-string (format "brew tap-info %s" tap-name)))))

(defconst pcmpl-homebrew-services-commands
  '("cleanup" "list" "restart" "start" "stop" "run")
  "List of homebrew services commands.")

;; caskroom/cask
(defun pcmpl-homebrew-cask-installed? ()
  (pcmpl-external-commands-installed? "homebrew/cask"))

(defvar pcmpl-homebrew-cask-installed? nil)

(when (pcmpl-homebrew-cask-installed?)
  (setq pcmpl-homebrew-cask-installed? t)

  (defconst pcmpl-homebrew-cask-commands
    '("audit" "cat" "create" "edit" "fetch" "help" "info" "install" "style" "uninstall" "zap")
    "List of homebrew cask commands.")

  (defvar pcmpl-homebrew-cask-all-casks '()
    "List of all casks.")

  (defun pcmpl-homebrew-cask-all-casks ()
    (pcmpl-homebrew-set-formulas
     pcmpl-homebrew-cask-all-casks
     (lambda ()
       (pcmpl-homebrew-get-formulas "search" "--casks"))))

  (defvar pcmpl-homebrew-cask-local-casks '()
    "List of local casks.")

  (defun pcmpl-homebrew-cask-local-casks ()
    (pcmpl-homebrew-set-formulas
     pcmpl-homebrew-cask-local-casks
     (lambda ()
       (pcmpl-homebrew-get-formulas "list" "--cask")))))


;;;###autoload
(defun pcomplete/brew ()
  (let ((command (nth 1 pcomplete-args)))
    (pcomplete-here* pcmpl-homebrew-commands)
    (while
        (cond
         ((pcomplete-match "^-" 0)
          (pcomplete-here (pcmpl-homebrew-get-command-options command)))
         ((string= command "reinstall")
          (pcomplete-here
           (if pcmpl-homebrew-cask-installed?
               (append (pcmpl-homebrew-installed-formulas) (pcmpl-homebrew-cask-local-casks))
             (pcmpl-homebrew-installed-formulas))))
         ((string= command "upgrade")
          (pcomplete-here
           (if (pcmpl-contains-options "--cask")
               (pcmpl-homebrew-cask-local-casks)
             (pcmpl-homebrew-installed-formulas))))
         ((member command pcmpl-homebrew-local-formulas-commands)
          (pcomplete-here (pcmpl-homebrew-installed-formulas)))
         ((member command pcmpl-homebrew-global-formulas-commands)
          (pcomplete-here (pcmpl-homebrew-all-formulas)))
         ((string= command "help")
          (pcomplete-here pcmpl-homebrew-commands))
         ((string= command "services")
          (pcomplete-here pcmpl-homebrew-services-commands))
         ((string= command "cask")
          (when pcmpl-homebrew-cask-installed?
            (let ((subcommand (nth 2 pcomplete-args)))
              (pcomplete-here pcmpl-homebrew-cask-commands)
              (cond ((member subcommand '("fetch" "home" "info"))
                     (pcomplete-here (pcmpl-homebrew-cask-all-casks)))
                    ((string= subcommand "install")
                     (while (pcomplete-here (pcmpl-homebrew-cask-all-casks))))
                    ((string= subcommand "uninstall")
                     (while (pcomplete-here (pcmpl-homebrew-cask-local-casks))))))))))))

(provide 'pcmpl-homebrew)

;;; pcmpl-homebrew.el ends here
