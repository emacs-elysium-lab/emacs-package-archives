;;; haskell-ng-project.el --- Support for Haskell projects  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author:  <magnus@therning.org>
;; Keywords: convenience, languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Variables and functions for Haskell projects

;;; Code:

(require 'dash)
(require 'f)
(require 'project)

;;;###autoload
(defun haskell-ng-is-project ()
  "Helper to recognise a haskell project"
  (let ((has-proj (-as-> (project-current) @
                         (project-root @)
                         (f-join @ "cabal.proj")
                         (f-file-p @)))
        (has-cabal (-as-> (project-current) @
                          (project-root @)
                          (f-glob "*.cabal" @))))
    (or has-proj has-cabal)))

(provide 'haskell-ng-project)
;;; haskell-ng-project.el ends here
