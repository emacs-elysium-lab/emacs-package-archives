;;; haskell-ng-mode.el --- A mode for Haskell -*- lexical-binding: t; -*-

;; Author: Magnus Therning
;; URL: https://gitlab.com/magus/haskell-ng-mode
;; Version: 0.0.1
;; Keywords: cabal haskell languages tree-sitter
;; Package-Requires: ((emacs "29") (projectile "2.8"))

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
;; ---------------------------------------------------------------------

;;; Commentary:
;; A collection modes for haskell and cabal files using built in tree-sitter
;; support and mode for interacting with haskell repls.

;;; Code:

(require 'treesit)
(require 'rx)

(defvar haskell-ng--reserved-words
  '("as" "case" "class" "data" "deriving" "do" "else" "hiding"
    "if" "import" "in" "infix" "infixl" "infixr" "instance" "let"
    "module" "newtype" "of" "qualified" "then" "type" "where"))

(defvar haskell-ng--font-lock-setting
  (treesit-font-lock-rules
   :feature 'comment
   :language 'haskell
   '((comment) @font-lock-comment-face
     (haddock) @font-lock-doc-face
     (pragma) @font-lock-preprocessor-face)

   :feature 'number
   :language 'haskell
   '([(integer) (float)] @font-lock-number-face)

   :feature 'string
   :language 'haskell
   '([(char) (string)] @font-lock-string-face)

   :feature 'keyword
   :language 'haskell
   `([,@haskell-ng--reserved-words] @font-lock-keyword-face)

   :feature 'operator
   :language 'haskell
   '((operator) @font-lock-operator-face
     ("->" @font-lock-operator-face))

   :feature 'type
   :language 'haskell
   '((type) @font-lock-type-face)

   :feature 'identifier
   :language 'haskell
   '((signature name: (variable) @font-lock-function-name-face)
     (function name: (variable) @font-lock-function-name-face)
     (bind name: (variable) @font-lock-function-name-face))

   :feature 'module-qualifier
   :language 'haskell
   '((qualified module: (module) @font-lock-warning-face)))
  "Tree-sitter font-lock settings.")

(defvar haskell-ng--imenu-settings
  `(("Imports" ,(rx bos "imports" eos) nil nil)
    ("Types" ,(rx bos (or "data_type" "newtype" "type_synomym") eos)
     haskell-ng--node-toplevel-p nil)
    ("Instances" ,(rx bos "instance" eos) haskell-ng--node-toplevel-p nil)
    ("Functions" ,(rx bos "signature" eos) nil nil)))

(defvar haskell-ng--defun-type-regexp
  (rx bos (or "import" "data_type" "newtype" "type_synomym" "instance"
              "signature" "bind" "function")
      eos))

(defvar haskell-ng--indent-rules
  `((haskell
     (no-node parent 0)))
  "Tree-sitter indent rules.")

(defvar haskell-ng-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "<backtab>") 'haskell-ng--de-indent-line)
    km))

;;;###autoload
(define-derived-mode haskell-ng-mode prog-mode "Haskell NG Mode"
  "A modern mode for Haskell files."
  :group 'haskell

  (unless (treesit-ready-p 'haskell)
    (error "Tree-sitter for Haskell is not available"))

  (treesit-parser-create 'haskell)
  ;; set up treesit
  (setq-local treesit-font-lock-feature-list
              '((comment number string keyword)
                (operator type module-qualifier)
                (identifier) ()))
  (setq-local treesit-simple-imenu-settings haskell-ng--imenu-settings)
  (setq-local treesit-font-lock-settings haskell-ng--font-lock-setting)
  (setq-local treesit-defun-type-regexp haskell-ng--defun-type-regexp)
  (setq-local treesit-defun-name-function #'haskell-ng--defun-name)
  (treesit-major-mode-setup)
  (setq-local treesit-simple-indent-rules haskell-ng--indent-rules)

  ;; comment setup
  (setq-local comment-start "-"
              comment-end ""
              comment-add 1
              comment-column 40
              comment-padding " "
              comment-auto-fill-only-comments t
              comment-start-skip (rx (or "--" "{-") (zero-or-more space))
              comment-end-skip (rx (zero-or-more space) (or "-}" (syntax comment-end))))

  (setq-local indent-tabs-mode nil)

  ;; things
  (setq-local treesit-thing-settings
              '((haskell (adt "adt")
                         (class "class")
                         (function "function")
                         (instance "instance")
                         (newtype "newtype")
                         (type-alias "type_alias")
                         (defun (or adt class function instance newtype type-alias)))))

  (setq-local beginning-of-defun-function 'treesit-beginning-of-defun)
  (setq-local end-of-defun-function 'treesit-end-of-defun))

(defun haskell-ng--node-toplevel-p (node)
  "Check if NODE is a named node."
  (when-let* ((type (treesit-node-type (treesit-node-parent node))))
    (string-match-p (rx (or "imports" "declarations")) type)))

(defun haskell-ng--get-text (node rx)
  "Obtain text of a child node of NODE matching RX."
  (if-let* ((node (treesit-search-subtree node rx)))
      (treesit-node-text node)
    ""))

(defun haskell-ng--defun-name (node)
  "Return name of defun represented by NODE."
  (pcase (treesit-node-type node)
    ("imports" "List of imports")
    ("import" (haskell-ng--get-text node 'module))
    ((or "data_type" "newtype" "type_synomym")
     (haskell-ng--get-text node (rx bos "name" eos)))
    ((or "signature" "function")
     (haskell-ng--get-text node (rx bos "variable" eos)))
    ("instance" (let ((start (treesit-node-start node)))
                  (goto-char start)
                  (re-search-forward (rx "instance" (* (or "\t" "\s" "\n"))))
                  (setq start (point))
                  (re-search-forward (rx (* "\t\s") (or "where" "\n")))
                  (buffer-substring
                   start (1- (match-beginning 0)))))))

(defun haskell-ng--de-indent-line ()
  "Naive and simple function for de-indenting the current line."
  (interactive "*")
  (let* ((curr-ind (current-indentation))
         (rem (cl-rem curr-ind 4))
         (new-ind (max 0 (if (eq rem 0) (- curr-ind 4) (- curr-ind rem)))))
    (indent-line-to new-ind)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-ng-mode))

;; TODO
;; - goto imports
;; - interactive mode
;; - session target
;; - cabal support

(provide 'haskell-ng-mode)
;;; haskell-ng-mode.el ends here
