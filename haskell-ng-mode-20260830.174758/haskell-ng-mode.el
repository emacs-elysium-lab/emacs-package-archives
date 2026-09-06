;;; haskell-ng-mode.el --- A mode for Haskell -*- lexical-binding: t; -*-

;; Author: Magnus Therning
;; URL: https://gitlab.com/magus/haskell-ng-mode
;; Version: 0.0.1
;; Keywords: cabal haskell languages tree-sitter
;; Package-Requires: ((emacs "29") (dash) (f) (project))

;; This file is not part of GNU Emacs.

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
;; A mode for the Haskell programming language.

;; - https://www.haskell.org/
;; - https://www.haskell.org/cabal/
;; - https://github.com/tree-sitter/tree-sitter-haskell
;; - https://gitlab.com/magus/tree-sitter-cabal.git

;;; Code:

(require 'dash)
(require 'rx)
(require 'treesit)

;;;###autoload
(defconst haskell-ng-grammar-recipe
  '(haskell "https://github.com/tree-sitter/tree-sitter-haskell" :revision "v0.23.1")
  "Tree-sitter grammar recipe for Haskell.
Suitable for use as the value of `treesit-language-source-alist'.")

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

(defconst haskell-ng--defun-pred
  (cons (rx (or "type_synomym" ;; yes, misspelled in the grammar!
                "newtype"
                "data_type"
                "class"
                "instance"
                "bind"
                "function"))
        (lambda (node) (string= (treesit-node-type (treesit-node-parent node)) "declarations")))
  "Predicate for recognizing a node as a 'defun'.")

(defvar haskell-ng--indent-rules
  '((haskell
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
  (setq-local treesit-font-lock-settings haskell-ng--font-lock-setting)
  (setq-local treesit-defun-type-regexp nil ; prefer defun from treesit-thing-settings
              treesit-defun-tactic 'top-level
              treesit-defun-name-function #'haskell-ng--defun-name)
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

  ;; electric
  (setq-local electric-pair-pairs
              '((?` . ?`) (?\( . ?\)) (?{ . ?}) (?\" . ?\") (?\[ . ?\])))

  (setq-local indent-tabs-mode nil)

  ;; things
  (setq-local treesit-thing-settings
              `((haskell
                 (sexp haskell-ng--sexp)
                 ;; sentence
                 (comment ,(rx (or "comment" "haddock")))
                 (string ,(rx "string"))
                 (text (or comment string))
                 (defun ,haskell-ng--defun-pred))))

  (setq-local beginning-of-defun-function 'treesit-beginning-of-defun)
  (setq-local end-of-defun-function 'treesit-end-of-defun))

(defun haskell-ng-goto-imports ()
  "Jump to the imports section."
  (interactive)
  (if-let* ((root (treesit-buffer-root-node 'haskell))
            (imports (treesit-node-child-by-field-name root "imports")))
      (progn
        (evil-set-jump)
        (goto-char (treesit-node-start imports)))
    (user-error "No imports found in this buffer!")))

(defun haskell-ng--defun-name (node)
  "Return name of defun represented by NODE."
  (when (member (treesit-node-type node) '("bind" "function"))
    (--> node
         (treesit-search-subtree it "variable")
         (treesit-node-text it))))

(defun haskell-ng--de-indent-line ()
  "Naive and simple function for de-indenting the current line."
  (interactive "*")
  (let* ((curr-ind (current-indentation))
         (rem (cl-rem curr-ind 4))
         (new-ind (max 0 (if (eq rem 0) (- curr-ind 4) (- curr-ind rem)))))
    (indent-line-to new-ind)))

(defun haskell-ng--sexp (node)
  "Return non-nil on a sexp NODE."
  (let ((node-text (treesit-node-text node 1)))
    (and
     (not (member node-text '( "{" "}" "[" "]" "(" ")" ";")))
     (not (and (string= "operator" (treesit-node-field-name node))
               (= 1 (length node-text)))))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-ng-mode))

;; TODO
;; - interactive mode
;; - session target
;; - cabal support

(provide 'haskell-ng-mode)
;;; haskell-ng-mode.el ends here
