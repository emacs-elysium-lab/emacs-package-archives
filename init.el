;;; init.el --- init file for github ci -*- coding: utf-8; lexical-binding: t; -*-



;; init file for github ci


;;; Code:

;;; Update by adding lines

;; Initialize package sources

(require 'package)
(eval-when-compile
  (require 'cl-lib))

(setq package-archives '(("elpa"   . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa"  . "https://melpa.org/packages/")))

(setq package-archive-priorities '(("melpa"  . 50)
                                   ("nongnu" . 30)
                                   ("elpa"   . 10)))

(package-refresh-contents)

(when (boundp 'native-comp-jit-compilation)
  (setq native-comp-jit-compilation nil))

(defvar mypackages
  '(gnu-elpa-keyring-update
    quelpa
    elpa-mirror
    auto-package-update)
  "A list of packages to ensure are installed at launch.")

;; (setq package-pinned-packages '((telega . "melpa-stable")
;;                                 ))

;; Scans the list in mypackages
;; If the package listed is not already installed, install it
(mapc #'(lambda (package)
          (unless (package-installed-p package)
            (package-install package)))
      mypackages)
(package-initialize)

;; quelpa packages https://github.com/quelpa/quelpa

(setq quelpa-checkout-melpa-p nil
      quelpa-update-melpa-p nil
      quelpa-melpa-recipe-stores nil
      quelpa-git-clone-depth 1)

(quelpa '(once :fetcher github :repo "emacs-magus/once" :files (:defaults "once-setup/once-setup.el")))


(defmacro package! (package)
  (when (and (listp package) (or (eq '\` (car package))
                                 (eq 'quote (car package))))
    (setq package (eval package)))
  (cl-destructuring-bind (&key url host repo files &allow-other-keys) (cdr package)
    (let* ((pkg (car package))
           ;; TODO: support doom's :fork, :pin keywords
           (repo-or-url (pcase host
                          ('github `(:fetcher ,host :repo ,repo))
                          ('gitlab `(:fetcher ,host :repo ,repo))
                          (`codeberg `(:fetcher git :url ,(concat "https://codeberg.org/" repo)))
                          (`sourcehut `(:fetcher git :url ,(concat "https://git.sr.ht/~" repo)))
                          (_ nil))))
      `(quelpa '(,pkg ,@(if repo-or-url repo-or-url url)  ,@(if files `(:files ,files)))))))

(package! (setup :host codeberg :repo "pkal/setup.el"))
(package! (eglot-grammarly :host github :repo "emacs-grammarly/eglot-grammarly"))
(package! (easy-kill-extras :host github
                            :build (:not autoloads)
                            :repo "knu/easy-kill-extras.el"))
(package! (bookmark+ :host github :repo "emacsmirror/bookmark-plus"))
(package! (sly-el-indent :host github :repo "cireu/sly-el-indent" :files ("*.el" "lib")))
(package! (ox-odt
           :host github
           :repo "kjambunathan/org-mode-ox-odt"
           :files ("lisp/ox-odt.el"
                   "lisp/odt.el"
                   "etc"
                   "docs"
                   "contrib/odt/LibreOffice")))
(package! (org-extra-emphasis :host github :repo "QiangF/org-extra-emphasis"))
(package! (denote :host sourcehut :repo "protesilaos/denote"))
(package! (consult-notes :type git :host github :repo "mclear-tools/consult-notes"))
(package! (lsp-java :type git :repo "emacs-lsp/lsp-java" :host github :files (:defaults "icons")))
(package! (chatgpt-shell :type git :host github :repo "xenodium/chatgpt-shell"))
(package! (ol-wl :host sourcehut :repo "bzg/org-contrib" :files ("lisp/ol-wl.el")))
(package! (eglot-booster :host github :repo "jdtsmith/eglot-booster"))
;;; init.el ends here
