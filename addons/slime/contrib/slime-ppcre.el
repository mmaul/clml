;;; slime-ppcre.el --- ppcre based find / replace in asdf components
;;
;; Author: Kilian Sprotte <kilian.sprotte@gmail.com>
;; License: GNU GPL (same license as Emacs)
;;
;;; Installation
;;
;; Add this to your .emacs:
;;
;;   (add-to-list 'load-path "<directory-of-this-file>")
;;   (add-hook 'slime-load-hook (lambda () (require 'slime-ppcre)))
;;


;;;# Utilities
(defvar *slime-ppcre-default-directory* (expand-file-name "~/")
  "Paths in *grep* buffer will be shown as enough-namestring
 against this directory.")

(defvar *slime-ppcre-fifo-path* "/tmp/slime-ppcre.fifo"
  "The fifo that swank will write into, which on the other side
is read by the *grep* buffer.")

(defun slime-ppcre-compilation-start (highlight-regexp)
  "Open the *grep* buffer, create a fifo at *slime-ppcre-fifo-path*
and start reading from that."
  (compilation-start (concat "cd " *slime-ppcre-default-directory*
                             ";mkfifo " *slime-ppcre-fifo-path*
                             ";cat " *slime-ppcre-fifo-path*)
                     'grep-mode nil highlight-regexp))

(defun slime-ppcre-delete-fifo-callback (res)
  "Delete the fifo."
  (delete-file *slime-ppcre-fifo-path*))

(defun slime-ppcre-occurrences-to-fifo (regex system-or-systems)
  "Find REGEX in SYSTEM-OR-SYSTEMS and write the occurrences
to *slime-ppcre-fifo-path*."
  (slime-eval-async
   `(swank:find-regex-systems-to-file
     ,regex ,system-or-systems ,*slime-ppcre-fifo-path*
     ,*slime-ppcre-default-directory*)
   #'slime-ppcre-delete-fifo-callback))

(defun slime-ppcre-replacement-message-callback (file-num)
  "Show replacement confirmation."
  (slime-message "Replacements done in %d file(s)." file-num))

(defun slime-ppcre-replace-regex (regex replace system-or-systems)
  "Replace all occurrences of REGEX with REPLACE in all SYSTEM-OR-SYSTEMS."
  (slime-eval-async
   `(swank:replace-regex-systems
     ,regex ,replace ,system-or-systems)
   #'slime-ppcre-replacement-message-callback))

(defun slime-ppcre-swank-symbol-value-form (symbol-name)
  "Return a form that swank will evaluate as
the symbol-value of SYMBOL-NAME (in slime-current-package)."
  `(cl:symbol-value (cl:intern (cl:string-upcase ,symbol-name)
                               ,(slime-current-package))))


;;;# Commands
;;;## Find
(defun slime-ppcre-find-regex-system (regex system)
  "Compile the results of finding REGEX in SYSTEM."
  (interactive (list (read-from-minibuffer "Regex: ")
                     (slime-read-system-name)))
  (slime-ppcre-compilation-start regex)
  (slime-ppcre-occurrences-to-fifo regex `(asdf:find-system ,system)))

(defun slime-ppcre-find-regex-systems-all (regex)
  "Compile the results of finding REGEX in all systems
known to asdf::*defined-systems*."
  (interactive (list (read-from-minibuffer "Regex: ")))
  (slime-ppcre-compilation-start regex)
  (slime-ppcre-occurrences-to-fifo regex '(swank:all-systems)))

(defun slime-ppcre-find-regex-systems-variable (regex variable-name)
  "Compile the results of finding REGEX in all systems denoted
by swank's symbol-value (atom or list) of VARIABLE-NAME."
  (interactive (list (read-from-minibuffer "Regex: ")
                     (slime-read-symbol-name "Systems variable: " t)))
  (slime-ppcre-compilation-start regex)
  (slime-ppcre-occurrences-to-fifo
   regex
   (slime-ppcre-swank-symbol-value-form variable-name)))

;;;## Replace
(defun slime-ppcre-replace-regex-system (regex replace system)
  "Replace all occurrences of REGEX with REPLACE in SYSTEM."
  (interactive (let ((regex (read-from-minibuffer "Regex: ")))
                 (list regex
                       (read-from-minibuffer (concat "Replace "
                                                     regex " with: "))
                       (slime-read-system-name))))
  (slime-ppcre-replace-regex regex replace `(asdf:find-system ,system)))

(defun slime-ppcre-replace-regex-systems-all (regex replace)
  "Replace all occurrences of REGEX with REPLACE in all systems
known to asdf::*defined-systems*.."
  (interactive (let ((regex (read-from-minibuffer "Regex: ")))
                 (list regex
                       (read-from-minibuffer (concat "Replace " regex " with: ")))))
  (slime-ppcre-replace-regex regex replace '(swank:all-systems)))

(defun slime-ppcre-replace-regex-systems-variable (regex replace variable-name)
  "Replace all occurrences of REGEX with REPLACE in all systems denoted
by swank's symbol-value (atom or list) of VARIABLE-NAME."
  (interactive (let ((regex (read-from-minibuffer "Regex: ")))
                 (list regex
                       (read-from-minibuffer (concat "Replace " regex " with: "))
                       (slime-read-symbol-name "Systems variable: " t))))
  (slime-ppcre-replace-regex
   regex
   replace
   (slime-ppcre-swank-symbol-value-form variable-name)))


;;;# Initialization
(defun slime-ppcre-init ()
  (add-hook 'slime-connected-hook 'slime-ppcre-on-connect)
  (slime-ppcre-bind-keys))

(defun slime-ppcre-define-key (key command)
  (define-key slime-mode-map key command)
  (define-key slime-repl-mode-map key command))

(defun slime-ppcre-bind-keys ()
  (let ((bindings '(("\C-cfs" . slime-ppcre-find-regex-system)
                    ("\C-cfa" . slime-ppcre-find-regex-systems-all)
                    ("\C-cfv" . slime-ppcre-find-regex-systems-variable)
                    ("\C-crs" . slime-ppcre-replace-regex-system)
                    ("\C-cra" . slime-ppcre-replace-regex-systems-all)
                    ("\C-crv" . slime-ppcre-replace-regex-systems-variable))))
    (dolist (binding bindings)
      (slime-ppcre-define-key (car binding) (cdr binding)))))

(defun slime-ppcre-on-connect ()
  (slime-eval-async '(swank:swank-require :swank-ppcre)))

(slime-ppcre-init)

(provide 'slime-ppcre)