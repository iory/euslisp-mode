;;; Test script for euslisp-mode
;;; Run with: emacs -Q -l test-local.el

;; Add current directory to load path
(add-to-list 'load-path (file-name-directory load-file-name))

;; Install required packages if not available
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Check and install required packages
(unless (package-installed-p 'helm)
  (package-refresh-contents)
  (package-install 'helm))

(unless (package-installed-p 's)
  (package-install 's))

(unless (package-installed-p 'exec-path-from-shell)
  (package-install 'exec-path-from-shell))

;; Load euslisp-mode
(require 'euslisp-mode)

;; Open a test file
(if (file-exists-p "test.l")
    (find-file "test.l")
  (progn
    (find-file "test.l")
    (insert ";; Sample Euslisp code for testing\n\n")
    (insert "(defun test-function (x y)\n")
    (insert "  (+ x y))\n\n")
    (insert "(defclass test-class\n")
    (insert "  :super object\n")
    (insert "  :slots (value))\n\n")
    (insert "(defmethod test-class\n")
    (insert "  (:init ()\n")
    (insert "    (setq value 0)\n")
    (insert "    self))\n")))

;; Enable euslisp-mode
(euslisp-mode)

(message "euslisp-mode loaded successfully! Test with M-x euslisp-find-definition-function")