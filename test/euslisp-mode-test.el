;;; euslisp-mode-test.el --- Tests for euslisp-mode -*- lexical-binding: t -*-

;; Copyright (C) 2024 iory

;; Author: iory <ab.ioryz@gmail.com>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; Test suite for euslisp-mode

;;; Code:

(require 'ert)
(require 'euslisp-mode)

(ert-deftest euslisp-mode-test-activation ()
  "Test that euslisp-mode can be activated."
  (with-temp-buffer
    (euslisp-mode)
    (should (eq major-mode 'euslisp-mode))))

(ert-deftest euslisp-mode-test-auto-mode-alist ()
  "Test that .l files are associated with euslisp-mode."
  (let ((auto-mode-entry (assoc "\\.l\\'" auto-mode-alist)))
    (should auto-mode-entry)
    (should (eq (cdr auto-mode-entry) 'euslisp-mode))))

(ert-deftest euslisp-mode-test-indentation ()
  "Test basic indentation in euslisp-mode."
  (with-temp-buffer
    (euslisp-mode)
    (insert "(defun test-func (x y)\n")
    (insert "(+ x y))")
    (goto-char (point-min))
    (forward-line 1)
    (indent-for-tab-command)
    (should (= (current-indentation) 2))))

(ert-deftest euslisp-mode-test-font-lock-keywords ()
  "Test that font-lock keywords are properly set."
  (with-temp-buffer
    (euslisp-mode)
    (insert "(defun test-func (x) x)")
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "defun")
    ;; Check that defun is fontified
    (should (get-text-property (match-beginning 0) 'face))))

(ert-deftest euslisp-mode-test-defforeign-keyword ()
  "Test that defforeign is recognized as a keyword."
  (with-temp-buffer
    (euslisp-mode)
    (insert "(defforeign test-foreign)")
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "defforeign")
    (should (member 'font-lock-keyword-face 
                    (ensure-list (get-text-property (match-beginning 0) 'face))))))

(ert-deftest euslisp-mode-test-self-constant ()
  "Test that 'self' is recognized as a constant."
  (with-temp-buffer
    (euslisp-mode)
    (insert "self")
    (font-lock-ensure)
    (goto-char (point-min))
    (should (member 'font-lock-constant-face
                    (ensure-list (get-text-property (point) 'face))))))

(ert-deftest euslisp-mode-test-send-builtin ()
  "Test that 'send' is recognized as a builtin."
  (with-temp-buffer
    (euslisp-mode)
    (insert "(send obj :method)")
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "send")
    (should (member 'font-lock-builtin-face
                    (ensure-list (get-text-property (match-beginning 0) 'face))))))

(ert-deftest euslisp-mode-test-helm-grep-function ()
  "Test that euslisp-helm-grep function is defined."
  (should (fboundp 'euslisp-helm-grep)))

(ert-deftest euslisp-mode-test-find-definition-function ()
  "Test that euslisp-find-definition-function is defined and interactive."
  (should (fboundp 'euslisp-find-definition-function))
  (should (commandp 'euslisp-find-definition-function)))

(ert-deftest euslisp-mode-test-print-current-word ()
  "Test euslisp-print-current-word function."
  (with-temp-buffer
    (euslisp-mode)
    (insert "test-word other-word")
    (goto-char (point-min))
    (search-forward "test")
    (should (string= (with-output-to-string
                       (euslisp-print-current-word))
                     ""))))  ; Message goes to *Messages* buffer

(ert-deftest euslisp-mode-test-indent-defclass ()
  "Test indentation for defclass."
  (with-temp-buffer
    (euslisp-mode)
    (insert "(defclass test-class\n")
    (insert ":super object\n")
    (insert ":slots (x y))")
    (goto-char (point-min))
    (forward-line 1)
    (indent-for-tab-command)
    (should (= (current-indentation) 2))
    (forward-line 1)
    (indent-for-tab-command)
    (should (= (current-indentation) 2))))

(ert-deftest euslisp-mode-test-package-requires ()
  "Test that package requirements are properly declared."
  (require 'lisp-mnt)  ; For lm-header function
  (let ((pkg-requires (with-temp-buffer
                        (insert-file-contents "euslisp-mode.el")
                        (lm-header "package-requires"))))
    (should pkg-requires)
    (should (string-match-p "helm" pkg-requires))
    (should (string-match-p "s" pkg-requires))
    (should (string-match-p "exec-path-from-shell" pkg-requires))))

;; Helper function for ensure-list compatibility
(unless (fboundp 'ensure-list)
  (defun ensure-list (object)
    "Return OBJECT as a list."
    (if (listp object)
        object
      (list object))))

(provide 'euslisp-mode-test)
;;; euslisp-mode-test.el ends here