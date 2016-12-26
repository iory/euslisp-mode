;;; euslisp-mode.el --- Major mode for Euslisp-formatted text -*- lexical-binding: t; -*-

;; Author: iory <ab.ioryz@gmail.com>
;; Maintainer: iory <ab.ioryz@gmail.com>
;; Created: April 13, 2016
;; Version: 0.0.3
;; Keywords: Euslisp, euslisp, GitHub
;; URL: https://github.com/iory/euslisp-mode


;;; Constants =================================================================

(defconst euslisp-mode-version "0.0.3"
  "Euslisp mode version number.")

(defconst euslisp-output-buffer-name "*euslisp-output*"
  "Name of temporary buffer for euslisp command output.")

(defconst euslisp-mode-source-dir
  (if load-file-name
      (file-name-directory load-file-name)
    default-directory
    )
  "Source dir of euslisp-mode")

(load (concatenate #'string euslisp-mode-source-dir "euslisp-find-definition.el"))

;;; Mode Definition  ==========================================================

(defun euslisp-show-version ()
  "Show the version number in the minibuffer."
  (interactive)
  (message "euslisp-mode, version %s" euslisp-mode-version))

;;;###autoload
(define-derived-mode euslisp-mode lisp-mode "Euslisp"
  "Major mode for editing Euslisp files."
  ;; Indentation
  (setq lisp-indent-function 'euslisp-indent-function)
  )

;;;###autoload
(setq auto-mode-alist
      (cons (cons "\\.l\\'" 'euslisp-mode) auto-mode-alist))

;;; Indentation ====================================================================

(defun euslisp-indent-function (indent-point state)
  "Indent the euslisp code."
  (interactive)
  (let ((normal-indent (current-column)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (if (and (elt state 2)
             (not (looking-at "\\sw\\|\\s_")))
        (progn
          (if (not (> (save-excursion (forward-line 1) (point))
                      calculate-lisp-indent-last-sexp))
              (progn (goto-char calculate-lisp-indent-last-sexp)
                     (beginning-of-line)
                     (parse-partial-sexp (point)
                                         calculate-lisp-indent-last-sexp 0 t)))
          (backward-prefix-chars)
          (current-column))
      (let ((function (buffer-substring (point)
                                        (progn (forward-sexp 1) (point))))
            method)
        (setq method (or (get (intern-soft function) 'lisp-indent-function)
                         (get (intern-soft function) 'lisp-indent-hook)))
        (cond ((or (eq method 'defun)
                   (string-match ":.*" function)
                   (and (null method)
                        (> (length function) 3)
                        (string-match "\\`def" function)))
               (lisp-indent-defform state indent-point))
              ((integerp method)
               (lisp-indent-specform method state
                                     indent-point normal-indent))
              (method
               (funcall method state indent-point)))))
    )
  )

(font-lock-add-keywords
 'euslisp-mode
 (list
  (list (concat "(" (regexp-opt '("defforeign") t) "\\>") '(1 font-lock-keyword-face nil t))
  (list "\\(self\\)\\>" '(1 font-lock-constant-face nil t))
  (list "\\(\\*\\w\+\\*\\)\\>" '(1 font-lock-constant-face nil t))
  (list "\\(#\\(\\+\\|\\-\\)\.\*\\)" '(1 font-lock-variable-name-face))
  (list "\\(throw-error\\)" '(1 font-lock-warning-face nil t))
  (list (concat "(" (regexp-opt '("warn" "warning-message") t) "\\>") '(1 font-lock-warning-face nil t))
  (list (concat "(" (regexp-opt '("send" "send-all" "send-super") t) "\\>") '(1 font-lock-builtin-face nil t))
  (list "\\(\\*[^ ]*\\*\\)" '(1 font-lock-constant-face nil t))
  (list (concat "(" (regexp-opt '("load") t) "\\>") '(1 font-lock-keyword-face nil t))
  (list (concat "(" (regexp-opt '("setq") t) "\\>") '(1 font-lock-type-face nil t))
  )
 )


(provide 'euslisp-mode)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; euslisp-mode.el ends here
