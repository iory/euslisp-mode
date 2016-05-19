;;; find-definition.el --- find-definition of euslisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  iory

;; Author: iory
;; Keywords: euslisp


;;; Constants =================================================================

(defconst euslisp-find-definition-py "find_euslispfunction.py"
  "Name of python script find lisp's definition"
  )

(defconst euslisp-find-definition-py-path
  (expand-file-name euslisp-find-definition-py euslisp-mode-source-dir)
  "find_euslispfunction.py's PATH"
  )


;;; Definition of find-definition =============================================

(defun euslisp-shell-command->string (cmd)
  (let ((r (shell-command-to-string cmd)))
    (substring-no-properties r 0 (1- (length r)))))

(defun euslisp-extract-load-files ()
  (let ((loads nil)
        (current-point (point)))
    (goto-char 0) ;;バッファの先頭に移動
    ;; (count-lines START END) START 位置から END のポイントまでの行数を数える
    (loop for i from 1 to (count-lines (point-min) (point-max)) do
          (cond ((string-match-p "(load " (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
                 (push (buffer-substring-no-properties (point-at-bol) (point-at-eol)) loads)
                 )
                ((string-match-p "(require " (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
                 (push (buffer-substring-no-properties (point-at-bol) (point-at-eol)) loads)
                 )
                (t
                 )
                )
          (forward-line 1))
    (goto-char current-point)
    loads)
  )

(defun euslisp-load-buffers (texts)
  (let (text
        start
        end
        loaded-file-name
        (paths "")
        )
    (loop for i from 0 to (1- (length texts)) do
          (setq text (elt texts i))
          (if (string-match "(load" text)
              (setq start (+ (length "(load") (string-match "(load" text)))
            ;; else
            (setq start (+ (length "(require") (string-match "(require" text)))
            )
          (setq end (1+ (string-match ")" text start)))
          (setq loaded-file-name (substring text start end))
          (if (string-match-p "package:\\/\\/" loaded-file-name)
              (let (package-name-start
                    package-name-end
                    file-path
                    )
                (setq package-name-start (+ (string-match "package:\\/\\/" loaded-file-name) (length "package://")))
                (setq package-name-end (string-match "\\/" loaded-file-name package-name-start))
                (setq file-path (euslisp-shell-command->string (concatenate #'string "rospack find " (substring loaded-file-name package-name-start package-name-end))))
                (when (file-exists-p file-path)
                  (setq paths (concatenate #'string paths file-path " "))
                  )
                )
            ;; else
            (progn
              (when (string-match "\\.l" loaded-file-name)
                (setq file-path (substring loaded-file-name (+ (length " \"") (string-match " \"" loaded-file-name)) (string-match "\\.l" loaded-file-name)))
                (setq file-path (concatenate #'string (substring (buffer-file-name) 0 (- (length (buffer-file-name)) (length (file-name-nondirectory (buffer-file-name)))))))
                (setq file-path (substring file-path 0 (1- (length file-path))))
                (when (file-directory-p file-path)
                  (setq paths (concatenate #'string paths file-path " "))
                  )
                )
              )
            )
          )
    paths)
  )

(defun euslisp-get-definition-path (fname)
  (let (paths
        (load-paths (euslisp-load-buffers (euslisp-extract-load-files))))
    (if load-paths
        (setq paths
              (euslisp-shell-command->string
               (concatenate #'string "python " euslisp-find-definition-py-path " " fname " " load-paths))
              )
      (setq paths
            (euslisp-shell-command->string
             (concatenate #'string "python " euslisp-find-definition-py-path " " fname))
            )
      )
    (split-string paths "\\*\\*\\*\\*")
    )
  )

(defun euslisp-goto-definition-implementation (fname)
  (helm-build-sync-source "goto-euslisp"
    :candidates (euslisp-get-definition-path fname)
    :action '(lambda (x)
               (let ((xlst (split-string x "\\*\\*\\*")))
                 (find-file (car xlst))
                 (goto-line (1+ (string-to-number (cadr xlst))))
                 (forward-char (string-match fname (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
                 ))
    :migemo t))

(defun euslisp-goto-definition (fname)
  (interactive)
  (helm :sources (euslisp-goto-definition-implementation fname) :buffer "*helm-euslisp*"))

(defun euslisp-find-definition-function ()
  (interactive)
  (load "~/.emacs.d/shellenv.el")
  (if (and transient-mark-mode mark-active)
      (euslisp-goto-definition (buffer-substring (region-beginning) (region-end)))
    (euslisp-goto-definition (euslisp-print-current-word))
    )
  )

(defun euslisp-print-current-word ()
  "print current word."
  (interactive)
  (let (p1 p2)
    (save-excursion
      (skip-chars-backward "-a-z0-9")
      (setq p1 (point))
      (skip-chars-forward "-a-z0-9")
      (setq p2 (point))
      (message "%s" (buffer-substring-no-properties p1 p2)))))


;;; Hook keymap ==========================================================================================================

(defun euslisp-enable-find-definition ()
  (define-key euslisp-mode-map "\C-c." 'euslisp-find-definition-function)
  )

(add-hook 'euslisp-mode-hook 'euslisp-enable-find-definition)


(provide 'euslisp-find-definition)
