;;;; schelong-mode

(require 'scheme)


(defun run-schelong ()
  (interactive)
  (switch-to-buffer (make-comint "schelong" "./schelong.scm"))
  (schelong-mode))

(define-derived-mode schelong-mode comint-mode "Schelong"
  "Major mode for interacting with Schelong interpreter"
  (setq comint-prompt-regexp ";;; M-Eval input:")
  (set-syntax-table scheme-mode-syntax-table)
  (set (make-local-variable 'font-lock-defaults)
       '((scheme-font-lock-keywords
  	  scheme-font-lock-keywords-1 scheme-font-lock-keywords-2)
  	 nil t (("+-*/.<>=!?$%_&~^:" . "w") (?#. "w 14"))
  	 beginning-of-defun
  	 (font-lock-mark-block-function . mark-defun)
  	 (font-lock-syntactic-face-function
  	  . scheme-font-lock-syntactic-face-function)
  	 (parse-sexp-lookup-proxperties . t)
  	 (font-lock-extra-managed-props syntax-table))))

(provide 'schelong)