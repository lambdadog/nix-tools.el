;;; -*- lexical-binding: t; -*-

;;; nix-eval -- Run nix code and receive the results
(require 'ansi-color) ; used for filtering out ansi codes in output
(require 'subr-x)

(defgroup nix-eval nil
  "Evaluate nix expressions from elisp"
  :prefix "nix-eval-")

(setq nix-eval--repl nil)
(setq nix-eval--repl-finished nil)

;;;###autoload
(defun nix-eval (nix-expr)
  "Evaluate a Nix expression and return the response. If you're going to be making many repeated similar calls a recommended optimization is to let-bind 'nix-eval-strict to nil then run 'nix-eval-clean afterwards to allow nix-repl to cache data."
  (interactive "MNix expression: ")
  (error "Unimplemented"))

;;;###autoload
(defun nix-eval-raw (nix-expr)
  "Evaluate a Nix expression and return a raw response. All caveats of 'nix-eval apply."
  (interactive "MNix expression: ")
  (nix-eval--eval-internal nix-expr (called-interactively-p 'interactive)))

(defun nix-eval--eval-internal (nix-expr &optional interactive)
  (when (nix-eval--expr-valid-p nix-expr interactive)

    ;; Create the process if it's not been already
    (unless (process-live-p nix-eval--repl)
      (nix-eval--init-repl)
      (nix-eval--wait-on-output nix-eval--repl))

    (let ((buffer (process-buffer nix-eval--repl)))
      (with-current-buffer buffer
	(let ((start (point-max)))
	  (process-send-string nix-eval--repl (format "%s\n" nix-expr))
	  (nix-eval--wait-on-output nix-eval--repl)
	  (let ((output (buffer-substring start (- (point-max) 1))))
	    (when interactive
	      (message output))
	    output))))))

(defun nix-eval--throw-or-print (msg interactive)
  (if interactive
      (message msg)
    (throw 'nix-eval-error msg))
  nil)

(defun nix-eval--expr-valid-p (nix-expr &optional interactive)
  (let ((expr (string-trim nix-expr)))
    (cond
     ((string= ":" (substring expr 0 1))
      (nix-eval--throw-or-print "repl-specific commands are not permitted for use in nix-eval" interactive))
     (t t))))

(defun nix-eval--init-repl ()
  (setq nix-eval--repl
	(make-process
	 :name "nix-eval repl"
	 :buffer "*nix-eval repl*"
	 :command '("nix" "repl")
	 :connection-type 'pipe
	 :filter 'nix-eval--process-filter
	 :noquery t)))

(defun nix-eval--process-filter (proc string)
  (let ((buffer (process-buffer proc)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
	(let ((moving (eq (point) (process-mark proc)))
	      (finished (string-match-p "^\n" string)))
	  (unless (string= "\n" string)
	    (save-excursion
	      (goto-char (process-mark proc))
	      ;; TODO: remove 'ansi-color dependency
	      (insert (replace-regexp-in-string "\n\n" "\n" (ansi-color-filter-apply string)))
	      (set-marker (process-mark proc) (point))))
	  (when moving (goto-char (process-mark proc)))
	  (when finished (setq nix-eval--repl-finished t)))))))

(defun nix-eval--wait-on-output (proc)
  (while (not nix-eval--repl-finished)
    (accept-process-output proc nil nil t))
  (setq nix-eval--repl-finished nil))

(provide 'nix-eval)
