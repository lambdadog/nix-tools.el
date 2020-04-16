;;; -*- lexical-binding: t; -*-

;;; nix-eval -- Run nix code and receive the results
(require 'ansi-color) ; used for filtering out ansi codes in output

(setq nix-eval--repl nil)
(setq nix-eval--repl-finished nil)

(defun nix-eval (nix-expr)
  "Evaluate Nix expression"
  (interactive "MNix expression: ")
  (unless (process-live-p nix-eval--repl)
    (nix-eval--init-repl)
    (nix-eval--wait-on-output nix-eval--repl))
  (let ((buffer (process-buffer nix-eval--repl)))
    (with-current-buffer buffer
      (let ((start (point-max)))
	(process-send-string nix-eval--repl (format "%s\n" nix-expr))
	(nix-eval--wait-on-output nix-eval--repl)
	(message (buffer-substring start (- (point-max) 1)))))))

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
	      (finished (string-match-p (regexp-quote "\n\n") string)))
	  (save-excursion
	    (goto-char (process-mark proc))
	    ;; TODO: remove 'ansi-color dependency
	    (insert (replace-regexp-in-string "\n\n" "\n" (ansi-color-filter-apply string)))
	    (set-marker (process-mark proc) (point)))
	  (when moving (goto-char (process-mark proc)))
	  (when finished (setq nix-eval--repl-finished t)))))))

(defun nix-eval--wait-on-output (proc)
  (while (not nix-eval--repl-finished)
    (accept-process-output proc nil nil t))
  (setq nix-eval--repl-finished nil))

(provide 'nix-eval)
