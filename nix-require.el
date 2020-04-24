;;; -*- lexical-binding: t; -*-

;;; nix-require --- Depend on a nix package inline in elisp
(require 'nix-eval)

;; Stores the active nix-repl
(setq nix-require--repl nil)

;;;###autoload
(defmacro nix-require (name nix-expr)
  "Require a nix derivation from elisp. Takes an absolute path,
acting as the nix derivation is its root."
  `(defun ,name (&optional path)
     (when (and (stringp path) (not (file-name-absolute-p path)))
       (error (format "Path passed to '%s must be absolute" (quote ,name))))
     (concat (nix-get ,nix-expr) path)))

;;;###autoload
(defun nix-get (nix-expr &optional no-root)
  "Build a nix derivation and return its store path, adding a
nix-root to ~/.emacs.d/nix-roots/ (or wherever
user-emacs-directory is set to in place of ~/.emacs.d). The
no-root optional argument will disable the adding of roots and
allow the derivation to be garbage collected next time
nix-collect-garbage is run."
  (interactive "MNix Expression: ")
  (let* ((outpath (nix-require--get-outpath nix-expr))
	 (add-root (if (not no-root)
		       (format "--out-link %s/nix-roots/%s"
			       user-emacs-directory
			       (file-name-nondirectory outpath))
		     "--no-out-link"))
	 (build-command (format "nix-build %s -E %S" add-root nix-expr)))
    (unless (zerop (call-process-shell-command build-command nil nil))
        (error (format "Failed to build derivation for %S" nix-expr)))
    (if (called-interactively-p 'interactive)
	(message outpath)
      outpath)))

(defun nix-require--get-outpath (nix-expr)
  (nix-require--apply nix-expr "%s.outPath"))

;; Currently fails to handle any kind of error intelligently
;; Will be fixed by implementing #'nix-eval
(defun nix-require--apply (nix-expr format-string)
  (let ((applied-expr (format format-string (format "(%s)" nix-expr))))
    (substring
     (nix-eval-raw applied-expr)
     1 -1)))

(provide 'nix-require)
