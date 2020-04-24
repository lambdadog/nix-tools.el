;;; -*- lexical-binding: t; -*-

;;; nix-require --- Depend on a nix package inline in elisp
(require 'nix-eval)

(defgroup nix-require nil
  "Adds the capability to \"require\" nix derivations from elisp code"
  :prefix "nix-require-")

(defcustom nix-path (or (getenv "NIX_PATH")
			 ;; Join on ":"
			 (concat (getenv "HOME") "/.nix-defexpr/channels:nixpkgs=/nix/var/nix/profiles/per-user/root/channels/nixpkgs:/nix/var/nix/profiles/per-user/root/channels"))
  "The NIX_PATH environmental variable. I've attempted to set this to a reasonable default based on my system, but if it's broken for you, you can either copy-paste the result of `echo $NIX_PATH` or use a package like `exec-path-from-shell` to derive it for you."
  :type 'string
  :group 'nix-require)

;; Stores the active nix-repl
(setq nix-require--repl nil)

;;;###autoload
(defmacro nix-require (name nix-expr)
  "Require a nix derivation from elisp. Takes an absolute path, acting as the nix derivation is its root."
  `(defun ,name (&optional path)
     (when (and (stringp path) (not (file-name-absolute-p path)))
       (error (format "Path passed to '%s must be absolute" (quote ,name))))
     (concat (nix-get ,nix-expr) path)))

;;;###autoload
(defun nix-get (nix-expr &optional no-root)
  (interactive "MNix Expression: ")
  (let* ((outpath (nix-require--get-outpath nix-expr))
	 (add-root (if (not no-root)
		       (format "--out-link %s/nix-roots/%s"
			       user-emacs-directory
			       (file-name-nondirectory outpath))
		     "--no-out-link"))
	 (build-command (format "NIX_PATH=%s nix-build %s -E %S" nix-path add-root nix-expr)))
    (unless (zerop (call-process-shell-command build-command nil nil))
        (error (format "Failed to build derivation for %S" nix-expr)))
      outpath))

(defun nix-require--get-outpath (nix-expr)
  (nix-require--apply nix-expr "%s.outPath"))

;; Currently fails to handle any kind of error intelligently
(defun nix-require--apply (nix-expr format-string)
  (let ((applied-expr (format format-string (format "(%s)" nix-expr))))
    (substring
     (nix-eval-raw applied-expr)
     1 -1)))

;; TODO: Figure out a way to avoid continuously opening up new processes. Perhaps a persistent nix repl?
(provide 'nix-require)
