# Nix Tools

Your nix pocket-knife in emacs.

A small suite of emacs packages that make working with nix in emacs
trivial, from requiring files from nix derivations inline in elisp to
using nix shells (implementation in-progress).

## Modules

### `nix-expr`

Uses a nix-repl subprocess to run arbitrary nix commands. Useful for
quickly querying nix information. Nixpkgs is *not* loaded in this
repl, and if you want to use it you're forced to either let-bind it or
just inline `(import <nixpkgs> {})` into your nix expression.

### `nix-require`

An macro for requiring nix derivations from your elisp code. Currently
fairly rudimentary. Uses `nix-eval` to ensure that the expression is a
derivation, etc.
