# Nix Tools

A small toolkit for working with Nix from elisp.

Largely intended for use from
[aether](https://github.com/transitracer/aether) but useful for
writing elisp code that neatly integrates with nix in general.

Intended more for package authors/emacs power-users than for
end-users.

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