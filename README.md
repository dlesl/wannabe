Wannabe
=====

A game which may be quite similar to Hanabi (but worse). Designed to be played
on mobile.

# Development

You'll need erlang, rebar3, clojure and node/npm. The easiest way to get a
working environment is to use the provided `shell.nix`.

    $ nix-shell
    $ rebar3 shell  # start backend

Build the frontend:
    
    $ cd frontend && npm install && npx shadow-cljs watch app

or `M-x cider-jack-in-cljs` -> `shadow` -> `:app`

Then point your browser at http://localhost:8080

# Build

To build a self-contained OTP release:
    
    $ make release

To build a docker image:

    $ make image
