# Racket pretty printer

This repo contains two packages: core which is used as a command line tool and vscode which is a vscode extension.

## core

This is a command line tool used to pretty print racket src files. (based on *A prettier printer Philip Wadler*

Inside core folder, make sure you have haskell `stack` command in path and call `stack install`. 
Then call `racket-pretty-printer --help` for full command line usage.

## vscode

Search `racket-pretty-printer` inside vscode extension market.
Use command `racket-pretty` inside vscode, or turn on `format-on-save`.

# TODO

- options
- config file support


