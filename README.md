# kak.el

An attempt to port [Kakoune](https://github.com/mawww/kakoune)'s multiple selections into [Emacs](https://www.gnu.org/software/emacs/)

This package provides functions that simulate Kakoune's multiple selection commands.
It's built on top of [`evil-mc`](https://github.com/gabesoft/evil-mc)'s multiple cursors and [`evil`](https://github.com/emacs-evil/evil)'s functions.

To use the package after installation

```emacs-lisp
(use-package kak)
```

An example mapping using [Doom Emacs](https://github.com/hlissner/doom-emacs)'s `map!` macro:

```emacs-lisp
(map!
  :v "|" #'kak-exec-shell-command
  :v "s" (lambda (beg end) (interactive "r") (kak-select beg end nil))
  :v "S" (lambda (beg end) (interactive "r") (kak-select beg end t))
  :v "M-k" (lambda () (interactive) (kak-filter t))
  :v "M-K" (lambda () (interactive) (kak-filter nil))
  :v ". #" #'kak-insert-index)
```
