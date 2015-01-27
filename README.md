# match-sagittarius

This library implements a pattern matcher macro `match` in R7RS scheme + er-macro-transformer (which is provided by sagittarius).

The pattern matcher is based on compiling patterns into instruction sequences that get merged into a tree as an optimization. The pattern syntax is quasiquoted forms.

# Files

* `trie.sld` and `trie.scm` implements the algorithm to merge a list of sequences into a tree.
* `compile-pattern.sld` and `compile-pattern.scm` translates quasiquotation patterns into match instructions.
* `interpret-tree.sld` and `interpret-tree.scm` is a success/failure continuation based interpreter for match instruction trees. This produces the code for an invocation of the match macro.
* `match.sld` and `match.scm` implements the actual macro using all these utilities.

# History

The gist [here](https://gist.github.com/orchid-hybrid/4901f7dd330be112d52e) contains the original development of the pattern matcher including some important bug fixes.

# Emacs

To get good indentation for this you can add the command to `.emacs`:

```
(put 'match 'scheme-indent-function 1)
```
