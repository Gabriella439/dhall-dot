# `dhall-dot`

This package provides [Dhall](https://dhall-lang.org/) support for the
[DOT language](https://graphviz.org/doc/info/lang.html) used by the
[Graphviz](https://graphviz.org/about/) package.  This support consists of:

* [GraphViz types](./types.dhall) encoded in Dhall
* A [`render`](./render.dhall) function to render a `Graph` as `Text`

See the [`package.dhall`](./package.dhall) file for example usage.
