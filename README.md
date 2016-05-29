# Glance
Glance is a visual syntax for the programming language Haskell. The goal of this project is to increase programmer happiness and productivity by allowing programmers to understand programs in new and different ways. Currently, the Glance executable will produce a visual representation of your code in the form of an SVG image when given a textuall Haskell source file. In the future, I hope to incorporate Glance into Haskell code editors, and to eventually create a visual editor for Haskell.

To build and run:
```
stack build
stack exec glance-exe -- -o images/fact.svg -w 500 examples/fact.hs -
firefox images/fact.svg
```
Here is a getting started guide for Glance rendered by Glance itself ([source here](examples/tutorial.hs)). To generate this image run

`stack exec glance-exe -- -o examples/tutorial.svg -w 873 examples/tutorial.hs c`

Glance is still in development, so for the time being, layout, routing, and icon design all have much room for improvement.
<img src="https://cdn.rawgit.com/rgleichman/glance/6e56b2e5d1d2d031eca88b08e1444b8987b242af/examples/tutorial.svg" alt="Introduction to Glance" />
