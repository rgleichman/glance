# Glance
Glance is a visual syntax for the programming language Haskell. The goal of this project is to increase programmer happiness and productivity by allowing programmers to understand programs in new and different ways. Currently, the Glance executable produces a visual representation of your code in the form of an SVG image when given a textual Haskell source file. In the future, I hope to incorporate Glance into Haskell code editors, and to eventually create a visual editor for Haskell.

## Building
First install Graphviz. For instance, in Ubuntu run:

```sudo apt install graphviz```

Then build and execute glance:
```
stack build
stack exec glance-exe -- -o images/fact.svg -w 500 examples/fact.hs -

```
and display the SVG
```
firefox --new-window images/fact.svg
```

## Issues
Glance is still in development, so for the time being, layout, routing, and icon design all have much room for improvement. Here are some specific issues:
* Text boxes may not display correctly with some SVG viewers.
* Images become very spread out when rendering medium to large functions.
* Only a subset of Haskell is rendered

## Getting started
Below is a getting started guide for Glance rendered by Glance itself ([source here](examples/tutorial.hs)). To generate this image run

`stack exec glance-exe -- -o examples/tutorial.svg -w 873 examples/tutorial.hs c`

Also, the [Glance wiki](../../wiki) has a brief introduction to the code architecture.

<img src="https://cdn.rawgit.com/rgleichman/glance/958611dde3827d650c56814f8c491ecf34f954da/examples/tutorial.svg" alt="Introduction to Glance" />
