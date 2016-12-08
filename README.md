# Glance
Glance is a visual syntax for the programming language Haskell. The goal of this project is to increase programmer happiness and productivity by allowing programmers to understand programs in new and different ways. Currently, the Glance executable will produce a visual representation of your code in the form of an SVG image when given a textuall Haskell source file. In the future, I hope to incorporate Glance into Haskell code editors, and to eventually create a visual editor for Haskell.

To build and run:

First install Graphviz. For instance, in Ubuntu:

```sudo apt install graphviz```

Then build and execute glance:
```
stack build
stack exec glance-exe -- -o images/fact.svg -w 500 examples/fact.hs -
firefox images/fact.svg
```
The [Glance wiki](../../wiki) has a brief introduction to the code architecture.

Below is a getting started guide for Glance rendered by Glance itself ([source here](examples/tutorial.hs)). To generate this image run

`stack exec glance-exe -- -o examples/tutorial.svg -w 873 examples/tutorial.hs c`

Glance is still in development, so for the time being, layout, routing, and icon design all have much room for improvement. 
Note: Text boxes may not display correctly on Microsoft browsers.
<img src="https://cdn.rawgit.com/rgleichman/glance/97cff80df023df0f1cf6e41962811832de110e36/examples/tutorial.svg" alt="Introduction to Glance" />
