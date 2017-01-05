# Glance
Glance is a visual syntax for the programming language Haskell. The goal of this project is to increase programmer happiness and productivity by allowing programmers to create and understand programs in new and different ways. Currently, the Glance executable produces a visual representation of your code in the form of an SVG image when given a textual Haskell source file. In the future, I hope to create a visual editor for Haskell. Please scroll down to see some example images.

The current Glance program is an experiment created to answer the question:  
*Is it possible to create a readable and compact graphical representation of Haskell function and value declarations?*

For small to medium functions, in my opinion Glance produces images good enough to indicate that the answer is yes. For large functions, the results right now are inconclusive due to issues with graph layout. Specifically, large programs unnecessarily become very spread out. If you have any ideas about how to improve Glance, please [create an issue](https://github.com/rgleichman/glance/issues/new).

The next steps for the project are to improve graph layout, and then to start work on a visual editor.

## Try it
First install Graphviz. For instance, in Ubuntu run:

```sudo apt install graphviz```

Then build and execute glance:
```
stack build
stack exec glance-exe -- examples/fact.hs images/fact.svg 500
```
To see the command line options run
```
stack exec glance-exe -- --help
```

Now display the SVG image in a web browser
```
firefox --new-window images/fact.svg
```
You should now see in your browser window a visual representation of the factorial function. Next, you will probably want to read the Getting Started guide below to help understand the images Glance generates.

## Issues
Glance is still in development, so for the time being, graph layout, line routing, and icon design all have much room for improvement. Here are some specific issues:
* Text may be misaligned with some SVG viewers.
* Images become very spread out when rendering medium to large functions.
* Only a subset of Haskell is rendered.

## Getting started
Below is a getting started guide for Glance rendered by Glance itself ([source here](examples/tutorial.hs)). To generate this image run

`stack exec glance-exe -- examples/tutorial.hs examples/tutorial.svg 873 -c`

Also, the [Glance wiki](../../wiki) has a brief introduction to the code architecture.

<img src="https://cdn.rawgit.com/rgleichman/glance/e8290791da1df12f3d77e45851b7a16b704357db/examples/tutorial.svg" alt="Introduction to Glance" />
