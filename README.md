[Link to project wiki](https://github.com/rgleichman/glance/wiki) (for mobile viewers)
# Glance
Glance is a visual syntax for the programming language Haskell. The goal of this project is to increase programmer happiness and productivity by allowing programmers to create and understand programs in new and different ways. Currently, the Glance executable produces a visual representation of your code in the form of an SVG image when given a textual Haskell source file. In the future, I hope to create a visual editor for Haskell. Please scroll down to see some example images.

The current Glance program is an experiment created to answer the question:  
*Is it possible to create a readable and compact graphical representation of Haskell function and value declarations?*

For small to medium functions, in my opinion Glance produces images good enough to indicate that the answer is yes. For large functions, the results right now are inconclusive due to issues with graph layout. Specifically, large programs unnecessarily become very spread out. If you have any ideas about how to improve Glance, please [create an issue](https://github.com/rgleichman/glance/issues/new).

The next steps for the project are to improve graph layout, and then to start work on a visual editor. **If you would like to help** and have any knowledge about graph layout, please make a comment on [this issue](https://github.com/rgleichman/glance/issues/1). All contributions to any part of Glance are very much encouraged.

## Try it
First install Graphviz. For instance, in Ubuntu run:
```
sudo apt install graphviz
```

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
* [Images become very spread out when rendering medium to large functions](https://github.com/rgleichman/glance/issues/1).
* Only a subset of Haskell is rendered.

## To learn more
To learn more about the project after reading this README, please check out the [Glance wiki](../../wiki).

## Getting started
Below is a getting started guide for Glance rendered by Glance itself ([source here](examples/tutorial.hs)). To generate this image run

`stack exec glance-exe -- examples/tutorial.hs examples/tutorial.svg 873 -c`

<img src="https://cdn.rawgit.com/rgleichman/glance/9d9862539dda7b7aceea33616816ac30ccf9b393/examples/tutorial.svg" alt="Introduction to Glance" />

You should now be able to understand Glance's visual syntax. If you would like to see how visual code might be an improvement over textual code, or if you just want to see more examples, please take a look at the Glance wiki page [Advantages of Glance](../../wiki/Advantages-of-Glance).

## Thanks
A large thanks to the creators of [diagrams](http://projects.haskell.org/diagrams/), the main Haskell library used in this project.

Thank you to the [Santa Monica Haskell Users Group](https://www.meetup.com/santa-monica-haskell/) for their support and feedback.

Also thanks to the [/r/haskell](https://www.reddit.com/r/haskell/) subreddit for [reviewing a very early design of the language](https://www.reddit.com/r/haskell/comments/35swgl/review_my_introduction_to_glance_a_new_visual/).
