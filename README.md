# Glance
A visual Haskell

To build and run:
```
stack build
stack exec glance-exe -- -o images/fact.svg -w 500 examples/fact.hs -
firefox images/fact.svg
```
Below is a mockup of what Glance may look like in the future

<img src="examples/factorial_goal.png" alt="factorial_goal" height="300"/>
</br>

Here is a getting started guide for Glance rendered by Glance ([source here](examples/tutorial.hs)). To generate this image run

`stack exec glance-exe -- -o examples/tutorial.svg -w 873 examples/tutorial.hs c`

Glance is still in development, so for the time being, layout, routing, and icon design all have room for improvement.
<img src="https://cdn.rawgit.com/rgleichman/glance/6e56b2e5d1d2d031eca88b08e1444b8987b242af/examples/tutorial.svg" alt="Introduction to Glance" />
