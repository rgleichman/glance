# Todo

## Todo Now
* Remove parameter from getUniqueName

* Consider adding binding variable names to the lambda icon and match icon. Don't display the name if it is only one character.

## Todo Later
* Add wiki pages discussing: Why a visual language?, Glance design goals, History of Glance, FAQ's, How to contribute, etc..

### Testing todos
* Fix the arrowheads being too big for SyntaxGraph drawings.

### Visual todos
* Draw bounding boxes for lambdas (use dashed lines)

* Use different line styles (e.g. dashed, solid, wavy) in addition to colors

* Consider improving nested apply icons embedded in case/guard icons.

* Let lines connect to ports other than the original source

* Use diagrams to shrink the drawing until icons start overlapping.

* Make an icon font/library with labeled ports. E.g. the apply icon would have text labels "function", "result", "arg 0", "arg 1", etc.

* Try giving lines a black border to make line crossings easier to see.

### Translate todos
* Fix applyComposeScore in Translate.hs not counting expressions that nest via reference. May need to move compose generation to after translate.

* Add proper RecConstr, and RecUpdate support.

* Special case for otherwise.
