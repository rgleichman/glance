# Todo

## Todo Now
* Change the PApp icon so that from left to right it looks like: list of args, constructor name, result. If there is only one arg, then the arg port can have an additional angle of (1/2 turn).

* Consider adding binding variable names to the lambda icon and match icon. Don't display the name if it is only one character.

## Todo Later
* Add documentation.

### Testing todos
* Fix the arrowheads being too big for SyntaxGraph drawings.

### Visual todos
* Draw bounding boxes for lambdas (use dashed lines)

* Make an icon font/library with labeled ports. E.g. the apply icon would have text labels "function", "result", "arg 0", "arg 1", etc.

* Don't rotate text and nested icons, give them rectangular bounding boxes in GraphViz. (Perhaps use a typeclass for isRotateAble)

* Give lines a black border to make line crossings easier to see.

* Let lines connect to ports other than the original source

* Use different line styles (e.g. dashed, solid, wavy) in addition to colors

* Use diagrams to shrink the drawing until icons start overlapping.

### Translate todos
* Allow case and guard nodes to embed simple patterns and expressions.

* Fix this test so that the line colors are correct. Consider connecting the t line to the origial rhs (3,4), not the pattern result.
y = let {t@(_,_) = (3,4)} in t + 3

* Fix applyComposeScore in Translate.hs not counting expressions that nest via reference. May need to move compose generation to after translate.

* Fix test case x of {0 -> 1; y -> y}.

* Add proper RecConstr, and RecUpdate support.

* Special case for otherwise.
