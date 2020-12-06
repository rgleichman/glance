# Todo

## GUI Todo Now

## Non-GUI Todo Now
* Redesign case icon to avoid non-locality.
* Move @ pattern circles so that they are on the same side as the variables.
* Add command line flags for color style, embedding, and whether to draw arrowheads.
* Add wiki pages discussing: Why a visual language?, History of Glance, How to contribute, Code guide [code style, ...], etc..

## GUI Todo Later
* Connect nodes with lines
* Display the undo state in the app [Bug #14](https://github.com/rgleichman/glance/issues/14)
* Scale based on mouse position
* Extract out GUI interface code (GTK and GDK) into a module.
* Refactor to make code extensible (e.g. records of functions instead of enums)
* Click a button that shows an image of the mouse and keyboard controls in a new window.

### Testing todos
* Fix the arrowheads being too big for SyntaxGraph drawings.

### Visual todos
* Use different line styles (e.g. dashed, solid, wavy) in addition to colors

* Consider improving nested apply icons embedded in case/guard icons.

* Let lines connect to ports other than the original source

* Use diagrams to shrink the drawing until icons start overlapping.

* Make an icon font/library with labeled ports. E.g. the apply icon would have text labels "function", "result", "arg 0", "arg 1", etc.

### Translate todos
* Fix applyComposeScore in Translate.hs not counting expressions that nest via reference. May need to move compose generation to after translate.

* Add proper RecConstr, and RecUpdate support.

* Special case for otherwise.

### Command line todos
* Tab completion
