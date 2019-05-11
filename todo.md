# Todo

## Todo Now
* Let lambda icons embed more icons.
* Refactor out embedded nodes from SyntaxNodes into a common data structure.
* Redesign case to avoid non-locality.
* Add command line flags for color style, embedding, and whether to draw arrowheads.
* Add wiki pages discussing: Why a visual language?, History of Glance, How to contribute, Code guide [code style, ...], etc..

## Todo Later

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
