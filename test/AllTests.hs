import Diagrams.Prelude hiding ((#), (&))
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Backend.SVG (renderSVG)

import Rendering(renderDrawing)
import Translate(translateString)
import Icons(textBox, colorScheme, ColorStyle(..))

nestedTests = [
  "y = f x",
  "y = f (g x)",
  "y = let x = 1 in f x",
  "y = let x = 1 in f (g x)",
  "y = f []",
  "y = f [1]",
  "y = f [1,2]",
  "y = f [g 3, h 5]"
  ]

dollarTests = [
  "y = f $ g 3",
  " y = f 1 $ g 2 "
  ]

specialTests = [
  "lookupTail EndAp1Arg = (arrowTail .~ dart')",
  "y = x .~ y",
  "initialIdState = IDState 0",
  "y = f x",
  "yyy = fff xxx",
  "yyyyy = fffff xxxxx"
  ]

negateTests = [
  "y = -1",
  "y = -1/2",
  "y = -x"
  ]

doTests = [
  "y = do {x1}",
  "y = do {x1; x2}",
  "y = do {x1; x2; x3}",
  "y = do {x1 <- m1; x2}",
  "y = do {(x1, x2) <- m1; x1 + x2}",
  "y = do {x1 <- m1; x2 <- f x1; g x2}",
  "y = do {let {x = 1}; x2 <- x; f x2}"
  ]

enumTests = [
  "y = [1..]",
  "y = [1,2..]",
  "y = [0..10]",
  "y = [0,1..10]"
  ]

tupleTests = [
  "y = ()",
  "(x, y) = (1,2)",
  "(x, y, z) = (1,2,3)"
  ]

listTests = [
  "y = []",
  "y = [1]",
  "y = [1,2]",
  "y = [1,2,3]",
  "[x] = 1",
  "[x, y] = 2",
  "[x, y, z] = 3"
  -- TODO: Add this test "(x:y) = 3"
  ]

caseTests = [
  "y = case x of {0 -> 1; 2 -> 3}",
  "y = case f x of {0 -> 1; 2 -> 3}",
  "y = case x of {Foo a -> a}",
  "y = case x of {Foo a -> f a; Bar a -> f a}",
  "y = case x of {F x -> x; G x -> x}",
  "y = case x of {F -> 0; G -> 1}",
  "z = case x of {0 -> 1; y -> y}"
  ]

guardTests = [
  "y x\n\
  \  | x == 0 = 1",
  "y x\n\
  \  | x == 0 = 1\n\
  \  | otherwise = 2"
  ]

patternTests = [
  "Foo _ x = 3",
  "y (F x) = x",
  "y = (\\(F x) -> x)",
  "y = let {g = 3; F x y = h g} in x y",
  "y = let {F x y = 3} in x y",
  "y = let {g = 3; F x y = g} in x y",
  "y = let F x y = g in x y",
  "F x = g x",
  "Foo (Bar x) (Baz y) = f 1 2 x y",
  "Foo x y = f 1 y x",
  "t@(x,y) = (x,y)",
  "y = let {t@(_,_) = (3,4)} in t + 3",
  "y = let {(x, y) = (1,2)} in x + y",
  -- TODO: Fix so that lines between patterns are Pattern Color.
  "y = let {(x, y) = (1,2); (z, w) = x; (m, g) = y} in foo x y z w m g",
  "(x:y) = 2"
  ]

lambdaTests = [
  "y = (\\x -> (\\x -> (\\x -> x) x) x)",
  "y = (\\x -> (\\x -> (\\x -> x)))",
  "y = (\\y -> y)",
  "y = (\\x1 -> (\\x2 -> (\\x3 -> x1 x2 x3)))",
  "y x = (\\z -> x)",
  "y = (\\x -> (\\z -> x))",
  "y x = x",
  "y x = y x",
  "y x = g y y",
  "y f x = f x",
  "y x = x y",
  "y x1 x2 = f x1 x3 x2",
  "y x1 x2 = f x1 x2",
  "y x = f x1 x2",
  "{y 0 = 1; y 1= 0}",
  "y (-1) = 2",
  "y 1 = 0",
  "{y (F x) = x; y (G x) = x}",
  "{y (F x) z = x z; y (G x) z = z x}",
  "y x = z 3 where z = f x y",
  "y x = z where z = f x y"
  ]

letTests = [
  "y = let {z = (\\x -> y x)} in z",
  "y = let {z x = y x} in z ",
  "y = x where x = f 3 y",
  "y x1 = let {x2 = x1; x3 = x2; x4 = f x3} in x4",
  "y x1 = let x2 = f x1 in x2 x1",
  "y x = let x = 3 in x",
  "y = let {a= 1; x = let {a = 27; x = f a 2} in x} in x",
  "y = let {a = b; b = a; d = f a} in d",
  "y = let {a = b; b = a} in a",
  "y = let x = x in x",
  "y = let {fibs = cons 0 (cons 1 (zipWith (+) fibs (tail fibs)))} in fibs",
  "fibs = cons 0 (cons 1 (zipWith (+) fibs (tail fibs)))",
  "y = let x = f x in x",
  "y = f y",
  "y = let {a = f b; b = g a} in b",
  "y = let {a = 48; b = a + 3} in b",
  "y = let {b = a; a = 84} in f b",
  "y = let {x = 1} in f x",
  "y = let z = 2 in z",
  "y = let {z = 3; z2 = z} in z2",
  "y x = let z = x in z"
  ]

operatorTests = [
  "y = 1 + 2",
  "y = map (1 ++) 3",
  "y = map (++ 1) 3"
  ]

otherTests = [
  "y = f 1 'c' 2.3 \"foobar\"",
  "fact x = if (x == 0) then 1 else (fact x (x - 1))",
  "fact x = if ((==) 0 x) then 1 else (fact x ((-) x 1))",
  "y x = if x then (if z then q else x) else w",
  "y x1 x2 x3 = if f x1 then g x2 else h x3",
  "y x1 x2 x3 = if x1 then x2 else x3",
  "y = if b then x else n",
  "y2 = f x1 x2 x3 x4",
  "y = x",
  "y = f x",
  "y = f (g x)",
  "y = f (g x1 x2) x3",
  "y = (f x1 x2) (g x1 x2)",
  "y = Foo.bar"
  ]

testDecls = mconcat [
  dollarTests
  ,nestedTests
  ,negateTests
  ,doTests
  ,enumTests
  ,caseTests
  ,lambdaTests
  ,guardTests
  ,patternTests
  ,specialTests
  ,tupleTests
  ,listTests
  ,letTests
  ,operatorTests
  ,otherTests
  ]


translateStringToDrawing :: String -> IO (Diagram B)
translateStringToDrawing s = do
  let
    (drawing, decl) = translateString s
  print decl
  putStr "\n"
  print drawing
  putStr "\n\n"
  renderDrawing drawing

renderAllTests :: IO ()
renderAllTests = do
  drawings <- traverse translateStringToDrawing testDecls
  let
    textDrawings = fmap (\t -> alignL $ textBox t False 0) testDecls
    vCattedDrawings = vsep 1 $ zipWith (===) (fmap alignL drawings) textDrawings
  --mainWith (bgFrame 1 (backgroundC colorScheme) vCattedDrawings :: Diagram B)
  renderSVG "test/test-output/all-tests.svg" (mkWidth 700) (bgFrame 1 (backgroundC colorScheme) vCattedDrawings :: Diagram B)

main :: IO ()
main = renderAllTests
