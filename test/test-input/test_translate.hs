initialIdState :: IDState
initialIdState = IDState 0

getId :: State IDState Int
getId = state incrementer where
  incrementer (IDState x) = (x, IDState checkedIncrement) where
    xPlusOne = x + 1
    checkedIncrement = if xPlusOne > x
      then xPlusOne
      else error "getId: the ID state has overflowed."

mapFst :: Functor f => (a -> b) -> f (a, c) -> f (b, c)
mapFst f = fmap (first f)

toNames :: (IsName a) => [(a, b)] -> [(Name, b)]
toNames = mapFst toName

noEnds :: (EdgeEnd, EdgeEnd)
noEnds = (EndNone, EndNone)

nameAndPort :: IsName a => a -> Int -> NameAndPort
nameAndPort n p = NameAndPort (toName n) (Just p)

justName :: IsName a => a -> NameAndPort
justName n = NameAndPort (toName n) Nothing

-- Edge constructors --
portToPort :: (IsName a, IsName b) => a -> Int -> b -> Int -> Edge
portToPort a b c d = Edge (nameAndPort a b, nameAndPort c d) noEnds

iconToPort :: (IsName a, IsName b) => a -> b -> Int -> Edge
iconToPort a   c d = Edge (justName a, nameAndPort c d) noEnds

iconToIcon :: (IsName a, IsName b) => a -> b -> Edge
iconToIcon a   c   = Edge (justName a, justName c) noEnds


-- If there are gaps between the arrow and the icon, try switching the first two arguments
-- with the last two arguments
iconToIconEnds :: (IsName a, IsName b) => a -> EdgeEnd -> b -> EdgeEnd -> Edge
iconToIconEnds a b c d = Edge (justName a, justName c) (b, d)

-- iconHeadToPort :: (IsName a, IsName b) => a -> EdgeEnd -> b -> Int -> Edge
-- iconHeadToPort a endHead c d = Edge (justName a, nameAndPort c d) (EndNone, endHead)

iconTailToPort :: (IsName a, IsName b) => a -> EdgeEnd -> b -> Int -> Edge
iconTailToPort a endTail c d = Edge (justName a, nameAndPort c d) (endTail, EndNone)

fromMaybeError :: String -> Maybe a -> a
fromMaybeError s = fromMaybe (error s)

-- CONSTANT
scaleFactor :: (Fractional a) => a
--scaleFactor = 0.013
scaleFactor = 0.05
--scaleFactor = 0.04

drawingToGraphvizScaleFactor :: Double
drawingToGraphvizScaleFactor = 0.3

-- CONVERTING Edges AND Icons TO DIAGRAMS --

-- | Convert a map of names and icons, to a list of names and diagrams.
-- The first argument is the subdiagram map used for the inside of lambdaIcons
-- The second argument is the map of icons that should be converted to diagrams.
--makeNamedMap :: IsName name => [(Name, Diagram B)] -> [(name, Icon)] -> [(name, Diagram B)]
makeNamedMap ::
   (RealFloat n, Typeable n, Renderable (Path V2 n) b,
      Renderable (Diagrams.TwoD.Text.Text n) b, IsName nm) =>
     [(Name, QDiagram b V2 n Any)]-> [(nm, Icon)] -> [(nm, QDiagram b V2 n Any)]
makeNamedMap subDiagramMap =
  map (\(name, icon) -> (name, nameDiagram name (iconToDiagram icon subDiagramMap)))

-- | Make an inductive Graph from a list of node names, and a list of Connections.
edgesToGraph :: [Name] -> [(NameAndPort, NameAndPort)] -> Gr Name ()
edgesToGraph iconNames edges = mkGraph iconNames simpleEdges
  where
    simpleEdges = map (\(NameAndPort a _, NameAndPort c _) -> (a, c, ())) edges

-- | Custom arrow tail for the arg1 result circle.
-- The ArrowHT type does not seem to be documented.
arg1ResT :: (RealFloat n) => ArrowHT n
arg1ResT len _ = (alignR $ circle (len / 2), mempty)

-- | Arrow head version of arg1ResT
arg1ResH :: (RealFloat n) => ArrowHT n
arg1ResH len _ = (alignL $ circle (len / 2), mempty)

getArrowOpts :: (RealFloat n, Typeable n) => (EdgeEnd, EdgeEnd) -> ArrowOpts n
getArrowOpts (t, h) = arrowOptions
  where
    ap1ArgTexture = solid (backgroundC colorScheme)
    ap1ArgStyle = lwG defaultLineWidth . lc (apply1C colorScheme)
    ap1ResultTexture = solid (apply1C colorScheme)

    lookupTail EndNone = id
    lookupTail EndAp1Arg = (arrowTail .~ dart')

    lookupHead EndNone = id
    lookupHead EndAp1Arg = (arrowHead .~ dart)
      . (headTexture .~ ap1ArgTexture) . (headStyle %~ ap1ArgStyle)
    lookupHead EndAp1Result = (arrowHead .~ arg1ResH) . (headTexture .~ ap1ResultTexture)

    arrowOptions =
      with & arrowHead .~ noHead
      & arrowTail .~ noTail
      & lengths .~ global 0.75
      & (shaftStyle %~ ((lwG defaultLineWidth) . lc (lineC colorScheme)))
      & lookupTail t & lookupHead h

connectMaybePorts ::
   (RealFloat n, Typeable n, Renderable (Path V2 n) b) =>
     Edge -> QDiagram b V2 n Any -> QDiagram b V2 n Any
connectMaybePorts (Edge opts ends (NameAndPort icon0 (Just port0), NameAndPort icon1 (Just port1))) =
  connect'
  (getArrowOpts ends opts)
  (icon0 .> port0)
  (icon1 .> port1)
connectMaybePorts (Edge opts ends (NameAndPort icon0 Nothing, NameAndPort icon1 (Just port1))) =
  connectOutside' (getArrowOpts ends opts) icon0 (icon1 .> port1)
connectMaybePorts (Edge opts ends (NameAndPort icon0 (Just port0), NameAndPort icon1 Nothing)) =
  connectOutside' (getArrowOpts ends opts) (icon0 .> port0) icon1
connectMaybePorts (Edge opts ends (NameAndPort icon0 Nothing, NameAndPort icon1 Nothing)) =
  connectOutside' (getArrowOpts ends opts) icon0 icon1

makeConnections ::
   (RealFloat n, Typeable n, Renderable (Path V2 n) b) =>
     [Edge] -> QDiagram b V2 n Any -> QDiagram b V2 n Any
makeConnections edges = applyAll connections
  where
    connections = map connectMaybePorts edges

totalLenghtOfLines :: Double -> P2 Double -> [(P2 Double, P2 Double)] -> Double
totalLenghtOfLines angle myLocation edges = sum $ map edgeDist edges
  where
    edgeDist :: (P2 Double, P2 Double) -> Double
    edgeDist (relativePortLocation, iconLocation) =
      -- The squaring here is arbitrary. Distance should be replaced with angle diff.
      (norm (absPortVec ^-^ iconLocationVec)) ** 2
      where
        P relPortVec = relativePortLocation
        P iconLocationVec = iconLocation
        P myLocVec = myLocation
        absPortVec = myLocVec ^+^ (rotateBy angle relPortVec)

angleWithMinDist :: P2 Double -> [(P2 Double, P2 Double)] -> (Double, Double)
angleWithMinDist myLocation edges =
  minimumBy (compare `on` snd) (map totalLength [0,(1/40)..1])
  where
    totalLength angle = (angle, totalLenghtOfLines angle myLocation edges)

getFromMapAndScale :: (Fractional a, Functor f, Ord k) => Map.Map k (f a) -> k -> f a
getFromMapAndScale posMap name = scaleFactor *^ (posMap Map.! name)

-- | Returns [(myport, other node, maybe other node's port)]
connectedPorts :: [Connection] -> Name -> [(Int, Name, Maybe Int)]
connectedPorts edges name = map edgeToPort (filter nameInEdge edges)
  where
    nameInEdge (NameAndPort name1 port1, NameAndPort name2 port2) = (name == name1 && isJust port1) || (name == name2 && isJust port2)
    edgeToPort (NameAndPort name1 port1, NameAndPort name2 port2) =
      if name == name1
        then (fromMaybeError "connectedPorts: port is Nothing" port1, name2, port2)
        else (fromMaybeError "connectedPorts: port is Nothing" port2, name1, port1)

placeNodes ::
   (Monoid m, Semigroup m) =>
     LayoutResult a
     -> [(Name, QDiagram b V2 Double m)]
     -> [Connection]
     -> QDiagram b V2 Double m
placeNodes layoutResult nameDiagramMap edges = mconcat placedNodes
  where
    (positionMap, _) = getGraph layoutResult
    rotatedNameDiagramMap = rotateNodes positionMap nameDiagramMap edges
    placedNodes = map placeNode rotatedNameDiagramMap
    --placedNodes = map placeNode nameDiagramMap
    -- todo: Not sure if the diagrams should already be centered at this point.
    placeNode (name, diagram) = place (centerXY diagram) (scaleFactor *^ (positionMap Map.! name))

(IconGraph icons1 edges1 subDrawings1 sinks1 sources1) <> (IconGraph icons2 edges2 subDrawings2 sinks2 sources2) =
  IconGraph (icons1 <> icons2) (edges1 <> edges2) (subDrawings1 <> subDrawings2) (sinks1 <> sinks2) (sources1 <> sources2)

getUniqueName base = fmap ((base ++). show) getId

edgesForRefPortList :: Bool -> [(Reference, NameAndPort)] -> IconGraph
edgesForRefPortList inPattern portExpPairs = mconcat $ fmap mkGraph portExpPairs where
  edgeOpts = if inPattern then [EdgeInPattern] else []
  mkGraph (ref, port) = case ref of
    Left str -> if inPattern
      then IconGraph mempty mempty mempty mempty [(str, Right port)]
      else IconGraph mempty mempty mempty [(str, port)] mempty
    Right resultPort -> IconGraph mempty [Edge edgeOpts noEnds (resultPort, port)] mempty mempty mempty

qualifyNameAndPort :: String -> NameAndPort -> NameAndPort
qualifyNameAndPort s (NameAndPort n p) = NameAndPort (s DIA..> n) p

iconGraphToDrawing :: IconGraph -> Drawing
iconGraphToDrawing (IconGraph icons edges subDrawings _ _) = Drawing icons edges subDrawings

makeApplyGraph :: Bool -> DIA.Name -> (IconGraph, Reference) -> [(IconGraph, Reference)] -> Int -> (IconGraph, NameAndPort)
makeApplyGraph inPattern applyIconName funVal argVals numArgs = (newGraph <> combinedGraph, nameAndPort applyIconName 1)
  where
    argumentPorts = map (nameAndPort applyIconName) [2,3..]
    functionPort = nameAndPort applyIconName 0
    combinedGraph = combineExpressions inPattern $ zip (funVal:argVals) (functionPort:argumentPorts)
    icons = [(applyIconName, Apply0NIcon numArgs)]
    newGraph = iconGraphFromIcons icons

namesInPattern :: GraphAndRef -> [String]
namesInPattern (_, Left str) = [str]
namesInPattern (IconGraph _ _ _ _ bindings, Right _) = fmap fst bindings

-- | Recursivly find the matching reference in a list of bindings.
-- TODO: Might want to present some indication if there is a reference cycle.
lookupReference :: [(String, Reference)] -> Reference -> Reference
lookupReference _ ref@(Right _) = ref
lookupReference bindings ref@(Left originalS) = lookupHelper ref where
  lookupHelper newRef@(Right _) = newRef
  lookupHelper newRef@(Left s)= case lookup s bindings of
    Just r -> failIfCycle r $ lookupHelper r
    Nothing -> newRef
    where
      failIfCycle r@(Left newStr) res = if newStr == originalS then r else res
      failIfCycle _ res = res

deleteBindings :: IconGraph -> IconGraph
deleteBindings (IconGraph a b c d _) = IconGraph a b c d mempty

makeEdgesCore :: [Sink] -> [(String, Reference)] -> ([Sink], [Edge])
makeEdgesCore sinks bindings = partitionEithers $ fmap renameOrMakeEdge sinks
  where
    renameOrMakeEdge :: (String, NameAndPort) -> Either (String, NameAndPort) Edge
    renameOrMakeEdge orig@(s, destPort) = case lookup s bindings of
      Just ref -> case lookupReference bindings ref of
        (Right sourcePort) -> Right $ makeSimpleEdge (sourcePort, destPort)
        (Left newStr) -> Left (newStr, destPort)
      Nothing -> Left orig

makeEdges :: IconGraph -> IconGraph
makeEdges (IconGraph icons edges c sinks bindings) = newGraph where
  (newSinks, newEdges) = makeEdgesCore sinks bindings
  newGraph = IconGraph icons (newEdges <> edges) c newSinks bindings

nTupleString :: Int -> String
nTupleString n = '(' : replicate (n -1) ',' ++ ")"

nListString :: Int -> String
-- TODO: Use something better than [_]
nListString 1 = "[_]"
nListString n = '[' : replicate (n -1) ',' ++ "]"


rotateNodes ::
   Semigroup m =>
     Map.Map Name (Point V2 Double)
     -> [(Name, QDiagram b V2 Double m)]
     -> [Connection]
     -> [(Name, QDiagram b V2 Double m)]
rotateNodes positionMap nameDiagramMap edges = map rotateDiagram nameDiagramMap
  where
    rotateDiagram (name, originalDia) = (name, transformedDia)
      where
        transformedDia = if flippedDist < unflippedDist
          then rotateBy flippedAngle flippedDia
          else rotateBy unflippedAngle originalDia
        flippedDia = reflectX originalDia
        (unflippedAngle, unflippedDist) = minAngleForDia originalDia
        (flippedAngle, flippedDist) = minAngleForDia flippedDia
        --minAngleForDia :: QDiagram b V2 Double m -> (Double, Double)
        minAngleForDia dia = minAngle where
        --ports = Debug.Trace.trace ((show $ names dia) ++ "\n") $ names dia
          ports = names dia
          namesOfPortsWithLines = connectedPorts edges name

          iconInMap :: (Int, Name, Maybe Int) -> Bool
          iconInMap (_, otherIconName, _) = Map.member otherIconName positionMap

          getPortPoint :: Int -> P2 Double
          getPortPoint x =
            -- TODO remove partial function head.
            head $ fromMaybeError ("port not found. Port: " ++ show name ++ ".> " ++ show x ++ ". Valid ports: " ++ show ports) (lookup (name .> x) ports)

          makePortEdge :: (Int, Name, Maybe Int) -> (P2 Double, P2 Double)
          makePortEdge (portInt, otherIconName, _) =
            (getPortPoint portInt, getFromMapAndScale positionMap otherIconName)

          portEdges = map makePortEdge (filter iconInMap namesOfPortsWithLines)

          minAngle = angleWithMinDist (getFromMapAndScale positionMap name) portEdges
