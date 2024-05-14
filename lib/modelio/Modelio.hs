module Modelio where
import Data.Int
import Data.Word (Word16, Word8, Word32)
import GHC.TypeError (ErrorMessage(Text))
import Scene (ShapeData (ShapeData, points, lines, radius), consShapeData, SceneData (SceneData, instances, shapes, materials), consInstanceData, consMaterialData, defaultMaterialDataColorType, MaterialType (MATTE), CameraData (CameraData), consCameraData, consSceneData, defaultSceneDataForCalculateCameraData, addMissingRadiusV, consEnvironmentData, defaultSceneDataForPlyScenes)
import qualified Data.Text as Text
import System.Exit (die)
import qualified Data.Vector as V
import Math (Vec3f(Vec3f), consVec3f, (|>), Vec2f, Vec4f, Vec4i, Vec3i, Vec2i, consVec3i, defaultFrame3f, Add (add), Div (divi), Norm (norm), Sub (sub), Mul (mult), lookAtFrame)
import Raytracing (computeBounds, Bbox3f (min, max))

-- PLY MODEL

data PlyType =
    PChar Int8
    | PUChar Word8
    | PShort Int16
    | PUShort Word16
    | PInt Int
    | PUInt Word32
    | PFloat Float
    | PDouble Double
    | Header
    deriving (Show)

extractPlyFloat (PFloat x) = x
extractPlyInt (PInt x) = x

data PlyProperty =
    PlySingle {
        name :: String,
        contentType :: String,
        contentSingle :: [PlyType]
    } |
    PlyList {
        name :: String,
        contentType :: String,
        contentList :: [[PlyType]]
    }
    deriving (Show)

data PlyElement = PlyElement {
    elemName :: String,
    count :: Int,
    properties :: [PlyProperty]
}
    deriving (Show)

type PlyModel = [PlyElement]

type PlyHeader = [PlyElement]

parsePropertyHeader :: String -> PlyProperty
parsePropertyHeader prop =
  case propType of
    "list" -> PlyList {name = splitted !! 4, contentType = splitted !! 3, contentList = []}
    _ -> PlySingle {name = splitted !! 2, contentType = propType, contentSingle = []}
  where
    splitted = words prop
    propType = splitted !! 1

parseElementHeader :: [String] -> PlyElement
parseElementHeader cont = PlyElement {elemName = elementName, count = elementCount, properties = elementProps}
  where
    splitted = words $ head cont
    elementName = splitted !! 1
    elementCount = read $ splitted !! 2 :: Int
    elementProps = map parsePropertyHeader (tail cont)

parseHeader :: [String] -> PlyHeader
parseHeader [] = []
parseHeader header =
  case head line of
    "ply" -> parseHeader $ tail header
    "comment" -> parseHeader $ tail header
    "format" -> parseHeader $ tail header
    "element" -> parseElementHeader (head header : element) : parseHeader rest
    _ -> parseHeader $ tail header
  where
    line = words $ head header
    element = takeWhile (\x -> head (words x) /= "element") (tail header)
    rest = dropWhile (\x -> head (words x) /= "element") (tail header)

parseSingleProp :: String -> String -> PlyType
parseSingleProp propType cont =
  case propType of
    "char" -> PChar (read cont)
    "uchar" -> PUChar (read cont)
    "short" -> PShort (read cont)
    "ushort" -> PUShort (read cont)
    "int" -> PInt (read cont)
    "uint" -> PUInt (read cont)
    "float" -> PFloat (read cont)
    "double" -> PDouble (read cont)

parsePropertyContent :: PlyProperty -> String -> PlyProperty
parsePropertyContent prop cont =
  case prop of
    PlySingle {name = name, contentType = contentType, contentSingle = contentSingle}
      -> PlySingle {name = name, contentType = contentType, contentSingle = contentSingle ++ [parseSingleProp contentType cont]}
    PlyList {name = propName, contentType = propType, contentList = contentList}
      -> PlyList {name = propName, contentType = propType, contentList = contentList ++ [map (parseSingleProp propType) contListDim]}
    where
      contListDim = tail $ words cont

parseElementContent :: PlyElement -> [String] -> PlyElement
parseElementContent e [] = e
parseElementContent
  PlyElement {elemName = name, count = count, properties = properties}
  cont
    | length properties == 1 =
        parseElementContent
          PlyElement {
            elemName = name,
            count = count,
            properties = [parsePropertyContent (head properties) (head cont)]
          }
          (tail cont)
    | otherwise =
        parseElementContent
          PlyElement {
            elemName = name,
            count = count,
            properties = zipWith parsePropertyContent properties (words (head cont))
          }
          (tail cont)

parseContent :: [String] -> PlyHeader -> PlyModel
parseContent [] _ = []
parseContent cont header = parseElementContent (head header) (take countForElement cont) : parseContent (drop countForElement cont) (tail header)
  where
    countForElement = head $ map count header

parseFile :: [String] -> PlyModel
parseFile file = cont header
    where
        header = parseHeader (takeWhile (/= "end_header") file)
        cont = parseContent $ tail (dropWhile (/= "end_header") file)

loadModel :: String -> IO PlyModel
loadModel filename = do
    file <- readFile filename
    pure $ parseFile $ tail $ Prelude.lines file

-- SHAPE PARTS

vertGet n model =
  model
    |> filter (\x -> elemName x == "vertex")
    |> head
    |> properties
    |> filter (\x -> name x == n)
    |> (\x ->
      if null x
      then []
      else x
        |> head
        |> contentSingle
        |> map extractPlyFloat)

getPositions :: PlyModel -> V.Vector Vec3f
getPositions model = V.fromList zipped
  where
    vertX = vertGet "x" model
    vertY = vertGet "y" model
    vertZ = vertGet "z" model
    zipped = zip3 vertX vertY vertZ
      |> map (\(x, y, z) -> consVec3f x y z)

getNormals :: PlyModel -> V.Vector Vec3f
getNormals model = V.fromList zipped
  where
    normX = vertGet "nx" model
    normY = vertGet "ny" model
    normZ = vertGet "nz" model
    zipped = zip3 normX normY normZ
      |> map (\(x, y, z) -> consVec3f x y z)

getTexcoords :: PlyModel -> V.Vector Vec2f
getTexcoords model = V.fromList []

getColors :: PlyModel -> V.Vector Vec4f
getColors model = V.fromList []

getRadius :: PlyModel -> V.Vector Float
getRadius model = V.fromList []

fromList3toTuple ls = (ls !! 0, ls !! 1, ls !! 2)

getFaces :: PlyModel -> (V.Vector Vec3i, V.Vector Vec4i)
getFaces model = (V.fromList faceGet, V.fromList [])
  where
    faceGet = model
      |> filter (\x -> elemName x == "face")
      |> head
      |> properties
      |> head
      |> contentList
      |> map (map extractPlyInt)
      |> map fromList3toTuple
      |> map (\(x, y, z) -> consVec3i x y z)

getLines :: PlyModel -> V.Vector Vec2i
getLines model = V.fromList []

getPoints :: PlyModel -> V.Vector Int
getPoints model = V.fromList []

-- GENERIC IO

loadShape :: String -> IO ShapeData
loadShape filename =
  case extension of
    "ply" -> do
      ply <- loadModel filename
      let positions = getPositions ply
      let normals = getNormals ply
      let texcoords = getTexcoords ply
      let colors = getColors ply
      let radius = getRadius ply
      let (tr, qd) = getFaces ply
      let lines = getLines ply
      let points = getPoints ply
      let tangents = V.fromList []
      pure $ consShapeData points lines tr qd positions normals texcoords colors radius tangents
    _ -> die "Error: file extension for the model not supported"
  where
    extension = Text.unpack $ last $ Text.splitOn "." (Text.pack filename)

calculateMissingCamera :: SceneData -> CameraData
calculateMissingCamera sceneData = consCameraData frame cameraOrtographic cameraLens cameraFilm cameraAspect focus cameraAperture
  where
    cameraName = "Camera"
    cameraOrtographic = False
    cameraFilm = 0.036
    cameraAspect = 16.0 / 9.0
    cameraAperture = 0
    cameraLens = 0.050
    bbox = computeBounds sceneData
    center = Math.divi (Math.add (Raytracing.min bbox) (Raytracing.max bbox)) (2 :: Float)
    bboxRadius = norm (sub (Raytracing.max bbox) (Raytracing.min bbox)) / (2 :: Float)
    cameraDir = consVec3f 0 0 1
    cameraDist = (bboxRadius * cameraLens / (cameraFilm / cameraAspect)) * 2
    from = add (mult cameraDir cameraDist) center
    to = center
    up = consVec3f 0 1 0
    frame = lookAtFrame from to up
    focus = norm (sub from to) :: Float

-- TODO: Da finire
loadPlyScene :: String  -> IO SceneData
loadPlyScene filename = do
  shapeData <- loadShape filename
  let instanceData = consInstanceData defaultFrame3f 0 0
  let materialData = defaultMaterialDataColorType MATTE (consVec3f 0.8 0.8 0.8)
  let sceneData = defaultSceneDataForCalculateCameraData instanceData shapeData materialData

  let cameraData = calculateMissingCamera sceneData
  let sceneData2 = addMissingRadiusV 0.001 sceneData
  let environmentData = consEnvironmentData defaultFrame3f (consVec3f 1 1 1) (-1)
  pure $ defaultSceneDataForPlyScenes cameraData (instances sceneData2) environmentData (shapes sceneData2) (materials sceneData2)
