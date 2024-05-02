module Modelio where
import Data.Int
import Data.Word (Word16, Word8, Word32)
import GHC.TypeError (ErrorMessage(Text))

data PlyType =
    PChar Int8
    | PUChar Word8
    | PShort Int16
    | PUShort Word16
    | PInt Int32
    | PUInt Word32
    | PFloat Float
    | PDouble Double
    | Header
    deriving (Show)

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
    name :: String,
    count :: Int,
    properties :: [PlyProperty]
}
    deriving (Show)

type PlyModel = [PlyElement]

type PlyHeader = [PlyElement]

parsePropertyHeader :: String -> PlyProperty
parsePropertyHeader prop =
  case propType of
    "list" -> PlyList {name = propListName, contentType = propListType, contentList = []}
    _ -> PlySingle {name = propName, contentType = propType, contentSingle = []}
  where
    splitted = words prop
    propType = splitted !! 1
    propName = splitted !! 2
    propListType = splitted !! 3
    propListName = splitted !! 4

parseElementHeader :: [String] -> PlyElement
parseElementHeader cont = PlyElement {name = elementName, count = elementCount, properties = elementProps}
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
  PlyElement {name = name, count = count, properties = properties}
  cont
    | length properties == 1 =
        parseElementContent
          PlyElement {
            name = name,
            count = count,
            properties = [parsePropertyContent (head properties) (head cont)]
          }
          (tail cont)
    | otherwise =
        parseElementContent
          PlyElement {
            name = name,
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
    pure $ parseFile $ tail $ lines file