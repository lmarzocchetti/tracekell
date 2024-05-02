module Modelio where
import Data.Int
import GHC.Natural

data PlyType =
    PChar Int8
    | PShort Int16
    | PInt Int32
    | PUnsigned Natural
    | PFloat Float
    | PDouble Double
    deriving (Show)

data PlyProperty = 
    PlySingle {
        name :: String,
        contentType :: String,
        contentSingle :: PlyType
    } |
    PlyList {
        name :: String,
        contentType :: String,
        contentList :: [PlyType]
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

parseHeader :: [String] -> PlyHeader
parseHeader header = []

parseContent :: [String] -> PlyHeader -> PlyModel
parseContent cont header = []

parseFile :: [String] -> PlyModel
parseFile file = []
    where
        header = parseHeader (takeWhile (/= "end_header") file)
        cont = parseContent $ tail (dropWhile (/= "end_header") file)

loadModel :: String -> IO PlyModel
loadModel filename = do
    file <- readFile filename
    pure $ parseFile $ tail $ lines file