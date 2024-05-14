module Raytracing where

import qualified Data.Vector as V

import Math (Vec3f (Vec3f), Minable (min), Maxable (max), consVec3f, Frame3f, Vec3f (x), Vec3f (y), Vec3f (z), frame3fGetX, Add (add), frame3fGetO, frame3fGetY, frame3fGetZ, mult, vec3iGetX, vec3iGetY, vec3iGetZ)
import Scene
    ( SceneData(shapes, instances, SceneData),
      ShapeData(positions),
      InstanceData(shape, instanceFrame),
      triangles )
import Data.Int (Int16, Int8, Int32)
import qualified Scene as SceneData

floatMax = 3.4028237 * (10**38) :: Float
floatMin = 1.175494 * (10**(-38)) :: Float

data Bbox3f = Bbox3f {
  min :: Vec3f,
  max :: Vec3f
}
  deriving (Show)

consBbox3f = Bbox3f

invalidBbox3f = consBbox3f (consVec3f floatMax floatMax floatMax) (consVec3f floatMin floatMin floatMin)

mergeBVf :: Bbox3f -> Vec3f -> Bbox3f
mergeBVf (Bbox3f {min = bmin, max = bmax}) vec = Bbox3f {min = Math.min bmin vec, max = Math.max bmax vec}

mergeBBf :: Bbox3f -> Bbox3f -> Bbox3f
mergeBBf (Bbox3f {min = bmin, max = bmax}) (Bbox3f {min = bmin', max = bmax'})
  = Bbox3f {min = Math.min bmin bmin', max = Math.max bmax bmax'}

transformPoint :: Frame3f -> Vec3f -> Vec3f
transformPoint frame vec = foldl Math.add (consVec3f 0 0 0) [frame3fGetO frame, Math.mult (frame3fGetX frame) (x vec), Math.mult (frame3fGetY frame) (y vec), Math.mult (frame3fGetZ frame) (z vec)]

-- TODO: Possible bug in the default xformed value
transformBbox :: Frame3f -> Bbox3f -> Bbox3f
transformBbox frame (Bbox3f {min = min, max = max}) = foldl mergeBVf xformed corners
  where
    xformed = invalidBbox3f
    corners_p = [
        consVec3f (x min) (y min) (z min),
        consVec3f (x min) (y min) (z max),
        consVec3f (x min) (y max) (z min),
        consVec3f (x min) (y max) (z max),
        consVec3f (x max) (y min) (z min),
        consVec3f (x max) (y min) (z max),
        consVec3f (x max) (y max) (z min),
        consVec3f (x max) (y max) (z max)
      ]
    corners = map (transformPoint frame) corners_p

computeBoundsInstance :: V.Vector InstanceData -> V.Vector Bbox3f -> Bbox3f -> Bbox3f
computeBoundsInstance instanceData shapeBbox acc =
  if V.length instanceData == 0
  then acc
  else computeBoundsInstance (V.tail instanceData) shapeBbox newBbox
    where
      instance' = V.head instanceData
      instShape = shape instance'
      instFrame = instanceFrame instance'
      sbvh = shapeBbox V.! instShape
      newBbox = mergeBBf acc (transformBbox instFrame sbvh)

computeBounds :: SceneData -> Bbox3f
computeBounds sceneData = computeBoundsInstance (instances sceneData) shapeBbox invalidBbox3f
  where
    shapeBbox = V.map
      (V.foldl
        mergeBVf
        invalidBbox3f . positions)
      (SceneData.shapes sceneData)

-- BVH

data BvhNode = BvhNode {
  bbox :: Bbox3f,
  start :: Int32,
  num :: Int16,
  axis :: Int8,
  internal :: Bool
}
  deriving (Show)

consBvhNode = BvhNode

defaultBvhNode = consBvhNode invalidBbox3f 0 0 0 False

data BvhTree = BvhTree {
  nodes :: V.Vector BvhNode,
  primitives :: V.Vector Int
}
  deriving (Show)

emptyBvhTree = BvhTree {nodes = V.empty, primitives = V.empty}

type ShapeBvh = BvhTree

data SceneBvh = SceneBvh {
  bvh :: BvhTree,
  shapes :: V.Vector ShapeBvh
}
  deriving (Show)

triangleBound :: Vec3f -> Vec3f -> Vec3f -> Bbox3f
triangleBound p0 p1 p2 = consBbox3f (Math.min p0 (Math.min p1 p2)) (Math.max p0 (Math.max p1 p2))

-- TODO: Make this generalize on points, lines, and quads. Here only implemented triangles
makeShapeBvh :: ShapeData -> ShapeBvh
makeShapeBvh shapeData = emptyBvhTree
  where
    poss = positions shapeData
    bboxes = V.map (\j -> triangleBound (poss V.! vec3iGetX j) (poss V.! vec3iGetY j) (poss V.! vec3iGetZ j)) (triangles shapeData)

-- TODO: Make this parallel
makeSceneBVH :: SceneData -> SceneBvh
makeSceneBVH sceneData = SceneBvh {bvh = emptyBvhTree, shapes = V.empty}
