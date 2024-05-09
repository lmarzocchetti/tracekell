module Scene where

import qualified Data.Vector as V

import Math
import Data.Vector (Vector)
import Prelude hiding (lines)
import Image (Image, consImage)

data ShapeData = ShapeData {
  points :: Vector Int,
  lines :: Vector Vec2i,
  triangles :: Vector Vec3i,
  quads :: Vector Vec4i,

  positions :: Vector Vec3f,
  normals :: Vector Vec3f,
  texcoords :: Vector Vec2f,
  colors :: Vector Vec4f,
  radius :: Vector Float,
  tangents :: Vector Vec4f
}
  deriving (Show)

consShapeData = ShapeData

data CameraData = CameraData {
  cameraFrame :: Frame3f,
  orthographic :: Bool,
  lens :: Float,
  film :: Float,
  aspect :: Float,
  focus :: Float,
  aperture :: Float
}
  deriving (Show)

consCameraData :: Frame3f -> Bool -> Float -> Float -> Float -> Float -> Float -> CameraData
consCameraData = CameraData

defaultCameraData = CameraData {
  cameraFrame = defaultFrame3f,
  orthographic = False, 
  lens = 0.050, 
  film = 0.036, 
  aspect = 1.500, 
  focus = 10000, 
  aperture = 0
}

data InstanceData = InstanceData {
  instanceFrame :: Frame3f,
  shape :: Int,
  material :: Int
}
  deriving (Show)

consInstanceData = InstanceData
defaultInstanceData = consInstanceData defaultFrame3f (-1) (-1)

data EnvironmentData = EnvironmentData {
  envFrame :: Frame3f,
  envEmission :: Vec3f,
  envEmissionTex :: Int
}
  deriving (Show)

consEnvironmentData = EnvironmentData
defaultEnvironmentData = consEnvironmentData defaultFrame3f (consVec3f 0 0 0) (-1)

data TextureData = TextureData {
  pixelf :: Image Vec4f,
  pixelb :: Image Vec4b,
  nearest :: Bool,
  clamp :: Bool
}
  deriving (Show)

consTextureData = TextureData
defaultTextureData = 
  consTextureData 
    (consImage (consVec2i 0 0) (V.fromList []))
    (consImage (consVec2i 0 0) (V.fromList []))
    False
    False

data MaterialType = 
    MATTE 
  | GLOSSY 
  | REFLECTIVE 
  | TRANSPARENT
  | REFRACTIVE
  | SUBSURFACE
  | VOLUMETRIC
  | GLTFPBR
  deriving (Show)

data MaterialData = MaterialData {
  materialType :: MaterialType,
  materialEmission :: Vec3f,
  color :: Vec3f,
  roughness :: Float,
  metallic :: Float,
  ior :: Float,
  scattering :: Vec3f,
  scanisotropy :: Float,
  trdepth :: Float,
  opacity :: Float,

  materialEmissionTex :: Int,
  colorTex :: Int,
  roughnessTex :: Int,
  scatteringTex :: Int,
  normalTex :: Int
}
  deriving (Show)

consMaterialData = MaterialData
defaultMaterialData = 
  consMaterialData
    MATTE
    (consVec3f 0 0 0)
    (consVec3f 0 0 0)
    0
    0
    1.5
    (consVec3f 0 0 0)
    0
    0.01
    1
    (-1)
    (-1)
    (-1)
    (-1)
    (-1)

defaultMaterialDataColorType matType matColor = 
  consMaterialData
    matType
    (consVec3f 0 0 0)
    matColor
    0
    0
    1.5
    (consVec3f 0 0 0)
    0
    0.01
    1
    (-1)
    (-1)
    (-1)
    (-1)
    (-1)

data SceneData = SceneData {
  cameras :: V.Vector CameraData,
  instances :: V.Vector InstanceData,
  environments :: V.Vector EnvironmentData,
  shapes :: V.Vector ShapeData,
  textures :: V.Vector TextureData,
  materials :: V.Vector MaterialData,

  cameraNames :: V.Vector String,
  textureNames :: V.Vector String,
  materialNames :: V.Vector String,
  shapeNames :: V.Vector String,
  instanceNames :: V.Vector String,
  environmentNames :: V.Vector String
}
  deriving (Show)

consSceneData = SceneData
defaultSceneData = 
  consSceneData
    (V.fromList [])
    (V.fromList [])
    (V.fromList [])
    (V.fromList [])
    (V.fromList [])
    (V.fromList [])
    (V.fromList [])
    (V.fromList [])
    (V.fromList [])
    (V.fromList [])
    (V.fromList [])
    (V.fromList [])
    