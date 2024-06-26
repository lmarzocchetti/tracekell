module Math where
import Data.Word (Word8)

(|>) :: t1 -> (t1 -> t2) -> t2
(|>) a f = f a

class Add a where
  add :: a -> a -> a

class Sub a where
  sub :: a -> a -> a

class Mul a b where
  mult :: a -> b -> a

class Div a b where
  divi :: a -> b -> a

class Minable a where
  min :: a -> a -> a

class Maxable a where
  max :: a -> a -> a

class CrossProd a where
  cross :: a -> a -> a

class DotProd a b where
  dot :: a -> a -> b

class DotProd a b => Norm a b where
  norm :: a -> b

data Vec2i = Vec2i {
  x :: Int,
  y :: Int
}
  deriving (Show)

consVec2i x y = Vec2i {x = x, y = y}

data Vec3i = Vec3i {
  x :: Int,
  y :: Int,
  z :: Int
}
  deriving (Show)

consVec3i x y z = Vec3i {x = x, y = y, z = z}

vec3iGetX Vec3i {x = x, y = _, z = _} = x
vec3iGetY Vec3i {x = _, y = y, z = _} = y
vec3iGetZ Vec3i {x = _, y = _, z = z} = z

data Vec4i = Vec4i {
  x :: Int,
  y :: Int,
  z :: Int,
  w :: Int
}
  deriving (Show)

consVec4i x y z w = Vec4i {x = x, y = y, z = z, w = w}

data Vec4b = Vec4b {
  x :: Word8,
  y :: Word8,
  z :: Word8,
  w :: Word8
}
  deriving (Show)

consVec4b = Vec4b

data Vec2f = Vec2f {
  x :: Float,
  y :: Float
}
  deriving (Show)

consVec2f x y = Vec2f {x = x, y = y}

data Vec3f = Vec3f {
  x :: Float,
  y :: Float,
  z :: Float
}
  deriving (Show)

instance Add Vec3f where
  add (Vec3f {x = x, y = y, z = z}) (Vec3f {x = x', y = y', z = z'}) = Vec3f {x = x + x', y = y + y', z = z + z'}

instance Sub Vec3f where
  sub (Vec3f {x = x, y = y, z = z}) (Vec3f {x = x', y = y', z = z'}) = Vec3f {x = x - x', y = y - y', z = z - z'}

instance Mul Vec3f Float where
  mult (Vec3f {x = x, y = y, z = z}) alpha = Vec3f {x = x * alpha, y = y * alpha, z = z * alpha}

instance Div Vec3f Float where
  divi (Vec3f {x = x, y = y, z = z}) alpha = Vec3f {x = x / alpha, y = y / alpha, z = z / alpha}

instance Minable Vec3f where
  min (Vec3f {x = x, y = y, z = z}) (Vec3f {x = x', y = y', z = z'}) = Vec3f {x = Prelude.min x x', y = Prelude.min y y', z = Prelude.min z z'}

instance Maxable Vec3f where
  max (Vec3f {x = x, y = y, z = z}) (Vec3f {x = x', y = y', z = z'}) = Vec3f {x = Prelude.max x x', y = Prelude.max y y', z = Prelude.max z z'}

instance CrossProd Vec3f where
  cross (Vec3f {x = x, y = y, z = z}) (Vec3f {x = x', y = y', z = z'}) = Vec3f {x = y * z' - z * y', y = z * x' - x * z', z = x * y' - y * x'}

instance DotProd Vec3f Float where
  dot (Vec3f {x = x, y = y, z = z}) (Vec3f {x = x', y = y', z = z'}) = x * x' + y * y' + z * z'

instance Norm Vec3f Float where
  norm a = sqrt (dot a a)

consVec3f x y z = Vec3f {x = x, y = y, z = z}

normalize :: Vec3f -> Vec3f
normalize a = if l /= (0 :: Float) then divi a l else a
  where
    l = norm a

data Vec4f = Vec4f {
  x :: Float,
  y :: Float,
  z :: Float,
  w :: Float
}
  deriving (Show)

consVec4f x y z w = Vec4f {x = x, y = y, z = z, w = w}

data Frame3f = Frame3f {
  x :: Vec3f,
  y :: Vec3f,
  z :: Vec3f,
  o :: Vec3f
}
  deriving (Show)

-- Getter
frame3fGetX Frame3f {x = x, y = y, z = z, o = o} = x
frame3fGetY Frame3f {x = x, y = y, z = z, o = o} = y
frame3fGetZ Frame3f {x = x, y = y, z = z, o = o} = z
frame3fGetO Frame3f {x = x, y = y, z = z, o = o} = o

consFrame3f x y z o = Frame3f {x = x, y = y, z = z, o = o}
defaultFrame3f = Frame3f {
  x = Vec3f {x = 1, y = 0, z = 0},
  y = Vec3f {x = 0, y = 1, z = 0},
  z = Vec3f {x = 0, y = 0, z = 1},
  o = Vec3f {x = 0, y = 0, z = 0}
}

lookAtFrame :: Vec3f -> Vec3f -> Vec3f -> Frame3f
lookAtFrame eye center up = consFrame3f u v w eye
  where
    w = normalize (sub eye center)
    u = normalize (cross up w)
    v = normalize (cross w u)