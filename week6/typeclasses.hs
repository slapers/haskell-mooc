module TypeClasses where

data Bright = Blue | Red deriving (Read, Show)
data Pastel = Turquoise | Tan deriving (Read, Show)

class Color a where
  dark :: a -> Bool
  lighten :: a -> a

instance Color Bright where
  dark Blue = True
  dark Red = False
  lighten Blue = Red
  lighten Red = Red

instance Color Pastel where
  dark Turquoise = True
  dark Tan = False
  lighten Turquoise = Tan
  lighten Tan = Tan

data Foo = Bar | Baz

instance Show Foo where
  show Bar = "Bar"
  show Baz = "Baz"
