import Prelude hiding (Left, Right)

-- TYPES

data Inst = Forward Int | Left Int | Right Int | Repeat Int [Inst]
  deriving (Read, Show)
type Prog = [Inst]

data Cursor = Cursor {x :: Double, y :: Double, a :: Double}
  deriving (Show)

-- MAIN

main :: IO ()
main = do
  input <- getLine
  let p = read input 
      o = Cursor {x=100.0, y=100.0, a=0.0}
  printSvgHead
  progCursorToSvg p o
  printSvgTail

-- HELPERS

progCursorToSvg :: Prog -> Cursor -> IO Cursor
progCursorToSvg [] c = return c
-- repeat case
progCursorToSvg ((Repeat n bloc):subprog) c 
  | n > 0 = let repeat' = Repeat (n-1) bloc
            in do c' <- progCursorToSvg bloc c
                  progCursorToSvg (repeat':subprog) c'
  | otherwise = progCursorToSvg subprog c
-- other cases
progCursorToSvg (inst:subprog) c =
  let c' = changeCursor c inst
  in case inst of
       (Forward v) -> do printSvgLine c c'
                         progCursorToSvg subprog c'
       otherwise   -> progCursorToSvg subprog c'

changeCursor c@(Cursor x y a) inst =
  case inst of
    (Forward v) -> let v' = fromIntegral v
                       dx = v' * (cos $ a / 180.0 * pi)
                       dy = v' * (sin $ a / 180.0 * pi)
                   in Cursor (x+dx) (y+dy) a
    (Left v)    -> let v' = fromIntegral v
                   in Cursor x y (changeAngle (a+v'))
    (Right v)   -> let v' = fromIntegral v
                   in Cursor x y (changeAngle (a-v'))

changeAngle a
  | a < 0     = changeAngle (a+360.0)
  | a >= 360  = changeAngle (a-360.0)
  | otherwise = a

printSvgLine (Cursor x y _) (Cursor x' y' _) =
  let linePrefix = ("<line" ++)
        . (" x1=\"" ++)
        . ((show x) ++)
        . ("\" y1=\"" ++)
        . ((show y) ++)
        . ("\" x2=\"" ++)
        . ((show x') ++)
        . ("\" y2=\"" ++)
        . ((show y') ++)
        . ("\" stroke=\"red\" />" ++)
  in putStrLn (linePrefix "")

printSvgHead = do
  putStrLn "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
  putStrLn "<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" width=\"200\" height=\"200\">"
  putStrLn "<title>Exemple</title>"

printSvgTail = putStrLn "</svg>"
