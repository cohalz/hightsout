import FreeGame
n = 5
winHeight = 500
winWidth = 500

main = runGame Windowed (Box (V2 0 0) (V2 winWidth winHeight)) $ do
  setTitle "hightsout"
  foreverFrame $ do
    color black $ mapM_ rectangle $ createLight n winWidth
    color red $ mapM_ line $ createBorder n winWidth winHeight "vertical"
    color red $ mapM_ line $ createBorder n winWidth winHeight "horizontal"

rectangle :: [V2 Double] -> Frame ()
rectangle ((V2 a b) : (V2 c d) : []) = polygon [V2 a b, V2 a d, V2 c d, V2 c b]
rectangle _ = undefined

createLight :: Double -> Double -> [[V2 Double]]
createLight num width = 
  let size = width / num
  in [[V2 (size * j) (size * i), V2 (size * (j+1)) (size * (i+1))] 
    | i <- [0..(n-1)] , j <- [0..(n-1)]]

createBorder :: Double -> Double -> Double -> String -> [[V2 Double]]
createBorder num width height str =
  let size = width / num
  in case str of
    "vertical" -> [[V2 (size*i) 0, V2 (size*i) height] | i <- [1..(n-1)]]
    "horizontal" -> [[V2 0 (size*i), V2 width (size*i)] | i <- [1..(n-1)]]
