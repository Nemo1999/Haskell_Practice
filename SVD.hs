import System.Environment
import Data.List
import qualified Numeric.LinearAlgebra as L 
import qualified Graphics.Image as I

seperateRGB :: I.Image I.VU I.RGB Double -> [I.Image I.VU I.Y Double] 
seperateRGB img =
  let takeR = \(I.PixelRGB r g b)-> I.PixelY r 
      takeG = \(I.PixelRGB r g b)-> I.PixelY g
      takeB = \(I.PixelRGB r g b)-> I.PixelY b
  in  map (\x->I.map x img) [takeR,takeG,takeB]      

combineRGB :: [[[Double]]] -> I.Image I.VU I.RGB Double
combineRGB [rs,gs,bs] = I.fromLists $ zipWith3 (zipWith3 I.PixelRGB) rs gs bs
  
compressBySVD :: Int -> L.Matrix Double -> L.Matrix Double
compressBySVD n matIn =
  let (u,s,v) = L.svd matIn
      sList = L.toList s
      minSingularValue = (sortOn (negate) sList)!!n
      sDiag = L.diagl $ map (\x->if x < minSingularValue then 0 else x) sList
  in  u <> sDiag <> L.tr v

main = do
  args <- getArgs
  let (imgPath , numSingularValue) = (head args , read (args!!1))
  img      <-  I.readImageRGB I.VU imgPath
  I.displayImage img
  let imgs@[rs,gs,bs] = seperateRGB img 
  let msMatrix = map (L.fromLists . (map . map) (\(I.PixelY x) -> x) . I.toLists) imgs
  let msCompressed = map (compressBySVD numSingularValue) msMatrix
  let img'  = combineRGB $ map (L.toLists) msCompressed 
  I.displayImage img'
  I.writeImage (takeWhile (/='.') imgPath ++
                "_SVD_"++
                show numSingularValue ++
                dropWhile (/= '.') imgPath)
    img'
  
  
