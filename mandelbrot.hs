import Data.Complex
import Data.Array
import Control.Monad
import System.CPUTime

height, width :: Int
height = 1024
width = 1024

min_x, max_x, min_y, max_y :: Double
min_x = -2.0
max_x = 0.47
min_y = -1.12
max_y = 1.12

scalex, scaley :: Double
scalex = (max_x - min_x) / fromIntegral width
scaley = (max_y - min_y) / fromIntegral height

max_iters :: Int
max_iters = 256

mandelbrot_0 :: Complex Double -> Int
mandelbrot_0 c = length $ takeWhile (\z -> magnitude z <= 2) $ take max_iters $ iterate (\z -> z*z + c) c

mandelbrot :: Array (Int, Int) Int
mandelbrot = array ((0,0),(height-1,width-1)) [((h,w), mandelbrot_0 (cx :+ cy)) | h <- [0..height-1], w <- [0..width-1], let cy = min_y + fromIntegral h * scaley, let cx = min_x + fromIntegral w * scalex]

main :: IO ()
main = forM_ [1..3] $ \i -> do
    start <- getCPUTime
    let result = elems mandelbrot
    print $ sum result
    end <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    putStrLn $ show i ++ " Execution Time: " ++ show diff ++ " " ++ show (sum result)