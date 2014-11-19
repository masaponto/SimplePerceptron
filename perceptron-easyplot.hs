--import Graphics.Gnuplot.Simple
import Graphics.EasyPlot

-- weight
wi :: [Double]
wi = [60, 2, 300]


-- identification rate
r :: Double
r = 1000


-- inner product
innerP :: (Num a) => [a] -> [a] -> a
innerP a b = sum $ zipWith (*) a b


-- signature
sign :: Double -> Double
sign n
    | n >= 0 = 1
    | otherwise = -1

-- update weight
wUpdate :: (Double -> Double -> Double) -> [Double] -> [Double] -> [Double]
wUpdate op w x = zipWith op w $ map (r*) x


-- training
train :: [Double] -> (Double, [Double]) -> [Double]
train w xsi
    | fst xsi < sign ( innerP w $ snd xsi ) = wUpdate (-) w $ snd xsi
    | fst xsi > sign ( innerP w $ snd xsi ) = wUpdate (+) w $ snd xsi
    | otherwise = w


-- is discriminal sucsess
isMatch :: [Double] -> (Double, [Double]) -> Bool
isMatch w xsi
    | fst xsi == sign ( innerP w $ snd xsi ) = True
    | otherwise = False


-- loop training
loopT :: [(Double,[Double])] -> [Double] -> [Double]
loopT xs w
    | not $ all (isMatch w) xs = loopT xs $ foldl train w xs
    | otherwise = w


-- discriminal function
discriFunc :: [(Double,[Double])] -> Double -> Double
discriFunc xs x = a * x + b
    where a = - (ws !! 1) / (ws !! 2)
          b = - head ws / (ws !! 2)
          ws = loopT xs wi


-- list of data to vector
data2vecs :: [(Double, [Double])] -> [(Double, Double)]
data2vecs datas = zip as bs
    where as = map ((!! 1).snd) datas
          bs = map ((!! 2).snd) datas


-- class separation datalist
classSepa :: Double -> [(Double, [Double])] -> [(Double, [Double])]
classSepa no = filter (\t -> fst t == no)


-- plot discriminal function
plotDiscriFunc :: [(Double,[Double])] -> IO Bool
plotDiscriFunc xs = plot (PNG "./rand.png") [ Function2D [Title $ "discriminal" ++ rate ++ defaultW , Color Red ] [] (discriFunc xs )
                          , Data2D [Title "class1", Color Black] [] class1Points
                          , Data2D [Title "class2", Color Blue] [] class2Points]
    where class1Points = data2vecs $ classSepa 1 xs
          class2Points = data2vecs $ classSepa (-1) xs
          rate = ", learning rate " ++ show r
          defaultW = ", default weight vector " ++ show wi
          func = show a ++ "x" ++ show b
          a = - (ws !! 1) / (ws !! 2)
          b = - head ws / (ws !! 2)
          ws = loopT xs wi

main :: IO Bool
main = do
  putStrLn "Please input data"
  line <- getLine
  putStrLn "start lerning"
  plotDiscriFunc ( read line :: [(Double,[Double])] )
  --epspdfPlot "./perceptrons" $ plotDiscriFunc ( read line :: [(Double,[Double])] )
  putStrLn "done!!"
  return True
