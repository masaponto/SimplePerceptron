import Graphics.Gnuplot.Simple
import Data.List

-- weight
wi :: [Double]
wi = [-1.0, -2.0, 1.0]


-- identification rate
r :: Double
r = 0.2


-- inner product
innerP :: (Num a) => [a] -> [a] -> a
innerP a b = sum $ zipWith (*) a b

--
calcError :: [Double] -> (Double,[Double]) -> Double
calcError w xsi
    | fst xsi == 1 = ret - 1
    | otherwise = ret
    where ret = innerP w $ snd xsi

-- judge is allowable error
isEnough :: Double -> Bool
isEnough x
    | abs x <= 0.1 = True
    | otherwise = False

-- update
wUpdate' :: [Double] -> [(Double, [Double])] -> [Double]
wUpdate' w xs = zipWith (-) w $ map ((*r). innerP b ) xp
    where b = map (calcError w) xs
          xp = transpose $ map snd xs

-- loop training
loopT' :: [(Double, [Double])] -> [Double] -> [Double]
loopT' xs w
    | not $ all isEnough $ map (calcError w ) xs = loopT' xs $ wUpdate' w xs
    | otherwise = w


-- discriminal function
discriFunc :: [(Double,[Double])] -> Double -> Double
discriFunc xs x = a * x + b
    where a = - (ws !! 1) / (ws !! 2)
          b = - head ws / (ws !! 2)
          ws = loopT' xs wi


-- list of data to vector
data2vecs :: [(Double, [Double])] -> [(Double, Double)]
data2vecs datas = zip as bs
    where as = map ((!! 1).snd) datas
          bs = map ((!! 2).snd) datas


-- class separation datalist
classSepa :: Double -> [(Double, [Double])] -> [(Double, [Double])]
classSepa no = filter (\t -> fst t == no)


-- plot discriminal function
plotDiscriFunc :: [(Double,[Double])] -> [Attribute] -> IO()
plotDiscriFunc xs attributeGraph = plotPathsStyle attributeGraph $ lineStyle ++ class1Style ++ class2Style
    where lineStyle = [(defaultStyle {lineSpec = CustomStyle [LineTitle "discriminal"]}, discriFuncPoints)]
          class1Style = [(defaultStyle {plotType = Points, lineSpec = CustomStyle [LineTitle "class 1", PointType 6]}, class1Points)]
          class2Style = [(defaultStyle {plotType = Points, lineSpec = CustomStyle [LineTitle "class 2", PointType 4]}, class2Points)]
          discriFuncPoints = zip xaxis $ map (discriFunc xs) xaxis
          class1Points = data2vecs $ classSepa 1 xs
          class2Points = data2vecs $ classSepa (-1) xs
          xaxis = [-10 .. 10]


main :: IO()
main = do
  putStrLn "Please input data"
  line <- getLine
  putStrLn "start lerning"
  plotDiscriFunc ( read line :: [(Double,[Double])] ) [Title $ "widrow-hoff: g(x)" ++ rate ++ defaultW, XRange(-10, 10), YRange(-10, 10) ]
  epspdfPlot "./widrowHoff" $ plotDiscriFunc ( read line :: [(Double,[Double])] )
  putStrLn "done!!"
  return ()
    where rate = ", identification rate " ++ show r
          defaultW = ", default weight vector " ++ show wi
