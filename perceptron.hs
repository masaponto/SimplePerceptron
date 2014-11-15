import Graphics.Gnuplot.Simple

-- weight
wi :: [Double]
wi = [1, 2, 3]


-- identification rate
r :: Double
r = 0.1


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
  line <- getLine
  putStrLn "Please input data"
  plotDiscriFunc ( read line :: [(Double,[Double])] ) [Title $ "perceptron: g(x)" ++ rate ++ defaultW, XRange(-10, 10), YRange(-10, 10) ]
  epspdfPlot "./perceptrons" $ plotDiscriFunc ( read line :: [(Double,[Double])] )
  return ()
    where rate = ", identification rate " ++ show r
          defaultW = ", default weight vector " ++ show wi
