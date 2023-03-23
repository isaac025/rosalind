module CDN where

data Nucleobases = A | C | G | T | U deriving (Show, Read, Eq)

type DNAString = [Nucleobases]

explode :: String -> [String]
explode = map (\x -> [x | x /= '\n']) 

countAdenine :: DNAString -> Int
countAdenine [] = 0
countAdenine (x:xs)
    | x == A = 1 + countAdenine xs
    | otherwise = countAdenine xs

countCytosine :: DNAString -> Int
countCytosine [] = 0
countCytosine (x:xs)
    | x == C = 1 + countCytosine xs
    | otherwise = countCytosine xs

countGuanine :: DNAString -> Int
countGuanine [] = 0
countGuanine (x:xs)
    | x == G = 1 + countGuanine xs
    | otherwise = countGuanine xs

countThymine :: DNAString -> Int
countThymine [] = 0
countThymine (x:xs)
    | x == T = 1 + countThymine xs
    | otherwise = countThymine xs

sampleDataset :: IO DNAString
sampleDataset = pure [A,G,C,T,T,T,T,C,A,T,T,C,T,G,A,C,T,G,C,A,A,C,G,G,G,C,A,A,T,A,T,G,T,C,T,C,T,G,T,G,T,G,G,A,T,T,A,A,A,A,A,A,A,G,A,G,T,G,T,C,T,G,A,T,A,G,C,A,G,C]

countIO :: String -> IO ()
countIO s = do
    let dna = init $ map read (explode s) :: DNAString
        a = countAdenine dna
        c = countCytosine dna
        g = countGuanine dna
        t = countThymine dna
        str = show a ++ " " ++ show c ++ " " ++ show g ++ " " ++ show t
    putStrLn str

