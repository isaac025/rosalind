module TDIR where

import CDN

transcribe :: DNAString -> DNAString
transcribe [] = []
transcribe (x:xs)
    | x == T = U: transcribe xs
    | otherwise = transcribe xs
