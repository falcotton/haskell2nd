helve x = (take n x,drop n x )
    where n = length x `div` 2


third x = head (tail (tail x))
third' x = x !! 2
third'' (_:_:x:_) = x

safetail x  = if null x then [] else tail x 
safetail' x | null x = []
            | otherwise = tail x

safetail'' [] = []
safetail'' (_:xs) = xs