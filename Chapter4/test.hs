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

True    || True     = True
False   || True     = True
True    || False    = True
False   || False    = False

(&&&) :: Bool -> Bool -> Bool
a &&& b = if a /= True then  False else
    if b /= True then False else True

(&&&&) :: Bool -> Bool -> Bool
a &&&& b = if a == True then b else False