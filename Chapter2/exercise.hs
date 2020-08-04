double x = x + x

quadruple x = double (double x)

factorial n = product [1..n]

average ns = sum ns `div` length ns

n = a `div` length xs
    where
        a = 10
        xs = [1..5]

last' n = head (reverse n)

init' n = take ((length n)-1) n

init'' n = reverse (tail (reverse n))