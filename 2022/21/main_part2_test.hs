
root :: Integer -> Bool
root humn = pppw == sjmn
  where
    dbpl = 5
    cczh = sllz + lgvd
    zczc = 2
    ptdq = humn - dvpt
    dvpt = 3
    lfqf = 4
    ljgn = 2
    sjmn = drzm * dbpl
    sllz = 4
    pppw = cczh `div` lfqf
    lgvd = ljgn * ptdq
    drzm = hmdt - zczc
    hmdt = 32

solution = head $ filter root [0..]

main = do
  print solution