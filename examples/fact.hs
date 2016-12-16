factorial :: (Num t, Eq t) => t -> t
factorial x =
  if x == 0
    then 1
    else x * factorial (x - 1)
