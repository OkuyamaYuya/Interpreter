let rec f n :Int->Int =
  if n == 0 then  0
  else n + f (n-1)
in
  f 100
