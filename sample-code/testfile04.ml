let rec f n :Int->Int = 
  if n == 1 then 1
  else if n == 2 then 1
  else f (n-1) + f (n-2)
in
  f 30
