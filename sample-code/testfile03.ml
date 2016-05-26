let rec f x : Int->Int = 
  if x == 0 then 1
  else if x == 1 then 1
  else f (x-1) + f (x-2)
in
  f 10
