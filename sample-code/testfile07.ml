let rec fib n : Int -> List Int =
  if n == 1 then [1,1]
  else if n == 2 then [1,1]
  else
    let l :List Int = fib (n-1) in
    let x1 : Int = l[0] in
    let x2 : Int = l[1] in
    [x1+x2,x1]
  in
    (fib 10)[0]
