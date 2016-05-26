let f : Int->Int = \ x:Int . x + 10
in
  (\f:Int->Int . f 3) f 
