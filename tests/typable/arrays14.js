var myArr = /*: Array<Int> */[];

myArr[1] = 900;
myArr[2] = 1000;

// Explicit type applications below.
var yourArr = 
  (/*: [Str] */ /*: [Int] */ (myArr.map))
  (function(x) /*: Int -> Str */ { return "lol"; });
