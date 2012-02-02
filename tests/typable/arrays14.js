var myArr = /*: Array<Num> */[];

myArr[1] = 900;
myArr[2] = 1000;

// Explicit type applications below.
var yourArr = 
  (/*: [Str] */ /*: [Num] */ (myArr.map))
  (function(x) /*: Num -> Str */ { return "lol"; });
