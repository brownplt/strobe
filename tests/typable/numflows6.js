var f = function() /*: -> Int */ { return 3; };
var a = /*:upcast String + Int */13;
a = a - f();
a -= f();
a -= 3;
a = a - 3;
a = "hi";
