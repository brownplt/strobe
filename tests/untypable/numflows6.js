var f = function() /*: -> Num */ { return 3; };
var a = /*:upcast Str + Num */13;
a = a - f();
a -= f();
a -= 3;
a = a - 3;
a = "hi";

