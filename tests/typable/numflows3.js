var f = function() /*: -> Num */ { return 3; };
var a = /*:upcast Str + Num */13;
a = a - f();
a = "hi";

