//have array anotations actually work

var f = function(x) /*: Array<Str> -> Str */ {
   return x[0];
};

f(["1", "2", "3"]);


