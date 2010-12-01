//have array anotations actually work

var f = function(x) /*: Array<Str> -> Str */ {
   return x[0];
};

f([/*: upcast Str */ "1", /*: upcast Str */ "2", /*: upcast Str */ "3"]);


