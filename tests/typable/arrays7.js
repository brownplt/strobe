//have array anotations actually work

var f = function(x) /*: Array<String> -> String */ {
   return x[0];
};

f(["1", "2", "3"]);


