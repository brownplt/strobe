var f = function(a) /*: [{x: Str}] Num -> Str */ {
  return this.x;
};

var obj = {foo: f, x : 39}; //x has the wrong type
obj.foo(10);

