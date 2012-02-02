var f = function(a) /*: [{x: Str}] Num -> Str */ {
  return this.x;
};

var obj = {foo: f, x : "HI"};
obj.foo(10);

