var f = function(a) /*: [{x: Str}] Int -> Str */ {
  return this.x;
};

var obj = {foo: f, x : "HI"};
obj.foo(10);

