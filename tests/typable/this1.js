var f = function(a) /*: [{x: String}] Int -> String */ {
  return this.x;
};

var obj = {foo: f, x : "HI"};
obj.foo(10);

