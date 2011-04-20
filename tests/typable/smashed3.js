var foo = function(obj) 
  /*: (rec a . #{ x: Num, y: Num, proto: {{ move : ['a] -> Num }} }) -> Num */ {
  return obj.move();
};

var bar = function(obj2)
  /*: (rec a . { x : Num, y : Num, proto: { move : ['a] -> Num } }) -> Num */ {
  return foo(obj2);
};