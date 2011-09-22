var foo = function(obj) 
  /*: (rec a . #{ x: Num, y: Num, * : _,
                  __proto__: #{ move : ['a] -> Num } }) -> Num */ {
  return obj.move();
};

var bar = function(obj2)
  /*: { x : Num, 
        y : Num, 
        * : _,
        __proto__: #{ 
	        __proto__: Null,
          move : 
             [ (rec a . #{ x: Num, y: Num, * : _,
                           __proto__: #{ move : ['a] -> Num } }) ] 
            -> Num 
        }
      } -> Num */ {
  return foo(obj2);
};
