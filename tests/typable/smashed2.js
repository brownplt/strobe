var foo = function(obj) /*: {{ x: Num, y: Num }} -> Num */ {
  return obj.x;
};

// this is a precise object type that gets subtyped
foo({ x : 900, y: 200 }); 