function(x) /*: Num + Undef -> Undef */ {
  var g = function(y) /*: Num -> Num */ {
    var prev = typeof x === "number" ? x : 0;
    x = y;
    return prev + y;
  };
  x = 90;
};
