function(x) /*: Int + Undef -> Undef */ {
  var g = function(y) /*: Int -> Int */ {
    var prev = typeof x === "number" ? x : 0;
    x = y;
    return prev + y;
  };
  if (typeof x === "number") {
    x + 10;
  }
};

