function(x) /*: Int + Undef -> Undef */ {
  var g = function(y) /*: Int -> Int */ {
    var prev = typeof x === "number" ? x : 0;
    return prev + y;
  };

  var h = function() /*: -> Undef */ {
    if (typeof x === "number") {
      x + 10;
    }
    else { x = 90; }
  };

};
