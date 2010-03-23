function(x) /*: Int + Void -> Void */ {
  var g = function(y) /*: Int -> Int */ {
    var prev = typeof x === "number" ? x : 0;
    return prev + y;
  };

  var h = function() /*: -> Void */ {
    if (typeof x === "number") {
      x + 10;
    }
    else { x = 90; }
  };

};
