function(x) /*: Int + Void -> Void */ {

  var g = function(y) /*: Int -> Int */ {
    var prev = typeof x === "number" ? x : 0;
    x = y;
    return prev + y;
  };

  var h = function() /*: -> Void */ {
    if (typeof x === "number") {
      x + 10; // we could call g and violate the predicate
    }
  };

};