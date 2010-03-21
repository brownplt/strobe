function(x) /*: Int + Void -> { g : -> Int, h : (-> Void) -> Void } */ {

  var g = function() /*: -> Int */ {
      x = undefined;
    return 200;
  };

  var h = function(z) /*: (-> Void) -> Void */ {
    if (typeof x === "number") {
        z();
        // The expression below is the continuation of z(), which is escaping.
        // Hence, x + 10 itself escapes, so the type-test above does not hold.
       x + 10;
    }
  };

  return { g : g, h : h };
};