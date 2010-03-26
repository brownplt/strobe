function(x) /*: Int + Void -> { h : (-> Void) -> Void } */ {


  var h = function(z) /*: (-> Void) -> Void */ {
    if (typeof x === "number") {
       z();
       // The expression below is the continuation of z(), which is escaping.
       // Hence, x + 10 itself escapes, so the type-test above does not hold.
       x + 10;
    }
  };

  return { h : h };
};