function(x) /*: Int + Void -> { h : (-> Void) -> Void } */ {

  var g = function() /*: -> Int */ {
    return x;
  };
  x = 90;
  g();

  var h = function(z) /*: (-> Void) -> Void */ {
    if (typeof x === "number") {
       z();
       // g is called from an escaping function (this continuation). Therefore,
       // it also escapes.
       g();
    }
  };

  return { h : h };
};