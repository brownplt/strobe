function(x) /*: Num + Undef -> { h : (-> Undef) -> Undef } */ {

  var g = function() /*: -> Num */ {
    return x;
  };
  x = 90;
  g();

  var h = function(z) /*: (-> Undef) -> Undef */ {
    if (typeof x === "number") {
       z();
       // g is called from an escaping function (this continuation). Therefore,
       // it also escapes.
       g();
    }
  };

  return { h : h };
};
