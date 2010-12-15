function(x) /*: Int + Undef -> { h : (-> Undef) -> Undef } */ {


  var h = function(z) /*: (-> Undef) -> Undef */ {
    if (typeof x === "number") {
       z();
       // The expression below is the continuation of z(), which is escaping.
       // Hence, x + 10 itself escapes, so the type-test above does not hold.
       x + 10;
    }
  };

  return { h : h };
};