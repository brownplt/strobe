function(x) /*: Num + Undef -> Undef */ {

  var g = function(y) /*: Num -> Num */ {
    var prev = typeof x === "number" ? x : 0;
    x = y;
    return prev + y;
  };

  var h = function() /*: -> Undef */ {
    if (typeof x === "number") {
      x + 10; // we could call g and violate the predicate
    }
  };

};
