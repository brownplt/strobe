function (x) /*: Any -> Any */ {
  return function() /*: Undef -> Undef */ {
    while (1) {
      var x = 4;
      x + 23;
    }
    return x;
  };
};
