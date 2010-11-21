var f = function(p) /*: Array<Int> + Int -> Int */ {
  if (p instanceof Array) {
    return p[0];
  }
  else {
    return p;
  }
};
