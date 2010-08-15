var foo = function(x) /*: Undef + { y: Str } -> Str */ {
  if (x) {  return x.y; }
  else { return "not found"; }
};
