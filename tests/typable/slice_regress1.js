function slice(stop) 
  /*: Int + Undef -> Int */ {
  if (typeof stop === "undefined") { stop = 0; }
  return stop;
}
