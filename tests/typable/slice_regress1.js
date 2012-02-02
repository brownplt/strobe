function slice(stop) 
  /*: Num + Undef -> Num */ {
  if (typeof stop === "undefined") { stop = 0; }
  return stop;
}
