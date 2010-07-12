function slice(arr, stop) 
    /*: forall a . Array<'a> * Int + Undef -> Int */ {
    var len =  /*: is Int */(arr.length);
  if (typeof stop === "undefined") { stop = len; }

  return stop;
}
