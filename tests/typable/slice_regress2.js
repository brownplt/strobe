function slice(arr, stop) 
    /*: forall a . Array<'a> * Num + Undef -> Num */ {
    var len =  /*: is Num */(arr.length);
  if (typeof stop === "undefined") { stop = len; }

  return stop;
}
