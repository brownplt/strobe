function slice(arr, start, stop) 
  /*: forall a . Array<'a> * Int * Int + Undef -> Array<'a> */ {
    var len =  /*: is Int */(arr.length);
  if (typeof stop === "undefined") { stop = len; }

  if (start < 0 || stop > len || start > stop) {
    throw "Invalid arguments";
  }

  var r = /*: 'a */ [ ];
  for (var i = 0; i < stop - start; i++) {
      r[i] = arr[start + i];
  }

  return r;
}
