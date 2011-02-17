function foo (obj) /*: (rec o . { ptr : 'o, v: Int }) -> Int */ {
  return obj.ptr.ptr.ptr.ptr.ptr.v;
}