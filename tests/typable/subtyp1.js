function foo (obj) /*: (rec o . { ptr : 'o, v: Int, proto: Null }) -> Int */ {
  return obj.ptr.ptr.ptr.ptr.ptr.v;
}
