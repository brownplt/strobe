function foo (obj) /*: (rec o . { ptr : 'o, v: Num, proto: Null }) -> Num */ {
  return obj.ptr.ptr.ptr.ptr.ptr.v;
}
