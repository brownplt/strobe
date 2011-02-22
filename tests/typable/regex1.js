function foo(o) /*: { /_(.*)_/ : Int} -> Int + Undef */ {
  return o.__proto__;
}
