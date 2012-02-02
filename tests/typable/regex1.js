function foo(o) /*: { /_(.*)_/ : Num} -> Num + Undef */ {
  return o.__proto__;
}
