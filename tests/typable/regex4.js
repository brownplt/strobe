function foo(o, s) 
/*: {__bah__: Num, _foo_: Str} * /_(.*)_/ -> Num + Str + Undef */ {
    return o[s];
}
