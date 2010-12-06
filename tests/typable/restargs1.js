function foo(f) /*: (Num + Undef ... -> Num) -> Num */ {
    return f(1,2,3);
}

function bar(f) /*: (Str * Int + Undef ... -> Str) -> Str */ {
    return f("foo");
}

function bar2(f) /*: (Str * Int + Undef ... -> Str) -> Str */ {
    return f("foo",1,2,3);
}
