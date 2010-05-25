function pf() /*: -> Num */ {
    return "heelo";
}

function getValue(a) /*: Str + Num -> Num */ {
    a = pf();

  return a;
}

function foo(x) /*: (Str + Num) -> Undef */ {
    return;
}

function pf1() /*: -> Undef */ {
    return foo(3.2);
}
var x = foo;
function pf2() /*: -> Undef */ {
    return foo("hi");
}
