function pf() /*: -> Double */ {
    return "heelo";
}

function getValue(a) /*: String + Double -> Double */ {
    a = pf();

  return a;
}

function foo(x) /*: (String + Double) -> Undef */ {
    return;
}

function pf1() /*: -> Undef */ {
    return foo(3.2);
}
var x = foo;
function pf2() /*: -> Undef */ {
    return foo("hi");
}
