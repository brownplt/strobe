function pf() /*: -> Double */ {
    return "heelo";
}

function getValue(a) /*: String + Double -> Double */ {
    a = pf();

  return a;
}

function foo(x) /*: (String + Double) -> Void */ {
    return;
}

function pf1() /*: -> Void */ {
    return foo(3.2);
}
var x = foo;
function pf2() /*: -> Void */ {
    return foo("hi");
}
