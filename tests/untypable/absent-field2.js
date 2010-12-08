// Can't set an absent field
function foo(o) /*: {*: Int, #proto: Object, #code: _; x: _} -> Undef */ {
    o.x = 5;
}