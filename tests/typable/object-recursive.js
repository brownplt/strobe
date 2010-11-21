function foo() /*: -> {x: Int, f: (Bool -> Int)} */ {
    var o = {x:5, f:function(b) /*: Bool -> Int */ { return b ? o.x : o.f(true); }};
    return o;
}