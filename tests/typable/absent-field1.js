// This should avoid the Int in the * field, because toString is absent
function foo(o) /*: {*: Int, #proto: Object, #code: _; toString: _} -> (-> Str) */ {
    return o.toString;
}
