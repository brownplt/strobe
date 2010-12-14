function foo(f) /*: [{*: Undef, #proto: Object, #code: _; x:{}} + Str] 
([{*: Undef, #proto: Object, #code: _; x:{}}] Bool -> Num) -> Num */ {

    return f.call(this, true);

}