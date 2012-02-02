function foo() /*: -> Undef */ { 

    function baz(o) /*: #{x: Num + Undef} -> Num + Undef*/ {
        var d = 5;
        return o.x;
    }
    function bar(o) /*: #{x: Num} -> Num + Undef */  {
        return baz(o);
    }
    bar({x:5});
    
}
