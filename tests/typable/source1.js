function foo() /*: -> Undef */ { 

    function baz(o) /*: #{x: Int + Undef} -> Int + Undef*/ {
        var d = 5;
        return o.x;
    }
    function bar(o) /*: #{x: Int} -> Int + Undef */  {
        return baz(o);
    }
    bar({x:5});
    
}
