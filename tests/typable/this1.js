function foo() /*: -> Undef */ {
    function f(a) /*: [{f: Int -> Str, x: Str}] Int -> Str */ {
        return this.x;
    }
    
    var obj = {f: function(n) /*: Int -> Str */ { return "str"; }, 
               x : /*: upcast Str */ "HI"};
    obj.f(4);
}

