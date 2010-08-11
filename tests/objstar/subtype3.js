function() /*: -> Undef */ {
    function foo(a) /*: {x:Int, #proto:Object, *:Bool} -> Undef*/
    {
    }

    foo({x:4, y:true}); // passes

    var arg = {x:4, y:true};

    foo(arg); // doesn't
};
