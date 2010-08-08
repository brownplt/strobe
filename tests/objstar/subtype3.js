function foo(a) /*: {x:Int, #proto:Object, *:Bool} -> Undef*/
{
}

var arg = {x:4, toString: function() /*: -> Str */ { return "foo"; }, y:true};

foo(arg); // should this work?
