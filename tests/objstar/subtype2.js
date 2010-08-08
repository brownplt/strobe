function foo(a) /*: {x:Num, #proto:Object, *:Bool} -> Undef*/
{
}

var arg = (/*: cheat {x:Num, y:Bool} */ {});

foo(arg); // should this work?  This is the event typing question
