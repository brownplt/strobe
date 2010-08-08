function foo(a) /*: {x:Num, #proto:Object, *:Bool} -> Undef*/
{
}

var arg = ({x: 5, y: 9});

foo(arg); // should this work?  This is the event typing question
