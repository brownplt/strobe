var o = 
    /*: upcast trec X . ({_banned_: {}, #proto: Object, *: 'X}) */ 
{_banned_:{}, toString: function() /*: -> Str */ { return "str"; }};


var n = /*: upcast trec X . ({_banned_: Undef + {}, #proto: Object, *: 'X} + Int) */ 5;
var o2 = 
    /*: upcast trec X . ({_banned_: Undef + {}, #proto: Object, *: 'X} + Int) */ 
{_banned_:{}, toString: function() /*: -> Str */ { return "str"; }, safe_field: n};    

