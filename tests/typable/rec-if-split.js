var o = /*: upcast trec X . Int + {left: Int, right: 'X, toString: -> Str} */ 
{left:4, right:
/*: upcast trec X . Int + {left: Int, right: 'X, toString: -> Str} */ 5};

var d = 0;

if(typeof o === "object") {
    d = o.left + 6;
}
else {
    d = o + 7;
}
