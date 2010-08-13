
function sum(t) /*: (trec X . Int + {left: Int, right: 'X}) -> Int */ {
    if(typeof t === "object") {
        return t.left + sum(t.right);
    }
    else {
        return t;
    }
}

var o = /*: upcast trec X . Int + {left: Int, right: 'X} */ 
{left: 5,
 right: /*: upcast trec X . Int + {left: Int, right: 'X} */
 {left: 10,
  right: /*: upcast trec X . Int + {left: Int, right: 'X} */ 
  {left: 4, 
   right: /*: upcast trec X . Int + {left: Int, right: 'X} */ 6}}};

sum(o);