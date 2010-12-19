function Point(x,y) /*: constructor (Int * Int -> {x: Int, y: Int}) */ {
    this.x = x;
    this.y = y;
}

Point.prototype = {dist : function() /*: [Point] -> Int */ { return this.x + this.y; }};

var p = new Point(4, 5);

function foo(n) /*: Int -> Int */ {
    return n;
}

foo(p.dist());