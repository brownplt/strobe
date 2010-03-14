function Point(x, y) /*: constructor (Int * Int -> {x : Int, y : Int}) */ {
    this.x = x;
    this.y = y;
  };

function sumPoint(pt) /*: Point -> Int */ {
  return pt.x + pt.y;
}

var p = new Point(10, 20);
sumPoint(p);

Point.prototype.sum = function() /*: -> Int */ { return this.x + this.y; };

//the same point can now be used to sum:
p.sum();