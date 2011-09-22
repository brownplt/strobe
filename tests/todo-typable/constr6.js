function Point(x) /*: constructor (Int -> {x : Int}) */ {
    this.x = x;
  };

//you can only call this function with a point once we add the right field...
function sumPoint(pt) /*: {x : Int, y : Int} -> Int */ {
  return pt.x + pt.y;
}

sumPoint({x:23, y:24});

var p = new Point(10);

Point.prototype.y = 12;
p.y;
sumPoint(p);
