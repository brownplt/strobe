var lastSum = 5;

function Point(x, y) /*: constructor (Int * Int -> {x : Int, y : Int}) */ {
    this.x = x;
    this.y = y;
    var addition = this.x + this.y;
    lastSum = addition;
  };

function sumPoint(pt) /*: {x : Int, y : Int} -> Int */ {
  return pt.x + pt.y;
}

var p = new Point(10, 20);
sumPoint(p);

