function Point(x, y) /*: constructor (Int * Int -> {x : Int, y : Int}) */ {
    this.x = x;
    this.y = y;
  };

var p = new Point(10, 20);
p.sum();

Point.prototype.sum = function() /*: -> Int */ { return this.x + this.y; };

