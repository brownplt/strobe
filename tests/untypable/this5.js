function Point(x,y) /*: constructor Int * Int -> {x:Int, y:Int} */ {
  this.x = x;
  this.y = y;
};
var f = function() /*: [Point] -> Int */ {
    return this.x + this.y;
};
Point.prototype.sum = f;

var p = new Point(10, 20);
p.sum();

var heck = {x:10, y:20, sum: f, blarg: f};
heck.blarg();
