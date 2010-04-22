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

var heck = {x:10, y:20, blarg: f};
//doesnt work, since f requires Point and heck is missing "sum"
heck.blarg();
