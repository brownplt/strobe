function Point(x, y) /*: constructor (Int * Int -> {x : Int, y : Int}) */ {
    this.x = x;
    this.y = y;

};


var f = function(p) /*: Point + Int -> Int */ {
  if (p instanceof Point) {
    return p.x;
  }
  else {
    return p;
  }
};
