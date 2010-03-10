var lastSum = 5;
function Point(x, y) /*: constructor (Int * Int -> {x : Int, y : Int}) */ {
    this.x = x;
    this.y = y;
    var addition = this.x + this.y;
    lastSum = addition;
  };
