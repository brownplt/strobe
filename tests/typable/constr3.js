//TODO: subtyping between objects and constructed objects =/.
//
//function Point(x, y) /*: constructor (Int * Int -> {x : Int, y : Int}) */ {
//    this.x = x;
//    this.y = y;
//  };
//
////subtyping =) ?
//function sumPoint(pt) /*: {y : Int, x : Int} -> Int */ {
//  return pt.x + pt.y;
//}
//function sumPoint2(pt) /*: {x : Int, y : Int} -> Int */ {
//  return pt.x + pt.y;
//}
//
//var p = new Point(10, 20);
//sumPoint(p);
//sumPoint2(p);