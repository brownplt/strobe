//from Calculator gadget:

function parseInt(a,b) /*: String * (Int + Void) -> Int */ {
  return 0;
}
function parseFloat(a) /*: String -> Double */ {
  return 0.0;
}
var base = undefined;
function getValue(a) /*: ((Int + String + Double) -> (Int + Double)) */ {
  if (a == '.')
    a += '0';

  if (base != 10) {
    a = parseInt(""+a, base);
  } else {
    a = parseFloat(""+a);
  }

  return a;
}