//from Calculator gadget:

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