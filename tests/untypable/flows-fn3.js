//from Calculator gadget:

var base = undefined;
function getValue(a) /*: ((Num + Str + Num) -> (Num + Num)) */ {
  if (a == '.')
    a += '0';

  if (base != 10) {
    a = parseNum(""+a, base);
  } else {
    a = parseFloat(""+a);
  }

  return a;
}
