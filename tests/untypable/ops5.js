//should have || idiom working

function f(a) /*: Str + Int -> Str */ {
  return "hi";
}

var strint = "hey" || 30;

f(strint);