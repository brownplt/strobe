//should have || idiom working

function f(a) /*: String + Int -> String */ {
  return "hi";
}

var strint = "hey" || 30;

f(strint);