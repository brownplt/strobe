function Point(x, y) /*: constructor (Num * Num -> {x : Num, y : Num}) */ {
    this.x = x;
    this.y = y;
}

var alarming = /*:upcast Undef + Point*/undefined;

function framework_audio_stop(arg) /*: Point -> Undef */ {
  return;
}

function need_undef(arg) /*: Undef -> Undef */ {
  return arg;
}

function stopAlarm() /*: -> Undef */ {
  if (typeof alarming === "undefined") { need_undef(alarming); }
  else { framework_audio_stop(alarming); }
}


