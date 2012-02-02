// Based on calmStaicE, etc. in Flapjax
function typ_close() /*: -> (-> Num) */ {
  var state = /*:upcast Undef  + Num */ undefined;
  return function() /*: -> Num */ {
    if (typeof state === "undefined") { state = 0; }
    state = state + 1;
    return state;
  };
}
