// Based on calmStaicE, etc. in Flapjax
function typ_close() /*: -> (-> Int) */ {
  var state = /*:upcast Void  + Int */ undefined;
  return function() /*: -> Int */ {
    if (typeof state === "undefined") { state = 0; }
    state = state + 1;
    return state;
  };
}
