function foo(wnd) /*: Window<> -> Window<> */ {
  return wnd.window;
}

window.setTimeout(function (_) { return; }, 200);
