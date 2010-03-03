/*::
  function _view_onopen : ( -> Void)
  function _view_onminimize : ( -> Void)
  function _view_onrestore : ( -> Void)
  function _view_onpopout : ( -> Void)
  function d : ( -> Void)
  function e : ( -> Void)
  function k : (Date -> Void)
  function h : (Date -> Void)
  function l : (Date -> Void)
  function m : ( -> Void)
  function f : ( -> Void)
  function g : (Bool -> Void)
  function j : ( -> Void)
*/
//I couldn't get minimize or 'j' to be called
//so i FILLED THEM IN. also inserted semicolons


//NOTE: un-minified with jsbeautifire.org

options.putDefaultValue("SecondHand", 1);
var _SecondInterval = 0,
    _minimized = false;

function _view_onopen() {
    f();
    d();
}
function _view_onminimize() {
    _minimized = true;
    d();
    if (_SecondInterval != 0) {
        clearInterval(_SecondInterval);
        _SecondInterval = 0;
    }
}
function _view_onrestore() {
    _minimized = false;
    view.caption = strings.PLUGIN_TITLE;
    f();
    d();
}
function _view_onpopout() {
    if (_minimized) {
        f();
        d();
    }
}
function d() {
    var a = new Date;
    if (_minimized) {
        var b = a.getHours();
        if (b > 12) b -= 12;
        if (b < 10) b = "0" + b;
        var c = a.getMinutes();
        if (c < 10) c = "0" + c;
        view.caption =
        b + ":" + c;
    } else {
        h(a);
        k(a);
    }
    var i = (61 - a.getSeconds()) * 1000;
    SetTimeout(d, i);
}
function e() {
    var a = new Date;
    l(a);
    h(a);
}
function k(a) {
    var b = a.getHours();
    if (b >= 12) b -= 12;
    var c = a.getMinutes() + 60 * b;
    HourHand.rotation = c / 2;
}
function h(a) {
    var b = a.getSeconds() + 60 * a.getMinutes();
    MinuteHand.rotation = b / 10;
}
var _UpdateSecondHandInterval = 0,
    _BounceRotationIncrement = 1.8,
    _NewRotation = 0;

function l(a) {
    if (_UpdateSecondHandInterval != 0) {
        clearInterval(_UpdateSecondHandInterval);
        _UpdateSecondHandInterval = 0;
    }
    var b = a.getMilliseconds() + a.getSeconds() * 1000;
    _NewRotation = b * 0.0060;
    SecondHand.rotation = _NewRotation + _BounceRotationIncrement;
    _UpdateSecondHandInterval = SetInterval(m, 50);
}
function m() {
    SecondHand.rotation = _NewRotation;
}
function f() {
    if (_SecondInterval != 0) {
        clearInterval(_SecondInterval);
        _SecondInterval = 0;
    }
    switch (options("SecondHand")) {
    case 0:
        g(false);
        break;
    case 2:
        g(true);
        e();
        _SecondInterval = SetInterval(e, 25);
        break;
    case 1:
        g(true);
        e();
        _SecondInterval = SetInterval(e, 1000);
        break;
    }
}
var _SecondHandFade = 0;

function g(a) {
    var b = a ? 255 : 0;
    if (_SecondHandFade != 0) {
        cancelAnimation(_SecondHandFade);
    }
    if (b != SecondHand.opacity) {
        _SecondHandFade = beginAnimation(j, SecondHand.opacity, b, Math.abs(SecondHand.opacity - b) * 5);
    }
}
function j() {
    SecondHand.opacity = event.value;
};