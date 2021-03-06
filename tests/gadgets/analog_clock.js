options.putDefaultValue("SecondHand", 1);
var _SecondNumerval = 0,
    _minimized = false;

var _SecondHandFade = 0;

var _UpdateSecondHandNumerval = 0,
    _BounceRotationIncrement = 1.8,
    _NewRotation = 0.0; // Claudiu: changed to number

function _view_onopen() /*:  -> Undef */ {
    f();
    d();
}
function _view_onminimize() /*:  -> Undef */ {
    _minimized = true;
    d();
    if (_SecondNumerval != 0) {
        clearNumerval(_SecondNumerval);
        _SecondNumerval = 0;
    }
}
function _view_onrestore() /*:  -> Undef */ {
    _minimized = false;
    view.caption = strings.PLUGIN_TITLE;
    f();
    d();
}

function _view_onpopout() /*:  -> Undef */ {
    if (_minimized) {
        f();
        d();
    }
}

function d() /*:  -> Undef */ {
  var a = new Date(undefined); // Claudiu : added undefined
    if (_minimized) {
        var b = /*:upcast Str + Num */(/*:is Num */(a.getHours()));
        if (b > 12) b = b - 12;
        if (b < 10) b = "0" + b;
        var c = /*:upcast Str + Num */(a.getMinutes());
        if (c < 10) c = "0" + c;
        view.caption = b + ":" + c;
    } else {
        h(a);
        k(a);
    }
    var i = (61 - a.getSeconds()) * 1000;
    setTimeout(d, i);
}

function e() /*:  -> Undef */ {
    var a = new Date(undefined);
    l(a);
    h(a);
}
function k(a) /*: Date -> Undef */ {
    var b = a.getHours();
    if (b >= 12) b -= 12;
    var c = a.getMinutes() + 60 * b;
    HourHand.rotation = c / 2;
}
function h(a) /*: Date -> Undef */ {
    var b = a.getSeconds() + 60 * a.getMinutes();
    MinuteHand.rotation = b / 10;
}

function l(a) /*: Date -> Undef */ {
    if (_UpdateSecondHandNumerval != 0) {
        clearNumerval(_UpdateSecondHandNumerval);
        _UpdateSecondHandNumerval = 0;
    }
    var b = a.getMilliseconds() + a.getSeconds() * 1000;
    _NewRotation = b * 0.0060;
    SecondHand.rotation = _NewRotation + _BounceRotationIncrement;
    _UpdateSecondHandNumerval = setNumerval(m, 50);
}
function m() /*:  -> Undef */ {
    SecondHand.rotation = _NewRotation;
}
function f() /*:  -> Undef */ {
    if (_SecondNumerval != 0) {
        clearNumerval(_SecondNumerval);
        _SecondNumerval = 0;
    }
    //switch (options("SecondHand")) {
    //Claudiu : options is callable?? or a bug
    switch (options.getValue("SecondHand")) {
    case 0:
        g(false);
        //break;
    case 2:
        g(true);
        e();
        _SecondNumerval = setNumerval(e, 25);
        //break;
    case 1:
        g(true);
        e();
        _SecondNumerval = setNumerval(e, 1000);
        //break;
    }
}

function g(a) /*: Bool -> Undef */ {
    var b = a ? 255 : 0;
    if (_SecondHandFade != 0) {
        cancelAnimation(_SecondHandFade);
    }
    if (b != SecondHand.opacity) {
        _SecondHandFade = beginAnimation(j, SecondHand.opacity, b, Math.abs(SecondHand.opacity - b) * 5);
    }
}
function j() /*:  -> Undef */ {
    SecondHand.opacity = event.value;
};
