options.putDefaultValue("SecondHand",1);
var _SecondInterval = 0,_minimized = false;
var _view_onopen = __typedjs(function  ()
                             {
                               f();
                               d();
                             },
                             undefined,
                             "_view_onopen",
                             "gadgets/AnalogClock_cc/0_compiled.js",
                             0);
var _view_onminimize = __typedjs(function  ()
                                 {
                                   _minimized = true;
                                   d();
                                   if (_SecondInterval != 0)
                                   {
                                     clearInterval(_SecondInterval);
                                     _SecondInterval = 0;
                                   };
                                 },
                                 undefined,
                                 "_view_onminimize",
                                 "gadgets/AnalogClock_cc/0_compiled.js",
                                 1);
var _view_onrestore = __typedjs(function  ()
                                {
                                  _minimized = false;
                                  view.caption = strings.PLUGIN_TITLE;
                                  f();
                                  d();
                                },
                                undefined,
                                "_view_onrestore",
                                "gadgets/AnalogClock_cc/0_compiled.js",
                                2);
var _view_onpopout = __typedjs(function  ()
                               {
                                 if (_minimized)
                                 {
                                   f();
                                   d();
                                 };
                               },
                               undefined,
                               "_view_onpopout",
                               "gadgets/AnalogClock_cc/0_compiled.js",
                               3);
var d = __typedjs(function  ()
                  {
                    var a = __new(Date,[]);
                    if (_minimized)
                    {
                      var b = a.getHours();
                      if (b > 12)
                      b -= 12;
                      if (b < 10)
                      b = "0" + b;
                      var c = a.getMinutes();
                      if (c < 10)
                      c = "0" + c;
                      view.caption = b + ":" + c;
                    }
                    else {
                           h(a);
                           k(a);
                         };
                    var i = (61 - a.getSeconds()) * 1000;
                    SetTimeout(d,i);
                  },
                  undefined,
                  "d",
                  "gadgets/AnalogClock_cc/0_compiled.js",
                  4);
var e = __typedjs(function  ()
                  {
                    var a = __new(Date,[]);
                    l(a);
                    h(a);
                  },
                  undefined,
                  "e",
                  "gadgets/AnalogClock_cc/0_compiled.js",
                  5);
var k = __typedjs(function  (a)
                  {
                    var b = a.getHours();
                    if (b >= 12)
                    b -= 12;
                    var c = a.getMinutes() + 60 * b;
                    HourHand.rotation = c / 2;
                  },
                  undefined,
                  "k",
                  "gadgets/AnalogClock_cc/0_compiled.js",
                  6);
var h = __typedjs(function  (a)
                  {
                    var b = a.getSeconds() + 60 * a.getMinutes();
                    MinuteHand.rotation = b / 10;
                  },
                  undefined,
                  "h",
                  "gadgets/AnalogClock_cc/0_compiled.js",
                  7);
var _UpdateSecondHandInterval = 0,
    _BounceRotationIncrement = 1.8,
    _NewRotation = 0;
var l = __typedjs(function  (a)
                  {
                    if (_UpdateSecondHandInterval != 0)
                    {
                      clearInterval(_UpdateSecondHandInterval);
                      _UpdateSecondHandInterval = 0;
                    };
                    var b = a.getMilliseconds() + a.getSeconds() * 1000;
                    _NewRotation = b * 0.6;
                    SecondHand.rotation = _NewRotation + _BounceRotationIncrement;
                    _UpdateSecondHandInterval = SetInterval(m,50);
                  },
                  undefined,
                  "l",
                  "gadgets/AnalogClock_cc/0_compiled.js",
                  8);
var m = __typedjs(function  ()
                  {
                    SecondHand.rotation = _NewRotation;
                  },
                  undefined,
                  "m",
                  "gadgets/AnalogClock_cc/0_compiled.js",
                  9);
var f = __typedjs(function  ()
                  {
                    if (_SecondInterval != 0)
                    {
                      clearInterval(_SecondInterval);
                      _SecondInterval = 0;
                    };
                    switch (options("SecondHand"))
                    {case
                     0 :
                       g(false);
                       break;
                     case
                     2 :
                       g(true);
                       e();
                       _SecondInterval = SetInterval(e,25);
                       break;
                     case
                     1 :
                       g(true);
                       e();
                       _SecondInterval = SetInterval(e,1000);
                       break;};
                  },
                  undefined,
                  "f",
                  "gadgets/AnalogClock_cc/0_compiled.js",
                  10);
var _SecondHandFade = 0;
var g = __typedjs(function  (a)
                  {
                    var b = a ? 255 : 0;
                    if (_SecondHandFade != 0)
                    {
                      cancelAnimation(_SecondHandFade);
                    };
                    if (b != SecondHand.opacity)
                    {
                      _SecondHandFade = beginAnimation(j,
                                                       SecondHand.opacity,
                                                       b,
                                                       Math.abs(SecondHand.opacity - b) * 5);
                    };
                  },
                  undefined,
                  "g",
                  "gadgets/AnalogClock_cc/0_compiled.js",
                  11);
var j = __typedjs(function  ()
                  {
                    SecondHand.opacity = event.value;
                  },
                  undefined,
                  "j",
                  "gadgets/AnalogClock_cc/0_compiled.js",
                  12);
;;
