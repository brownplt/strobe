ig_a = __typedjs(function  (a)
                 {
                   throw a;
                 },
                 undefined,
                 "ig_a",
                 0);
var ig_b = true,ig_ = null,ig_c = false,ig_d;
ig_e = __typedjs(function  (a)
                 {
                   var b = window;
                   if (a != "")
                   for (a = a.split("."); a.length;)
                   {
                     var c = a.shift();
                     if (typeof b[c] != "object")
                     b[c] = {};
                     b = b[c];
                   };
                   return b;
                 },
                 undefined,
                 "ig_e",
                 1);
(__typedjs(function  ()
           {
             a = __typedjs(function  (c,d)
                           {
                             c = ig_e(c);
                             for (var e = 0; e < d.length; e += 2)
                             c[d[e]] = d[e + 1];
                           },
                           arguments.callee,
                           "a",
                           0);
             b = __typedjs(function  (c,d,e,f,h)
                           {
                             if (f)
                             for (var g = 0; g < f.length; g += 2)
                             e.prototype[f[g]] = f[g + 1];
                             if (h)
                             for (g = 0; g < h.length; g += 2)
                             e[h[g]] = h[g + 1];
                             a(c,[d,e]);
                           },
                           arguments.callee,
                           "b",
                           1);
             a("",["_exportSymbols",a]);
             a("",["_exportClass",b]);
           },
           undefined,
           "",
           2))();
ig_f = __typedjs(function  (a,b,c)
                 {
                   if (! a.className)
                   return ig_c;
                   for (var d = a.className.split(" "),
                            e = [],
                            f = ig_c,
                            h = 0; h < d.length; ++h)
                   if (d[h] != b)
                   e.push(d[h])
                   else {
                          f = ig_b;
                          c && e.push(c);
                        };
                   a.className = e.join(" ");
                   return f;
                 },
                 undefined,
                 "ig_f",
                 3);
;;
ig_g = __typedjs(function  (a)
                 {
                   a = a || document.location.href;
                   var b = a.indexOf("?");
                   if (b == - 1)
                   return "";
                   var c = a.indexOf("#");
                   a = c == - 1 ? a.substr(b + 1) : a.substr(b + 1,
                                                             c - b - 1) + "&" + a.substr(c + 1);
                   return a.split("&");
                 },
                 undefined,
                 "ig_g",
                 4);
_esc = __typedjs(function  (a)
                 {
                   return window.encodeURIComponent ? encodeURIComponent(a) : escape(a);
                 },
                 undefined,
                 "_esc",
                 5);
_unesc = __typedjs(function  (a)
                   {
                     return window.decodeURIComponent ? decodeURIComponent(a) : unescape(a);
                   },
                   undefined,
                   "_unesc",
                   6);
_argsUrl = __typedjs(function  (a)
                     {
                       var b = {};
                       a = ig_g(a);
                       for (var c = 0; c < a.length; c++)
                       {
                         var d = a[c].indexOf("=");
                         if (d != - 1)
                         {
                           var e = a[c].substring(0,d);
                           d = a[c].substring(d + 1);
                           d = d.replace(/\+/g," ");
                           b[e] = _unesc(d);
                         };
                       };
                       return b;
                     },
                     undefined,
                     "_argsUrl",
                     7);
;;
var ig_aa = {domload: 1, xsetpdone: 1, moduleedit: 1, modulesocialedit: 1, modulecanceledit: 1, moduledelete: 1, moduleundelete: 1, modulezip: 1, moduleunzip: 1, modulemaximize: 1, moduleunmaximize: 1, moduledragstart: 1, moduledragend: 1, moduletitleclick: 1, load: 1, unload: 1, resize: 1},
    ig_h = {},
    ig_ba = {dragstart: 1, dragend: 1, titleclick: 1, edit: 1, socialedit: 1, canceledit: 1, "delete": 1, undelete: 1, zip: 1, unzip: 1, maximize: 1, unmaximize: 1};
ig_ca = __typedjs(function  (a)
                  {
                    if (a.hasOwnProperty && a.hasOwnProperty("ig_event_hashcode_"))
                    return a.ig_event_hashcode_;
                    a.ig_event_hashcode_ || (a.ig_event_hashcode_ = (++ig_i.ue));
                    return a.ig_event_hashcode_;
                  },
                  undefined,
                  "ig_ca",
                  8);
ig_da = __typedjs(function  (a)
                  {
                    return "builtin_" + a;
                  },
                  undefined,
                  "ig_da",
                  9);
ig_ea = __typedjs(function  (a,b)
                  {
                    if (a.indexOf && a.indexOf("m_") == 0)
                    a = a.substring(2);
                    return "builtin_m" + a + "_" + b;
                  },
                  undefined,
                  "ig_ea",
                  10);
ig_fa = __typedjs(function  (a,b)
                  {
                    return "builtin_" + ig_i.fe(a) + "_" + b;
                  },
                  undefined,
                  "ig_fa",
                  11);
ig_ga = __typedjs(function  (a)
                  {
                    return "custom_" + a;
                  },
                  undefined,
                  "ig_ga",
                  12);
ig_ha = __typedjs(function  ()
                  {
                    for (var a in ig_i.c)
                    {
                      for (var b = 0; b < ig_i.c[a].length; b++)
                      delete ig_i.c[a][b];
                      if (a in ig_i.oa)
                      {
                        var c = ig_i.oa[a];
                        b = c[0];
                        var d = c[1];
                        c = c[2];
                        if (b.removeEventListener)
                        b.removeEventListener(d,c,ig_c)
                        else b.detachEvent && b.detachEvent("on" + d,c);
                        delete ig_i.oa[a];
                      };
                    };
                    ig_h = {};
                  },
                  undefined,
                  "ig_ha",
                  13);
ig_ia = __typedjs(function  (a,b,c)
                  {
                    for (c = c; c < b.length; c++)
                    a[a.length] = b[c];
                    return a;
                  },
                  undefined,
                  "ig_ia",
                  14);
ig_ja = __typedjs(function  (a,b)
                  {
                    if (a in ig_i.c)
                    for (var c = 0; c < ig_i.c[a].length; c++)
                    if (ig_i.c[a][c] === b)
                    {
                      ig_i.c[a][c] = ig_;
                      return ig_b;
                    };
                    return ig_c;
                  },
                  undefined,
                  "ig_ja",
                  15);
ig_ka = __typedjs(function  (a)
                  {
                    if (a in ig_i.c)
                    for (var b = 0; b < ig_i.c[a].length; b++)
                    if (ig_i.c[a][b])
                    {
                      for (var c = [],d = 1; d < arguments.length; d++)
                      c[c.length] = arguments[d];
                      ig_i.c[a][b].apply(this,c);
                    };
                  },
                  undefined,
                  "ig_ka",
                  16);
ig_la = __typedjs(function  (a,b)
                  {
                    if (! (a in ig_i.Mf))
                    ig_a(Error("Unsupported event type: " + a));
                    a = ig_i.Wa(a);
                    a in ig_i.c || (ig_i.c[a] = []);
                    ig_i.c[a][ig_i.c[a].length] = b;
                    return ig_i.c[a].length;
                  },
                  undefined,
                  "ig_la",
                  17);
ig_ma = __typedjs(function  (a,b,c)
                  {
                    if (! (b in ig_i.Nf))
                    ig_a(Error("Unsupported module event type: " + b));
                    a = ig_i.Xa(a,b);
                    a in ig_i.c || (ig_i.c[a] = []);
                    ig_i.c[a][ig_i.c[a].length] = c;
                  },
                  undefined,
                  "ig_ma",
                  18);
ig_na = __typedjs(function  (a,b,c)
                  {
                    var d = ig_i.Ub(a,b);
                    if (! (d in ig_i.c))
                    {
                      ig_i.c[d] = [];
                      var e = __typedjs(function  (f)
                                        {
                                          if (! f)
                                          f = window.event;
                                          ig_i.Fa.apply(a,[d,f]);
                                        },
                                        arguments.callee,
                                        "e",
                                        0);
                      if (a.addEventListener)
                      a.addEventListener(b,e,ig_c)
                      else if (a.attachEvent)
                           a.attachEvent("on" + b,e)
                           else ig_a(Error("Object {" + a + "} does not support DOM events."));
                      ig_i.oa[d] = [a,b,e];
                    };
                    e = ig_i.c[d].length;
                    if (a === window && b == "unload" && e > 0)
                    {
                      ig_i.c[d][e] = ig_i.c[d][e - 1];
                      ig_i.c[d][e - 1] = c;
                    }
                    else if (ig_h[b])
                         setTimeout(c,10)
                         else ig_i.c[d][e] = c;
                  },
                  undefined,
                  "ig_na",
                  19);
ig_oa = __typedjs(function  (a,b)
                  {
                    a = ig_i.Va(a);
                    a in ig_i.c || (ig_i.c[a] = []);
                    ig_i.c[a][ig_i.c[a].length] = b;
                  },
                  undefined,
                  "ig_oa",
                  20);
ig_pa = __typedjs(function  (a,b)
                  {
                    a = ig_i.Wa(a);
                    return ig_i.Ca(a,b);
                  },
                  undefined,
                  "ig_pa",
                  21);
ig_qa = __typedjs(function  (a,b,c)
                  {
                    a = ig_i.Xa(a,b);
                    return ig_i.Ca(a,c);
                  },
                  undefined,
                  "ig_qa",
                  22);
ig_ra = __typedjs(function  (a,b,c)
                  {
                    a = ig_i.Ub(a,b);
                    return ig_i.Ca(a,c);
                  },
                  undefined,
                  "ig_ra",
                  23);
ig_sa = __typedjs(function  (a,b)
                  {
                    a = ig_i.Va(a);
                    return ig_i.Ca(a,b);
                  },
                  undefined,
                  "ig_sa",
                  24);
ig_ta = __typedjs(function  (a)
                  {
                    var b = a == "load" || a == "domload",c = ig_c;
                    if (b)
                    {
                      (c = ig_h[a]) || _IG_TriggerCustomEvent(a + "_start");
                      ig_h[a] = ig_b;
                    };
                    var d = ig_i.R([ig_i.Wa(a)],arguments,1);
                    ig_i.Fa.apply(window,d);
                    b && ! c && _IG_TriggerCustomEvent(a + "_end");
                  },
                  undefined,
                  "ig_ta",
                  25);
ig_ua = __typedjs(function  ()
                  {
                    var a = ig_i.R([],arguments,0);
                    setTimeout(__typedjs(function  ()
                                         {
                                           ig_i.cd.apply(window,a);
                                         },
                                         arguments.callee,
                                         "",
                                         0),
                               0);
                  },
                  undefined,
                  "ig_ua",
                  26);
ig_va = __typedjs(function  (a,b)
                  {
                    var c = ig_i.R([ig_i.Xa(a,b)],arguments,2);
                    ig_i.Fa.apply(window,c);
                  },
                  undefined,
                  "ig_va",
                  27);
ig_wa = __typedjs(function  ()
                  {
                    var a = ig_i.R([],arguments,0);
                    setTimeout(__typedjs(function  ()
                                         {
                                           ig_i.yb.apply(window,a);
                                         },
                                         arguments.callee,
                                         "",
                                         0),
                               0);
                  },
                  undefined,
                  "ig_wa",
                  28);
ig_xa = __typedjs(function  (a)
                  {
                    var b = ig_i.R([ig_i.Va(a)],arguments,1);
                    ig_i.Fa.apply(window,b);
                  },
                  undefined,
                  "ig_xa",
                  29);
ig_ya = __typedjs(function  ()
                  {
                    var a = ig_i.R([],arguments,0);
                    setTimeout(__typedjs(function  ()
                                         {
                                           ig_i.ad.apply(window,a);
                                         },
                                         arguments.callee,
                                         "",
                                         0),
                               0);
                  },
                  undefined,
                  "ig_ya",
                  30);
ig_j = __typedjs(function  (a,b)
                 {
                   switch (a)
                   {case
                    "delete" :
                    case
                    "undelete" :
                    case
                    "edit" :
                    case
                    "socialedit" :
                    case
                    "canceledit" :
                    case
                    "zip" :
                    case
                    "unzip" :
                    case
                    "maximize" :
                    case
                    "unmaximize" :
                      ig_wa(b,a);
                      ig_ua("module" + a,b);
                      break;};
                 },
                 undefined,
                 "ig_j",
                 31);
_IG_delayScript = __typedjs(function  (a,b)
                            {
                              c = __typedjs(function  ()
                                            {
                                              clearTimeout(d);
                                              _IG_RemoveEventHandler("load",c);
                                              setTimeout(a,0);
                                            },
                                            arguments.callee,
                                            "c",
                                            0);
                              var d;
                              b = ! isNaN(b) && b || 10000.0;
                              d = setTimeout(c,b);
                              _IG_AddEventHandler("load",c);
                            },
                            undefined,
                            "_IG_delayScript",
                            32);
var ig_i = {ue: 0, c: {}, oa: {}, Mf: ig_aa, Nf: ig_ba, fe: ig_ca, Wa: ig_da, Xa: ig_ea, Ub: ig_fa, Va: ig_ga, Pd: ig_ha, R: ig_ia, Ca: ig_ja, Fa: ig_ka, sd: ig_la, Ob: ig_ma, rd: ig_na, qd: ig_oa, jf: ig_pa, Oc: ig_qa, gf: ig_ra, ff: ig_sa, cd: ig_ta, yb: ig_va, ad: ig_xa, Uf: ig_ua, bd: ig_wa, Tf: ig_ya},
    _IG_AddEventHandler = ig_i.sd,
    _IG_AddModuleEventHandler = ig_i.Ob,
    _IG_AddGadgetEventHandler = ig_i.Ob,
    _IG_AddDOMEventHandler = ig_i.rd,
    _IG_AddCustomEventHandler = ig_i.qd,
    _IG_RemoveEventHandler = ig_i.jf,
    _IG_RemoveModuleEventHandler = ig_i.Oc,
    _IG_RemoveGadgetEventHandler = ig_i.Oc,
    _IG_RemoveDOMEventHandler = ig_i.gf,
    _IG_RemoveCustomEventHandler = ig_i.ff,
    _IG_TriggerEvent = ig_i.cd,
    _IG_TriggerModuleEvent = ig_i.yb,
    _IG_TriggerGadgetEvent = ig_i.yb,
    _IG_TriggerCustomEvent = ig_i.ad,
    _IG_TriggerDelayedEvent = ig_i.Uf,
    _IG_TriggerDelayedModuleEvent = ig_i.bd,
    _IG_TriggerDelayedGadgetEvent = ig_i.bd,
    _IG_TriggerDelayedCustomEvent = ig_i.Tf;
_IG_AddDOMEventHandler(window,"unload",ig_i.Pd);
_IG_AddDOMEventHandler(window,
                       "load",
                       __typedjs(function  ()
                                 {
                                   _IG_TriggerEvent("load");
                                 },
                                 undefined,
                                 "",
                                 33));
_IG_AddDOMEventHandler(window,
                       "unload",
                       __typedjs(function  ()
                                 {
                                   _IG_TriggerEvent("unload");
                                 },
                                 undefined,
                                 "",
                                 34));
_IG_AddDOMEventHandler(window,
                       "resize",
                       __typedjs(function  ()
                                 {
                                   _IG_TriggerEvent("resize");
                                 },
                                 undefined,
                                 "",
                                 35));
_IG_RegisterOnloadHandler = __typedjs(function  (a)
                                      {
                                        _IG_AddEventHandler("domload",a);
                                      },
                                      undefined,
                                      "_IG_RegisterOnloadHandler",
                                      36);
_IG_LoadLibraryDeferred = __typedjs(function  (a,b)
                                    {
                                      _IG_RegisterOnloadHandler(__typedjs(function  ()
                                                                          {
                                                                            _IG_LoadScript(a,b);
                                                                          },
                                                                          arguments.callee,
                                                                          "",
                                                                          0));
                                    },
                                    undefined,
                                    "_IG_LoadLibraryDeferred",
                                    37);
_exportSymbols("google.gadgets.Events",
               ["addHandler",
                _IG_AddEventHandler,
                "removeHandler",
                _IG_RemoveEventHandler,
                "addOnloadHandler",
                _IG_RegisterOnloadHandler]);
_IG_RegisterMaximizeHandler = __typedjs(function  (a,b)
                                        {
                                          _IG_AddModuleEventHandler(a,
                                                                    "unmaximize",
                                                                    __typedjs(function  ()
                                                                              {
                                                                                b(ig_c);
                                                                              },
                                                                              arguments.callee,
                                                                              "",
                                                                              0));
                                          _IG_AddModuleEventHandler(a,
                                                                    "maximize",
                                                                    __typedjs(function  ()
                                                                              {
                                                                                b(ig_b);
                                                                              },
                                                                              arguments.callee,
                                                                              "",
                                                                              1));
                                        },
                                        undefined,
                                        "_IG_RegisterMaximizeHandler",
                                        38);
;;
ig_k = __typedjs(function  (a,b)
                 {
                   c = __typedjs(function  ()
                                 {
                                 },
                                 arguments.callee,
                                 "c",
                                 0);
                   c.prototype = a.prototype;
                   b.prototype = new c();
                 },
                 undefined,
                 "ig_k",
                 39);
_gel = __typedjs(function  (a)
                 {
                   return document.getElementById(a);
                 },
                 undefined,
                 "_gel",
                 40);
_gelstn = __typedjs(function  (a,b)
                    {
                      b = b || document;
                      if (a == "*" && b.all)
                      return b.all;
                      return b.getElementsByTagName ? b.getElementsByTagName(a) : [];
                    },
                    undefined,
                    "_gelstn",
                    41);
_gelsbyregex = __typedjs(function  (a,b,c)
                         {
                           a = _gelstn(a,c);
                           c = [];
                           for (var d = 0,e; e = a[d]; ++d)
                           b.test(e.id) && c.push(e);
                           return c;
                         },
                         undefined,
                         "_gelsbyregex",
                         42);
_uc = __typedjs(function  (a)
                {
                  return a.toUpperCase();
                },
                undefined,
                "_uc",
                43);
_trim = __typedjs(function  (a)
                  {
                    return a.replace(/^\s*|\s*$/g,"");
                  },
                  undefined,
                  "_trim",
                  44);
_jesc = __typedjs(function  (a)
                  {
                    return a.replace(/\\/g,"\\\\").replace(/'/g,"\\\'").replace(/"/g,
                                                                                "\\\"");
                  },
                  undefined,
                  "_jesc",
                  45);
_toggle = __typedjs(function  (a)
                    {
                      if (a)
                      if (a.style.display == "" || a.style.display == "block")
                      a.style.display = "none"
                      else if (a.style.display == "none")
                           a.style.display = "block";
                    },
                    undefined,
                    "_toggle",
                    46);
_hesc = __typedjs(function  (a)
                  {
                    return a.replace(/</g,"&lt;").replace(/>/g,"&gt;").replace(/"/g,
                                                                               "&quot;").replace(/'/g,
                                                                                                 "&#39;");
                  },
                  undefined,
                  "_hesc",
                  47);
_striptags = __typedjs(function  (a)
                       {
                         return a.replace(/<\/?[^>]+>/gi,"");
                       },
                       undefined,
                       "_striptags",
                       48);
_ig_gmid_ = __typedjs(function  (a)
                      {
                        return a.split("_")[2];
                      },
                      undefined,
                      "_ig_gmid_",
                      49);
_showUserPrefs = __typedjs(function  (a,b)
                           {
                             if (b)
                             {
                               ig_l("m_" + a + "_editbox","none");
                               ig_l("m_" + a + "_aclbox","block");
                               ig_j("socialedit","m_" + a);
                             }
                             else {
                                    ig_l("m_" + a + "_editbox","");
                                    ig_l("m_" + a + "_aclbox","none");
                                    ig_j("edit","m_" + a);
                                  };
                             if (a = _gel("m_" + a))
                             a.className = "modbox_e";
                           },
                           undefined,
                           "_showUserPrefs",
                           50);
_closeUserPrefs = __typedjs(function  (a)
                            {
                              ig_l("m_" + a + "_editbox","none");
                              ig_l("m_" + a + "_aclbox","none");
                              if (a = _gel("m_" + a))
                              a.className = "modbox";
                            },
                            undefined,
                            "_closeUserPrefs",
                            51);
_min = __typedjs(function  (a,b)
                 {
                   return a < b ? a : b;
                 },
                 undefined,
                 "_min",
                 52);
_max = __typedjs(function  (a,b)
                 {
                   return a > b ? a : b;
                 },
                 undefined,
                 "_max",
                 53);
_IG_LoadScript = __typedjs(function  (a,b)
                           {
                             _sendx(a,
                                    __typedjs(function  (c)
                                              {
                                                window.eval(c);
                                                eval(b);
                                              },
                                              arguments.callee,
                                              "",
                                              0));
                           },
                           undefined,
                           "_IG_LoadScript",
                           54);
_IG_LoadScriptCallback = __typedjs(function  (a,b)
                                   {
                                     _sendx(a,
                                            __typedjs(function  (c)
                                                      {
                                                        window.eval(c);
                                                        b && b();
                                                      },
                                                      arguments.callee,
                                                      "",
                                                      0));
                                   },
                                   undefined,
                                   "_IG_LoadScriptCallback",
                                   55);
_IG_LoadScriptXDomain = __typedjs(function  (a,b,c)
                                  {
                                    d = __typedjs(function  ()
                                                  {
                                                    eval(b) ? c() : setTimeout(__typedjs(function  ()
                                                                                         {
                                                                                           d();
                                                                                         },
                                                                                         arguments.callee,
                                                                                         "",
                                                                                         0),
                                                                               100);
                                                  },
                                                  arguments.callee,
                                                  "d",
                                                  0);
                                    var e = document.createElement("script");
                                    e.src = a;
                                    _gelstn("head")[0].appendChild(e);
                                    b && d();
                                  },
                                  undefined,
                                  "_IG_LoadScriptXDomain",
                                  56);
_IG_LoadCss = __typedjs(function  (a)
                        {
                          var b = _gelstn("head")[0];
                          if (! b)
                          ig_a(new Error("Document is missing <head> element.  Failed to load CSS."));
                          var c = document.createElement("style");
                          c.type = "text/css";
                          if (c.styleSheet)
                          c.styleSheet.cssText = a
                          else c.appendChild(document.createTextNode(a));
                          b.insertBefore(c,b.firstChild);
                        },
                        undefined,
                        "_IG_LoadCss",
                        57);
_log = __typedjs(function  (a)
                 {
                   a = "/ig/log?msg=" + _esc(a);
                   _sendx(a);
                 },
                 undefined,
                 "_log",
                 58);
_sendx = __typedjs(function  (a,b,c,d)
                   {
                     var e = ig_m();
                     e.open(d ? "POST" : "GET",a,ig_b);
                     if (b)
                     e.onreadystatechange = __typedjs(function  ()
                                                      {
                                                        if (e.readyState == 4)
                                                        b(c && e.responseXML ? e.responseXML : e.responseText);
                                                      },
                                                      arguments.callee,
                                                      "e.onreadystatechange",
                                                      0);
                     e.send(d || ig_);
                   },
                   undefined,
                   "_sendx",
                   59);
ig_m = __typedjs(function  ()
                 {
                   var a = ig_;
                   if (window.ActiveXObject)
                   (a = new ActiveXObject("Msxml2.XMLHTTP")) || (a = new ActiveXObject("Microsoft.XMLHTTP"))
                   else if (window.XMLHttpRequest)
                        a = new XMLHttpRequest();
                   return a;
                 },
                 undefined,
                 "ig_m",
                 60);
var _et = "",
    _brand = "",
    _bmod = "",
    _source = "",
    _ae = "",
    _pid = "",
    _authpath = "",
    _prefid = "",
    _setp_url = "/ig/setp",
    _hl = "",
    _old_html = ig_b,
    _use_old_feed_styles = ig_b,
    ig_n = ig_,
    ig_o = ig_;
ig_p = __typedjs(function  (a,b,c)
                 {
                   var d = a.elements[b];
                   if (d)
                   d.value = c
                   else {
                          d = document.createElement("input");
                          d.type = "hidden";
                          d.name = b;
                          d.value = c;
                          a.appendChild(d);
                        };
                 },
                 undefined,
                 "ig_p",
                 61);
_args = __typedjs(function  ()
                  {
                    return _argsUrl(document.location.href);
                  },
                  undefined,
                  "_args",
                  62);
_IG_isIE = __typedjs(function  ()
                     {
                       return /msie/i.test(navigator.userAgent);
                     },
                     undefined,
                     "_IG_isIE",
                     63);
_qs = __typedjs(function  (a)
                {
                  var b = {pid: 1, host: 1, gl: 1, hl: 1, enable_flag: 1, e: 1, expid: 1};
                  a = a || _args();
                  var c = ["?"];
                  for (var d in a)
                  b[d] && c.push(d,"=",_esc(a[d]),"&");
                  return c.join("");
                },
                undefined,
                "_qs",
                64);
var ig_za = /^([^?#]+)(?:\?([^#]*))?(#.*)?/;
ig_q = __typedjs(function  (a,b)
                 {
                   a = ig_za.exec(a);
                   var c = [a[1],"?",a[2]];
                   c.push(a[2] ? "&" : "",b.join("&"),a[3]);
                   return c.join("");
                 },
                 undefined,
                 "ig_q",
                 65);
ig_Aa = __typedjs(function  (a)
                  {
                    var b = {url: document.location, et: _et, brand: _brand, bmod: _bmod, source: _source, ae: _ae, pid: _pid, ap: _authpath, prefid: _prefid};
                    for (var c in b)
                    b[c] && ig_p(a,c,b[c]);
                  },
                  undefined,
                  "ig_Aa",
                  66);
_submit_form = __typedjs(function  (a,b)
                         {
                           a.action = _setp_url + _qs() + b;
                           a.method = "get";
                           ig_Aa(a);
                           a.submit();
                         },
                         undefined,
                         "_submit_form",
                         67);
_fsetp = __typedjs(function  (a,b,c)
                   {
                     a.action = _setp_url;
                     a.method = "get";
                     ig_Aa(a);
                     ig_p(a,"m_" + b + "_t",c);
                     c = _args();
                     b = c.host;
                     c = c.hl;
                     b && ig_p(a,"host",b);
                     c && ig_p(a,"hl",c);
                     return ig_b;
                   },
                   undefined,
                   "_fsetp",
                   68);
var ig_r = [],ig_s = ig_c;
ig_Ba = __typedjs(function  ()
                  {
                    var a;
                    a = [["et",_et],
                         ["pid",_pid],
                         ["ap",_authpath],
                         ["brand",_brand],
                         ["bmod",_bmod],
                         ["source",_source],
                         ["ae",_ae],
                         ["prefid",_prefid]];
                    for (var b = [],c = 0; c < a.length; ++c)
                    if (a[c].length == 2)
                    {
                      var d = a[c][0],e = a[c][1];
                      e != ig_ && e != "" && b.push(d + "=" + e);
                    };
                    return a = b.join("&");
                  },
                  undefined,
                  "ig_Ba",
                  69);
ig_Ca = __typedjs(function  ()
                  {
                    if (ig_r.length == 0)
                    ig_s = ig_c
                    else {
                           ig_s = ig_b;
                           var a = _setp_url + _qs() + ig_Ba(),b = ig_r.join("&");
                           ig_r = [];
                           var c = a.length + b.length >= 1800;
                           if (c)
                           _sendx(a,ig_Da,ig_c,b)
                           else {
                                  a += "&" + b;
                                  _sendx(a,ig_Da,ig_c,ig_);
                                };
                         };
                  },
                  undefined,
                  "ig_Ca",
                  70);
ig_Da = __typedjs(function  ()
                  {
                    _IG_TriggerDelayedEvent("xsetpdone");
                    ig_Ca();
                  },
                  undefined,
                  "ig_Da",
                  71);
_xsetp = __typedjs(function  (a)
                   {
                     ig_r.push(a);
                     ig_s || ig_Ca();
                   },
                   undefined,
                   "_xsetp",
                   72);
_xsetp_is_done = __typedjs(function  ()
                           {
                             return ig_r.length == 0 && ig_s == ig_c;
                           },
                           undefined,
                           "_xsetp_is_done",
                           73);
_dlsetp = __typedjs(function  (a,b)
                    {
                      b || (b = _esc(document.location));
                      document.location = _setp_url + _qs() + ig_Ba() + "&url=" + b + "&" + a;
                    },
                    undefined,
                    "_dlsetp",
                    74);
_removeTabParam = __typedjs(function  (a)
                            {
                              a = a.split("#");
                              a[0] = a[0].replace(/([?&])t=[^&#]*([&#]|$)/g,"$1");
                              a[0] = a[0].replace(/[?&]$/,"");
                              return a.join("#");
                            },
                            undefined,
                            "_removeTabParam",
                            75);
_select_tab = __typedjs(function  (a,b)
                        {
                          b = b || "/ig";
                          ig_t(b + _qs() + "t=" + a);
                        },
                        undefined,
                        "_select_tab",
                        76);
_select_tab_and_log = __typedjs(function  (a,b)
                                {
                                  _sendx("/ig/ui?xp=v2&action=selectTab&t=" + a);
                                  _select_tab(a,b);
                                },
                                undefined,
                                "_select_tab_and_log",
                                77);
_select_gadget_and_log = __typedjs(function  (a,b)
                                   {
                                     _sendx("/ig/ui?xp=v2&action=maximizeMod&from=selectMod&mid=" + b);
                                     window.nd ? ig_t("/ig" + _qs() + "t=" + a + "&g=" + b) : ig_t("/ig" + _qs() + "t=" + a + "#max" + b);
                                   },
                                   undefined,
                                   "_select_gadget_and_log",
                                   78);
ig_t = __typedjs(function  (a)
                 {
                   window.location.href = a;
                 },
                 undefined,
                 "ig_t",
                 79);
ig_l = __typedjs(function  (a,b)
                 {
                   if (a = _gel(a))
                   a.style.display = b;
                 },
                 undefined,
                 "ig_l",
                 80);
_exportSymbols("",["setDisplayStyle",ig_l]);
_ssbc = __typedjs(function  (a,b,c)
                  {
                    for (var d = _gelstn("*"),e = 0,f; f = d[e]; e++)
                    if (f.className == a)
                    f.style[b] = c;
                  },
                  undefined,
                  "_ssbc",
                  81);
_findPos = __typedjs(function  (a)
                     {
                       for (var b = 0,c = 0; a != ig_;)
                       {
                         b += a.offsetLeft;
                         c += a.offsetTop;
                         a = a.offsetParent;
                       };
                       return [b,c];
                     },
                     undefined,
                     "_findPos",
                     82);
_getGadgetContainer = __typedjs(function  (a)
                                {
                                  var b = _gel("m_" + a + "_b");
                                  if (! b)
                                  {
                                    b = _gel("remote_" + a);
                                    if (! b)
                                    b = document.body;
                                  };
                                  return b;
                                },
                                undefined,
                                "_getGadgetContainer",
                                83);
_appendMaxAgeParameter = __typedjs(function  (a,b)
                                   {
                                     var c = /[?&]$/.test(a);
                                     return a + (c ? "" : "&") + "max_age=" + b + (c ? "&" : "");
                                   },
                                   undefined,
                                   "_appendMaxAgeParameter",
                                   84);
_getStyle = __typedjs(function  (a,b)
                      {
                        if (typeof a.currentStyle != "undefined")
                        a = a.currentStyle
                        else if (document.defaultView.getComputedStyle != "undefined")
                             a = document.defaultView.getComputedStyle(a,ig_)
                             else return "";
                        return a[b];
                      },
                      undefined,
                      "_getStyle",
                      85);
ig_u = __typedjs(function  ()
                 {
                   for (var a = _IG_MD,b = 0; b < arguments.length; ++b)
                   {
                     a = a[arguments[b]];
                     if (a == ig_)
                     return ig_;
                   };
                   return a;
                 },
                 undefined,
                 "ig_u",
                 86);
_bidi_span_wrap = __typedjs(function  (a,b)
                            {
                              return _bidi_span_wrap_html(_hesc(a),b);
                            },
                            undefined,
                            "_bidi_span_wrap",
                            87);
_bidi_span_wrap_html = __typedjs(function  (a,b)
                                 {
                                   if (b == "rtl")
                                   return "<span dir=rtl>" + a + "</span>&lrm;";
                                   return a;
                                 },
                                 undefined,
                                 "_bidi_span_wrap_html",
                                 88);
_bidi_unicode_wrap = __typedjs(function  (a,b)
                               {
                                 if (b == "rtl")
                                 return "+" + a + ",";
                                 return a;
                               },
                               undefined,
                               "_bidi_unicode_wrap",
                               89);
_exportSymbols("google.gadgets.Util",["args",_args]);
_exportSymbols("",["addParamPairsToUrl",ig_q,"xmlhttp",ig_m]);
if (! window.FLAG_isInSplitJS)
window.FLAG_isInSplitJS = ig_c;
ig_v = __typedjs(function  (a)
                 {
                   this.A = [];
                   this.jb = a;
                   this.ta("_IG_DD_open");
                   this.ta("_IG_hoverHeader");
                   this.ta("_IG_hoverModule");
                   this.O("_IG_DD_create");
                   this.O("_IG_DD_init");
                   this.O("_IG_initDrag");
                   this.O("_IG_PushHistory");
                   this.O("_IG_PU_MakeDialogButton");
                 },
                 undefined,
                 "ig_v",
                 90);
ig_v.prototype.Mc = ig_c;
ig_v.prototype.cc = ig_c;
ig_v.jd = 10000.0;
ig_d = ig_v.prototype;
ig_d.O = __typedjs(function  (a)
                   {
                     if (this.jb)
                     {
                       var b = window,c = a.lastIndexOf(".");
                       if (c >= 0)
                       {
                         b = ig_e(a.substring(0,c));
                         a = a.substring(c + 1);
                       };
                       if (b[a] && ! b[a].zc)
                       ig_a(new Error(a + " is already defined."));
                       b[a] = this.Ld(a);
                     };
                   },
                   undefined,
                   "ig_d.O",
                   91);
ig_d.ta = __typedjs(function  (a)
                    {
                      if (this.jb)
                      {
                        var b = window,c = a.lastIndexOf(".");
                        if (c >= 0)
                        {
                          b = ig_e(a.substring(0,c));
                          a = a.substring(c + 1);
                        };
                        b[a] = __typedjs(function  ()
                                         {
                                         },
                                         arguments.callee,
                                         "",
                                         0);
                      };
                    },
                    undefined,
                    "ig_d.ta",
                    92);
ig_d.Ld = __typedjs(function  (a)
                    {
                      var b = this;
                      c = __typedjs(function  ()
                                    {
                                      for (var d = [],e = 0; e < arguments.length; e++)
                                      d.push(arguments[e]);
                                      b.A.push([a,this,d]);
                                    },
                                    arguments.callee,
                                    "c",
                                    0);
                      c.zc = ig_b;
                      return c;
                    },
                    undefined,
                    "ig_d.Ld",
                    93);
ig_d.Qb = __typedjs(function  ()
                    {
                      if (this.A.length != 0)
                      {
                        var a = this.A.shift(),b = a[0],c = a[1];
                        a = a[2];
                        try
                        {
                          var d = b instanceof Function ? b : window[b];
                          d.apply(c,a);
                        }
                        catch (e) {
                                  };
                        if (this.A.length > 0)
                        {
                          var f = this;
                          setTimeout(__typedjs(function  ()
                                               {
                                                 f.Qb();
                                               },
                                               arguments.callee,
                                               "",
                                               0),
                                     0);
                        }
                        else {
                               this.cc = ig_b;
                               this.hf();
                             };
                      };
                    },
                    undefined,
                    "ig_d.Qb",
                    94);
ig_d.Hd = __typedjs(function  ()
                    {
                      for (var a = [],b = 0; b < this.A.length; b++)
                      {
                        var c = this.A[b][0];
                        if (! (c instanceof Function))
                        if (window[c].zc)
                        {
                          a.push(c);
                          this.A.splice(b,1);
                          b--;
                        };
                      };
                      var d = this;
                      setTimeout(__typedjs(function  ()
                                           {
                                             d.Qb();
                                           },
                                           arguments.callee,
                                           "",
                                           0),
                                 0);
                      if (a.length > 0)
                      ig_a(new Error("The following functions were not found in the post load file: " + a.join(", ")));
                    },
                    undefined,
                    "ig_d.Hd",
                    95);
ig_d.hf = __typedjs(function  ()
                    {
                      for (var a = _gelstn("*"),b = 0; b < a.length; b++)
                      {
                        var c = a[b];
                        c.className && c.className.split && ig_f(c,"disabled",ig_);
                      };
                    },
                    undefined,
                    "ig_d.hf",
                    96);
ig_d.Me = __typedjs(function  (a,b)
                    {
                      b = this.Le(b);
                      _IG_AddEventHandler(a,b);
                      setTimeout(b,ig_v.jd);
                    },
                    undefined,
                    "ig_d.Me",
                    97);
ig_d.Le = __typedjs(function  (a)
                    {
                      var b = this;
                      return __typedjs(function  ()
                                       {
                                         if (! b.Mc)
                                         {
                                           _IG_LoadScriptXDomain(a,
                                                                 "window._IG_DD_position",
                                                                 __typedjs(function  ()
                                                                           {
                                                                             b.Hd();
                                                                           },
                                                                           arguments.callee,
                                                                           "",
                                                                           0));
                                           b.Mc = ig_b;
                                         };
                                       },
                                       arguments.callee,
                                       "",
                                       0);
                    },
                    undefined,
                    "ig_d.Le",
                    98);
ig_d.Dd = __typedjs(function  (a)
                    {
                      this.cc || ! this.jb ? a() : this.A.push([a,ig_,[]]);
                    },
                    undefined,
                    "ig_d.Dd",
                    99);
var ig_w;
ig_w || (ig_w = new ig_v(window.FLAG_isInSplitJS));
_IG_callPostLoad = __typedjs(function  (a)
                             {
                               ig_w.Dd(a);
                             },
                             undefined,
                             "_IG_callPostLoad",
                             100);
_IG_deferFunction = __typedjs(function  (a)
                              {
                                ig_w.O(a);
                              },
                              undefined,
                              "_IG_deferFunction",
                              101);
_IG_ignoreFunction = __typedjs(function  (a)
                               {
                                 ig_w.ta(a);
                               },
                               undefined,
                               "_IG_ignoreFunction",
                               102);
_IG_loadPostloadJS = __typedjs(function  (a,b)
                               {
                                 ig_w.Me(a,b);
                               },
                               undefined,
                               "_IG_loadPostloadJS",
                               103);
;;
_edit = __typedjs(function  (a,b)
                  {
                    c = __typedjs(function  ()
                                  {
                                    b && b();
                                    _sendx("/ig/ui?action=editsettings&m=" + a);
                                    ig_j("edit","m_" + a);
                                  },
                                  arguments.callee,
                                  "c",
                                  0);
                    var d = ig_Ea[a];
                    if (d)
                    {
                      d();
                      c();
                    }
                    else _IG_LL_load("m_" + a + "_editbox",
                                     {mid: a},
                                     __typedjs(function  ()
                                               {
                                                 _showUserPrefs(a,ig_c);
                                                 c();
                                               },
                                               arguments.callee,
                                               "",
                                               1));
                    return ig_c;
                  },
                  undefined,
                  "_edit",
                  104);
var ig_Ea = {};
_IG_AddCustomEditHandler = __typedjs(function  (a,b)
                                     {
                                       ig_Ea[a] = b;
                                     },
                                     undefined,
                                     "_IG_AddCustomEditHandler",
                                     105);
_cedit = __typedjs(function  (a)
                   {
                     _closeUserPrefs(a);
                     var b = _gel("m_" + a + "_form");
                     if (b)
                     {
                       b.reset();
                       _sendx("/ig/ui?action=canceledit&m=" + a);
                       ig_j("canceledit","m_" + a);
                       return ig_c;
                     };
                   },
                   undefined,
                   "_cedit",
                   106);
_uhc = __typedjs(function  (a,b,c)
                 {
                   b = "m_" + a + "_" + b;
                   var d = _gel(b);
                   if (! d)
                   {
                     d = document.createElement("INPUT");
                     d.type = "hidden";
                     d.disabled = ig_b;
                     d.name = b;
                     _gel("m_" + a + "_form").appendChild(d);
                   };
                   d.value = c;
                   d.disabled = ig_c;
                 },
                 undefined,
                 "_uhc",
                 107);
_confirmDel = __typedjs(function  (a,b,c,d)
                        {
                          var e = _bidi_unicode_wrap(_get_module_title(a),
                                                     _get_module_title_dir(a));
                          d = d.replace(/MODULE_TITLE/,e);
                          confirm(d) && _del(a,b,c);
                        },
                        undefined,
                        "_confirmDel",
                        108);
_get_module_title = __typedjs(function  (a)
                              {
                                if (typeof a == "object")
                                a = a.id.replace("m_","")
                                else if (typeof a == "string")
                                     a = a.replace("m_","");
                                if (typeof _IG_MD != "undefined")
                                return _IG_MD.m[a].ti
                                else return (a = _gel("m_" + a + "_title")) ? _striptags(a.innerHTML) : "";
                              },
                              undefined,
                              "_get_module_title",
                              109);
_get_module_title_dir = __typedjs(function  (a)
                                  {
                                    if (typeof a == "object")
                                    a = a.id.replace("m_","")
                                    else if (typeof a == "string")
                                         a = a.replace("m_","");
                                    if (typeof _IG_MD != "undefined")
                                    return _IG_MD.m[a].ti_dir;
                                  },
                                  undefined,
                                  "_get_module_title_dir",
                                  110);
_get_all_modules = __typedjs(function  ()
                             {
                               var a,b = _gelsbyregex("DIV",/^c_[0-9]+$/);
                               a = [];
                               for (var c = 0; c < b.length; c++)
                               b[c].style.display != "none" && a.push(b[c]);
                               a = a;
                               b = [];
                               c = /^m_[0-9]+$/;
                               for (var d = 0; d < a.length; d++)
                               {
                                 b[d] = [];
                                 for (var e = b[d],f = 0; f < a[d].childNodes.length; f++)
                                 {
                                   var h = a[d].childNodes[f];
                                   h.tagName == "DIV" && h.style.display != "none" && h.id && c.test(h.id) && e.push(h);
                                 };
                               };
                               if (window.gadgets && window.gadgets.views && window.gadgets.views.ViewManager && window.gadgets.views.ViewManager.getCurrentMaxId)
                               (a = window.gadgets.views.ViewManager.getCurrentMaxId()) && b.length > 0 && a && b[0].unshift(_gel("m_" + a));
                               return b;
                             },
                             undefined,
                             "_get_all_modules",
                             111);
var ig_x = ig_c;
_del_is_done = __typedjs(function  ()
                         {
                           return _xsetp_is_done() && ig_x;
                         },
                         undefined,
                         "_del_is_done",
                         112);
_del = __typedjs(function  (a,b,c)
                 {
                   ig_x = ig_c;
                   _xsetp("m_" + a + "_enab=0&m_" + a + "_t=" + b);
                   if (b = _gel("undel_msg"))
                   {
                     _gel("undel_title").innerHTML = "&quot;" + _bidi_span_wrap(_get_module_title(a),
                                                                                _get_module_title_dir(a)) + "&quot; ";
                     b.style.display = "block";
                     ig_l("undo_msg","none");
                     ig_l("undo_restore_msg","none");
                   };
                   ig_l("m_" + a,"none");
                   ig_n = a;
                   ig_o = c;
                   ig_l(c,"");
                   _mod = ig_b;
                   ig_j("delete","m_" + a);
                   ig_x = ig_b;
                   return ig_c;
                 },
                 undefined,
                 "_del",
                 113);
_undel = __typedjs(function  ()
                   {
                     if (ig_n != ig_)
                     {
                       var a = "m_" + ig_n,b = _gel(a);
                       if (b)
                       {
                         ig_l(a,"block");
                         _xsetp("undel");
                       };
                       ig_l("undel_msg","none");
                       ig_o != ig_ && ig_l(ig_o,"none");
                       ig_o = ig_n = ig_;
                       ig_j("undelete",a);
                     };
                   },
                   undefined,
                   "_undel",
                   114);
_isModuleZipped = __typedjs(function  (a)
                            {
                              if (a = _gel("m_" + a + "_b"))
                              return a.style.display == "none";
                              return ig_c;
                            },
                            undefined,
                            "_isModuleZipped",
                            115);
_zm = __typedjs(function  (a,b,c)
                {
                  var d = _gel("m_" + a + "_b");
                  if (d)
                  {
                    var e = ! _isModuleZipped(a);
                    d.style.display = e ? "none" : "block";
                    if (d = _gel("m_" + a + "_zippy"))
                    d.className = e ? d.className.replace(/minbox/,
                                                          "maxbox") : d.className.replace(/maxbox/,
                                                                                          "minbox");
                    c = c ? "mmz" : "mz";
                    _xsetp(c + "=" + a + ":" + (e ? "1" : "0") + "&t=" + b);
                    ig_j(e ? "zip" : "unzip",a);
                  };
                  return ig_c;
                },
                undefined,
                "_zm",
                116);
var _uli,
    _pnlo,
    _mpnlo,
    _pl,
    _mod,
    _cbp = ig_c,
    ig_Fa = ig_c,
    _table = ig_,
    _tabs = ig_;
_upc = __typedjs(function  ()
                 {
                   var a = [];
                   _cbp || (a[a.length] = ["medit","display",_uli ? "" : "none"]);
                   a[a.length] = ["panelo","display",_pnlo ? "" : "none"];
                   a[a.length] = ["panelc","display",_pnlo ? "none" : ""];
                   if (_mod)
                   {
                     a[a.length] = ["unmod","display","none"];
                     a[a.length] = ["mod","display",""];
                   }
                   else {
                          a[a.length] = ["mod","display","none"];
                          a[a.length] = ["unmod","display",""];
                        };
                   a = a;
                   for (var b = _gelstn("*"),c = 0,d; d = b[c]; c++)
                   for (var e = 0; e < a.length; e++)
                   if (d.className == a[e][0])
                   d.style[a[e][1]] = a[e][2];
                   _IG_callPostLoad(_init_drag_drop);
                 },
                 undefined,
                 "_upc",
                 117);
_init_drag_drop = __typedjs(function  ()
                            {
                              if (_pl)
                              if (_cbp || _uli)
                              if (! ig_Fa && ! _mpnlo)
                              {
                                _IG_TriggerCustomEvent("initdrag",_table,_tabs);
                                ig_Fa = ig_b;
                              };
                            },
                            undefined,
                            "_init_drag_drop",
                            118);
var ig_y = 0;
_tp = __typedjs(function  (a)
                {
                  ig_y > 0 && clearInterval(ig_y);
                  _pnlo = a;
                  _mod = ig_b;
                  _xsetp("pnlo=" + (a ? 1 : 0));
                  _upc();
                  var b = _gel("cpnl"),c = _gel("cpnlc"),d = b.offsetWidth,e,f;
                  if (a)
                  {
                    e = c.offsetWidth;
                    f = "visible";
                    ig_l("ehdr","");
                    ig_l("nhdr","none");
                  }
                  else {
                         e = 1;
                         f = "hidden";
                         ig_l("ehdr","none");
                         ig_l("nhdr","");
                       };
                  b.style.overflow = "hidden";
                  a = 100;
                  var h = 10,g = 0;
                  ig_y = setInterval(__typedjs(function  ()
                                               {
                                                 var k = g / h,j = d + (e - d) * k;
                                                 b.style.width = j + "px";
                                                 g++;
                                                 if (k >= 1)
                                                 {
                                                   clearInterval(ig_y);
                                                   ig_y = 0;
                                                   b.style.width = e + "px";
                                                   b.style.overflow = f;
                                                 };
                                               },
                                               arguments.callee,
                                               "ig_y",
                                               0),
                                     a / h);
                  return ig_c;
                },
                undefined,
                "_tp",
                119);
_ts = __typedjs(function  (a,b)
                {
                  a = _gel(a + b);
                  if (a.className == "mlist_open")
                  {
                    a.className = "mlist_closed";
                    a = "pnlsc";
                  }
                  else {
                         a.className = "mlist_open";
                         a = "pnlso";
                       };
                  _xsetp(a + "=" + _esc(b));
                  return ig_c;
                },
                undefined,
                "_ts",
                120);
_add_m = __typedjs(function  (a,b)
                   {
                     _dlsetp(a,b);
                   },
                   undefined,
                   "_add_m",
                   121);
_add_m_confirm = __typedjs(function  (a,b,c)
                           {
                             confirm(b) && _add_m(a,c);
                           },
                           undefined,
                           "_add_m_confirm",
                           122);
_add_f = __typedjs(function  (a)
                   {
                     _add_m("n_25=" + _esc("url=" + _esc(a)));
                   },
                   undefined,
                   "_add_f",
                   123);
var ig_Ga = /^_add_m(_confirm)?\(\"[^"]+\"(, *\"[^"]+\")?\)$/,
    ig_z = ig_b;
_find_feed_is_done = __typedjs(function  ()
                               {
                                 return ig_z;
                               },
                               undefined,
                               "_find_feed_is_done",
                               124);
_find_feed = __typedjs(function  (a)
                       {
                         ig_z = ig_c;
                         var b = "acd";
                         if (! a)
                         {
                           var c = _gelstn("div");
                           if (c)
                           for (var d = 0,e; e = c[d]; d++)
                           e.id && e.id.indexOf("ps") == 0 && e.className == "mlist_open" && _ts("ps",
                                                                                                 e.id.substring(2));
                           if (_gel("add_custom"))
                           a = _gel("add_custom").value;
                         };
                         if (! a || a == "")
                         {
                           b = "advdsrch";
                           a = _gel("add_advd").value;
                         };
                         ig_l("ffresults","none");
                         ig_l("ffloading","block");
                         _sendx("/ig/feeds" + _qs() + "q=" + _esc(a) + "&page=" + _esc(b),
                                ig_Ha);
                         return ig_c;
                       },
                       undefined,
                       "_find_feed",
                       125);
ig_Ha = __typedjs(function  (a)
                  {
                    ig_l("ffloading","none");
                    ig_l("ffresults","block");
                    if (a.length > 0 && a.charAt(0) == "<")
                    {
                      if (_gel("ffresults"))
                      _gel("ffresults").innerHTML = a;
                    }
                    else {
                           a.match(ig_Ga) != ig_;
                           eval(a);
                         };
                    ig_z = ig_b;
                  },
                  undefined,
                  "ig_Ha",
                  126);
_add_remote_module = __typedjs(function  (a,b,c)
                               {
                                 _sendx("/ig/feeds" + _qs() + "module=1&q=" + _esc(a),
                                        __typedjs(function  (d)
                                                  {
                                                    b();
                                                    d = d;
                                                    var e = c,
                                                        f = /_add_m_confirm\((\"[^"]+\"),\s*(\"[^"]+\")(,\s*\"[^"]+\")?\)/,
                                                        h = /^alert\(\"[^"]+\"\)$/;
                                                    if (d.match(ig_Ga) != ig_ || d.match(h) != ig_)
                                                    {
                                                      if (e)
                                                      d = d.replace(f,"_add_m($1$3)");
                                                      eval(d);
                                                    };
                                                  },
                                                  arguments.callee,
                                                  "",
                                                  0));
                                 return ig_c;
                               },
                               undefined,
                               "_add_remote_module",
                               127);
_ListApp = __typedjs(function  (a,b,c)
                     {
                       this.items = a;
                       this.na = [];
                       this.Dc = b;
                       this.ba = c;
                       this.Ra = "m_" + c + "_App";
                       this.fc = _gel("m_" + c + "_disp");
                       this.Jf = ig_b;
                       this.U = _gel("m_" + c + "_val");
                       this.H = _gel("m_" + c + "_name");
                       if (! this.H)
                       this.H = this.U;
                       if (this.H)
                       {
                         this.$b = this.H.value;
                         this.bc = this.U.value;
                       };
                       this.Nc = [].concat(this.items);
                     },
                     undefined,
                     "_ListApp",
                     128);
ig_d = _ListApp.prototype;
ig_d.reset = __typedjs(function  ()
                       {
                         this.items = [].concat(this.Nc);
                         this.na = [];
                         this.refresh();
                       },
                       undefined,
                       "ig_d.reset",
                       129);
ig_d.sort = __typedjs(function  (a,b)
                      {
                        return a.Ua(b);
                      },
                      undefined,
                      "ig_d.sort",
                      130);
ig_d.df = __typedjs(function  ()
                    {
                      var a = "";
                      if (_old_html)
                      a = "<table cellspacing=0 cellpadding=0 border=0>";
                      for (var b = "",c = this.items,d = 0; d < c.length; d++)
                      if (c[d])
                      {
                        a += _old_html ? "<tr><td nowrap><font size=-1>" + c[d].gb() + "</font></td><td><a href=\"#\" onclick=\"" + this.Ra + "._del(" + d + ");return false\"><img src=\"/ig/images/x.gif\" width=16 height=13 border=0></a></td></tr>" : "<a href=\"#\" onclick=\"" + this.Ra + "._del(" + d + ");return false\" class=\"delbox listdelbox\"></a>" + c[d].gb() + "<div class=\"c\"></div>";
                        if (parseInt(c[d].Pa,10) < 0)
                        b += c[d].Ha(d);
                      }
                      else {
                             this.items.splice(d,1);
                             d--;
                           };
                      c = this.na;
                      var e = "";
                      for (d = 0; d < c.length; d++)
                      if (parseInt(c[d].Pa,10) >= 0)
                      e += "," + c[d].Pa;
                      a += this.nc(b,e);
                      return a;
                    },
                    undefined,
                    "ig_d.df",
                    131);
ig_d.nc = __typedjs(function  (a,b)
                    {
                      var c = "<input type=hidden name=m_" + this.ba + "_del value=\"" + b + "\"><input type=hidden name=m_" + this.ba + "_add value=\"" + a + "\">";
                      if (_old_html)
                      c = "</table><input type=hidden name=m_" + this.ba + "_del value=\"" + b + "\"><input type=hidden name=m_" + this.ba + "_add value=\"" + a + "\">";
                      return c;
                    },
                    undefined,
                    "ig_d.nc",
                    132);
ig_d.refresh = __typedjs(function  ()
                         {
                           this.Jf && this.items.sort(this.sort);
                           this.fc.innerHTML = "<font size=-1>" + this.df() + "</font>";
                         },
                         undefined,
                         "ig_d.refresh",
                         133);
ig_d.add = __typedjs(function  (a,b)
                     {
                       a || (a = _trim(this.H.value));
                       b || (b = _trim(this.U.value));
                       a = new this.Dc(a,b,- 1);
                       if (a.mb())
                       {
                         this.items[this.items.length] = a;
                         this.refresh();
                         this.H.value = this.$b;
                         this.U.value = this.bc;
                       };
                     },
                     undefined,
                     "ig_d.add",
                     134);
ig_d._del = __typedjs(function  (a)
                      {
                        this.na[this.na.length] = this.items[a];
                        this.items.splice(a,1);
                        this.refresh();
                      },
                      undefined,
                      "ig_d._del",
                      135);
_PrefListApp = __typedjs(function  (a,b,c,d,e)
                         {
                           var f = typeof c == "string" ? [] : c;
                           d = new _ListApp(f,d,e);
                           for (var h in d)
                           this[h] = d[h];
                           typeof c == "string" && this.$e(c);
                           this.cf = a;
                           this.lg = b;
                           this.Ra = "m_" + e + "_" + a + "_App";
                           this.fc = _gel("m_" + e + "_" + a + "_disp");
                           this.H = this.U = _gel("m_" + e + "_" + a + "_val");
                           this.bc = this.$b = "";
                           this.nc = _PrefListApp_get_tail;
                         },
                         undefined,
                         "_PrefListApp",
                         136);
_PrefListApp_get_tail = __typedjs(function  ()
                                  {
                                    var a = "</table>";
                                    _gel("m_" + this.ba + "_" + this.cf).value = this.Kf();
                                    return a;
                                  },
                                  undefined,
                                  "_PrefListApp_get_tail",
                                  137);
_PrefListApp.prototype.$e = __typedjs(function  (a)
                                      {
                                        if (a.length != 0)
                                        {
                                          a = _IG_Prefs.Nb(a);
                                          for (var b = [],c = 0; c < a.length; c++)
                                          {
                                            var d = new this.Dc(a[c],a[c],- 1);
                                            b[b.length] = d;
                                          };
                                          this.items = b;
                                          this.Nc = [].concat(b);
                                        };
                                      },
                                      undefined,
                                      "_PrefListApp.prototype.$e",
                                      138);
_PrefListApp.prototype.Kf = __typedjs(function  ()
                                      {
                                        for (var a = [],b = 0; b < this.items.length; b++)
                                        a[a.length] = this.items[b].F;
                                        return _IG_Prefs.Lb(a);
                                      },
                                      undefined,
                                      "_PrefListApp.prototype.Kf",
                                      139);
_ListItem = __typedjs(function  (a,b,c)
                      {
                        this.p = a;
                        this.F = b;
                        this.Pa = c;
                      },
                      undefined,
                      "_ListItem",
                      140);
_ListItem.prototype.mb = __typedjs(function  ()
                                   {
                                     return this.p != "";
                                   },
                                   undefined,
                                   "_ListItem.prototype.mb",
                                   141);
_ListItem.prototype.Ua = __typedjs(function  ()
                                   {
                                     return 0;
                                   },
                                   undefined,
                                   "_ListItem.prototype.Ua",
                                   142);
_ListItem.prototype.gb = __typedjs(function  ()
                                   {
                                     return _hesc(this.p);
                                   },
                                   undefined,
                                   "_ListItem.prototype.gb",
                                   143);
_ListItem.prototype.Ha = __typedjs(function  ()
                                   {
                                     return "&" + _esc(this.p) + "=" + _esc(this.F);
                                   },
                                   undefined,
                                   "_ListItem.prototype.Ha",
                                   144);
_BMListItem = __typedjs(function  (a,b,c)
                        {
                          _ListItem.call(this,a,b,c);
                        },
                        undefined,
                        "_BMListItem",
                        145);
ig_k(_ListItem,_BMListItem);
ig_d = _BMListItem.prototype;
ig_d.mb = __typedjs(function  ()
                    {
                      return _ListItem.prototype.mb.call(this) && this.p != "http://";
                    },
                    undefined,
                    "ig_d.mb",
                    146);
ig_d.eb = __typedjs(function  ()
                    {
                      if (this.F)
                      return this.F
                      else {
                             var a = this.p;
                             if (a.indexOf("http://") == 0)
                             a = a.substring(7);
                             if (a.indexOf("www.") == 0)
                             a = a.substring(4);
                             return a;
                           };
                    },
                    undefined,
                    "ig_d.eb",
                    147);
ig_d.gb = __typedjs(function  ()
                    {
                      return "<a href=\"" + this.p + "\" target=bmwindow>" + _hesc(this.eb()) + "</a>";
                    },
                    undefined,
                    "ig_d.gb",
                    148);
ig_d.Ha = __typedjs(function  (a)
                    {
                      return "&b" + a + "=" + _esc(this.p) + "&t" + a + "=" + _esc(this.F);
                    },
                    undefined,
                    "ig_d.Ha",
                    149);
ig_d.Ua = __typedjs(function  (a)
                    {
                      var b = _uc(this.eb());
                      a = _uc(a.eb());
                      if (b == a)
                      return 0;
                      return b < a ? - 1 : 1;
                    },
                    undefined,
                    "ig_d.Ua",
                    150);
_WthrListItem = __typedjs(function  (a,b,c)
                          {
                            _ListItem.call(this,a,b,c);
                          },
                          undefined,
                          "_WthrListItem",
                          151);
ig_k(_ListItem,_WthrListItem);
_WthrListItem.prototype.Ha = __typedjs(function  ()
                                       {
                                         var a = "&" + _esc(this.p);
                                         if (this.F)
                                         a += "=" + _esc(this.F);
                                         return a;
                                       },
                                       undefined,
                                       "_WthrListItem.prototype.Ha",
                                       152);
_FListItem = __typedjs(function  (a,b,c,d)
                       {
                         _ListItem.call(this,a,b,c);
                         this.ja = d || 0;
                       },
                       undefined,
                       "_FListItem",
                       153);
ig_k(_ListItem,_FListItem);
_FListItem.prototype.Ha = __typedjs(function  ()
                                    {
                                      return "&" + _esc(this.p);
                                    },
                                    undefined,
                                    "_FListItem.prototype.Ha",
                                    154);
_FListItem.prototype.Ua = __typedjs(function  (a)
                                    {
                                      var b = this;
                                      if (b.ja < a.ja)
                                      return - 1;
                                      if (b.ja > a.ja)
                                      return 1;
                                      b = _uc(b.p);
                                      a = _uc(a.p);
                                      if (b < a)
                                      return - 1;
                                      if (b > a)
                                      return 1;
                                      return 0;
                                    },
                                    undefined,
                                    "_FListItem.prototype.Ua",
                                    155);
ig_A = __typedjs(function  (a)
                 {
                   if (! a)
                   a = document.body;
                   return a.scrollHeight > a.offsetHeight ? a.scrollHeight : a.offsetHeight;
                 },
                 undefined,
                 "ig_A",
                 156);
ig_B = __typedjs(function  ()
                 {
                   return self.pageYOffset ? self.pageYOffset : document.documentElement && document.documentElement.scrollTop ? document.documentElement.scrollTop : document.body.scrollTop;
                 },
                 undefined,
                 "ig_B",
                 157);
_closePromoBox = __typedjs(function  (a)
                           {
                             _toggle(_gel("promo" + a));
                             _xsetp("gpc=" + a + ":-1");
                           },
                           undefined,
                           "_closePromoBox",
                           158);
_exportSymbols("",["pageHeight",ig_A,"scrollTop",ig_B]);
var ig_C = {},ig_Ia = {};
_place = __typedjs(function  (a,b)
                   {
                     var c = ig_Ia[a];
                     if (c)
                     c(b)
                     else {
                            ig_C[a] || (ig_C[a] = []);
                            ig_C[a].push(b);
                          };
                   },
                   undefined,
                   "_place",
                   159);
_register_place = __typedjs(function  (a,b)
                            {
                              ig_Ia[a] = b;
                              var c = ig_C[a];
                              if (c)
                              {
                                for (var d = 0; d < c.length; d++)
                                b(c[d]);
                                ig_C[a] = ig_;
                              };
                            },
                            undefined,
                            "_register_place",
                            160);
_enableGS = __typedjs(function  (a,b)
                      {
                        a.action = "https://www.google.com/accounts/CheckCookie";
                        a.method = "get";
                        ig_p(a,"service",b);
                        ig_p(a,"continue",document.location);
                        ig_p(a,"skipvpage",ig_b);
                        return ig_b;
                      },
                      undefined,
                      "_enableGS",
                      161);
;;
_reload = __typedjs(function  (a,b)
                    {
                      var c = a - (new Date()).getTime();
                      if (c > 1000.0)
                      setTimeout("_reload(" + a + ")",c)
                      else if (b)
                           document.location.replace(ig_Ja(document.location.href,
                                                           document.location.pathname))
                           else {
                                  document.cookie = "IGREL=1";
                                  document.location.replace(document.location.href.split("#")[0]);
                                };
                    },
                    undefined,
                    "_reload",
                    162);
var ig_Ka = /^([^?#]+)(?:\?([^#]*))?(#.*)?/;
ig_Ja = __typedjs(function  (a,b)
                  {
                    var c = ig_Ka.exec(a),d = c[1];
                    if (b == "/")
                    d += "ig";
                    d += "?";
                    if (b = c[2])
                    d += b;
                    if (! _argsUrl(a).refresh)
                    {
                      if (b)
                      d += "&";
                      d += "refresh=1";
                    };
                    return d;
                  },
                  undefined,
                  "ig_Ja",
                  163);
var ig_D = {db: 0, $c: 1800, pb: ig_, ob: ig_, Qa: ig_b, Z: ig_, gd: ig_, Rb: 600, ga: __typedjs(function  ()
                                                                                                 {
                                                                                                   ig_D.db = (new Date()).getTime() + ig_D.$c * 1000.0;
                                                                                                 },
                                                                                                 undefined,
                                                                                                 "ig_D",
                                                                                                 164), Ze: __typedjs(function  ()
                                                                                                                     {
                                                                                                                       if ((new Date()).getTime() > ig_D.db)
                                                                                                                       window.gTalkNotifier && window.gTalkNotifier._isAnyMoleOpen && window.gTalkNotifier._isAnyMoleOpen() ? ig_D.ga() : _testConnection(__typedjs(function  ()
                                                                                                                                                                                                                                                                    {
                                                                                                                                                                                                                                                                      _reload(ig_D.db,
                                                                                                                                                                                                                                                                              ig_D.gd);
                                                                                                                                                                                                                                                                    },
                                                                                                                                                                                                                                                                    arguments.callee,
                                                                                                                                                                                                                                                                    "",
                                                                                                                                                                                                                                                                    0),
                                                                                                                                                                                                                                                          ig_D.ga);
                                                                                                                     },
                                                                                                                     undefined,
                                                                                                                     "ig_D",
                                                                                                                     165), rb: __typedjs(function  (a)
                                                                                                                                         {
                                                                                                                                           ig_D.ga();
                                                                                                                                           ig_D.pb && ig_D.pb(a);
                                                                                                                                         },
                                                                                                                                         undefined,
                                                                                                                                         "ig_D",
                                                                                                                                         166), qb: __typedjs(function  (a)
                                                                                                                                                             {
                                                                                                                                                               ig_D.ga();
                                                                                                                                                               ig_D.ob && ig_D.ob(a);
                                                                                                                                                             },
                                                                                                                                                             undefined,
                                                                                                                                                             "ig_D",
                                                                                                                                                             167)};
_IG_disable_refresh_cycle = __typedjs(function  ()
                                      {
                                        ig_D.Qa = ig_c;
                                        _IG_suspend_refresh_cycle();
                                      },
                                      undefined,
                                      "_IG_disable_refresh_cycle",
                                      168);
_IG_suspend_refresh_cycle = __typedjs(function  ()
                                      {
                                        if (ig_D.Z != ig_)
                                        {
                                          clearInterval(ig_D.Z);
                                          ig_D.Z = ig_;
                                        };
                                      },
                                      undefined,
                                      "_IG_suspend_refresh_cycle",
                                      169);
_IG_restart_refresh_cycle = __typedjs(function  ()
                                      {
                                        ! ig_D.Qa || ig_D.Z != ig_ || ig_La();
                                      },
                                      undefined,
                                      "_IG_restart_refresh_cycle",
                                      170);
_IG_start_refresh_cycle = __typedjs(function  (a,b,c)
                                    {
                                      if (ig_D.Qa)
                                      {
                                        ig_D.pb = document.onmousedown;
                                        ig_D.ob = document.onkeyup;
                                        ig_D.$c = a;
                                        ig_D.Rb = b;
                                        if (c)
                                        ig_D.gd = c;
                                        if (document.addEventListener)
                                        {
                                          document.addEventListener("keyup",ig_D.qb,ig_c);
                                          document.addEventListener("mousedown",ig_D.rb,ig_c);
                                        }
                                        else if (document.attachEvent)
                                             {
                                               document.attachEvent("onkeyup",ig_D.qb);
                                               document.attachEvent("onmousedown",ig_D.rb);
                                             }
                                             else {
                                                    document.onkeyup = ig_D.qb;
                                                    document.onmousedown = ig_D.rb;
                                                  };
                                        ig_La();
                                      };
                                    },
                                    undefined,
                                    "_IG_start_refresh_cycle",
                                    171);
ig_La = __typedjs(function  ()
                  {
                    ig_D.ga();
                    ig_D.Z = setInterval(ig_D.Ze,ig_D.Rb * 1000.0);
                  },
                  undefined,
                  "ig_La",
                  172);
_testConnection = __typedjs(function  (a,b,c)
                            {
                              var d = ig_m(),e = "/ig/nop?t=" + (new Date()).getTime();
                              c || (c = 3500);
                              d.open("GET",e,ig_b);
                              d.onreadystatechange = __typedjs(function  ()
                                                               {
                                                                 if (d.readyState == 4 && a)
                                                                 if (200 <= d.status && d.status < 300 || d.status == 1223)
                                                                 {
                                                                   b = ig_;
                                                                   a();
                                                                 };
                                                               },
                                                               arguments.callee,
                                                               "d.onreadystatechange",
                                                               0);
                              try
                              {
                                d.send(ig_);
                              }
                              catch (f) {
                                        };
                              h = __typedjs(function  ()
                                            {
                                              if (b)
                                              {
                                                a = ig_;
                                                b();
                                              };
                                            },
                                            arguments.callee,
                                            "h",
                                            1);
                              setTimeout(h,c);
                            },
                            undefined,
                            "_testConnection",
                            173);
;;
_IG_GetCachedUrl = __typedjs(function  (a,b)
                             {
                               b = typeof b == "object" ? b : {};
                               var c = window.location.href,
                                   d = "/ig/proxy?",
                                   e = /^http:\/\/[^\/]+\/ig\/ifr[?]/;
                               if ((e = e.exec(c)) || c.indexOf("http://www.google.cn/ig/china") == 0)
                               {
                                 if (window.location.host != "p.gmodules.com" && _et)
                                 d += "et=" + _esc(_et) + "&";
                               }
                               else d = "http://p.gmodules.com/ig/proxy?";
                               if (! isNaN(b.refreshInterval) && b.refreshInterval >= 0)
                               d = _appendMaxAgeParameter(d,b.refreshInterval);
                               return d + "url=" + _esc(a) + "&log=1";
                             },
                             undefined,
                             "_IG_GetCachedUrl",
                             174);
_IG_GetImageUrl = __typedjs(function  (a,b)
                            {
                              return _IG_GetCachedUrl(a,b);
                            },
                            undefined,
                            "_IG_GetImageUrl",
                            175);
_IG_GetImage = __typedjs(function  (a,b)
                         {
                           var c = document.createElement("img");
                           c.src = _IG_GetCachedUrl(a,b);
                           return c;
                         },
                         undefined,
                         "_IG_GetImage",
                         176);
;;
_IG_Callback = __typedjs(function  (a)
                         {
                           var b = arguments;
                           return __typedjs(function  ()
                                            {
                                              for (var c = [],d = 0; d < arguments.length; d++)
                                              c[c.length] = arguments[d];
                                              for (d = 1; d < b.length; d++)
                                              c[c.length] = b[d];
                                              a.apply(ig_,c);
                                            },
                                            arguments.callee,
                                            "",
                                            0);
                         },
                         undefined,
                         "_IG_Callback",
                         177);
;;
var _IG_Layouts = {};
_IG_Layouts._selectLayout = __typedjs(function  (a)
                                      {
                                        for (var b = _gel("edit_this_tab_form").edit_this_tab_selected_layout,
                                                 c = 0; c < b.length; c++)
                                        {
                                          var d = b[c].value,
                                              e = _gel("edit_this_tab_layoutimg_" + d);
                                          if (d != a)
                                          {
                                            b[c].checked = ig_c;
                                            if (e.src.indexOf("_highlight") != - 1)
                                            e.src = e.src.substring(0,
                                                                    e.src.indexOf("_highlight")) + ".gif";
                                          }
                                          else {
                                                 b[c].checked = ig_b;
                                                 if (e.src.indexOf("_highlight") == - 1)
                                                 e.src = e.src.substring(0,
                                                                         e.src.indexOf(".gif")) + "_highlight.gif";
                                               };
                                        };
                                      },
                                      undefined,
                                      "_IG_Layouts._selectLayout",
                                      178);
var ig_E = __typedjs(function  ()
                     {
                       var a = ig_c,b = [],c = ig_,d = ig_;
                       return {init: __typedjs(function  ()
                                               {
                                               },
                                               arguments.callee,
                                               "",
                                               0), load: __typedjs(function  (e,f,h)
                                                                   {
                                                                     if (_gel(e))
                                                                     h(f)
                                                                     else {
                                                                            b.push({id: e, args: f, callback: h});
                                                                            if (! a)
                                                                            if (c)
                                                                            {
                                                                              d(c,b);
                                                                              b = [];
                                                                            }
                                                                            else {
                                                                                   _sendx("/ig/ui?action=lazy&id=" + e,
                                                                                          ig_,
                                                                                          ig_c,
                                                                                          ig_);
                                                                                   if (! (c || a))
                                                                                   {
                                                                                     a = ig_b;
                                                                                     _IG_LoadScript("/ig/ll" + window.location.search,
                                                                                                    "");
                                                                                   };
                                                                                 };
                                                                          };
                                                                   },
                                                                   arguments.callee,
                                                                   "",
                                                                   1), Ne: __typedjs(function  (e,f)
                                                                                     {
                                                                                       c = e;
                                                                                       d = f;
                                                                                       a = ig_c;
                                                                                       d(c,b);
                                                                                       b = [];
                                                                                     },
                                                                                     arguments.callee,
                                                                                     "",
                                                                                     2), pg: __typedjs(function  ()
                                                                                                       {
                                                                                                         a = ig_c;
                                                                                                         b = [];
                                                                                                         d = c = ig_;
                                                                                                       },
                                                                                                       arguments.callee,
                                                                                                       "",
                                                                                                       3)};
                     },
                     undefined,
                     "ig_E",
                     179)(),
    _IG_LL_init = ig_E.init,
    _IG_LL_load = ig_E.load,
    _IG_LL_loaded = ig_E.Ne;
var ig_F = [];
_IG_isModuleInVisibleArea = __typedjs(function  (a)
                                      {
                                        var b = ig_B(),c,d = c = 0;
                                        if (window.innerHeight)
                                        {
                                          c = window.innerWidth;
                                          d = window.innerHeight;
                                        }
                                        else if (document.documentElement && document.documentElement.clientHeight)
                                             {
                                               c = document.documentElement.clientWidth;
                                               d = document.documentElement.clientHeight;
                                             }
                                             else if (document.body.clientHeight)
                                                  {
                                                    c = document.body.clientWidth;
                                                    d = document.body.clientHeight;
                                                  };
                                        c = {width: c, height: d};
                                        c = c.height;
                                        d = parseInt(a.style.height,10);
                                        a = _findPos(a)[1];
                                        return a + d > b && a < b + c;
                                      },
                                      undefined,
                                      "_IG_isModuleInVisibleArea",
                                      180);
_IG_delayLoadGadget = __typedjs(function  (a)
                                {
                                  ig_F.push(a);
                                },
                                undefined,
                                "_IG_delayLoadGadget",
                                181);
_IG_loadDelayLoadGadgets = __typedjs(function  ()
                                     {
                                       for (var a = 0; a < ig_F.length; ++a)
                                       {
                                         var b = _gel("remote_iframe_" + ig_F[a].id);
                                         if (b)
                                         {
                                           var c = b.getAttribute("src");
                                           if (c == ig_ || c == "")
                                           b.src = ig_F[a].base_iframe_url;
                                         };
                                       };
                                     },
                                     undefined,
                                     "_IG_loadDelayLoadGadgets",
                                     182);
;;
_IG_MD_Generate = __typedjs(function  (a)
                            {
                              var b = {t: {}, dt: [], m: {}, f: {}},c = a,d = b;
                              if (c.t)
                              for (var e = 0; e < c.t.length; ++e)
                              {
                                var f = c.t[e];
                                d.t[f.i] = f;
                                d.t[f.i].m = {};
                              };
                              c = a;
                              d = b;
                              if (c.f)
                              for (e = 0; e < c.f.length; ++e)
                              {
                                f = c.f[e];
                                d.f[f.n] = f;
                              };
                              c = a;
                              d = b;
                              if (c.m)
                              for (e = 0; e < c.m.length; ++e)
                              {
                                f = c.m[e];
                                f.fa || (f.fa = {});
                                d.m[f.i] = f;
                                var h = d.t[f.t];
                                if (h)
                                h.m[f.i] = f;
                              };
                              if (a.ct != ig_ && b.t)
                              b.ct = b.t[a.ct];
                              a = a;
                              c = b;
                              if (a.dt)
                              for (d = 0; d < a.dt.length; ++d)
                              c.dt[d] = c.t[a.dt[d]];
                              return b;
                            },
                            undefined,
                            "_IG_MD_Generate",
                            183);
;;
var ig_G;
ig_G || (ig_G = function  ()
                {
                  function a(j)
                  {
                    return j < 10 ? "0" + j : j;
                  };
                  function b(j)
                  {
                    e.lastIndex = 0;
                    return e.test(j) ? "\"" + j.replace(e,
                                                        function  (l)
                                                        {
                                                          var m = g[l];
                                                          if (typeof m === "string")
                                                          return m;
                                                          return "\\u" + ("0000" + (+ l.charCodeAt(0)).toString(16)).slice(- 4);
                                                        }) + "\"" : "\"" + j + "\"";
                  };
                  function c(j,l)
                  {
                    var m,n,i = f,o,p = l[j];
                    if (p && typeof p === "object" && typeof p.toJSON === "function")
                    p = p.toJSON(j);
                    if (typeof k === "function")
                    p = k.call(l,j,p);
                    switch (typeof p)
                    {case
                     "string" :
                       return b(p);
                     case
                     "number" :
                       return isFinite(p) ? String(p) : "null";
                     case
                     "boolean" :
                     case
                     "null" :
                       return String(p);
                     case
                     "object" :
                       if (! p)
                       return "null";
                       f += h;
                       o = [];
                       if (typeof p.length === "number" && ! p.propertyIsEnumerable("length"))
                       {
                         n = p.length;
                         for (j = 0; j < n; j += 1)
                         o[j] = c(j,p) || "null";
                         l = o.length === 0 ? "[]" : f ? "[\n" + f + o.join(",\n" + f) + "\n" + i + "]" : "[" + o.join(",") + "]";
                         f = i;
                         return l;
                       };
                       if (k && typeof k === "object")
                       {
                         n = k.length;
                         for (j = 0; j < n; j += 1)
                         {
                           m = k[j];
                           if (typeof m === "string")
                           if (l = c(m,p))
                           o.push(b(m) + (f ? ": " : ":") + l);
                         };
                       }
                       else for (m in p)
                            if (Object.hasOwnProperty.call(p,m))
                            if (l = c(m,p))
                            o.push(b(m) + (f ? ": " : ":") + l);
                       l = o.length === 0 ? "{}" : f ? "{\n" + f + o.join(",\n" + f) + "\n" + i + "}" : "{" + o.join(",") + "}";
                       f = i;
                       return l;};
                  };
                  Date.prototype.toJSON = function  ()
                                          {
                                            return this.getUTCFullYear() + "-" + a(this.getUTCMonth() + 1) + "-" + a(this.getUTCDate()) + "T" + a(this.getUTCHours()) + ":" + a(this.getUTCMinutes()) + ":" + a(this.getUTCSeconds()) + "Z";
                                          };
                  String.prototype.toJSON = Number.prototype.toJSON = Boolean.prototype.toJSON = function  ()
                                                                                                 {
                                                                                                   return this.valueOf();
                                                                                                 };
                  var d = /[\u0000\u00ad\u0600-\u0604\u070f\u17b4\u17b5\u200c-\u200f\u2028-\u202f\u2060-\u206f\ufeff\ufff0-\uffff]/g,
                      e = /[\\\"\x00-\x1f\x7f-\x9f\u00ad\u0600-\u0604\u070f\u17b4\u17b5\u200c-\u200f\u2028-\u202f\u2060-\u206f\ufeff\ufff0-\uffff]/g,
                      f,
                      h,
                      g = {"\b": "\\b", "\t": "\\t", "\n": "\\n", "\f": "\\f", "\r": "\\r", "\"": "\\\"", "\\": "\\\\"},
                      k;
                  return {stringify: function  (j,l,m)
                                     {
                                       l = l;
                                       m = m;
                                       var n;
                                       h = f = "";
                                       if (typeof m === "number")
                                       for (n = 0; n < m; n += 1)
                                       h += " "
                                       else if (typeof m === "string")
                                            h = m;
                                       if ((k = l) && typeof l !== "function" && (typeof l !== "object" || typeof l.length !== "number"))
                                       ig_a(new Error("JSON.stringify"));
                                       return c("",{"": j});
                                     }, parse: function  (j,l)
                                               {
                                                 function m(i,o)
                                                 {
                                                   var p,r,q = i[o];
                                                   if (q && typeof q === "object")
                                                   for (p in q)
                                                   if (Object.hasOwnProperty.call(q,p))
                                                   {
                                                     r = m(q,p);
                                                     if (r !== undefined)
                                                     q[p] = r
                                                     else delete q[p];
                                                   };
                                                   return n.call(i,o,q);
                                                 };
                                                 var n = l;
                                                 d.lastIndex = 0;
                                                 if (d.test(j))
                                                 j = j.replace(d,
                                                               function  (i)
                                                               {
                                                                 return "\\u" + ("0000" + (+ i.charCodeAt(0)).toString(16)).slice(- 4);
                                                               });
                                                 if (/^[\],:{}\s]*$/.test(j.replace(/\\(?:["\\\/bfnrt]|u[0-9a-fA-F]{4})/g,
                                                                                    "@").replace(/"[^"\\\n\r]*"|true|false|null|-?\d+(?:\.\d*)?(?:[eE][+\-]?\d+)?/g,
                                                                                                 "]").replace(/(?:^|:|,)(?:\s*\[)+/g,
                                                                                                              "")))
                                                 {
                                                   j = eval("(" + j + ")");
                                                   return typeof n === "function" ? m({"": j},
                                                                                      "") : j;
                                                 };
                                                 ig_a(new Error("JSON.parse"));
                                               }};
                }());
ig_G.parse = __typedjs(function  ()
                       {
                         var a = ig_G.parse;
                         return __typedjs(function  (b,c)
                                          {
                                            try
                                            {
                                              return a(b,c);
                                            }
                                            catch (d) {
                                                        return ig_c;
                                                      };
                                          },
                                          arguments.callee,
                                          "",
                                          0);
                       },
                       undefined,
                       "ig_G.parse",
                       185)();
ig_Oa = __typedjs(function  (a)
                  {
                    var b = document.createElement("DIV");
                    b.innerHTML = "<iframe onload=\'this.pool_locked=false\'></iframe>";
                    var c = b.getElementsByTagName("IFRAME")[0];
                    c.style.visibility = "hidden";
                    c.style.width = c.style.height = "0px";
                    c.style.border = "0px";
                    c.style.position = "absolute";
                    c.Lc = a;
                    this.l[this.l.length] = c;
                    b.removeChild(c);
                    return c;
                  },
                  undefined,
                  "ig_Oa",
                  186);
ig_Pa = __typedjs(function  (a)
                  {
                    if (a.match(/^http[s]?:\/\//))
                    {
                      var b = this;
                      window.setTimeout(__typedjs(function  ()
                                                  {
                                                    for (var c = ig_,
                                                             d = b.l.length - 1; d >= 0; d--)
                                                    {
                                                      var e = b.l[d];
                                                      if (e && ! e.Lc)
                                                      {
                                                        e.parentNode.removeChild(e);
                                                        if (window.ActiveXObject)
                                                        {
                                                          b.l[d] = ig_;
                                                          b.l.splice(d,1);
                                                        }
                                                        else {
                                                               e.Lc = ig_b;
                                                               c = e;
                                                               break;
                                                             };
                                                      };
                                                    };
                                                    c = c ? c : b.Md(ig_b);
                                                    c.src = a;
                                                    document.body.appendChild(c);
                                                  },
                                                  arguments.callee,
                                                  "",
                                                  0),
                                        0);
                    };
                  },
                  undefined,
                  "ig_Pa",
                  187);
ig_Qa = __typedjs(function  ()
                  {
                    for (var a = 0; a < this.l.length; a++)
                    {
                      this.l[a].onload = ig_;
                      this.l[a] = ig_;
                    };
                    this.l.length = 0;
                    this.l = [];
                  },
                  undefined,
                  "ig_Qa",
                  188);
ig_Ra = __typedjs(function  ()
                  {
                    for (var a = 0; a < this.l.length; a++)
                    {
                      var b = this.l[a];
                      b && b.parentNode.removeChild(b);
                    };
                  },
                  undefined,
                  "ig_Ra",
                  189);
ig_Sa = __typedjs(function  ()
                  {
                    this.l = [];
                    this.Md = ig_Oa;
                    this.vc = ig_Pa;
                    this.clear = ig_Qa;
                    this.gc = ig_Ra;
                  },
                  undefined,
                  "ig_Sa",
                  190);
ig_Ta = __typedjs(function  (a,b)
                  {
                    _IFPC.ea[a] = b;
                  },
                  undefined,
                  "ig_Ta",
                  191);
ig_Ua = __typedjs(function  (a)
                  {
                    delete _IFPC.ea[a];
                  },
                  undefined,
                  "ig_Ua",
                  192);
ig_Va = __typedjs(function  (a,b,c,d,e,f,h,g)
                  {
                    if (! _IFPC.ab)
                    {
                      var k = 4095 - d.length;
                      k = parseInt(k / 3,10);
                      if (typeof h == "undefined")
                      h = ig_b;
                      if (g)
                      {
                        h = {s: b, a: c, f: "..", t: window._isk[a.split("_")[2]]};
                        a = [a,
                             "..",
                             _IFPC.Ta,
                             1,
                             1,
                             encodeURIComponent(ig_G.stringify(h))].join("&");
                        _IFPC.sa.vc(d + "#" + a);
                      }
                      else {
                             c = c.slice(0);
                             c.unshift(_IFPC.ef(e));
                             c.unshift(f);
                             c.unshift(b);
                             c.unshift(a);
                             b = _IFPC.hc(c);
                             c = parseInt(b.length / k,10);
                             if (b.length % k > 0)
                             c += 1;
                             for (e = 0; e < c; e++)
                             {
                               f = b.substr(e * k,k);
                               f = [a,_IFPC.Ta,c,e,f,h];
                               _IFPC.sa.vc(d + "#" + _IFPC.hc(f));
                             };
                           };
                      _IFPC.Ta++;
                    };
                  },
                  undefined,
                  "ig_Va",
                  193);
ig_Wa = __typedjs(function  ()
                  {
                    _IFPC.ea = {};
                    _IFPC.L = {};
                    _IFPC.sa.clear();
                  },
                  undefined,
                  "ig_Wa",
                  194);
ig_Xa = __typedjs(function  ()
                  {
                    if (! _IFPC.ab)
                    {
                      _IFPC.ab = ig_b;
                      _IFPC.sa.gc();
                      _IFPC.clear();
                    };
                  },
                  undefined,
                  "ig_Xa",
                  195);
ig_Ya = __typedjs(function  (a)
                  {
                    var b = _IFPC.Za(a)[0],c = ig_;
                    try
                    {
                      c = window.parent.frames[b];
                    }
                    catch (d) {
                              };
                    try
                    {
                      if (! c && window.parent.parent.frames[b] != window.parent)
                      {
                        window.parent.parent.frames[b].setTimeout(__typedjs(function  ()
                                                                            {
                                                                            },
                                                                            arguments.callee,
                                                                            "",
                                                                            0),
                                                                  0);
                        c = window.parent.parent.frames[b];
                      };
                    }
                    catch (e) {
                              };
                    if (! c)
                    c = window.parent.parent;
                    if (! c || c._IFPC == undefined)
                    c = ig_
                    else {
                           b = __typedjs(function  ()
                                         {
                                           c._IFPC.handleRequest(a);
                                         },
                                         arguments.callee,
                                         "b",
                                         1);
                           window.ActiveXObject ? b() : c.setTimeout(b,0);
                         };
                  },
                  undefined,
                  "ig_Ya",
                  196);
ig_Za = __typedjs(function  (a)
                  {
                    a = _IFPC.Za(a);
                    var b = a.shift(),
                        c = a.shift(),
                        d = a.shift(),
                        e = a.shift(),
                        f = a.shift();
                    a = a.shift();
                    b = b + "_" + c;
                    _IFPC.I[b] || (_IFPC.I[b] = []);
                    _IFPC.I[b].push([e,f]);
                    if (_IFPC.I[b].length == d)
                    {
                      _IFPC.I[b].sort(__typedjs(function  (g,k)
                                                {
                                                  return parseInt(g[0],10) - parseInt(k[0],10);
                                                },
                                                arguments.callee,
                                                "",
                                                0));
                      f = "";
                      for (e = 0; e < d; e++)
                      f += _IFPC.I[b][e][1];
                      _IFPC.I[b] = ig_;
                      e = _IFPC.Za(f);
                      b = e.shift();
                      c = e.shift();
                      d = e.shift();
                      f = e.shift();
                      var h = _IFPC.ne(c);
                      if (h)
                      {
                        a = h.apply(ig_,e);
                        if (_IFPC.Ee(f))
                        {
                          a.unshift(f);
                          _IFPC.call(b,_IFPC.Bb,a,d,ig_,"");
                        };
                      }
                      else if (a)
                           ig_a(new Error("Service " + c + " not registered."));
                    };
                  },
                  undefined,
                  "ig_Za",
                  197);
ig__a = __typedjs(function  (a)
                  {
                    return _IFPC.ea.hasOwnProperty(a) ? _IFPC.ea[a] : ig_;
                  },
                  undefined,
                  "ig__a",
                  198);
ig_0a = __typedjs(function  (a)
                  {
                    var b = "";
                    if (a && typeof a == "function")
                    {
                      b = _IFPC.ke();
                      _IFPC.L[b] = a;
                    };
                    return b;
                  },
                  undefined,
                  "ig_0a",
                  199);
ig_1a = __typedjs(function  (a)
                  {
                    if (_IFPC.L.hasOwnProperty(a))
                    _IFPC.L[a] = ig_;
                  },
                  undefined,
                  "ig_1a",
                  200);
ig_2a = __typedjs(function  (a)
                  {
                    if (a && _IFPC.L.hasOwnProperty(a))
                    return _IFPC.L[a];
                    return ig_;
                  },
                  undefined,
                  "ig_2a",
                  201);
ig_3a = __typedjs(function  ()
                  {
                    return _IFPC.Ab + _IFPC.Ed++;
                  },
                  undefined,
                  "ig_3a",
                  202);
ig_4a = __typedjs(function  (a)
                  {
                    return (a + "").indexOf(_IFPC.Ab) == 0;
                  },
                  undefined,
                  "ig_4a",
                  203);
ig_5a = __typedjs(function  (a)
                  {
                    a = a.split("&");
                    for (var b = 0; b < a.length; b++)
                    {
                      var c = decodeURIComponent(a[b]);
                      try
                      {
                        c = ig_G.parse(c);
                      }
                      catch (d) {
                                };
                      a[b] = c;
                    };
                    return a;
                  },
                  undefined,
                  "ig_5a",
                  204);
ig_6a = __typedjs(function  (a)
                  {
                    for (var b = [],c = 0; c < a.length; c++)
                    {
                      var d = ig_G.stringify(a[c]);
                      b.push(encodeURIComponent(d));
                    };
                    return b.join("&");
                  },
                  undefined,
                  "ig_6a",
                  205);
ig_7a = __typedjs(function  (a)
                  {
                    var b = _IFPC.de(a);
                    if (b)
                    {
                      for (var c = [],d = 1; d < arguments.length; d++)
                      c[c.length] = arguments[d];
                      b.apply(ig_,c);
                      _IFPC.Wf(a);
                    }
                    else ig_a(new Error("Invalid callbackId"));
                  },
                  undefined,
                  "ig_7a",
                  206);
var _IFPC = {};
ig_8a = __typedjs(function  (a)
                  {
                    if ((a = window._argsUrl && window._argsUrl(a)) && a.exp_rpc_js == 1)
                    window.FLAG_use_rpc_js = ig_b;
                    if (window.FLAG_use_rpc_js)
                    {
                    }
                    else {
                           _IFPC = {registerService: ig_Ta, unregisterService: ig_Ua, call: ig_Va, clear: ig_Wa, gc: ig_Xa, og: ig_Ya, mg: ig_Ya, handleRequest: ig_Za, Ab: "cbid", Bb: "ifpc_callback", sa: new ig_Sa(), I: {}, ea: {}, L: {}, Ed: 0, Ta: 0, ab: ig_c, ne: ig__a, ef: ig_0a, Wf: ig_1a, de: ig_2a, ke: ig_3a, Ee: ig_4a, Za: ig_5a, hc: ig_6a, Fd: ig_7a};
                           _IFPC.registerService(_IFPC.Bb,_IFPC.Fd);
                         };
                  },
                  undefined,
                  "ig_8a",
                  207);
ig_8a(document.location.href);
_IG_Prefs_PrefsData = __typedjs(function  ()
                                {
                                },
                                undefined,
                                "_IG_Prefs_PrefsData",
                                208);
_IG_Prefs_PrefsData.prototype.value = ig_;
_IG_Prefs = __typedjs(function  (a)
                      {
                        a = a;
                        if (typeof remote_modules !== "object")
                        {
                          a = (a = _args().mid) ? a : 0;
                          _IG_Prefs._parseURL(a);
                          _IG_Prefs.va[a] = ig_c;
                        }
                        else {
                               if (! a)
                               ig_a(new Error("Module ID is required as first argument to _IG_Prefs() for inlined modules"));
                               _IG_Prefs.va[a] = ig_b;
                             };
                        _IG_Prefs.g[_IG_Prefs.D + a] || (_IG_Prefs.g[_IG_Prefs.D + a] = {});
                        this.g = _IG_Prefs.g[_IG_Prefs.D + a];
                        this.d = a;
                        this.kg = /(.*)(\<ph.*?\>\s*(\<ex\>(.*?)\<\/ex\>)?\s*%1\s*\<\/ph\>)(.*)/;
                      },
                      undefined,
                      "_IG_Prefs",
                      209);
ig_d = _IG_Prefs.prototype;
ig_d.Y = __typedjs(function  (a,b)
                   {
                     if (b == undefined)
                     b = ig_;
                     a = this.g[a];
                     if (a == undefined)
                     return b;
                     a = a.value;
                     return a != undefined ? a : b;
                   },
                   undefined,
                   "ig_d.Y",
                   210);
ig_d.fb = __typedjs(function  (a,b)
                    {
                      return this.Y(_IG_Prefs.k + a,b);
                    },
                    undefined,
                    "ig_d.fb",
                    211);
ig_d.getString = __typedjs(function  (a)
                           {
                             var b = "";
                             a = this.fb(a,b);
                             return a != ig_ ? a + "" : b;
                           },
                           undefined,
                           "ig_d.getString",
                           212);
ig_d.ge = __typedjs(function  (a)
                    {
                      var b = 0;
                      a = parseInt(this.fb(a,b),10);
                      return isNaN(a) ? b : a;
                    },
                    undefined,
                    "ig_d.ge",
                    213);
ig_d.getBool = __typedjs(function  (a)
                         {
                           a = this.getString(a);
                           return a === "true" || ! ! parseInt(a,10);
                         },
                         undefined,
                         "ig_d.getBool",
                         214);
ig_d.ce = __typedjs(function  (a)
                    {
                      return _IG_Prefs.Nb(this.fb(a,""));
                    },
                    undefined,
                    "ig_d.ce",
                    215);
ig_d.tf = __typedjs(function  (a,b)
                    {
                      this.set(a,_IG_Prefs.Lb(b));
                    },
                    undefined,
                    "ig_d.tf",
                    216);
ig_d.ee = __typedjs(function  ()
                    {
                      return this.getString(".country");
                    },
                    undefined,
                    "ig_d.ee",
                    217);
ig_d.he = __typedjs(function  ()
                    {
                      return this.getString(".lang");
                    },
                    undefined,
                    "ig_d.he",
                    218);
ig_d.ie = __typedjs(function  (a)
                    {
                      return this.Y(_IG_Prefs.Fb + a,"") + "";
                    },
                    undefined,
                    "ig_d.ie",
                    219);
ig_d.set = __typedjs(function  ()
                     {
                       var a = Math.floor(arguments.length / 2);
                       if (a != 0)
                       {
                         for (var b = [],c = 0; c < a; ++c)
                         {
                           var d = arguments[c * 2],
                               e = _IG_Prefs.k + d,
                               f = arguments[c * 2 + 1] + "";
                           this.Zf(e,f) && b.push(d,f);
                         };
                         if (b.length != 0)
                         _IG_Prefs.va[this.d] ? _IG_Prefs._setP(this.d,
                                                                b) : _IFPC_SetPref(b);
                       };
                     },
                     undefined,
                     "ig_d.set",
                     220);
ig_9a = __typedjs(function  (a)
                  {
                    var b = "todos",c = _ig_gmid_(a);
                    b = (new _IG_Prefs(Number(c))).getString(b);
                    if (navigator.userAgent.indexOf("MSIE") != - 1 && b.length > 1000.0)
                    {
                      c = Math.ceil(b.length / 1000.0);
                      for (var d = 0; d < c; d++)
                      {
                        var e = b.substr(d * 1000.0,1000.0);
                        ig_$a(a,e,d,c);
                      };
                    }
                    else ig_$a(a,b);
                  },
                  undefined,
                  "ig_9a",
                  221);
ig_$a = __typedjs(function  (a,b,c,d)
                  {
                    var e = _ig_gmid_(a);
                    b = [b];
                    d && b.push(c,d);
                    _IFPC.call(a,
                               "populate_todos_pref",
                               b,
                               window._ifpc_relay_url[e],
                               ig_,
                               "http://" + window.location.host + "/ig/images/rpc_relay.html",
                               ig_b,
                               ig_b);
                  },
                  undefined,
                  "ig_$a",
                  222);
window._IG_CONTAINER && _IFPC.registerService("get_todos_pref_for_migration",
                                              ig_9a);
_IG_Prefs.prototype.le = __typedjs(function  ()
                                   {
                                     var a = ["__module_id__=" + this.d];
                                     for (var b in this.g)
                                     a.push(encodeURIComponent(b) + "=" + encodeURIComponent(this.g[b].value));
                                     return a.join("&");
                                   },
                                   undefined,
                                   "_IG_Prefs.prototype.le",
                                   223);
_IG_Prefs.prototype.dump = __typedjs(function  ()
                                     {
                                       document.write("<pre>");
                                       for (var a in this.g)
                                       document.writeln(a + " = " + this.Y(a,ig_));
                                       document.write("</pre>");
                                     },
                                     undefined,
                                     "_IG_Prefs.prototype.dump",
                                     224);
_IG_Prefs.prototype.Zf = __typedjs(function  (a,b)
                                   {
                                     var c = this.g[a];
                                     if (c && c.value === b)
                                     return ig_c;
                                     if (c)
                                     c.value = b
                                     else {
                                            c = new _IG_Prefs_PrefsData();
                                            c.value = b;
                                            this.g[a] = c;
                                          };
                                     return ig_b;
                                   },
                                   undefined,
                                   "_IG_Prefs.prototype.Zf",
                                   225);
_IG_Prefs.va = {};
_IG_Prefs.g = {};
_IG_Prefs.D = "m_";
_IG_Prefs.k = "up_";
_IG_Prefs.md = "up_";
_IG_Prefs.Fb = "msg_";
_IG_Prefs.Ka = {};
_IG_Prefs.Ka[_IG_Prefs.md] = "value";
_IG_Prefs.eg = __typedjs(function  (a,b)
                         {
                           if (a.indexOf)
                           return a.indexOf(b)
                           else {
                                  for (var c = 0; c < a.length; ++c)
                                  if (a[c] === b)
                                  return c;
                                  return - 1;
                                };
                         },
                         undefined,
                         "_IG_Prefs.eg",
                         226);
_IG_Prefs._parseURL = __typedjs(function  (a)
                                {
                                  _IG_Prefs.va[a] = ig_c;
                                  for (var b = {},c = ig_g(),d = 0; d < c.length; ++d)
                                  {
                                    var e = c[d].indexOf("=");
                                    if (e >= 0)
                                    {
                                      var f = c[d].substring(0,e);
                                      f = f.replace(/\+/g," ");
                                      e = c[d].substring(e + 1);
                                      e = e.replace(/\+/g," ");
                                      e = _unesc(e);
                                      var h = ig_c;
                                      for (var g in _IG_Prefs.Ka)
                                      if (f.indexOf(g) === 0)
                                      {
                                        h = ig_b;
                                        f = f.substring(g.length);
                                        f = _IG_Prefs.k + f;
                                        b[f] = b[f] ? b[f] : new _IG_Prefs_PrefsData();
                                        if (g === _IG_Prefs.dg || g === _IG_Prefs.cg)
                                        e = e == "true";
                                        b[f][_IG_Prefs.Ka[g]] = e;
                                        break;
                                      };
                                      if (! h)
                                      if (f.indexOf(_IG_Prefs.Fb) === 0)
                                      _IG_Prefs.K(a,f,e)
                                      else if (f === ".lang" || f === ".country")
                                           _IG_Prefs.K(a,_IG_Prefs.k + f,e);
                                    };
                                  };
                                  if (window.IDIModule)
                                  {
                                    b = window.IDIModule.getPrefs();
                                    for (d in b)
                                    _IG_Prefs.K(a,d,b[d]);
                                  }
                                  else for (f in b)
                                       _IG_Prefs.K(a,f,b[f].value);
                                },
                                undefined,
                                "_IG_Prefs._parseURL",
                                227);
_IG_Prefs.K = __typedjs(function  (a,b,c)
                        {
                          a = _IG_Prefs.D + a;
                          if (typeof _IG_Prefs.g[a] !== "object")
                          _IG_Prefs.g[a] = {};
                          if (typeof _IG_Prefs.g[a][b] !== "object")
                          {
                            _IG_Prefs.g[a][b] = new _IG_Prefs_PrefsData();
                            b = _IG_Prefs.g[a][b];
                            b.value = c;
                          };
                        },
                        undefined,
                        "_IG_Prefs.K",
                        228);
_IG_Prefs.Bf = __typedjs(function  (a,b,c)
                         {
                           var d = _IG_Prefs.D + a;
                           if (typeof _IG_Prefs.g[d] !== "object")
                           _IG_Prefs.K(a,b,c)
                           else if (typeof _IG_Prefs.g[d][b] !== "object")
                                _IG_Prefs.g[d][b] = new _IG_Prefs_PrefsData();
                           a = _IG_Prefs.g[d][b];
                           a.value = c;
                         },
                         undefined,
                         "_IG_Prefs.Bf",
                         229);
_IG_Prefs._addAll = __typedjs(function  (a,b)
                              {
                                for (var c = 0; c < b.length; ++c)
                                _IG_Prefs.K(a,b[c][0],b[c][1]);
                              },
                              undefined,
                              "_IG_Prefs._addAll",
                              230);
_IG_Prefs.Lb = __typedjs(function  (a)
                         {
                           var b = a.length && a.join;
                           if (b)
                           {
                             b = [];
                             for (var c = 0; c < a.length; ++c)
                             b.push(a[c].replace(/\|/g,"%7C"));
                             return b.join("|");
                           }
                           else return a;
                         },
                         undefined,
                         "_IG_Prefs.Lb",
                         231);
_IG_Prefs.Nb = __typedjs(function  (a)
                         {
                           a = a.length > 0 ? a.split("|") : [];
                           for (var b = 0; b < a.length; ++b)
                           a[b] = a[b].replace(/%7C/g,"|");
                           return a;
                         },
                         undefined,
                         "_IG_Prefs.Nb",
                         232);
_IG_Prefs.W = __typedjs(function  (a,b)
                        {
                          a = _IG_Prefs.D + a;
                          a = _IG_Prefs.g[a];
                          if (a == ig_)
                          return b;
                          b = b.split("#");
                          var c = b[0].split("?"),d = c;
                          if (b.length > 1 && ("&" + b[1]).indexOf("&" + _IG_Prefs.k) !== - 1 && (c.length === 1 || ("&" + c[1]).indexOf("&" + _IG_Prefs.k) === - 1))
                          d = b;
                          var e = [];
                          if (d.length > 1 && d[1].length !== 0)
                          e = d[1].split("&");
                          for (var f = 0; f < e.length; ++f)
                          e[f].indexOf(_IG_Prefs.k) === 0 && e.splice(f--,1);
                          for (var h in a)
                          h.indexOf(_IG_Prefs.k) !== 0 || h.indexOf(_IG_Prefs.k + ".lang") === 0 || h.indexOf(_IG_Prefs.k + ".country") === 0 || h.indexOf(_IG_Prefs.k + "synd") === 0 || e.push(_esc(h) + "=" + _esc(a[h].value));
                          d[1] = e.join("&");
                          b[0] = c.join("?");
                          return b.join("#");
                        },
                        undefined,
                        "_IG_Prefs.W",
                        233);
_IG_Prefs._setPrefs = __typedjs(function  (a,b)
                                {
                                  for (var c = Math.floor(b.length / 2),d = 0; d < c; ++d)
                                  {
                                    var e = _IG_Prefs.k + b[d * 2],f = b[d * 2 + 1];
                                    _IG_Prefs.Bf(a,e,f);
                                  };
                                },
                                undefined,
                                "_IG_Prefs._setPrefs",
                                234);
_IG_Prefs._setP = __typedjs(function  (a,b)
                            {
                              if (_args().synd !== "open")
                              {
                                for (var c = Math.floor(b.length / 2),d = [],e = 0; e < c; ++e)
                                {
                                  var f = _IG_Prefs.k + b[e * 2],h = b[e * 2 + 1];
                                  d.push(_IG_Prefs.D + a + "_" + f + "=" + _esc(h));
                                };
                                d.length !== 0 && _xsetp(d.join("&"));
                              };
                            },
                            undefined,
                            "_IG_Prefs._setP",
                            235);
_exportClass("google.gadgets",
             "Prefs",
             _IG_Prefs,
             ["getString",
              _IG_Prefs.prototype.getString,
              "getInt",
              _IG_Prefs.prototype.ge,
              "getBool",
              _IG_Prefs.prototype.getBool,
              "getArray",
              _IG_Prefs.prototype.ce,
              "setArray",
              _IG_Prefs.prototype.tf,
              "getLang",
              _IG_Prefs.prototype.he,
              "getCountry",
              _IG_Prefs.prototype.ee,
              "getMsg",
              _IG_Prefs.prototype.ie,
              "getPreloadedString",
              _IG_Prefs.prototype.le,
              "set",
              _IG_Prefs.prototype.set,
              "dump",
              _IG_Prefs.prototype.dump]);
_IG_FR_getFeed = __typedjs(function  (a)
                           {
                             return window["FEED" + a] || {};
                           },
                           undefined,
                           "_IG_FR_getFeed",
                           236);
_IG_FR_toggle = __typedjs(function  (a,b,c,d)
                          {
                            c = c || "";
                            d = d || "";
                            var e = _gel("ft_" + a + "_" + b),
                                f = _gel("fb_" + a + "_" + b),
                                h = _gel("ftl_" + a + "_" + b);
                            if (f.style.display == "block")
                            ig_ab(e,f,h,c)
                            else {
                                   var g = _IG_FR_getFeed(a);
                                   if (! g.has_entries && ! g.is_fetching)
                                   {
                                     g.is_fetching = ig_b;
                                     _IG_FetchFeedAsJSON(g.url,
                                                         _IG_Callback(ig_bb,a,b,c,d),
                                                         g.num_items + 15,
                                                         ig_b);
                                   }
                                   else {
                                          for (var k = 0; k < g.num_items; ++k)
                                          {
                                            var j = _gel("ftl_" + a + "_" + k);
                                            j && ig_ab(_gel("ft_" + a + "_" + k),
                                                       _gel("fb_" + a + "_" + k),
                                                       j,
                                                       c);
                                          };
                                          a = a;
                                          b = b;
                                          e = e;
                                          c = f;
                                          g = h;
                                          d = d;
                                          e.className = ig_H(e) ? "fminbox_reverse_directionality" : "fminbox";
                                          e.title = d;
                                          if (_old_html || _use_old_feed_styles)
                                          e.className = "fbox fminbox";
                                          c.style.display = "block";
                                          c.style.maxHeight = c.offsetWidth * 1.5 + "px";
                                          g.className = ig_H(e) ? "sftl_reverse_directionality" : "sftl";
                                          window._IG_FRUC_setFeedAsRead && window._IG_FRUC_setFeedAsRead(a,
                                                                                                         b);
                                          if (f.style.maxHeight && parseInt(f.style.maxHeight,
                                                                            10) < ig_cb(h.offsetWidth,
                                                                                        f))
                                          f.style.height = f.style.maxHeight;
                                        };
                                 };
                          },
                          undefined,
                          "_IG_FR_toggle",
                          237);
ig_bb = __typedjs(function  (a,b,c,d,e)
                  {
                    var f = _IG_FR_getFeed(b);
                    f.is_fetching = ig_c;
                    if (a && a.Entry)
                    {
                      for (var h = 0; h < a.Entry.length; h++)
                      if (a.Entry[h].ID != "no_id")
                      {
                        var g = _gel("fb_" + b + "_" + a.Entry[h].ID);
                        if (g)
                        g.innerHTML = a.Entry[h].Summary;
                      };
                      f.has_entries = ig_b;
                      _IG_FR_toggle(b,c,d,e);
                    };
                  },
                  undefined,
                  "ig_bb",
                  238);
ig_H = __typedjs(function  (a)
                 {
                   return a.className.match(/reverse_directionality$/) == "reverse_directionality";
                 },
                 undefined,
                 "ig_H",
                 239);
ig_ab = __typedjs(function  (a,b,c,d)
                  {
                    a.className = ig_H(a) ? "fmaxbox_reverse_directionality" : "fmaxbox";
                    a.title = d;
                    if (_old_html || _use_old_feed_styles)
                    a.className = "fbox fmaxbox";
                    b.style.display = "none";
                    b.style.maxHeight = "";
                    c.className = ig_H(a) ? "uftl_reverse_directionality" : "uftl";
                  },
                  undefined,
                  "ig_ab",
                  240);
ig_cb = __typedjs(function  (a,b)
                  {
                    var c = document.createElement("div");
                    c.style.left = - screen.width;
                    c.style.top = - screen.height;
                    c.style.width = a;
                    c.innerHTML = b.innerHTML;
                    document.body.appendChild(c);
                    a = c.clientHeight;
                    document.body.removeChild(c);
                    return a;
                  },
                  undefined,
                  "ig_cb",
                  241);
_IG_RegisterContainerScrollHandler = __typedjs(function  (a)
                                               {
                                                 if (navigator.userAgent.indexOf("Firefox") >= 0)
                                                 window.addEventListener("scroll",a,ig_c)
                                                 else window.onscroll = a;
                                               },
                                               undefined,
                                               "_IG_RegisterContainerScrollHandler",
                                               242);
_IG_GetContainerVisibleHeight = __typedjs(function  ()
                                          {
                                            return document.documentElement.clientHeight;
                                          },
                                          undefined,
                                          "_IG_GetContainerVisibleHeight",
                                          243);
_IG_GetContainerTotalHeight = __typedjs(function  ()
                                        {
                                          return document.body.scrollHeight;
                                        },
                                        undefined,
                                        "_IG_GetContainerTotalHeight",
                                        244);
_IG_GetContainerScrollTop = __typedjs(function  ()
                                      {
                                        return document.documentElement.scrollTop;
                                      },
                                      undefined,
                                      "_IG_GetContainerScrollTop",
                                      245);
ig_db = __typedjs(function  (a)
                  {
                    var b = _gel("hl");
                    b = b ? b.value : "en";
                    return "/reader/ig/feed/" + _esc(_IG_FR_getFeed(a).url) + "?hl=" + b;
                  },
                  undefined,
                  "ig_db",
                  246);
ig_eb = __typedjs(function  (a,b)
                  {
                    if (window._max_strategies)
                    {
                      b = ig_db(a) + (b ? "&i=" + b : "");
                      window._max_strategies[a]._updateIframeSrc(b);
                    };
                  },
                  undefined,
                  "ig_eb",
                  247);
_IG_FR_maxModule = __typedjs(function  (a,b,c)
                             {
                               ig_eb(a,b);
                               window._IG_maximize_view(a,"feed",c);
                             },
                             undefined,
                             "_IG_FR_maxModule",
                             248);
_IG_FR_init = __typedjs(function  (a)
                        {
                          window._max_strategies && _IG_callPostLoad(function  ()
                                                                     {
                                                                       window._max_strategies[a] = new window._IG_inlinedTransitionStrategy(a,
                                                                                                                                            ig_db(a),
                                                                                                                                            ig_,
                                                                                                                                            _IG_isIE());
                                                                     });
                          _IG_AddModuleEventHandler(a,
                                                    "unmaximize",
                                                    __typedjs(function  ()
                                                              {
                                                                ig_eb(a);
                                                              },
                                                              arguments.callee,
                                                              "",
                                                              1));
                        },
                        undefined,
                        "_IG_FR_init",
                        249);
var _IG_FR_listOfFeeds = [];
ig_I = __typedjs(function  (a)
                 {
                   var b;
                   b = a;
                   var c;
                   if (window.ActiveXObject)
                   {
                     c = new ActiveXObject("Microsoft.XMLDOM");
                     c.async = ig_c;
                     c.validateOnParse = ig_c;
                     c.resolveExternals = ig_c;
                     c.loadXML(b);
                   }
                   else {
                          c = new DOMParser();
                          c = c.parseFromString(b,"text/xml");
                        };
                   b = c;
                   if ((c = b.documentElement) && c.nodeName == "parsererror" || navigator.userAgent.indexOf("Safari") >= 0 && _gelstn("parsererror",
                                                                                                                                       b)[0])
                   return a;
                   return b;
                 },
                 undefined,
                 "ig_I",
                 250);
var ig_fb = (new Date()).getTime();
ig_gb = __typedjs(function  (a)
                  {
                    if (! a.bf && ! a.headers && window._pl_data && _pl_data[a.url] && ! (! isNaN(a.refreshInterval) && a.refreshInterval >= 0 && (new Date()).getTime() > ig_fb + a.refreshInterval * 1000.0))
                    if (a.Sa)
                    {
                      var b = ig_I(_pl_data[a.url]);
                      a.callback(b);
                    }
                    else a.callback(_pl_data[a.url])
                    else {
                           b = "/ig/jsonp?";
                           if (_et != "")
                           b += "et=" + _et + "&";
                           if (! isNaN(a.refreshInterval) && a.refreshInterval >= 0)
                           b = _appendMaxAgeParameter(b,a.refreshInterval);
                           if (a.encoding)
                           b += "enc=" + _esc(a.encoding) + "&";
                           if (a.headers)
                           {
                             for (var c = [],d = 0; d < a.headers.length; d++)
                             c.push(_esc(a.headers[d][0] + ":" + a.headers[d][1]));
                             b += "hdrs=" + _esc(c.join(",")) + "&";
                           };
                           b += "url=" + _esc(a.url);
                           _sendx(b,
                                  __typedjs(function  (e)
                                            {
                                              var f = e,h = a.url;
                                              e = a.callback;
                                              var g = a.Sa;
                                              if (! f || f == "" || f.length <= 27 || f.charAt(27) != "{")
                                              e(g ? ig_ : "")
                                              else {
                                                     f = eval("(" + f.substring(27) + ")");
                                                     if (h in f)
                                                     {
                                                       h = f[h].body;
                                                       e(g ? ig_I(h) : h);
                                                     };
                                                   };
                                            },
                                            arguments.callee,
                                            "",
                                            0),
                                  ig_c,
                                  a.bf);
                         };
                  },
                  undefined,
                  "ig_gb",
                  251);
ig_hb = __typedjs(function  (a)
                  {
                    var b = {};
                    for (var c in a)
                    b[c] = a[c];
                    return b;
                  },
                  undefined,
                  "ig_hb",
                  252);
_IG_FetchContent = __typedjs(function  (a,b,c)
                             {
                               c = c != ig_ && typeof c == "object" ? ig_hb(c) : {};
                               c.url = a;
                               c.callback = b;
                               c.Sa = ig_c;
                               ig_gb(c);
                             },
                             undefined,
                             "_IG_FetchContent",
                             253);
_IG_FetchXmlContent = __typedjs(function  (a,b,c)
                                {
                                  c = c != ig_ && typeof c == "object" ? ig_hb(c) : {};
                                  c.url = a;
                                  c.callback = b;
                                  c.Sa = ig_b;
                                  ig_gb(c);
                                },
                                undefined,
                                "_IG_FetchXmlContent",
                                254);
ig_ib = __typedjs(function  (a,b,c)
                  {
                    _IG_FetchContent(a,
                                     __typedjs(function  (d)
                                               {
                                                 if (d)
                                                 {
                                                   d = ig_I(d);
                                                   if (typeof d == "object" && d.firstChild)
                                                   {
                                                     var e = d.firstChild;
                                                     e.nodeType == 7 && e.nodeName == "xml" && d.removeChild(e);
                                                     b(d);
                                                     return;
                                                   };
                                                 };
                                                 b(ig_);
                                               },
                                               arguments.callee,
                                               "",
                                               0),
                                     c);
                  },
                  undefined,
                  "ig_ib",
                  255);
ig_jb = __typedjs(function  (a,b)
                  {
                    var c = "/ig/feedjson";
                    _sendx(c,
                           __typedjs(function  (d)
                                     {
                                       var e = {};
                                       try
                                       {
                                         e = eval("(" + d.slice(27) + ")");
                                       }
                                       catch (f) {
                                                   e = {};
                                                 };
                                       for (var h in b)
                                       {
                                         d = e[h] ? e[h] : ig_;
                                         b[h](d);
                                       };
                                       b = ig_;
                                     },
                                     arguments.callee,
                                     "",
                                     0),
                           ig_c,
                           a);
                  },
                  undefined,
                  "ig_jb",
                  256);
var ig_kb = ig_c,ig_lb = 0,ig_J = "",ig_K = {};
_IG_FetchFeedAsJSON = __typedjs(function  (a,b,c,d,e)
                                {
                                  if (typeof e != "object")
                                  e = {};
                                  var f = "fr_" + ig_lb++;
                                  a = "url=" + _esc(a);
                                  if (! isNaN(e.refreshInterval) && e.refreshInterval >= 0)
                                  a = _appendMaxAgeParameter(a,e.refreshInterval);
                                  if (c)
                                  a += "&val=" + _esc(c);
                                  if (d)
                                  a += "&sum=1";
                                  c = f + "=" + _esc(a);
                                  if (ig_kb)
                                  {
                                    d = {};
                                    d[f] = b;
                                    ig_jb(c,d);
                                  }
                                  else {
                                         if (ig_J != "")
                                         ig_J += "&";
                                         ig_J += c;
                                         ig_K[f] = b;
                                       };
                                },
                                undefined,
                                "_IG_FetchFeedAsJSON",
                                257);
ig_mb = __typedjs(function  ()
                  {
                    ig_kb = ig_b;
                    ig_J != "" && ig_jb(ig_J,ig_K);
                    ig_J = "";
                    ig_K = ig_;
                  },
                  undefined,
                  "ig_mb",
                  258);
_IG_AddEventHandler("domload",ig_mb);
_exportSymbols("google.gadgets.Fetchers",
               ["getContent",
                _IG_FetchContent,
                "getXMLContent",
                ig_ib,
                "getFeedAsJSON",
                _IG_FetchFeedAsJSON]);
ig_nb = __typedjs(function  (a,b,c)
                  {
                    var d = ig_u("m",b);
                    if (d)
                    d.ti = a;
                    var e = _gel("m_" + b + "_title");
                    if (e)
                    e.innerHTML = _hesc(a);
                    if (e = _gel("left_nav_m_" + b + "_title"))
                    if (e = e.firstChild)
                    {
                      e.title = a;
                      e.innerHTML = _hesc(a);
                      c || (e.offsetWidth == 0 ? _IG_RegisterOnloadHandler(function  ()
                                                                           {
                                                                             ig_L(e);
                                                                           }) : ig_L(e));
                    };
                  },
                  undefined,
                  "ig_nb",
                  259);
ig_L = __typedjs(function  (a,b)
                 {
                   b = b || 121;
                   var c = a.title;
                   if (! (a.offsetWidth <= b))
                   {
                     var d = 0,e = c.length,f,h = c.match(/\(\d+\)$/);
                     if (h = h ? h[0] : "")
                     {
                       h = " " + h;
                       e = c.length - h.length;
                       f = 5 * h.length;
                       if (f > b >> 1)
                       h = "";
                     };
                     for (; f = e - d >> 1;)
                     {
                       f = d + f;
                       a.innerHTML = ig_ob(c,f,h);
                       if (a.offsetWidth > b)
                       e = f
                       else d = f;
                     };
                     a.innerHTML = ig_ob(c,d,h);
                   };
                 },
                 undefined,
                 "ig_L",
                 260);
ig_ob = __typedjs(function  (a,b,c)
                  {
                    a = [a.substring(0,b),"..."];
                    c && a.push(c);
                    return _hesc(a.join(""));
                  },
                  undefined,
                  "ig_ob",
                  261);
ig_M = __typedjs(function  (a)
                 {
                   return typeof a == "undefined" || ! a || a == "undefined";
                 },
                 undefined,
                 "ig_M",
                 262);
ig_pb = __typedjs(function  (a,b)
                  {
                    ig_M(a) || ig_nb(b,_ig_gmid_(a));
                  },
                  undefined,
                  "ig_pb",
                  263);
_IFPC.registerService("set_title",ig_pb);
_IG_SetTitle = __typedjs(function  (a,b,c)
                         {
                           if (ig_M(b))
                           ig_a(new Error("Inline modules must specify their __MODULE_ID__ when using _IG_SetTitle"))
                           else ig_nb(a,b,c);
                         },
                         undefined,
                         "_IG_SetTitle",
                         264);
_IG_SetTitleDir = __typedjs(function  (a,b)
                            {
                              if (ig_M(b))
                              ig_a(new Error("Inline modules must specify their __MODULE_ID__ when using _IG_SetTitleDir"))
                              else {
                                     var c = ig_u("m",b);
                                     if (c)
                                     if (a)
                                     c.ti_dir = a
                                     else delete c.ti_dir;
                                     if (c = _gel("m_" + b + "_title"))
                                     a ? c.setAttribute("dir",a) : c.removeAttribute("dir");
                                     if (c = _gel("left_nav_m_" + b + "_title"))
                                     a ? c.setAttribute("dir",a) : c.removeAttribute("dir");
                                   };
                            },
                            undefined,
                            "_IG_SetTitleDir",
                            265);
_IG_FormatLeftNavTitles = __typedjs(function  ()
                                    {
                                      _IG_RegisterOnloadHandler(__typedjs(function  ()
                                                                          {
                                                                            var a = _gel("full_nav"),
                                                                                b = _gelsbyregex("div",
                                                                                                 /^left_nav_m_\d+_title$/,
                                                                                                 a);
                                                                            if (b)
                                                                            for (a = 0; a < b.length; ++a)
                                                                            {
                                                                              var c = b[a];
                                                                              (c = c.firstChild) && ig_L(c);
                                                                            };
                                                                            for (a = 0; a < _IG_MD.dt.length; ++a)
                                                                            if (c = _gel("tab" + _IG_MD.dt[a].i + "_view_title"))
                                                                            {
                                                                              b = c.className == "selected_tab_view_title" ? 80 : 100;
                                                                              (c = c.firstChild) && ig_L(c,
                                                                                                         b);
                                                                            };
                                                                          },
                                                                          arguments.callee,
                                                                          "",
                                                                          0));
                                    },
                                    undefined,
                                    "_IG_FormatLeftNavTitles",
                                    266);
;;
_delpromobox = __typedjs(function  ()
                         {
                           _xsetp("hp=0");
                           var a = _gel("promobox");
                           a.parentNode.removeChild(a);
                           return ig_c;
                         },
                         undefined,
                         "_delpromobox",
                         267);
_delmessage = __typedjs(function  ()
                        {
                          _xsetp("gm=0");
                          var a = _gel("googlemessage");
                          a.parentNode.removeChild(a);
                          return ig_c;
                        },
                        undefined,
                        "_delmessage",
                        268);
_delpromomod = __typedjs(function  ()
                         {
                           _xsetp("pm=0");
                           var a = _gel("promomod");
                           a.parentNode.removeChild(a);
                           return ig_c;
                         },
                         undefined,
                         "_delpromomod",
                         269);
;;
var ig_N = new (__typedjs(function  ()
                          {
                            this.da = {};
                            var a = window.onbeforeunload;
                            window.onbeforeunload = __typedjs(function  ()
                                                              {
                                                                a && a();
                                                                ig_N.Qe();
                                                              },
                                                              arguments.callee,
                                                              "window.onbeforeunload",
                                                              0);
                          },
                          undefined,
                          "ig_N",
                          270))();
ig_N.ud = __typedjs(function  (a,b)
                    {
                      this.da[a] = b;
                    },
                    undefined,
                    "ig_N.ud",
                    271);
ig_N.lf = __typedjs(function  (a)
                    {
                      delete this.da[a];
                    },
                    undefined,
                    "ig_N.lf",
                    272);
ig_N.Y = __typedjs(function  (a)
                   {
                     return this.da[a];
                   },
                   undefined,
                   "ig_N.Y",
                   273);
ig_N.Qe = __typedjs(function  ()
                    {
                      var a = [];
                      for (var b in this.da)
                      a.push(b + "=" + this.da[b]);
                      if (a.length)
                      {
                        b = ig_m();
                        a = "/ig/setp?et=" + _et + "&" + a.join("&");
                        b.open("GET",a,ig_c);
                        b.send(ig_);
                      };
                    },
                    undefined,
                    "ig_N.Qe",
                    274);
_IG_add_setp_parameter_onbeforeunload = __typedjs(function  (a,b)
                                                  {
                                                    ig_N.ud(a,b);
                                                  },
                                                  undefined,
                                                  "_IG_add_setp_parameter_onbeforeunload",
                                                  275);
_IG_remove_setp_parameter_onbeforeunload = __typedjs(function  (a)
                                                     {
                                                       ig_N.lf(a);
                                                     },
                                                     undefined,
                                                     "_IG_remove_setp_parameter_onbeforeunload",
                                                     276);
_IG_get_setp_parameter_onbeforeunload = __typedjs(function  (a)
                                                  {
                                                    return ig_N.Y(a);
                                                  },
                                                  undefined,
                                                  "_IG_get_setp_parameter_onbeforeunload",
                                                  277);
;;
var ig_O = {Oa: 0, Gb: 1, Kb: 2, Cb: 3},
    ig_P = __typedjs(function  ()
                     {
                       a = __typedjs(function  ()
                                     {
                                       var i = [],o = [];
                                       for (var p in j)
                                       if (j[p] && ! k[p])
                                       i.push(p)
                                       else ! j[p] && k[p] && o.push(p);
                                       i.length ? _IG_add_setp_parameter_onbeforeunload("exptabs",
                                                                                        i.join(":")) : _IG_remove_setp_parameter_onbeforeunload("exptabs");
                                       o.length ? _IG_add_setp_parameter_onbeforeunload("coltabs",
                                                                                        o.join(":")) : _IG_remove_setp_parameter_onbeforeunload("coltabs");
                                       _IG_add_setp_parameter_onbeforeunload("roster",l + "");
                                     },
                                     arguments.callee,
                                     "a",
                                     0);
                       b = __typedjs(function  (i)
                                     {
                                       _gel("gadget_set" + i).style.display = "block";
                                       _gel("triangle" + i).className = "v2minbox";
                                       j[i] = ig_b;
                                       _sendx("/ig/ui?xp=v2&action=expandTab&t=" + i,ig_,ig_c,ig_);
                                     },
                                     arguments.callee,
                                     "b",
                                     1);
                       c = __typedjs(function  (i)
                                     {
                                       _gel("gadget_set" + i).style.display = "none";
                                       _gel("triangle" + i).className = "v2maxbox";
                                       j[i] = ig_c;
                                       _sendx("/ig/ui?xp=v2&action=collapseTab&t=" + i,
                                              ig_,
                                              ig_c,
                                              ig_);
                                     },
                                     arguments.callee,
                                     "c",
                                     2);
                       d = __typedjs(function  ()
                                     {
                                       m.style.display = "none";
                                       n.style.display = "none";
                                       switch (e)
                                       {case
                                        ig_O.Oa :
                                          var i = _gel("section" + f + "_contents");
                                          i.className = "section_contents leftunselectedtab leftborder";
                                          _gel("tab" + f).className = "section_title unselected_section_title";
                                          _gel("tab_separator" + f).style.display = "";
                                          if (h)
                                          _gel("tab_separator" + h).style.display = ""
                                          else if (_gel("tab_separator_bot"))
                                               _gel("tab_separator_bot").style.display = "";
                                          break;
                                        case
                                        ig_O.Gb :
                                          i = _IG_getCurrentMaxId();
                                          i = _gel("left_nav_m_" + i + "_title");
                                          i.className = "gadget_title";
                                          break;
                                        case
                                        ig_O.Kb :
                                          i = _gel("updates_left_nav");
                                          i.className = "gadget_title";
                                          break;
                                        case
                                        ig_O.Cb :
                                          i = _gel("shares_left_nav");
                                          i.className = "gadget_title";
                                          break;};
                                     },
                                     arguments.callee,
                                     "d",
                                     3);
                       var e = ig_O.Oa,
                           f = ig_,
                           h = ig_,
                           g = {},
                           k = {},
                           j = {},
                           l = ig_b,
                           m = ig_,
                           n = ig_;
                       return {De: __typedjs(function  ()
                                             {
                                               m = _gel("left_nav_top_rounded_corners");
                                               n = _gel("left_nav_bottom_rounded_corners");
                                             },
                                             arguments.callee,
                                             "",
                                             4), zf: __typedjs(function  (i)
                                                               {
                                                                 f = i;
                                                               },
                                                               arguments.callee,
                                                               "",
                                                               5), qg: __typedjs(function  (i)
                                                                                 {
                                                                                   e = i;
                                                                                 },
                                                                                 arguments.callee,
                                                                                 "",
                                                                                 6), me: __typedjs(function  ()
                                                                                                   {
                                                                                                     return f;
                                                                                                   },
                                                                                                   arguments.callee,
                                                                                                   "",
                                                                                                   7), yf: __typedjs(function  ()
                                                                                                                     {
                                                                                                                       for (var i = _gel("section" + f + "_contents").nextSibling; i;)
                                                                                                                       {
                                                                                                                         if (i.id && i.id.match(/section\d+_contents/))
                                                                                                                         {
                                                                                                                           h = i.id.match(/\d+/);
                                                                                                                           break;
                                                                                                                         };
                                                                                                                         i = i.nextSibling;
                                                                                                                       };
                                                                                                                     },
                                                                                                                     arguments.callee,
                                                                                                                     "",
                                                                                                                     8), tg: __typedjs(function  ()
                                                                                                                                       {
                                                                                                                                         d();
                                                                                                                                       },
                                                                                                                                       arguments.callee,
                                                                                                                                       "",
                                                                                                                                       9), rc: __typedjs(function  (i)
                                                                                                                                                         {
                                                                                                                                                           d();
                                                                                                                                                           i = _gel("left_nav_m_" + i + "_title");
                                                                                                                                                           i.className = "selected_gadget_title gadget_title leftselectedtab surroundborder";
                                                                                                                                                           m.style.display = "";
                                                                                                                                                           n.style.display = "";
                                                                                                                                                           i.parentNode.insertBefore(m,
                                                                                                                                                                                     i);
                                                                                                                                                           i.parentNode.insertBefore(n,
                                                                                                                                                                                     i.nextSibling);
                                                                                                                                                           e = ig_O.Gb;
                                                                                                                                                         },
                                                                                                                                                         arguments.callee,
                                                                                                                                                         "",
                                                                                                                                                         10), sc: __typedjs(function  ()
                                                                                                                                                                            {
                                                                                                                                                                              d();
                                                                                                                                                                              var i = _gel("section" + f + "_contents");
                                                                                                                                                                              i.className = "selected_section_contents leftselectedtab surroundborder";
                                                                                                                                                                              _gel("tab" + f).className = "section_title selected_section_title";
                                                                                                                                                                              _gel("tab_separator" + f).style.display = "none";
                                                                                                                                                                              if (h)
                                                                                                                                                                              _gel("tab_separator" + h).style.display = "none"
                                                                                                                                                                              else if (_gel("tab_separator_bot"))
                                                                                                                                                                                   _gel("tab_separator_bot").style.display = "none";
                                                                                                                                                                              m.style.display = "";
                                                                                                                                                                              n.style.display = "";
                                                                                                                                                                              i.parentNode.insertBefore(m,
                                                                                                                                                                                                        i);
                                                                                                                                                                              i.parentNode.insertBefore(n,
                                                                                                                                                                                                        i.nextSibling);
                                                                                                                                                                              e = ig_O.Oa;
                                                                                                                                                                            },
                                                                                                                                                                            arguments.callee,
                                                                                                                                                                            "",
                                                                                                                                                                            11), Ae: __typedjs(function  ()
                                                                                                                                                                                               {
                                                                                                                                                                                                 d();
                                                                                                                                                                                                 var i = _gel("shares_left_nav");
                                                                                                                                                                                                 i.className = "selected_gadget_title gadget_title leftselectedtab surroundborder";
                                                                                                                                                                                                 m.style.display = "";
                                                                                                                                                                                                 n.style.display = "";
                                                                                                                                                                                                 i.parentNode.insertBefore(m,
                                                                                                                                                                                                                           i);
                                                                                                                                                                                                 i.parentNode.insertBefore(n,
                                                                                                                                                                                                                           i.nextSibling);
                                                                                                                                                                                                 e = ig_O.Cb;
                                                                                                                                                                                               },
                                                                                                                                                                                               arguments.callee,
                                                                                                                                                                                               "",
                                                                                                                                                                                               12), Be: __typedjs(function  ()
                                                                                                                                                                                                                  {
                                                                                                                                                                                                                    d();
                                                                                                                                                                                                                    var i = _gel("updates_left_nav");
                                                                                                                                                                                                                    i.className = "selected_gadget_title gadget_title leftselectedtab surroundborder";
                                                                                                                                                                                                                    m.style.display = "";
                                                                                                                                                                                                                    n.style.display = "";
                                                                                                                                                                                                                    i.parentNode.insertBefore(m,
                                                                                                                                                                                                                                              i);
                                                                                                                                                                                                                    i.parentNode.insertBefore(n,
                                                                                                                                                                                                                                              i.nextSibling);
                                                                                                                                                                                                                    e = ig_O.Kb;
                                                                                                                                                                                                                  },
                                                                                                                                                                                                                  arguments.callee,
                                                                                                                                                                                                                  "",
                                                                                                                                                                                                                  13), Of: __typedjs(function  (i)
                                                                                                                                                                                                                                     {
                                                                                                                                                                                                                                       var o = _gel("m_" + i + "_home_menu");
                                                                                                                                                                                                                                       if (o)
                                                                                                                                                                                                                                       o.style.display = "none";
                                                                                                                                                                                                                                       if (i = _gel("m_" + i + "_canvas_menu"))
                                                                                                                                                                                                                                       i.style.display = "";
                                                                                                                                                                                                                                     },
                                                                                                                                                                                                                                     arguments.callee,
                                                                                                                                                                                                                                     "",
                                                                                                                                                                                                                                     14), Pf: __typedjs(function  (i)
                                                                                                                                                                                                                                                        {
                                                                                                                                                                                                                                                          var o = _gel("m_" + i + "_canvas_menu");
                                                                                                                                                                                                                                                          if (o)
                                                                                                                                                                                                                                                          o.style.display = "none";
                                                                                                                                                                                                                                                          if (i = _gel("m_" + i + "_home_menu"))
                                                                                                                                                                                                                                                          i.style.display = "";
                                                                                                                                                                                                                                                        },
                                                                                                                                                                                                                                                        arguments.callee,
                                                                                                                                                                                                                                                        "",
                                                                                                                                                                                                                                                        15), xe: __typedjs(function  (i)
                                                                                                                                                                                                                                                                           {
                                                                                                                                                                                                                                                                             if (i = _gel("left_nav_" + i + "_title"))
                                                                                                                                                                                                                                                                             i.style.display = "none";
                                                                                                                                                                                                                                                                           },
                                                                                                                                                                                                                                                                           arguments.callee,
                                                                                                                                                                                                                                                                           "",
                                                                                                                                                                                                                                                                           16), Ff: __typedjs(function  (i)
                                                                                                                                                                                                                                                                                              {
                                                                                                                                                                                                                                                                                                if (i = _gel("left_nav_" + i + "_title"))
                                                                                                                                                                                                                                                                                                i.style.display = "";
                                                                                                                                                                                                                                                                                              },
                                                                                                                                                                                                                                                                                              arguments.callee,
                                                                                                                                                                                                                                                                                              "",
                                                                                                                                                                                                                                                                                              17), Gc: __typedjs(function  (i)
                                                                                                                                                                                                                                                                                                                 {
                                                                                                                                                                                                                                                                                                                   var o = _gel("m_" + i + "_b");
                                                                                                                                                                                                                                                                                                                   g[i] = o && o.style.display == "none";
                                                                                                                                                                                                                                                                                                                   i = ! _gel("remote_iframe_" + i);
                                                                                                                                                                                                                                                                                                                   var p = ig_b;
                                                                                                                                                                                                                                                                                                                   o.style.display = i && p ? "none" : "";
                                                                                                                                                                                                                                                                                                                 },
                                                                                                                                                                                                                                                                                                                 arguments.callee,
                                                                                                                                                                                                                                                                                                                 "",
                                                                                                                                                                                                                                                                                                                 18), Hc: __typedjs(function  (i)
                                                                                                                                                                                                                                                                                                                                    {
                                                                                                                                                                                                                                                                                                                                      var o = _gel("m_" + i + "_b");
                                                                                                                                                                                                                                                                                                                                      o.style.display = g[i] ? "none" : "";
                                                                                                                                                                                                                                                                                                                                    },
                                                                                                                                                                                                                                                                                                                                    arguments.callee,
                                                                                                                                                                                                                                                                                                                                    "",
                                                                                                                                                                                                                                                                                                                                    19), Hf: __typedjs(function  ()
                                                                                                                                                                                                                                                                                                                                                       {
                                                                                                                                                                                                                                                                                                                                                         if (f != "0")
                                                                                                                                                                                                                                                                                                                                                         {
                                                                                                                                                                                                                                                                                                                                                           _gel("rcbg").style.display = "";
                                                                                                                                                                                                                                                                                                                                                           _gel("col2").className = "botborder";
                                                                                                                                                                                                                                                                                                                                                         };
                                                                                                                                                                                                                                                                                                                                                       },
                                                                                                                                                                                                                                                                                                                                                       arguments.callee,
                                                                                                                                                                                                                                                                                                                                                       "",
                                                                                                                                                                                                                                                                                                                                                       20), Wc: __typedjs(function  ()
                                                                                                                                                                                                                                                                                                                                                                          {
                                                                                                                                                                                                                                                                                                                                                                            if (f == "0")
                                                                                                                                                                                                                                                                                                                                                                            {
                                                                                                                                                                                                                                                                                                                                                                              _gel("rcbg").style.display = "";
                                                                                                                                                                                                                                                                                                                                                                              _gel("col2").className = "botborder";
                                                                                                                                                                                                                                                                                                                                                                            };
                                                                                                                                                                                                                                                                                                                                                                          },
                                                                                                                                                                                                                                                                                                                                                                          arguments.callee,
                                                                                                                                                                                                                                                                                                                                                                          "",
                                                                                                                                                                                                                                                                                                                                                                          21), qc: __typedjs(function  ()
                                                                                                                                                                                                                                                                                                                                                                                             {
                                                                                                                                                                                                                                                                                                                                                                                               if (f == "0")
                                                                                                                                                                                                                                                                                                                                                                                               {
                                                                                                                                                                                                                                                                                                                                                                                                 _gel("rcbg").style.display = "none";
                                                                                                                                                                                                                                                                                                                                                                                                 _gel("col2").className = "topbotborder";
                                                                                                                                                                                                                                                                                                                                                                                               };
                                                                                                                                                                                                                                                                                                                                                                                             },
                                                                                                                                                                                                                                                                                                                                                                                             arguments.callee,
                                                                                                                                                                                                                                                                                                                                                                                             "",
                                                                                                                                                                                                                                                                                                                                                                                             22), Xd: __typedjs(function  (i)
                                                                                                                                                                                                                                                                                                                                                                                                                {
                                                                                                                                                                                                                                                                                                                                                                                                                  k[i] = ig_b;
                                                                                                                                                                                                                                                                                                                                                                                                                  j[i] = ig_b;
                                                                                                                                                                                                                                                                                                                                                                                                                },
                                                                                                                                                                                                                                                                                                                                                                                                                arguments.callee,
                                                                                                                                                                                                                                                                                                                                                                                                                "",
                                                                                                                                                                                                                                                                                                                                                                                                                23), Jd: __typedjs(function  (i)
                                                                                                                                                                                                                                                                                                                                                                                                                                   {
                                                                                                                                                                                                                                                                                                                                                                                                                                     k[i] = ig_c;
                                                                                                                                                                                                                                                                                                                                                                                                                                     j[i] = ig_c;
                                                                                                                                                                                                                                                                                                                                                                                                                                   },
                                                                                                                                                                                                                                                                                                                                                                                                                                   arguments.callee,
                                                                                                                                                                                                                                                                                                                                                                                                                                   "",
                                                                                                                                                                                                                                                                                                                                                                                                                                   24), Rf: __typedjs(function  (i,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                 o)
                                                                                                                                                                                                                                                                                                                                                                                                                                                      {
                                                                                                                                                                                                                                                                                                                                                                                                                                                        if (! i)
                                                                                                                                                                                                                                                                                                                                                                                                                                                        i = window.event;
                                                                                                                                                                                                                                                                                                                                                                                                                                                        i.cancelBubble = ig_b;
                                                                                                                                                                                                                                                                                                                                                                                                                                                        i.stopPropagation && i.stopPropagation();
                                                                                                                                                                                                                                                                                                                                                                                                                                                        _gel("gadget_set" + o).style.display == "none" ? b(o) : c(o);
                                                                                                                                                                                                                                                                                                                                                                                                                                                        a();
                                                                                                                                                                                                                                                                                                                                                                                                                                                      },
                                                                                                                                                                                                                                                                                                                                                                                                                                                      arguments.callee,
                                                                                                                                                                                                                                                                                                                                                                                                                                                      "",
                                                                                                                                                                                                                                                                                                                                                                                                                                                      25), Qf: __typedjs(function  ()
                                                                                                                                                                                                                                                                                                                                                                                                                                                                         {
                                                                                                                                                                                                                                                                                                                                                                                                                                                                           if (_gel("talk_container"))
                                                                                                                                                                                                                                                                                                                                                                                                                                                                           {
                                                                                                                                                                                                                                                                                                                                                                                                                                                                             l = ! l;
                                                                                                                                                                                                                                                                                                                                                                                                                                                                             _gel("roster_triangle").className = l ? "v2minbox" : "v2maxbox";
                                                                                                                                                                                                                                                                                                                                                                                                                                                                             if (window.gTalkNotifier && window.gTalkNotifier._setRosterExpanded)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                             window.gTalkNotifier._setRosterExpanded(l)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                             else _gel("talk_container").style.display = l ? "" : "none";
                                                                                                                                                                                                                                                                                                                                                                                                                                                                             _log("roster_toggle:" + (l ? "1" : "0"));
                                                                                                                                                                                                                                                                                                                                                                                                                                                                             a();
                                                                                                                                                                                                                                                                                                                                                                                                                                                                           };
                                                                                                                                                                                                                                                                                                                                                                                                                                                                         },
                                                                                                                                                                                                                                                                                                                                                                                                                                                                         arguments.callee,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                         "",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                         26), rf: __typedjs(function  (i)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            {
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              l = i;
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            },
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            arguments.callee,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            "",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            27), Je: __typedjs(function  ()
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               {
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 return l;
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               },
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               arguments.callee,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               "",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               28), ye: __typedjs(function  (i)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  {
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    if (i = _gel(i))
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    i.style.display = "none";
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  },
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  arguments.callee,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  "",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  29), Zc: __typedjs(function  (i)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     {
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       var o = _gel("DD_" + i + "_zip");
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       if (o)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       {
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         i = (i = _gel("m_" + i + "_b")) && i.style.display == "none";
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         o.innerHTML = i ? IG_MSGS.MAXG : IG_MSGS.MING;
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       };
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     },
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     arguments.callee,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     "",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     30), Yc: __typedjs(function  (i)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        {
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          var o = _gel("m_" + i + "_b");
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          i = o && o.style.display == "none";
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          if ((o = o.nextSibling) && o.className)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          o.className = i ? "rnd_modtitle" : "rnd_modboxin";
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        },
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        arguments.callee,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        "",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        31)};
                     },
                     undefined,
                     "ig_P",
                     278)();
_IG_LN_init = __typedjs(function  (a)
                        {
                          _IG_AddEventHandler("modulemaximize",ig_P.rc);
                          _IG_AddEventHandler("modulemaximize",ig_P.Of);
                          _IG_AddEventHandler("modulemaximize",ig_P.Gc);
                          _IG_AddEventHandler("modulemaximize",ig_P.Wc);
                          _IG_AddEventHandler("moduleunmaximize",ig_P.sc);
                          _IG_AddEventHandler("moduleunmaximize",ig_P.Pf);
                          _IG_AddEventHandler("moduleunmaximize",ig_P.Hc);
                          _IG_AddEventHandler("moduleunmaximize",ig_P.qc);
                          _IG_AddEventHandler("moduledelete",ig_P.xe);
                          _IG_AddEventHandler("moduleundelete",ig_P.Ff);
                          _IG_AddEventHandler("modulezip",ig_P.Zc);
                          _IG_AddEventHandler("modulezip",ig_P.Yc);
                          _IG_AddEventHandler("moduleunzip",ig_P.Zc);
                          _IG_AddEventHandler("moduleunzip",ig_P.Yc);
                          _IG_RegisterOnloadHandler(ig_P.yf);
                          _IG_RegisterOnloadHandler(ig_P.Hf);
                          _IG_RegisterOnloadHandler(ig_P.De);
                          ig_P.zf(a);
                        },
                        undefined,
                        "_IG_LN_init",
                        279);
var _IG_get_selected_tab_id = ig_P.me,
    _IG_expand_tab_onload = ig_P.Xd,
    _IG_collapse_tab_onload = ig_P.Jd,
    _IG_toggle_tab = ig_P.Rf,
    _IG_toggle_roster = ig_P.Qf,
    _IG_roster_state_onload = ig_P.rf,
    _IG_is_roster_expanded = ig_P.Je,
    _IG_hide_promo = ig_P.ye,
    _IG_maybe_unzip_module = ig_P.Gc,
    _IG_maybe_zip_module = ig_P.Hc,
    _IG_hideRoundedCornerOnRestore = ig_P.qc,
    _IG_showRoundedCornerOnMaximize = ig_P.Wc,
    _IG_highlight_invites = ig_P.Ae,
    _IG_highlight_updates = ig_P.Be,
    _IG_highlight_module = ig_P.rc,
    _IG_highlight_tab = ig_P.sc;
