(__typedjs(function  ()
           {
             var g = true,
                 h = null,
                 j = false,
                 aa = (__new(Date,[])).getTime(),
                 k = __typedjs(function  (a)
                               {
                                 var b = (new Date()).getTime() - aa;
                                 b = "&dtd=" + (b < 1000.0 ? b : "M");
                                 return a + b;
                               },
                               arguments.callee,
                               "k",
                               0);
             var l = this,
                 ba = __typedjs(function  (a,b,c)
                                {
                                  a = a.split(".");
                                  c = c || l;
                                  ! (a[0] in c) && c.execScript && c.execScript("var " + a[0]);
                                  for (var d; a.length && (d = a.shift());)
                                  if (! a.length && b !== undefined)
                                  c[d] = b
                                  else c = c[d] ? c[d] : (c[d] = {});
                                },
                                arguments.callee,
                                "ba",
                                1),
                 m = __typedjs(function  (a)
                               {
                                 var b = typeof a;
                                 if (b == "object")
                                 if (a)
                                 {
                                   if (a instanceof Array || ! (a instanceof Object) && Object.prototype.toString.call(a) == "[object Array]" || typeof a.length == "number" && typeof a.splice != "undefined" && typeof a.propertyIsEnumerable != "undefined" && ! a.propertyIsEnumerable("splice"))
                                   return "array";
                                   if (! (a instanceof Object) && (Object.prototype.toString.call(a) == "[object Function]" || typeof a.call != "undefined" && typeof a.propertyIsEnumerable != "undefined" && ! a.propertyIsEnumerable("call")))
                                   return "function";
                                 }
                                 else return "null"
                                 else if (b == "function" && typeof a.call == "undefined")
                                      return "object";
                                 return b;
                               },
                               arguments.callee,
                               "m",
                               2),
                 o = __typedjs(function  (a)
                               {
                                 return m(a) == "array";
                               },
                               arguments.callee,
                               "o",
                               3),
                 ca = __typedjs(function  (a)
                                {
                                  var b = m(a);
                                  return b == "array" || b == "object" && typeof a.length == "number";
                                },
                                arguments.callee,
                                "ca",
                                4),
                 p = __typedjs(function  (a)
                               {
                                 return typeof a == "string";
                               },
                               arguments.callee,
                               "p",
                               5),
                 da = __typedjs(function  (a)
                                {
                                  a = m(a);
                                  return a == "object" || a == "array" || a == "function";
                                },
                                arguments.callee,
                                "da",
                                6),
                 ea = __typedjs(function  (a)
                                {
                                  var b = m(a);
                                  if (b == "object" || b == "array")
                                  {
                                    if (a.clone)
                                    return a.clone.call(a);
                                    b = b == "array" ? [] : {};
                                    for (var c in a)
                                    b[c] = ea(a[c]);
                                    return b;
                                  };
                                  return a;
                                },
                                arguments.callee,
                                "ea",
                                7),
                 fa = __typedjs(function  (a,b)
                                {
                                  var c = b || l;
                                  if (arguments.length > 2)
                                  {
                                    var d = Array.prototype.slice.call(arguments,2);
                                    return __typedjs(function  ()
                                                     {
                                                       var e = Array.prototype.slice.call(arguments);
                                                       Array.prototype.unshift.apply(e,d);
                                                       return a.apply(c,e);
                                                     },
                                                     arguments.callee,
                                                     "",
                                                     0);
                                  }
                                  else return __typedjs(function  ()
                                                        {
                                                          return a.apply(c,arguments);
                                                        },
                                                        arguments.callee,
                                                        "",
                                                        1);
                                },
                                arguments.callee,
                                "fa",
                                8),
                 ga = Date.now || function  ()
                                  {
                                    return + new Date();
                                  },
                 q = __typedjs(function  (a,b,c)
                               {
                                 ba(a,b,c);
                               },
                               arguments.callee,
                               "q",
                               10);
             var r = Array.prototype,
                 ha = r.forEach ? __typedjs(function  (a,b,c)
                                            {
                                              r.forEach.call(a,b,c);
                                            },
                                            arguments.callee,
                                            "ha",
                                            11) : __typedjs(function  (a,b,c)
                                                            {
                                                              for (var d = a.length,
                                                                       e = p(a) ? a.split("") : a,
                                                                       f = 0; f < d; f++)
                                                              f in e && b.call(c,e[f],f,a);
                                                            },
                                                            arguments.callee,
                                                            "ha",
                                                            12),
                 ia = __typedjs(function  ()
                                {
                                  return r.concat.apply(r,arguments);
                                },
                                arguments.callee,
                                "ia",
                                13),
                 ja = __typedjs(function  (a)
                                {
                                  if (o(a))
                                  return ia(a)
                                  else {
                                         for (var b = [],c = 0,d = a.length; c < d; c++)
                                         b[c] = a[c];
                                         return b;
                                       };
                                },
                                arguments.callee,
                                "ja",
                                14);
             var s = __typedjs(function  (a,b)
                               {
                                 this.width = a;
                                 this.height = b;
                               },
                               arguments.callee,
                               "s",
                               15);
             s.prototype.clone = __typedjs(function  ()
                                           {
                                             return __new(s,[this.width,this.height]);
                                           },
                                           arguments.callee,
                                           "s.prototype.clone",
                                           16);
             s.prototype.toString = __typedjs(function  ()
                                              {
                                                return "(" + this.width + " x " + this.height + ")";
                                              },
                                              arguments.callee,
                                              "s.prototype.toString",
                                              17);
             s.prototype.ceil = __typedjs(function  ()
                                          {
                                            this.width = Math.ceil(this.width);
                                            this.height = Math.ceil(this.height);
                                            return this;
                                          },
                                          arguments.callee,
                                          "s.prototype.ceil",
                                          18);
             s.prototype.floor = __typedjs(function  ()
                                           {
                                             this.width = Math.floor(this.width);
                                             this.height = Math.floor(this.height);
                                             return this;
                                           },
                                           arguments.callee,
                                           "s.prototype.floor",
                                           19);
             s.prototype.round = __typedjs(function  ()
                                           {
                                             this.width = Math.round(this.width);
                                             this.height = Math.round(this.height);
                                             return this;
                                           },
                                           arguments.callee,
                                           "s.prototype.round",
                                           20);
             s.prototype.scale = __typedjs(function  (a)
                                           {
                                             this.width *= a;
                                             this.height *= a;
                                             return this;
                                           },
                                           arguments.callee,
                                           "s.prototype.scale",
                                           21);
             var ka = __typedjs(function  (a,b,c)
                                {
                                  for (var d in a)
                                  b.call(c,a[d],d,a);
                                },
                                arguments.callee,
                                "ka",
                                22);
             var qa = __typedjs(function  (a,b)
                                {
                                  if (b)
                                  return a.replace(la,"&amp;").replace(ma,"&lt;").replace(na,
                                                                                          "&gt;").replace(oa,
                                                                                                          "&quot;")
                                  else {
                                         if (! pa.test(a))
                                         return a;
                                         if (a.indexOf("&") != - 1)
                                         a = a.replace(la,"&amp;");
                                         if (a.indexOf("<") != - 1)
                                         a = a.replace(ma,"&lt;");
                                         if (a.indexOf(">") != - 1)
                                         a = a.replace(na,"&gt;");
                                         if (a.indexOf("\"") != - 1)
                                         a = a.replace(oa,"&quot;");
                                         return a;
                                       };
                                },
                                arguments.callee,
                                "qa",
                                23),
                 la = /&/g,
                 ma = /</g,
                 na = />/g,
                 oa = /\"/g,
                 pa = /[&<>\"]/,
                 ta = __typedjs(function  (a)
                                {
                                  if (a.indexOf("&") != - 1)
                                  return "document" in l && a.indexOf("<") == - 1 ? ra(a) : sa(a);
                                  return a;
                                },
                                arguments.callee,
                                "ta",
                                24),
                 ra = __typedjs(function  (a)
                                {
                                  var b = l.document.createElement("a");
                                  b.innerHTML = a;
                                  b.normalize && b.normalize();
                                  a = b.firstChild.nodeValue;
                                  b.innerHTML = "";
                                  return a;
                                },
                                arguments.callee,
                                "ra",
                                25),
                 sa = __typedjs(function  (a)
                                {
                                  return a.replace(/&([^;]+);/g,
                                                   __typedjs(function  (b,c)
                                                             {
                                                               switch (c)
                                                               {case
                                                                "amp" :
                                                                  return "&";
                                                                case
                                                                "lt" :
                                                                  return "<";
                                                                case
                                                                "gt" :
                                                                  return ">";
                                                                case
                                                                "quot" :
                                                                  return "\"";
                                                                default:
                                                                  if (c.charAt(0) == "#")
                                                                  {
                                                                    c = Number("0" + c.substr(1));
                                                                    if (! isNaN(c))
                                                                    return String.fromCharCode(c);
                                                                  };
                                                                  return b;};
                                                             },
                                                             arguments.callee,
                                                             "",
                                                             0));
                                },
                                arguments.callee,
                                "sa",
                                26),
                 ua = __typedjs(function  (a,b)
                                {
                                  for (var c = b.length,d = 0; d < c; d++)
                                  {
                                    var e = c == 1 ? b : b.charAt(d);
                                    if (a.charAt(0) == e && a.charAt(a.length - 1) == e)
                                    return a.substring(1,a.length - 1);
                                  };
                                  return a;
                                },
                                arguments.callee,
                                "ua",
                                27),
                 va = __typedjs(function  (a,b)
                                {
                                  var c = 0;
                                  a = String(a).replace(/^[\s\xa0]+|[\s\xa0]+$/g,"").split(".");
                                  b = String(b).replace(/^[\s\xa0]+|[\s\xa0]+$/g,"").split(".");
                                  for (var d = Math.max(a.length,b.length),
                                           e = 0; c == 0 && e < d; e++)
                                  {
                                    var f = a[e] || "",
                                        i = b[e] || "",
                                        n = __new(RegExp,["(\\d*)(\\D*)","g"]),
                                        z = __new(RegExp,["(\\d*)(\\D*)","g"]);
                                    do
                                    {
                                      var G = n.exec(f) || ["","",""],H = z.exec(i) || ["","",""];
                                      if (G[0].length == 0 && H[0].length == 0)
                                      break;
                                      c = G[1].length == 0 ? 0 : parseInt(G[1],10);
                                      var ob = H[1].length == 0 ? 0 : parseInt(H[1],10);
                                      c = t(c,ob) || t(G[2].length == 0,H[2].length == 0) || t(G[2],
                                                                                               H[2]);
                                    } while (c == 0);
                                  };
                                  return c;
                                },
                                arguments.callee,
                                "va",
                                28),
                 t = __typedjs(function  (a,b)
                               {
                                 if (a < b)
                                 return - 1
                                 else if (a > b)
                                      return 1;
                                 return 0;
                               },
                               arguments.callee,
                               "t",
                               29);
             ga();
             var u,
                 v,
                 w,
                 x,
                 y,
                 wa,
                 xa,
                 ya,
                 za,
                 Aa = __typedjs(function  ()
                                {
                                  return l.navigator ? l.navigator.userAgent : h;
                                },
                                arguments.callee,
                                "Aa",
                                30),
                 A = __typedjs(function  ()
                               {
                                 return l.navigator;
                               },
                               arguments.callee,
                               "A",
                               31),
                 Ba = __typedjs(function  ()
                                {
                                  y = x = w = v = u = j;
                                  var a;
                                  if (a = Aa())
                                  {
                                    var b = A();
                                    u = a.indexOf("Opera") == 0;
                                    v = ! u && a.indexOf("MSIE") != - 1;
                                    x = (w = ! u && a.indexOf("WebKit") != - 1) && a.indexOf("Mobile") != - 1;
                                    y = ! u && ! w && b.product == "Gecko";
                                  };
                                },
                                arguments.callee,
                                "Ba",
                                32);
             Ba();
             var B = u,
                 C = v,
                 Ca = y,
                 D = w,
                 Da = x,
                 Ea = __typedjs(function  ()
                                {
                                  var a = A();
                                  return a && a.platform || "";
                                },
                                arguments.callee,
                                "Ea",
                                33),
                 E = Ea(),
                 Fa = __typedjs(function  ()
                                {
                                  wa = E.indexOf("Mac") != - 1;
                                  xa = E.indexOf("Win") != - 1;
                                  ya = E.indexOf("Linux") != - 1;
                                  za = ! ! A() && (A().appVersion || "").indexOf("X11") != - 1;
                                },
                                arguments.callee,
                                "Fa",
                                34);
             Fa();
             var Ga = wa,
                 Ha = xa,
                 Ia = ya,
                 Ja = __typedjs(function  ()
                                {
                                  var a = "",b;
                                  if (B && l.opera)
                                  {
                                    a = l.opera.version;
                                    a = typeof a == "function" ? a() : a;
                                  }
                                  else {
                                         if (Ca)
                                         b = /rv\:([^\);]+)(\)|;)/
                                         else if (C)
                                              b = /MSIE\s+([^\);]+)(\)|;)/
                                              else if (D)
                                                   b = /WebKit\/(\S+)/;
                                         if (b)
                                         a = (a = b.exec(Aa())) ? a[1] : "";
                                       };
                                  return a;
                                },
                                arguments.callee,
                                "Ja",
                                35),
                 Ka = Ja(),
                 La = {},
                 F = __typedjs(function  (a)
                               {
                                 return La[a] || (La[a] = va(Ka,a) >= 0);
                               },
                               arguments.callee,
                               "F",
                               36);
             var Ma = __typedjs(function  (a)
                                {
                                  return p(a) ? document.getElementById(a) : a;
                                },
                                arguments.callee,
                                "Ma",
                                37),
                 Na = Ma,
                 Pa = __typedjs(function  (a,b)
                                {
                                  ka(b,
                                     __typedjs(function  (c,d)
                                               {
                                                 if (d == "style")
                                                 a.style.cssText = c
                                                 else if (d == "class")
                                                      a.className = c
                                                      else if (d == "for")
                                                           a.htmlFor = c
                                                           else if (d in Oa)
                                                                a.setAttribute(Oa[d],c)
                                                                else a[d] = c;
                                               },
                                               arguments.callee,
                                               "",
                                               0));
                                },
                                arguments.callee,
                                "Pa",
                                38),
                 Oa = {cellpadding: "cellPadding", cellspacing: "cellSpacing", colspan: "colSpan", rowspan: "rowSpan", valign: "vAlign", height: "height", width: "width", usemap: "useMap", frameborder: "frameBorder", type: "type"},
                 Qa = __typedjs(function  (a)
                                {
                                  var b = a.document;
                                  if (D && ! F("500") && ! Da)
                                  {
                                    if (typeof a.innerHeight == "undefined")
                                    a = window;
                                    b = a.innerHeight;
                                    var c = a.document.documentElement.scrollHeight;
                                    if (a == a.top)
                                    if (c < b)
                                    b -= 15;
                                    return __new(s,[a.innerWidth,b]);
                                  };
                                  a = b.compatMode == "CSS1Compat" && (! B || B && F("9.50")) ? b.documentElement : b.body;
                                  return __new(s,[a.clientWidth,a.clientHeight]);
                                },
                                arguments.callee,
                                "Qa",
                                39),
                 Sa = __typedjs(function  ()
                                {
                                  return Ra(document,arguments);
                                },
                                arguments.callee,
                                "Sa",
                                40),
                 Ra = __typedjs(function  (a,b)
                                {
                                  var c = b[0],d = b[1];
                                  if (C && d && (d.name || d.type))
                                  {
                                    c = ["<",c];
                                    d.name && c.push(" name=\"",qa(d.name),"\"");
                                    if (d.type)
                                    {
                                      c.push(" type=\"",qa(d.type),"\"");
                                      d = ea(d);
                                      delete d.type;
                                    };
                                    c.push(">");
                                    c = c.join("");
                                  };
                                  var e = a.createElement(c);
                                  if (d)
                                  if (p(d))
                                  e.className = d
                                  else Pa(e,d);
                                  if (b.length > 2)
                                  {
                                    d = __typedjs(function  (i)
                                                  {
                                                    if (i)
                                                    e.appendChild(p(i) ? a.createTextNode(i) : i);
                                                  },
                                                  arguments.callee,
                                                  "d",
                                                  0);
                                    for (c = 2; c < b.length; c++)
                                    {
                                      var f = b[c];
                                      ca(f) && ! (da(f) && f.nodeType > 0) ? ha(Ta(f) ? ja(f) : f,
                                                                                d) : d(f);
                                    };
                                  };
                                  return e;
                                },
                                arguments.callee,
                                "Ra",
                                41),
                 Ua = __typedjs(function  (a,b)
                                {
                                  a.appendChild(b);
                                },
                                arguments.callee,
                                "Ua",
                                42),
                 Ta = __typedjs(function  (a)
                                {
                                  if (a && typeof a.length == "number")
                                  if (da(a))
                                  return typeof a.item == "function" || typeof a.item == "string"
                                  else if (m(a) == "function")
                                       return typeof a.item == "function";
                                  return j;
                                },
                                arguments.callee,
                                "Ta",
                                43);
             I = __typedjs(function  (a,b)
                           {
                             a = parseFloat(a);
                             return isNaN(a) || a > 1 || a < 0 ? b : a;
                           },
                           arguments.callee,
                           "I",
                           44);
             Va = __typedjs(function  (a,b)
                            {
                              if (a == "true")
                              return g;
                              if (a == "false")
                              return j;
                              return b;
                            },
                            arguments.callee,
                            "Va",
                            45);
             J = __typedjs(function  (a,b)
                           {
                             var c = /^([\w-]+\.)+[\w-]{2,}(\:[0-9]+)?$/;
                             return c.test(a) ? a : b;
                           },
                           arguments.callee,
                           "J",
                           46);
             ;;
             var Wa = document,
                 Xa = Va("false",j),
                 Ya = Va("false",j),
                 Za = Va("false",j),
                 K = window;
             var $a = "pagead2.googlesyndication.com",
                 ab = "googleads.g.doubleclick.net",
                 bb = "googleads2.g.doubleclick.net",
                 cb = "pubads.g.doubleclick.net",
                 db = "securepubads.g.doubleclick.net",
                 eb = "partner.googleadservices.com",
                 fb = J("pagead2.googlesyndication.com",$a),
                 gb = J("googleads.g.doubleclick.net",ab),
                 hb = J("",bb),
                 ib = J("pagead2.googlesyndication.com",$a);
             J("pubads.g.doubleclick.net",cb);
             J("partner.googleadservices.com",eb);
             J("securepubads.g.doubleclick.net",db);
             var L = __typedjs(function  (a,b)
                               {
                                 for (var c in a)
                                 Object.prototype.hasOwnProperty.call(a,c) && b.call(h,a[c],c,a);
                               },
                               arguments.callee,
                               "L",
                               47),
                 jb = __typedjs(function  (a)
                                {
                                  if (arguments.length < 2)
                                  return a.length;
                                  for (var b = 1,c = arguments.length; b < c; ++b)
                                  a.push(arguments[b]);
                                  return a.length;
                                },
                                arguments.callee,
                                "jb",
                                48);
             M = __typedjs(function  (a)
                           {
                             return typeof encodeURIComponent == "function" ? encodeURIComponent(a) : escape(a);
                           },
                           arguments.callee,
                           "M",
                           49);
             kb = __typedjs(function  (a,b,c)
                            {
                              var d = document.createElement("script");
                              d.type = "text/javascript";
                              if (b)
                              d.onload = b;
                              if (c)
                              d.id = c;
                              d.src = a;
                              var e = document.getElementsByTagName("head")[0];
                              if (! e)
                              return j;
                              window.setTimeout(__typedjs(function  ()
                                                          {
                                                            e.appendChild(d);
                                                          },
                                                          arguments.callee,
                                                          "",
                                                          0),
                                                0);
                              return g;
                            },
                            arguments.callee,
                            "kb",
                            50);
             lb = __typedjs(function  (a,b)
                            {
                              a.google_image_requests || (a.google_image_requests = []);
                              var c = __new(Image,[]);
                              c.src = b;
                              a.google_image_requests.push(c);
                            },
                            arguments.callee,
                            "lb",
                            51);
             mb = __typedjs(function  (a)
                            {
                              if (a in nb)
                              return nb[a];
                              return nb[a] = navigator.userAgent.toLowerCase().indexOf(a) != - 1;
                            },
                            arguments.callee,
                            "mb",
                            52);
             var nb = {};
             pb = __typedjs(function  ()
                            {
                              if (navigator.plugins && navigator.mimeTypes.length)
                              {
                                var a = navigator.plugins["Shockwave Flash"];
                                if (a && a.description)
                                return a.description.replace(/([a-zA-Z]|\s)+/,"").replace(/(\s)+r/,
                                                                                          ".");
                              }
                              else if (navigator.userAgent && navigator.userAgent.indexOf("Windows CE") >= 0)
                                   {
                                     a = 3;
                                     for (var b = 1; b;)
                                     try
                                     {
                                       b = __new(ActiveXObject,
                                                 ["ShockwaveFlash.ShockwaveFlash." + (a + 1)]);
                                       a++;
                                     }
                                     catch (c) {
                                                 b = h;
                                               };
                                     return a.toString();
                                   }
                                   else if (mb("msie") && ! window.opera)
                                        {
                                          b = h;
                                          try
                                          {
                                            b = __new(ActiveXObject,
                                                      ["ShockwaveFlash.ShockwaveFlash.7"]);
                                          }
                                          catch (d) {
                                                      a = 0;
                                                      try
                                                      {
                                                        b = __new(ActiveXObject,
                                                                  ["ShockwaveFlash.ShockwaveFlash.6"]);
                                                        a = 6;
                                                        b.AllowScriptAccess = "always";
                                                      }
                                                      catch (e) {
                                                                  if (a == 6)
                                                                  return a.toString();
                                                                };
                                                      try
                                                      {
                                                        b = __new(ActiveXObject,
                                                                  ["ShockwaveFlash.ShockwaveFlash"]);
                                                      }
                                                      catch (f) {
                                                                };
                                                    };
                                          if (b)
                                          {
                                            a = b.GetVariable("$version").split(" ")[1];
                                            return a.replace(/,/g,".");
                                          };
                                        };
                              return "0";
                            },
                            arguments.callee,
                            "pb",
                            53);
             N = __typedjs(function  (a)
                           {
                             var b = a.google_ad_format;
                             if (b)
                             return b.indexOf("_0ads") > 0;
                             return a.google_ad_output != "html" && a.google_num_radlinks > 0;
                           },
                           arguments.callee,
                           "N",
                           54);
             O = __typedjs(function  (a)
                           {
                             return ! ! a && a.indexOf("_sdo") != - 1;
                           },
                           arguments.callee,
                           "O",
                           55);
             P = __typedjs(function  (a,b)
                           {
                             var c = Math.random();
                             if (c < b)
                             {
                               b = Math.floor(c / b * a.length);
                               return a[b];
                             };
                             return "";
                           },
                           arguments.callee,
                           "P",
                           56);
             var qb = __typedjs(function  (a)
                                {
                                  a.u_tz = - (__new(Date,[])).getTimezoneOffset();
                                  a.u_his = window.history.length;
                                  a.u_java = navigator.javaEnabled();
                                  if (window.screen)
                                  {
                                    a.u_h = window.screen.height;
                                    a.u_w = window.screen.width;
                                    a.u_ah = window.screen.availHeight;
                                    a.u_aw = window.screen.availWidth;
                                    a.u_cd = window.screen.colorDepth;
                                  };
                                  if (navigator.plugins)
                                  a.u_nplug = navigator.plugins.length;
                                  if (navigator.mimeTypes)
                                  a.u_nmime = navigator.mimeTypes.length;
                                },
                                arguments.callee,
                                "qb",
                                57),
                 rb = __typedjs(function  (a)
                                {
                                  var b = K;
                                  if (a && b.top != b)
                                  b = b.top;
                                  try
                                  {
                                    return b.document && ! b.document.body ? __new(s,
                                                                                   [- 1,
                                                                                    - 1]) : Qa(b || window);
                                  }
                                  catch (c) {
                                              return __new(s,[- 12245933,- 12245933]);
                                            };
                                },
                                arguments.callee,
                                "rb",
                                58),
                 sb = __typedjs(function  (a,b)
                                {
                                  var c = a.length;
                                  if (c == 0)
                                  return 0;
                                  b = b || 305419896;
                                  for (var d = 0; d < c; d++)
                                  {
                                    var e = a.charCodeAt(d);
                                    b ^= (b << 5) + (b >> 2) + e & -1;
                                  };
                                  return b;
                                },
                                arguments.callee,
                                "sb",
                                59),
                 tb = __typedjs(function  (a)
                                {
                                  if (a == a.top)
                                  return 0;
                                  var b = [];
                                  b.push(document.URL);
                                  a.name && b.push(a.name);
                                  a = g;
                                  a = rb(! a);
                                  b.push(a.width.toString());
                                  b.push(a.height.toString());
                                  b = sb(b.join(""));
                                  return b > 0 ? b : 0 + b;
                                },
                                arguments.callee,
                                "tb",
                                60);
             var ub = {google_ad_channel: "channel", google_ad_host: "host", google_ad_host_channel: "h_ch", google_ad_host_tier_id: "ht_id", google_ad_section: "region", google_ad_type: "ad_type", google_adtest: "adtest", google_allow_expandable_ads: "ea", google_alternate_ad_url: "alternate_ad_url", google_alternate_color: "alt_color", google_bid: "bid", google_city: "gcs", google_color_bg: "color_bg", google_color_border: "color_border", google_color_line: "color_line", google_color_link: "color_link", google_color_text: "color_text", google_color_url: "color_url", google_contents: "contents", google_country: "gl", google_cust_age: "cust_age", google_cust_ch: "cust_ch", google_cust_gender: "cust_gender", google_cust_id: "cust_id", google_cust_interests: "cust_interests", google_cust_job: "cust_job", google_cust_l: "cust_l", google_cust_lh: "cust_lh", google_cust_u_url: "cust_u_url", google_disable_video_autoplay: "disable_video_autoplay", google_ed: "ed", google_encoding: "oe", google_feedback: "feedback_link", google_flash_version: "flash", google_font_face: "f", google_font_size: "fs", google_hints: "hints", google_kw: "kw", google_kw_type: "kw_type", google_language: "hl", google_page_url: "url", google_region: "gr", google_reuse_colors: "reuse_colors", google_safe: "adsafe", google_tag_info: "gut", google_targeting: "targeting", google_ui_features: "ui", google_ui_version: "uiv", google_video_doc_id: "video_doc_id", google_video_product_type: "video_product_type"},
                 vb = {google_ad_client: "client", google_ad_format: "format", google_ad_output: "output", google_ad_callback: "callback", google_ad_height: "h", google_ad_override: "google_ad_override", google_ad_slot: "slotname", google_ad_width: "w", google_ctr_threshold: "ctr_t", google_image_size: "image_size", google_last_modified_time: "lmt", google_max_num_ads: "num_ads", google_max_radlink_len: "max_radlink_len", google_num_radlinks: "num_radlinks", google_num_radlinks_per_unit: "num_radlinks_per_unit", google_only_ads_with_video: "only_ads_with_video", google_rl_dest_url: "rl_dest_url", google_rl_filtering: "rl_filtering", google_rl_mode: "rl_mode", google_rt: "rt", google_skip: "skip"},
                 wb = {google_only_pyv_ads: "pyv", google_with_pyv_ads: "withpyv"};
             xb = __typedjs(function  (a,b)
                            {
                              try
                              {
                                return a.top.document.URL == b.URL;
                              }
                              catch (c) {
                                        };
                              return j;
                            },
                            arguments.callee,
                            "xb",
                            61);
             yb = __typedjs(function  (a,b,c,d)
                            {
                              c = c || a.google_ad_width;
                              d = d || a.google_ad_height;
                              if (xb(a,b))
                              return j;
                              var e = b.documentElement;
                              if (c && d)
                              {
                                var f = 1,i = 1;
                                if (a.innerHeight)
                                {
                                  f = a.innerWidth;
                                  i = a.innerHeight;
                                }
                                else if (e && e.clientHeight)
                                     {
                                       f = e.clientWidth;
                                       i = e.clientHeight;
                                     }
                                     else if (b.body)
                                          {
                                            f = b.body.clientWidth;
                                            i = b.body.clientHeight;
                                          };
                                if (i > 2 * d || f > 2 * c)
                                return j;
                              };
                              return g;
                            },
                            arguments.callee,
                            "yb",
                            62);
             zb = __typedjs(function  (a,b)
                            {
                              L(b,
                                __typedjs(function  (c,d)
                                          {
                                            a["google_" + d] = c;
                                          },
                                          arguments.callee,
                                          "",
                                          0));
                            },
                            arguments.callee,
                            "zb",
                            63);
             Ab = __typedjs(function  (a,b)
                            {
                              if (! b)
                              return a.URL;
                              return a.referrer;
                            },
                            arguments.callee,
                            "Ab",
                            64);
             Bb = __typedjs(function  (a,b)
                            {
                              if (! b && a.google_referrer_url == h)
                              return "0"
                              else if (b && a.google_referrer_url == h)
                                   return "1"
                                   else if (! b && a.google_referrer_url != h)
                                        return "2"
                                        else if (b && a.google_referrer_url != h)
                                             return "3";
                              return "4";
                            },
                            arguments.callee,
                            "Bb",
                            65);
             Cb = __typedjs(function  (a,b,c,d)
                            {
                              a.page_url = Ab(c,d);
                              a.page_location = h;
                            },
                            arguments.callee,
                            "Cb",
                            66);
             Db = __typedjs(function  (a,b,c,d)
                            {
                              a.page_url = b.google_page_url;
                              a.page_location = Ab(c,d) || "EMPTY";
                            },
                            arguments.callee,
                            "Db",
                            67);
             Eb = __typedjs(function  (a,b)
                            {
                              var c = {},d = yb(a,b,a.google_ad_width,a.google_ad_height);
                              c.iframing = Bb(a,d);
                              a.google_page_url ? Db(c,a,b,d) : Cb(c,a,b,d);
                              c.last_modified_time = b.URL == c.page_url ? Date.parse(b.lastModified) / 1000.0 : h;
                              c.referrer_url = d ? a.google_referrer_url : a.google_page_url && a.google_referrer_url ? a.google_referrer_url : b.referrer;
                              return c;
                            },
                            arguments.callee,
                            "Eb",
                            68);
             Fb = __typedjs(function  (a)
                            {
                              var b = {},c = a.URL.substring(a.URL.lastIndexOf("http"));
                              b.iframing = h;
                              b.page_url = c;
                              b.page_location = a.URL;
                              b.last_modified_time = h;
                              b.referrer_url = c;
                              return b;
                            },
                            arguments.callee,
                            "Fb",
                            69);
             Gb = __typedjs(function  (a,b)
                            {
                              b = Hb(a,b);
                              zb(a,b);
                            },
                            arguments.callee,
                            "Gb",
                            70);
             Hb = __typedjs(function  (a,b)
                            {
                              return a = a.google_page_url == h && Ib[b.domain] ? Fb(b) : Eb(a,
                                                                                             b);
                            },
                            arguments.callee,
                            "Hb",
                            71);
             var Ib = {};
             Ib["ad.yieldmanager.com"] = g;
             var Jb = __typedjs(function  (a,b,c)
                                {
                                  b = fa(b,l,a);
                                  a = window.onerror;
                                  window.onerror = b;
                                  try
                                  {
                                    c();
                                  }
                                  catch (d) {
                                              c = d.toString();
                                              var e = "";
                                              if (d.fileName)
                                              e = d.fileName;
                                              var f = - 1;
                                              if (d.lineNumber)
                                              f = d.lineNumber;
                                              b = b(c,e,f);
                                              if (! b)
                                              throw d;
                                            };
                                  window.onerror = a;
                                },
                                arguments.callee,
                                "Jb",
                                72);
             q("google_protectAndRun",Jb);
             var Lb = __typedjs(function  (a,b,c,d)
                                {
                                  if (Math.random() < 0.1)
                                  {
                                    var e = Wa;
                                    a = ["http://",
                                         ib,
                                         "/pagead/gen_204",
                                         "?id=jserror",
                                         "&jscb=",
                                         Xa ? 1 : 0,
                                         "&jscd=",
                                         Ya ? 1 : 0,
                                         "&context=",
                                         M(a),
                                         "&msg=",
                                         M(b),
                                         "&file=",
                                         M(c),
                                         "&line=",
                                         M(d.toString()),
                                         "&url=",
                                         M(e.URL.substring(0,512)),
                                         "&ref=",
                                         M(e.referrer.substring(0,512))];
                                    a.push(Kb());
                                    lb(K,a.join(""));
                                  };
                                  return ! Za;
                                },
                                arguments.callee,
                                "Lb",
                                73);
             q("google_handleError",Lb);
             var Nb = __typedjs(function  (a)
                                {
                                  Mb |= a;
                                },
                                arguments.callee,
                                "Nb",
                                74),
                 Mb = 0,
                 Kb = __typedjs(function  ()
                                {
                                  var a = ["&client=",
                                           M(K.google_ad_client),
                                           "&format=",
                                           M(K.google_ad_format),
                                           "&slotname=",
                                           M(K.google_ad_slot),
                                           "&output=",
                                           M(K.google_ad_output),
                                           "&ad_type=",
                                           M(K.google_ad_type)];
                                  return a.join("");
                                },
                                arguments.callee,
                                "Kb",
                                75);
             var Q = "",
                 Qb = __typedjs(function  ()
                                {
                                  if (window.google_ad_frameborder == h)
                                  window.google_ad_frameborder = 0;
                                  if (window.google_ad_output == h)
                                  window.google_ad_output = "html";
                                  if (O(window.google_ad_format))
                                  {
                                    var a = window.google_ad_format.match(/^(\d+)x(\d+)_.*/);
                                    if (a)
                                    {
                                      window.google_ad_width = parseInt(a[1],10);
                                      window.google_ad_height = parseInt(a[2],10);
                                      window.google_ad_output = "html";
                                    };
                                  };
                                  window.google_ad_format = Ob(window.google_ad_format,
                                                               window.google_ad_output,
                                                               window.google_ad_width,
                                                               window.google_ad_height,
                                                               window.google_ad_slot,
                                                               ! ! window.google_override_format);
                                  Q = window.google_ad_client || "";
                                  window.google_ad_client = Pb(window.google_ad_format,
                                                               window.google_ad_client);
                                  Gb(window,document);
                                  if (window.google_num_slots_by_channel == h)
                                  window.google_num_slots_by_channel = {};
                                  if (window.google_viewed_host_channels == h)
                                  window.google_viewed_host_channels = {};
                                  if (window.google_num_slots_by_client == h)
                                  window.google_num_slots_by_client = {};
                                  if (window.google_prev_ad_formats_by_region == h)
                                  window.google_prev_ad_formats_by_region = {};
                                  if (window.google_prev_ad_slotnames_by_region == h)
                                  window.google_prev_ad_slotnames_by_region = {};
                                  if (window.google_correlator == h)
                                  window.google_correlator = (__new(Date,[])).getTime();
                                  if (window.google_adslot_loaded == h)
                                  window.google_adslot_loaded = {};
                                  if (window.google_adContentsBySlot == h)
                                  window.google_adContentsBySlot = {};
                                  if (window.google_flash_version == h)
                                  window.google_flash_version = pb();
                                  if (window.google_new_domain_checked == h)
                                  window.google_new_domain_checked = 0;
                                  if (window.google_new_domain_enabled == h)
                                  window.google_new_domain_enabled = 0;
                                  if (! window.google_num_ad_slots)
                                  window.google_num_ad_slots = 0;
                                  if (! window.google_num_0ad_slots)
                                  window.google_num_0ad_slots = 0;
                                  if (! window.google_num_sdo_slots)
                                  window.google_num_sdo_slots = 0;
                                  window.google_ad_section = window.google_ad_section || window.google_ad_region || "";
                                  window.google_country = window.google_country || window.google_gl || "";
                                  a = (__new(Date,[])).getTime();
                                  if (o(window.google_color_bg))
                                  window.google_color_bg = R(window.google_color_bg,a);
                                  if (o(window.google_color_text))
                                  window.google_color_text = R(window.google_color_text,a);
                                  if (o(window.google_color_link))
                                  window.google_color_link = R(window.google_color_link,a);
                                  if (o(window.google_color_url))
                                  window.google_color_url = R(window.google_color_url,a);
                                  if (o(window.google_color_border))
                                  window.google_color_border = R(window.google_color_border,a);
                                  if (o(window.google_color_line))
                                  window.google_color_line = R(window.google_color_line,a);
                                },
                                arguments.callee,
                                "Qb",
                                76),
                 Rb = __typedjs(function  (a)
                                {
                                  L(ub,
                                    __typedjs(function  (b,c)
                                              {
                                                a[c] = h;
                                              },
                                              arguments.callee,
                                              "",
                                              0));
                                  L(vb,
                                    __typedjs(function  (b,c)
                                              {
                                                a[c] = h;
                                              },
                                              arguments.callee,
                                              "",
                                              1));
                                  L(wb,
                                    __typedjs(function  (b,c)
                                              {
                                                a[c] = h;
                                              },
                                              arguments.callee,
                                              "",
                                              2));
                                  a.google_container_id = h;
                                  a.google_eids = h;
                                  a.google_page_location = h;
                                  a.google_referrer_url = h;
                                  a.google_ad_region = h;
                                  a.google_gl = h;
                                },
                                arguments.callee,
                                "Rb",
                                77),
                 R = __typedjs(function  (a,b)
                               {
                                 Nb(2);
                                 return a[b % a.length];
                               },
                               arguments.callee,
                               "R",
                               78),
                 Pb = __typedjs(function  (a,b)
                                {
                                  if (! b)
                                  return "";
                                  b = b.toLowerCase();
                                  return b = O(a) ? Sb(b) : Tb(b);
                                },
                                arguments.callee,
                                "Pb",
                                79),
                 Tb = __typedjs(function  (a)
                                {
                                  if (a && a.substring(0,3) != "ca-")
                                  a = "ca-" + a;
                                  return a;
                                },
                                arguments.callee,
                                "Tb",
                                80),
                 Sb = __typedjs(function  (a)
                                {
                                  if (a && a.substring(0,7) != "ca-aff-")
                                  a = "ca-aff-" + a;
                                  return a;
                                },
                                arguments.callee,
                                "Sb",
                                81),
                 Ob = __typedjs(function  (a,b,c,d,e,f)
                                {
                                  if (! a && b == "html")
                                  a = c + "x" + d;
                                  return a = Ub(a,e,f) ? a.toLowerCase() : "";
                                },
                                arguments.callee,
                                "Ob",
                                82),
                 Ub = __typedjs(function  (a,b,c)
                                {
                                  if (! a)
                                  return j;
                                  if (! b)
                                  return g;
                                  return c;
                                },
                                arguments.callee,
                                "Ub",
                                83);
             var S = document,T = navigator,U = window;
             Vb = __typedjs(function  ()
                            {
                              var a = S.cookie,
                                  b = Math.round((new Date()).getTime() / 1000.0),
                                  c = U.google_analytics_domain_name;
                              c = typeof c == "undefined" ? Wb("auto") : Wb(c);
                              var d = a.indexOf("__utma=" + c + ".") > - 1,
                                  e = a.indexOf("__utmb=" + c) > - 1,
                                  f = a.indexOf("__utmc=" + c) > - 1,
                                  i = {},
                                  n = ! ! U && ! ! U.gaGlobal;
                              if (d)
                              {
                                a = a.split("__utma=" + c + ".")[1].split(";")[0].split(".");
                                i.sid = e && f ? a[3] + "" : n && U.gaGlobal.sid ? U.gaGlobal.sid : b + "";
                                i.vid = a[0] + "." + a[1];
                                i.from_cookie = g;
                              }
                              else {
                                     i.sid = n && U.gaGlobal.sid ? U.gaGlobal.sid : b + "";
                                     i.vid = n && U.gaGlobal.vid ? U.gaGlobal.vid : (Math.round(Math.random() * 2147483647) ^ Xb() & 2147483647) + "." + b;
                                     i.from_cookie = j;
                                   };
                              i.dh = c;
                              i.hid = n && U.gaGlobal.hid ? U.gaGlobal.hid : Math.round(Math.random() * 2147483647);
                              return U.gaGlobal = i;
                            },
                            arguments.callee,
                            "Vb",
                            84);
             Xb = __typedjs(function  ()
                            {
                              var a = S.cookie ? S.cookie : "",
                                  b = U.history.length,
                                  c,
                                  d = [T.appName,
                                       T.version,
                                       T.language ? T.language : T.browserLanguage,
                                       T.platform,
                                       T.userAgent,
                                       T.javaEnabled() ? 1 : 0].join("");
                              if (U.screen)
                              d += U.screen.width + "x" + U.screen.height + U.screen.colorDepth
                              else if (U.java)
                                   {
                                     c = java.awt.Toolkit.getDefaultToolkit().getScreenSize();
                                     d += c.screen.width + "x" + c.screen.height;
                                   };
                              d += a;
                              d += S.referrer ? S.referrer : "";
                              for (a = d.length; b > 0;)
                              d += b-- ^ a++;
                              return Yb(d);
                            },
                            arguments.callee,
                            "Xb",
                            85);
             Yb = __typedjs(function  (a)
                            {
                              var b = 1,c = 0,d;
                              if (! (a == undefined || a == ""))
                              {
                                b = 0;
                                for (d = a.length - 1; d >= 0; d--)
                                {
                                  c = a.charCodeAt(d);
                                  b = (b << 6 & 268435455) + c + (c << 14);
                                  c = b & 266338304;
                                  b = c != 0 ? b ^ c >> 21 : b;
                                };
                              };
                              return b;
                            },
                            arguments.callee,
                            "Yb",
                            86);
             Wb = __typedjs(function  (a)
                            {
                              if (! a || a == "" || a == "none")
                              return 1;
                              if ("auto" == a)
                              {
                                a = S.domain;
                                if ("www." == a.substring(0,4))
                                a = a.substring(4,a.length);
                              };
                              return Yb(a.toLowerCase());
                            },
                            arguments.callee,
                            "Wb",
                            87);
             ;;
             var V = __typedjs(function  ()
                               {
                                 this.defaultBucket = [];
                                 this.layers = {};
                                 for (var a = 0,b = arguments.length; a < b; ++a)
                                 this.layers[arguments[a]] = "";
                               },
                               arguments.callee,
                               "V",
                               88),
                 Zb = __typedjs(function  (a)
                                {
                                  for (var b = new V(),c = 0,d = a.defaultBucket.length; c < d; ++c)
                                  b.defaultBucket.push(a.defaultBucket[c]);
                                  L(a.layers,fa(V.prototype.f,b));
                                  return b;
                                },
                                arguments.callee,
                                "Zb",
                                89),
                 $b = __typedjs(function  (a)
                                {
                                  if (! (a && a.defaultBucket && a.defaultBucket.push && a.layers))
                                  return j;
                                  var b = g,
                                      c = __typedjs(function  (d,e)
                                                    {
                                                      if (! (e in a) || typeof d != typeof a[e])
                                                      b = j;
                                                    },
                                                    arguments.callee,
                                                    "c",
                                                    0);
                                  L(V.prototype,c);
                                  return b;
                                },
                                arguments.callee,
                                "$b",
                                90);
             V.prototype.f = __typedjs(function  (a,b)
                                       {
                                         this.layers[b] = a;
                                       },
                                       arguments.callee,
                                       "V.prototype.f",
                                       91);
             V.prototype.z = __typedjs(function  (a,b)
                                       {
                                         if (a == "")
                                         return "";
                                         if (! b)
                                         {
                                           this.defaultBucket.push(a);
                                           return a;
                                         };
                                         if (this.layers.hasOwnProperty(b))
                                         return this.layers[b] = a;
                                         return "";
                                       },
                                       arguments.callee,
                                       "V.prototype.z",
                                       92);
             V.prototype.d = __typedjs(function  (a,b,c)
                                       {
                                         if (! c || this.p(c))
                                         {
                                           var d = Math.random();
                                           if (d < b)
                                           {
                                             b = Math.floor(a.length * d / b);
                                             if (a = a[b])
                                             return this.z(a,c);
                                           };
                                         };
                                         return "";
                                       },
                                       arguments.callee,
                                       "V.prototype.d",
                                       93);
             V.prototype.p = __typedjs(function  (a)
                                       {
                                         return this.layers.hasOwnProperty(a) && this.layers[a] == "";
                                       },
                                       arguments.callee,
                                       "V.prototype.p",
                                       94);
             V.prototype.a = __typedjs(function  (a)
                                       {
                                         if (this.layers.hasOwnProperty(a))
                                         return this.layers[a];
                                         return "";
                                       },
                                       arguments.callee,
                                       "V.prototype.a",
                                       95);
             V.prototype.o = __typedjs(function  ()
                                       {
                                         var a = [],
                                             b = __typedjs(function  (c)
                                                           {
                                                             c != "" && a.push(c);
                                                           },
                                                           arguments.callee,
                                                           "b",
                                                           0);
                                         L(this.layers,b);
                                         if (this.defaultBucket.length > 0 && a.length > 0)
                                         return this.defaultBucket.join(",") + "," + a.join(",");
                                         return this.defaultBucket.join(",") + a.join(",");
                                       },
                                       arguments.callee,
                                       "V.prototype.o",
                                       96);
             var ac = {google: 1, googlegroups: 1, gmail: 1, googlemail: 1, googleimages: 1, googleprint: 1};
             bc = __typedjs(function  (a)
                            {
                              a = a.google_page_location || a.google_page_url;
                              if (! a)
                              return j;
                              a = a.toString();
                              if (a.indexOf("http://") == 0)
                              a = a.substring(7,a.length)
                              else if (a.indexOf("https://") == 0)
                                   a = a.substring(8,a.length);
                              var b = a.indexOf("/");
                              if (b == - 1)
                              b = a.length;
                              a = a.substring(0,b);
                              a = a.split(".");
                              b = j;
                              if (a.length >= 3)
                              b = a[a.length - 3] in ac;
                              if (a.length >= 2)
                              b = b || a[a.length - 2] in ac;
                              return b;
                            },
                            arguments.callee,
                            "bc",
                            97);
             cc = __typedjs(function  (a,b,c)
                            {
                              if (bc(a))
                              {
                                a.google_new_domain_checked = 1;
                                return j;
                              };
                              if (a.google_new_domain_checked == 0)
                              {
                                var d = Math.random();
                                if (d <= c)
                                {
                                  c = "http://" + gb + "/pagead/test_domain.js";
                                  d = "script";
                                  b.write("<" + d + " src=\"" + c + "\"></" + d + ">");
                                  a.google_new_domain_checked = 1;
                                  return g;
                                };
                              };
                              return j;
                            },
                            arguments.callee,
                            "cc",
                            98);
             var dc = __typedjs(function  (a,b)
                                {
                                  if (! b)
                                  return j;
                                  if (b.a("1") == "44901211")
                                  return 0 == a % 2;
                                  if (b.a("1") == "44901215")
                                  return 0 == Math.floor(a / 2) % 2;
                                  if (b.a("1") == "44901216")
                                  return 1 == Math.floor(a / 2) % 2;
                                  return j;
                                },
                                arguments.callee,
                                "dc",
                                99);
             ec = __typedjs(function  (a,b,c)
                            {
                              if (! bc(a) && a.google_new_domain_enabled == 1)
                              return dc(b,c) ? "http://" + hb : "http://" + gb;
                              return "http://" + ib;
                            },
                            arguments.callee,
                            "ec",
                            100);
             ;;
             var W = __typedjs(function  (a)
                               {
                                 this.A = a;
                                 this.i = [];
                                 this.h = 0;
                                 this.b = [];
                                 this.t = 0;
                                 this.c = [];
                                 this.r = j;
                                 this.j = this.k = "";
                                 this.q = j;
                               },
                               arguments.callee,
                               "W",
                               101);
             W.prototype.v = __typedjs(function  (a,b)
                                       {
                                         var c = this.A[b],d = this.i;
                                         this.A[b] = __typedjs(function  (e)
                                                               {
                                                                 if (e && e.length > 0)
                                                                 {
                                                                   var f = e.length > 1 ? e[1].url : h;
                                                                   d.push([a,ta(e[0].url),f]);
                                                                 };
                                                                 c(e);
                                                               },
                                                               arguments.callee,
                                                               "",
                                                               0);
                                       },
                                       arguments.callee,
                                       "W.prototype.v",
                                       102);
             W.prototype.u = __typedjs(function  ()
                                       {
                                         this.h++;
                                       },
                                       arguments.callee,
                                       "W.prototype.u",
                                       103);
             W.prototype.w = __typedjs(function  (a)
                                       {
                                         this.b.push(a);
                                       },
                                       arguments.callee,
                                       "W.prototype.w",
                                       104);
             W.prototype.s = __typedjs(function  ()
                                       {
                                         if (! this.r)
                                         {
                                           kb("http://" + fb + "/pagead/osd.js");
                                           this.r = g;
                                         };
                                       },
                                       arguments.callee,
                                       "W.prototype.s",
                                       105);
             W.prototype.l = __typedjs(function  (a)
                                       {
                                         if (this.h > 0)
                                         for (var b = document.getElementsByTagName("iframe"),
                                                  c = this.q ? "google_ads_iframe_" : "google_ads_frame",
                                                  d = 0; d < b.length; d++)
                                         {
                                           var e = b.item(d);
                                           e.src && e.name && e.name.indexOf(c) == 0 && a(e,e.src);
                                         };
                                       },
                                       arguments.callee,
                                       "W.prototype.l",
                                       106);
             W.prototype.m = __typedjs(function  (a)
                                       {
                                         var b = this.i;
                                         if (b.length > 0)
                                         for (var c = document.getElementsByTagName("a"),
                                                  d = 0; d < c.length; d++)
                                         for (var e = 0; e < b.length; e++)
                                         if (c.item(d).href == b[e][1])
                                         {
                                           var f = c.item(d).parentNode;
                                           if (b[e][2])
                                           for (var i = f,n = 0; n < 4; n++)
                                           {
                                             if (i.innerHTML.indexOf(b[e][2]) > 0)
                                             {
                                               f = i;
                                               break;
                                             };
                                             i = i.parentNode;
                                           };
                                           a(f,b[e][0]);
                                           b.splice(e,1);
                                           break;
                                         };
                                       },
                                       arguments.callee,
                                       "W.prototype.m",
                                       107);
             W.prototype.n = __typedjs(function  (a)
                                       {
                                         for (var b = 0; b < this.b.length; b++)
                                         {
                                           var c = this.b[b],d = fc(c);
                                           if (d)
                                           (d = document.getElementById("google_ads_div_" + d)) && a(d,
                                                                                                     c);
                                         };
                                       },
                                       arguments.callee,
                                       "W.prototype.n",
                                       108);
             W.prototype.e = __typedjs(function  (a)
                                       {
                                         this.m(a);
                                         this.n(a);
                                         this.l(a);
                                       },
                                       arguments.callee,
                                       "W.prototype.e",
                                       109);
             W.prototype.setupOsd = __typedjs(function  (a,b,c)
                                              {
                                                this.t = a;
                                                this.k = b;
                                                this.j = c;
                                              },
                                              arguments.callee,
                                              "W.prototype.setupOsd",
                                              110);
             W.prototype.getOsdMode = __typedjs(function  ()
                                                {
                                                  return this.t;
                                                },
                                                arguments.callee,
                                                "W.prototype.getOsdMode",
                                                111);
             W.prototype.getEid = __typedjs(function  ()
                                            {
                                              return this.k;
                                            },
                                            arguments.callee,
                                            "W.prototype.getEid",
                                            112);
             W.prototype.getCorrelator = __typedjs(function  ()
                                                   {
                                                     return this.j;
                                                   },
                                                   arguments.callee,
                                                   "W.prototype.getCorrelator",
                                                   113);
             W.prototype.g = __typedjs(function  ()
                                       {
                                         return this.i.length + this.h + this.b.length;
                                       },
                                       arguments.callee,
                                       "W.prototype.g",
                                       114);
             W.prototype.setValidOutputTypes = __typedjs(function  (a)
                                                         {
                                                           this.c = a;
                                                         },
                                                         arguments.callee,
                                                         "W.prototype.setValidOutputTypes",
                                                         115);
             W.prototype.registerAdBlockByType = __typedjs(function  (a,b,c)
                                                           {
                                                             if (this.c.length > 0)
                                                             {
                                                               for (var d = 0; d < this.c.length; d++)
                                                               if (this.c[d] == a)
                                                               {
                                                                 this.q = c;
                                                                 if (a == "js")
                                                                 this.v(b,"google_ad_request_done")
                                                                 else if (a == "html")
                                                                      this.u()
                                                                      else a == "json_html" && this.w(b);
                                                               };
                                                               this.s();
                                                             };
                                                           },
                                                           arguments.callee,
                                                           "W.prototype.registerAdBlockByType",
                                                           116);
             var fc = __typedjs(function  (a)
                                {
                                  if ((a = a.match(/[&\?](?:slotname)=([^&]+)/)) && a.length == 2)
                                  return a[1];
                                  return "";
                                },
                                arguments.callee,
                                "fc",
                                117),
                 gc = __typedjs(function  ()
                                {
                                  window.__google_ad_urls || (window.__google_ad_urls = new W(window));
                                  return window.__google_ad_urls;
                                },
                                arguments.callee,
                                "gc",
                                118);
             q("Goog_AdSense_getAdAdapterInstance",gc);
             q("Goog_AdSense_OsdAdapter",W);
             q("Goog_AdSense_OsdAdapter.prototype.numBlocks",W.prototype.g);
             q("Goog_AdSense_OsdAdapter.prototype.findBlocks",W.prototype.e);
             q("Goog_AdSense_OsdAdapter.prototype.getOsdMode",
               W.prototype.getOsdMode);
             q("Goog_AdSense_OsdAdapter.prototype.getEid",W.prototype.getEid);
             q("Goog_AdSense_OsdAdapter.prototype.getCorrelator",
               W.prototype.getCorrelator);
             q("Goog_AdSense_OsdAdapter.prototype.setValidOutputTypes",
               W.prototype.setValidOutputTypes);
             q("Goog_AdSense_OsdAdapter.prototype.setupOsd",
               W.prototype.setupOsd);
             q("Goog_AdSense_OsdAdapter.prototype.registerAdBlockByType",
               W.prototype.registerAdBlockByType);
             var hc = __typedjs(function  (a,b)
                                {
                                  var c = a.nodeType == 9 ? a : a.ownerDocument || a.document;
                                  if (c.defaultView && c.defaultView.getComputedStyle)
                                  if (a = c.defaultView.getComputedStyle(a,""))
                                  return a[b];
                                  return h;
                                },
                                arguments.callee,
                                "hc",
                                119),
                 ic = __typedjs(function  (a,b)
                                {
                                  return hc(a,
                                            b) || (a.currentStyle ? a.currentStyle[b] : h) || a.style[b];
                                },
                                arguments.callee,
                                "ic",
                                120),
                 jc = __typedjs(function  (a,b,c,d)
                                {
                                  if (/^\d+px?$/.test(b))
                                  return parseInt(b,10)
                                  else {
                                         var e = a.style[c],f = a.runtimeStyle[c];
                                         a.runtimeStyle[c] = a.currentStyle[c];
                                         a.style[c] = b;
                                         b = a.style[d];
                                         a.style[c] = e;
                                         a.runtimeStyle[c] = f;
                                         return b;
                                       };
                                },
                                arguments.callee,
                                "jc",
                                121),
                 kc = __typedjs(function  (a)
                                {
                                  var b = a.nodeType == 9 ? a : a.ownerDocument || a.document,
                                      c = "";
                                  if (b.createTextRange)
                                  {
                                    c = b.body.createTextRange();
                                    c.moveToElementText(a);
                                    c = c.queryCommandValue("FontName");
                                  };
                                  if (! c)
                                  {
                                    c = ic(a,"fontFamily");
                                    if (B && Ia)
                                    c = c.replace(/ \[[^\]]*\]/,"");
                                  };
                                  a = c.split(",");
                                  if (a.length > 1)
                                  c = a[0];
                                  return ua(c,"\"\'");
                                },
                                arguments.callee,
                                "kc",
                                122),
                 lc = /[^\d]+$/,
                 mc = __typedjs(function  (a)
                                {
                                  return (a = a.match(lc)) && a[0] || h;
                                },
                                arguments.callee,
                                "mc",
                                123),
                 nc = {cm: 1, "in": 1, mm: 1, pc: 1, pt: 1},
                 oc = {em: 1, ex: 1},
                 pc = __typedjs(function  (a)
                                {
                                  var b = ic(a,"fontSize"),c = mc(b);
                                  if (b && "px" == c)
                                  return parseInt(b,10);
                                  if (C)
                                  if (c in nc)
                                  return jc(a,b,"left","pixelLeft")
                                  else if (a.parentNode && a.parentNode.nodeType == 1 && c in oc)
                                       {
                                         a = a.parentNode;
                                         c = ic(a,"fontSize");
                                         return jc(a,b == c ? "1em" : b,"left","pixelLeft");
                                       };
                                  c = Sa("span",
                                         {style: "visibility:hidden;position:absolute;line-height:0;padding:0;margin:0;border:0;height:1em;"});
                                  Ua(a,c);
                                  b = c.offsetHeight;
                                  c && c.parentNode && c.parentNode.removeChild(c);
                                  return b;
                                },
                                arguments.callee,
                                "pc",
                                124);
             var qc,X = {};
             rc = __typedjs(function  (a)
                            {
                              if (a == 1)
                              return g;
                              return ! X[a];
                            },
                            arguments.callee,
                            "rc",
                            125);
             sc = __typedjs(function  (a,b)
                            {
                              if (! (! a || a == ""))
                              if (b == 1)
                              if (X[b])
                              X[b] += "," + a
                              else X[b] = a
                              else X[b] = a;
                            },
                            arguments.callee,
                            "sc",
                            126);
             tc = __typedjs(function  ()
                            {
                              var a = [];
                              L(X,
                                __typedjs(function  (b)
                                          {
                                            a.push(b);
                                          },
                                          arguments.callee,
                                          "",
                                          0));
                              return a.join(",");
                            },
                            arguments.callee,
                            "tc",
                            127);
             uc = __typedjs(function  (a,b)
                            {
                              if (o(a))
                              for (var c = 0; c < a.length; c++)
                              p(a[c]) && sc(a[c],b);
                            },
                            arguments.callee,
                            "uc",
                            128);
             var Y = j;
             vc = __typedjs(function  (a,b)
                            {
                              var c = "script";
                              Y = wc(a,b);
                              if (! Y)
                              a.google_allow_expandable_ads = j;
                              var d = ! xc();
                              Y && d && b.write("<" + c + " src=\"http://" + fb + "/pagead/expansion_embed.js\"></" + c + ">");
                              a = cc(a,b,I("1",0.1));
                              (d = d || a) && mb("msie") && ! window.opera ? b.write("<" + c + " src=\"http://" + fb + "/pagead/render_ads.js\"></" + c + ">") : b.write("<" + c + ">google_protectAndRun(\"ads_core.google_render_ad\", google_handleError, google_render_ad);</" + c + ">");
                            },
                            arguments.callee,
                            "vc",
                            129);
             Z = __typedjs(function  (a)
                           {
                             return a != h ? "\"" + a + "\"" : "\"\"";
                           },
                           arguments.callee,
                           "Z",
                           130);
             yc = __typedjs(function  (a)
                            {
                              var b = "google_unique_id";
                              if (a[b])
                              ++a[b]
                              else a[b] = 1;
                              return a[b];
                            },
                            arguments.callee,
                            "yc",
                            131);
             var zc = __typedjs(function  (a,b)
                                {
                                  var c = b.slice(- 1),d = c == "?" || c == "#" ? "" : "&",e = [b];
                                  b = __typedjs(function  (f,i)
                                                {
                                                  if (f || f === 0 || f === j)
                                                  {
                                                    if (typeof f == "boolean")
                                                    f = f ? 1 : 0;
                                                    jb(e,d,i,"=",M(f));
                                                    d = "&";
                                                  };
                                                },
                                                arguments.callee,
                                                "b",
                                                0);
                                  L(a,b);
                                  return e.join("");
                                },
                                arguments.callee,
                                "zc",
                                132);
             Ac = __typedjs(function  ()
                            {
                              var a = C && F("6"),b = Ca && F("1.8.1"),c = D && F("525");
                              if (Ha && (a || b || c))
                              return g
                              else if (Ga && (c || b))
                                   return g
                                   else if (Ia && b)
                                        return g;
                              return j;
                            },
                            arguments.callee,
                            "Ac",
                            133);
             xc = __typedjs(function  ()
                            {
                              return (typeof ExpandableAdSlotFactory == "function" || typeof ExpandableAdSlotFactory == "object") && typeof ExpandableAdSlotFactory.createIframe == "function";
                            },
                            arguments.callee,
                            "xc",
                            134);
             wc = __typedjs(function  (a,b)
                            {
                              if (a.google_allow_expandable_ads === j || ! b.body || a.google_ad_output != "html" || yb(a,
                                                                                                                        b) || ! Bc(a) || isNaN(a.google_ad_height) || isNaN(a.google_ad_width) || ! Ac())
                              return j;
                              return g;
                            },
                            arguments.callee,
                            "wc",
                            135);
             Bc = __typedjs(function  (a)
                            {
                              var b = a.google_ad_format;
                              if (O(b))
                              return j;
                              if (N(a) && b != "468x15_0ads_al")
                              return j;
                              return g;
                            },
                            arguments.callee,
                            "Bc",
                            136);
             Cc = __typedjs(function  ()
                            {
                              var a;
                              if (K.google_ad_output == "html" && ! (N(K) || O(K.google_ad_format)) && rc(0))
                              {
                                a = ["6083035","6083034"];
                                a = P(a,I("0",0));
                                sc(a,0);
                              };
                              return a == "6083035";
                            },
                            arguments.callee,
                            "Cc",
                            137);
             Dc = __typedjs(function  (a,b)
                            {
                              if ((a.google_unique_id || 0) != 0 || O(a.google_ad_format))
                              return "";
                              var c = "";
                              a = N(a);
                              if (b == "html" || a)
                              c = P(["36815001","36815002"],I("0.01",0));
                              if (c == "" && (b == "js" || a))
                              c = P(["36815003","36815004"],I("0.01",0));
                              if (c == "" && (b == "html" || b == "js"))
                              c = P(["36813005","36813006"],I("0.008",0));
                              return c;
                            },
                            arguments.callee,
                            "Dc",
                            138);
             Ec = __typedjs(function  ()
                            {
                              var a = gc(),b = window.google_enable_osd,c;
                              if (b === g)
                              {
                                c = "36813006";
                                Fc(c,a);
                              }
                              else if (b !== j && rc(0))
                                   {
                                     c = a.getEid();
                                     if (c == "")
                                     (c = Dc(window,window.google_ad_output)) && Fc(c,a)
                                     else if (c != "36815001" && c != "36815002" && c != "36815003" && c != "36815004" && c != "36813005" && c != "36813006")
                                          c = "";
                                   };
                              if (c)
                              {
                                sc(c,0);
                                return c;
                              };
                              return "";
                            },
                            arguments.callee,
                            "Ec",
                            139);
             Fc = __typedjs(function  (a,b)
                            {
                              var c = b.getOsdMode(),d = [];
                              switch (a)
                              {case
                               "36815004" :
                                 c = 1;
                                 d = ["js"];
                                 break;
                               case
                               "36815002" :
                                 c = 1;
                                 d = ["html"];
                                 break;
                               case
                               "36813006" :
                                 c = 0;
                                 d = ["html","js"];
                                 break;};
                              d.length > 0 && b.setValidOutputTypes(d);
                              b.setupOsd(c,a,window.google_correlator);
                            },
                            arguments.callee,
                            "Fc",
                            140);
             Gc = __typedjs(function  (a,b,c,d)
                            {
                              var e = yc(a);
                              c = zc({ifi: e},c);
                              c = c.substring(0,1992);
                              c = c.replace(/%\w?$/,"");
                              var f = "script";
                              if ((a.google_ad_output == "js" || a.google_ad_output == "json_html") && (a.google_ad_request_done || a.google_radlink_request_done))
                              b.write("<" + f + " language=\"JavaScript1.1\" src=" + Z(k(c)) + "></" + f + ">")
                              else if (a.google_ad_output == "html")
                                   if (Y && xc())
                                   {
                                     b = a.google_container_id || d || h;
                                     a["google_expandable_ad_slot" + e] = ExpandableAdSlotFactory.createIframe("google_ads_frame" + e,
                                                                                                               k(c),
                                                                                                               a.google_ad_width,
                                                                                                               a.google_ad_height,
                                                                                                               b);
                                   }
                                   else {
                                          e = "<iframe name=\"google_ads_frame\" width=" + Z(a.google_ad_width) + " height=" + Z(a.google_ad_height) + " frameborder=" + Z(a.google_ad_frameborder) + " src=" + Z(k(c)) + " marginwidth=\"0\" marginheight=\"0\" vspace=\"0\" hspace=\"0\" allowtransparency=\"true\" scrolling=\"no\"></iframe>";
                                          a.google_container_id ? Hc(a.google_container_id,
                                                                     b,
                                                                     e) : b.write(e);
                                        };
                              return c;
                            },
                            arguments.callee,
                            "Gc",
                            141);
             Ic = __typedjs(function  (a)
                            {
                              Rb(a);
                            },
                            arguments.callee,
                            "Ic",
                            142);
             Jc = __typedjs(function  (a)
                            {
                              var b = Kc().a("ac1") == "44901217";
                              if (! Lc(b))
                              return j;
                              b = Cc();
                              var c = ec(window,window.google_unique_id || 0,Kc());
                              a = Mc(a);
                              b = c + Nc(a.google_ad_format,b);
                              window.google_ad_url = zc(a,b);
                              return g;
                            },
                            arguments.callee,
                            "Jc",
                            143);
             var Qc = __typedjs(function  (a)
                                {
                                  a.dt = aa;
                                  var b = window.google_prev_ad_formats_by_region,
                                      c = window.google_ad_section,
                                      d = window.google_ad_format,
                                      e = window.google_ad_slot;
                                  if (b[c])
                                  if (! O(d))
                                  {
                                    a.prev_fmts = b[c];
                                    if (window.google_num_slots_by_client.length > 1)
                                    a.slot = window.google_num_slots_by_client[Q];
                                  };
                                  var f = window.google_prev_ad_slotnames_by_region;
                                  if (f[c])
                                  a.prev_slotnames = f[c].toLowerCase();
                                  if (d)
                                  {
                                    if (! O(d))
                                    if (b[c])
                                    b[c] += "," + d
                                    else b[c] = d;
                                  }
                                  else if (e)
                                       if (f[c])
                                       f[c] += "," + e
                                       else f[c] = e;
                                  a.correlator = window.google_correlator;
                                  if (window.google_new_domain_checked == 1 && window.google_new_domain_enabled == 0)
                                  a.dblk = 1;
                                  if (window.google_ad_channel)
                                  {
                                    b = window.google_num_slots_by_channel;
                                    c = "";
                                    d = window.google_ad_channel.split(Oc);
                                    for (e = 0; e < d.length; e++)
                                    {
                                      f = d[e];
                                      if (b[f])
                                      c += f + "+"
                                      else b[f] = 1;
                                    };
                                    a.pv_ch = c;
                                  };
                                  if (window.google_ad_host_channel)
                                  {
                                    b = Pc(window.google_ad_host_channel,
                                           window.google_viewed_host_channels);
                                    a.pv_h_ch = b;
                                  };
                                  if (Xa)
                                  a.jscb = 1;
                                  if (Ya)
                                  a.jscd = 1;
                                  a.frm = window.google_iframing;
                                  b = Vb();
                                  a.ga_vid = b.vid;
                                  a.ga_sid = b.sid;
                                  a.ga_hid = b.hid;
                                  a.ga_fc = b.from_cookie;
                                  a.ga_wpids = window.google_analytics_uacct;
                                },
                                arguments.callee,
                                "Qc",
                                144),
                 Rc = __typedjs(function  (a)
                                {
                                  var b = g;
                                  if (b = rb(b))
                                  {
                                    a.biw = b.width;
                                    a.bih = b.height;
                                  };
                                },
                                arguments.callee,
                                "Rc",
                                145),
                 Sc = __typedjs(function  (a)
                                {
                                  var b = tb(window);
                                  if (b != 0)
                                  a.ifk = b.toString();
                                },
                                arguments.callee,
                                "Sc",
                                146);
             Pc = __typedjs(function  (a,b)
                            {
                              var c = a.split("|");
                              a = - 1;
                              for (var d = [],e = 0; e < c.length; e++)
                              {
                                var f = c[e].split(Oc);
                                b[e] || (b[e] = {});
                                for (var i = "",n = 0; n < f.length; n++)
                                {
                                  var z = f[n];
                                  if (z != "")
                                  if (b[e][z])
                                  i += "+" + z
                                  else b[e][z] = 1;
                                };
                                i = i.slice(1);
                                d[e] = i;
                                if (i != "")
                                a = e;
                              };
                              b = "";
                              if (a > - 1)
                              {
                                for (e = 0; e < a; e++)
                                b += d[e] + "|";
                                b += d[a];
                              };
                              return b;
                            },
                            arguments.callee,
                            "Pc",
                            147);
             var $ = __typedjs(function  ()
                               {
                                 if (window.google_exp_persistent && $b(window.google_exp_persistent))
                                 return window.google_exp_persistent;
                                 return __new(V,["1","ac1"]);
                               },
                               arguments.callee,
                               "$",
                               148);
             q("google_exp_persistent",$());
             var Kc = __typedjs(function  ()
                                {
                                  qc || (qc = Zb($()));
                                  return qc;
                                },
                                arguments.callee,
                                "Kc",
                                149);
             Tc = __typedjs(function  ()
                            {
                              var a = ["44901211","44901215","44901216","44901212"];
                              $().d(a,I("0.0001",0),"1");
                              a = ["44901217","44901218"];
                              $().d(a,I("0",0),"ac1");
                            },
                            arguments.callee,
                            "Tc",
                            150);
             Uc = __typedjs(function  ()
                            {
                              0 == (window.google_unique_id || 0) && Tc();
                              var a = Ec(),b = Math.random() < 0.1,c = h,d = "";
                              if (b)
                              {
                                d = "google_temp_span";
                                c = Vc(d);
                              };
                              b = Jc(c);
                              c && c.id == d && (c && c.parentNode ? c.parentNode.removeChild(c) : h);
                              if (b)
                              {
                                c = Gc(window,document,window.google_ad_url);
                                a && gc().registerAdBlockByType(window.google_ad_output,c,j);
                                Ic(window);
                              };
                            },
                            arguments.callee,
                            "Uc",
                            151);
             var Wc = __typedjs(function  (a)
                                {
                                  L(vb,
                                    __typedjs(function  (b,c)
                                              {
                                                a[b] = window[c];
                                              },
                                              arguments.callee,
                                              "",
                                              0));
                                  L(ub,
                                    __typedjs(function  (b,c)
                                              {
                                                a[b] = window[c];
                                              },
                                              arguments.callee,
                                              "",
                                              1));
                                  L(wb,
                                    __typedjs(function  (b,c)
                                              {
                                                a[b] = window[c];
                                              },
                                              arguments.callee,
                                              "",
                                              2));
                                },
                                arguments.callee,
                                "Wc",
                                152),
                 Xc = __typedjs(function  (a)
                                {
                                  uc(window.google_eids,1);
                                  a.eid = tc();
                                  var b = Kc().o();
                                  if (a.eid.length > 0 && b.length > 0)
                                  a.eid += ",";
                                  a.eid += b;
                                },
                                arguments.callee,
                                "Xc",
                                153);
             Yc = __typedjs(function  (a,b,c,d)
                            {
                              a = Lb(a,b,c,d);
                              vc(window,document);
                              return a;
                            },
                            arguments.callee,
                            "Yc",
                            154);
             Zc = __typedjs(function  ()
                            {
                              Qb();
                            },
                            arguments.callee,
                            "Zc",
                            155);
             $c = __typedjs(function  (a)
                            {
                              var b = {};
                              a = a.split("?");
                              a = a[a.length - 1].split("&");
                              for (var c = 0; c < a.length; c++)
                              {
                                var d = a[c].split("=");
                                if (d[0])
                                try
                                {
                                  b[d[0].toLowerCase()] = d.length > 1 ? window.decodeURIComponent ? decodeURIComponent(d[1].replace(/\+/g,
                                                                                                                                     " ")) : unescape(d[1]) : "";
                                }
                                catch (e) {
                                          };
                              };
                              return b;
                            },
                            arguments.callee,
                            "$c",
                            156);
             ad = __typedjs(function  ()
                            {
                              var a = window,b = $c(document.URL);
                              if (b.google_ad_override)
                              {
                                a.google_ad_override = b.google_ad_override;
                                a.google_adtest = "on";
                              };
                            },
                            arguments.callee,
                            "ad",
                            157);
             Hc = __typedjs(function  (a,b,c)
                            {
                              if (a)
                              if ((a = b.getElementById(a)) && c && c.length != "")
                              {
                                a.style.visibility = "visible";
                                a.innerHTML = c;
                              };
                            },
                            arguments.callee,
                            "Hc",
                            158);
             var Nc = __typedjs(function  (a,b)
                                {
                                  return a = O(a) ? "/pagead/sdo?" : b ? "/pagead/render_iframe_ads.html#" : "/pagead/ads?";
                                },
                                arguments.callee,
                                "Nc",
                                159),
                 bd = __typedjs(function  (a,b)
                                {
                                  b.dff = kc(a);
                                  b.dfs = pc(a);
                                },
                                arguments.callee,
                                "bd",
                                160),
                 cd = __typedjs(function  (a)
                                {
                                  a.ref = window.google_referrer_url;
                                  a.loc = window.google_page_location;
                                },
                                arguments.callee,
                                "cd",
                                161),
                 Lc = __typedjs(function  (a)
                                {
                                  var b = window.google_prev_ad_formats_by_region,
                                      c = window.google_prev_ad_slotnames_by_region,
                                      d = window.google_ad_section;
                                  if (O(window.google_ad_format))
                                  {
                                    window.google_num_sdo_slots += 1;
                                    if (! a && window.google_num_sdo_slots > 4)
                                    return j;
                                  }
                                  else if (N(window))
                                       {
                                         window.google_num_0ad_slots += 1;
                                         if (! a && window.google_num_0ad_slots > 3)
                                         return j;
                                       }
                                       else {
                                              window.google_num_ad_slots += 1;
                                              if (window.google_num_slots_to_rotate)
                                              {
                                                Nb(1);
                                                b[d] = h;
                                                c[d] = h;
                                                if (window.google_num_slot_to_show == h)
                                                window.google_num_slot_to_show = (new Date()).getTime() % window.google_num_slots_to_rotate + 1;
                                                if (window.google_num_slot_to_show != window.google_num_ad_slots)
                                                return j;
                                              }
                                              else if (! a && window.google_num_ad_slots > 6 && d == "")
                                                   return j;
                                            };
                                  a = window.google_num_slots_by_client;
                                  if (a[Q])
                                  a[Q] += 1
                                  else {
                                         a[Q] = 1;
                                         a.length += 1;
                                       };
                                  return g;
                                },
                                arguments.callee,
                                "Lc",
                                162),
                 Mc = __typedjs(function  (a)
                                {
                                  var b = {};
                                  Wc(b);
                                  Qc(b);
                                  qb(b);
                                  a && bd(a,b);
                                  Rc(b);
                                  Sc(b);
                                  Xc(b);
                                  cd(b);
                                  b.fu = Mb;
                                  return b;
                                },
                                arguments.callee,
                                "Mc",
                                163),
                 Vc = __typedjs(function  (a)
                                {
                                  var b = window.google_container_id && Na(window.google_container_id) || Na(a);
                                  if (! b && ! window.google_container_id && a)
                                  {
                                    document.write("<span id=" + a + "></span>");
                                    b = Na(a);
                                  };
                                  return b;
                                },
                                arguments.callee,
                                "Vc",
                                164),
                 Oc = /[+, ]/;
             window.google_render_ad = Uc;
             dd = __typedjs(function  ()
                            {
                              if (Za && typeof K.alternateShowAds == "function")
                              K.alternateShowAds.call(h)
                              else {
                                     ad();
                                     Jb("show_ads.google_init_globals",Yc,Zc);
                                     vc(window,document);
                                   };
                            },
                            arguments.callee,
                            "dd",
                            165);
             Jb("show_ads.main",Lb,dd);
           },
           undefined,
           "",
           100))();
