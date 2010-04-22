__initnumargs("curConvert/curConvert.js",[0,0,1,2,0]);
var Currency = __typedjs(function  ()
                         {
                           __thisref(this,
                                     arguments.callee).rates = {"AED": 5.465, "ARS": 5.2657, "AUD": 1.5765, "BDT": 94.7681, "BGN": 1.9553, "BHD": 0.5181, "BND": 1.9441, "BRL": 2.5667, "BWP": 9.482, "CAD": 1.4633, "CHF": 1.4708, "CLP": 744.337, "CNY": 9.38, "COP": 2697.749, "CYP": 0.0, "CZK": 26.105, "DKK": 7.4461, "EEK": 15.6964, "EGP": 7.5189, "EUR": 1.0, "GBP": 0.8736, "HKD": 10.6766, "HRK": 7.3245000000000005, "HUF": 274.7623, "IDR": 12894.5127, "ILS": 5.1411999999999995, "INR": 64.1497, "IRR": 13602.967, "ISK": 176.9372, "JMD": 122.5975, "JPY": 123.852, "KRW": 1574.6831, "KWD": 0.3964, "LKR": 157.4546, "LTL": 3.4466, "LVL": 0.7112, "LYD": 1.7233, "MTL": 0.0, "MUR": 41.6307, "MVR": 17.5865, "MXN": 18.138, "MYR": 4.7498, "NOK": 8.1943, "NPR": 102.9088, "NZD": 1.984, "OMR": 0.5287999999999999, "PEN": 3.9638, "PHP": 64.1291, "PKR": 116.7857, "PLN": 4.635, "QAR": 5.8, "RON": 4.1394, "RUB": 41.7021, "SAR": 5.1527, "SEK": 10.2023, "SGD": 1.9502000000000002, "SIT": 239.64, "SKK": 29.6402, "THB": 45.6426, "TND": 1.8843999999999999, "TRY": 2.815, "TTD": 8.7658, "TWD": 44.1112, "UAH": 11.259, "USD": 1.3739, "VEB": 0.0, "XAG": 0.10300000000000001, "XAL": 0.0, "XAU": 0.13, "XCD": 3.5929, "XPT": 0.12, "ZAR": 10.5666, "FOO": 1};
                           __thisref(this,
                                     arguments.callee).konq = (navigator.userAgent.indexOf("Konqueror") >= 0);
                         },
                         0,
                         "Currency",
                         "curConvert/curConvert.js",
                         0);
Currency.prototype.convert = __typedjs(function  ()
                                       {
                                         var from = document.getElementById("from").value;
                                         var to = document.getElementById("to").value;
                                         var rate = 1.0;
                                         var re = document.getElementById("result");
                                         var val = document.getElementById("value").value;
                                         if (val.length === 0)
                                         {
                                           re.value = "";
                                         }
                                         else {
                                                re.value = __thisref(this,
                                                                     arguments.callee).format(val * rate);
                                              };
                                       },
                                       0,
                                       "Currency.prototype.convert",
                                       "curConvert/curConvert.js",
                                       1);
Currency.prototype.format = __typedjs(function  (num)
                                      {
                                        num = (num.toString()).replace(/\'/g,"");
                                        if (isNaN(num))
                                        {
                                          num = "0";
                                        };
                                        var sign = (num == (num = Math.abs(num)));
                                        num = Math.floor(num * 100 + 0.5000000000099999);
                                        var cents = num % 100;
                                        num = Math.floor(num / 100).toString();
                                        if (cents < 10)
                                        {
                                          cents = "0" + cents;
                                        };
                                        for (var i = 0; i < Math.floor((num.length - (1 + i)) / 3); i++)
                                        {
                                          num = num.substring(0,
                                                              num.length - (4 * i + 3)) + "," + num.substring(num.length - (4 * i + 3));
                                        };
                                        return ((sign ? "" : "-") + num + "." + cents);
                                      },
                                      0,
                                      "Currency.prototype.format",
                                      "curConvert/curConvert.js",
                                      2);
Currency.prototype.label = __typedjs(function  (el,sl)
                                     {
                                       var txt = document.createTextNode(sl.value);
                                       var e = document.getElementById(el);
                                       e.replaceChild(txt,e.firstChild);
                                     },
                                     0,
                                     "Currency.prototype.label",
                                     "curConvert/curConvert.js",
                                     3);
var cnvtr = __new(Currency,[]);
var swapFromTo = __typedjs(function  ()
                           {
                             var tmp = document.getElementById("from").value;
                             document.getElementById("from").value = document.getElementById("to").value;
                             document.getElementById("to").value = tmp;
                             cnvtr.label("from_s",document.getElementById("from"));
                             cnvtr.label("to_s",document.getElementById("to"));
                           },
                           0,
                           "swapFromTo",
                           "curConvert/curConvert.js",
                           4);
