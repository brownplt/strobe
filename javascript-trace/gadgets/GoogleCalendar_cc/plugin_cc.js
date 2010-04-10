var MONTHS_LONG = [MONTH_LONG_0,
                   MONTH_LONG_1,
                   MONTH_LONG_2,
                   MONTH_LONG_3,
                   MONTH_LONG_4,
                   MONTH_LONG_5,
                   MONTH_LONG_6,
                   MONTH_LONG_7,
                   MONTH_LONG_8,
                   MONTH_LONG_9,
                   MONTH_LONG_10,
                   MONTH_LONG_11];
var MONTHS_SHORT = [MONTH_SHORT_0,
                    MONTH_SHORT_1,
                    MONTH_SHORT_2,
                    MONTH_SHORT_3,
                    MONTH_SHORT_4,
                    MONTH_SHORT_5,
                    MONTH_SHORT_6,
                    MONTH_SHORT_7,
                    MONTH_SHORT_8,
                    MONTH_SHORT_9,
                    MONTH_SHORT_10,
                    MONTH_SHORT_11];
var DOW = [DOW_0,DOW_1,DOW_2,DOW_3,DOW_4,DOW_5,DOW_6];
var DOW_SHORT = DOW_SHORT_STR.split(",");
var DOW_TINY = DOW_TINY_STR.split(",");
var itemHeight = 16;
var europeanDate = options.GetValue("european") == "true" ? 1 : 0;
var format24 = options.GetValue("24") == "true";
var hideMonthView = options.GetValue("hidemonth") == "true";
var showWeekNum = options.GetValue("showweek") == "true";
var kCurrentVersion = "2.0.0.0";
var kClientLoginAppName = "GCAL-" + kCurrentVersion;
var kProductHomePageUri = "http://www.google.com/googlecalendar/gd_help_0_9_8.html";
var kCalendarUrl = "http://www.google.com/calendar";
var kAutomatedCalendarUrl = "http://desktop1.google.com/calendar";
var kAccountsUrl = "https://www.google.com/accounts";
var kGoogleCalendarUrl = kCalendarUrl + "/render";
var kCalendarUserFeedUrl = kAutomatedCalendarUrl + "/feeds/default";
var kCalendarEventUrl = kCalendarUrl + "/event";
var kCalendarCreateEventUrl = kCalendarUrl + "/feeds/default/private/full";
var kGoogleClientLoginUrl = kAccountsUrl + "/ClientLogin";
var kUpdateIntervalMs = 1000 * 60 * 10;
var image_rep = {};
image_rep[false] = framework.graphics.LoadImage("repeat_dark.gif");
image_rep[true] = framework.graphics.LoadImage("repeat_white.gif");
var image_alarm = {};
image_alarm[false] = framework.graphics.LoadImage("alarm_dark.gif");
image_alarm[true] = framework.graphics.LoadImage("alarm_white.gif");
var image_people = {};
image_people[false] = framework.graphics.LoadImage("people_dark.gif");
image_people[true] = framework.graphics.LoadImage("people_white.gif");
var authReq = null;
var authToken = "";
var lsid = "";
var sid = "";
var numAuthTriesSinceLastSuccess = 0;
var minimized = false;
var PresizeWindow = __typedjs(function  ()
                              {
                                if (PresizeWindow.count > 0)
                                {
                                  var lines = 15;
                                  AddStaticItem("",0,0,0,itemHeight * lines);
                                  PresizeWindow.count--;
                                };
                              },
                              undefined,
                              "PresizeWindow",
                              "gadgets/GoogleCalendar_cc/plugin.js",
                              0);
var stateChange = __typedjs(function  (newState)
                            {
                              minimized = gddTileDisplayStateMinimized == newState;
                              UpdateTimeMonthYear();
                              drawCalendar();
                            },
                            undefined,
                            "stateChange",
                            "gadgets/GoogleCalendar_cc/plugin.js",
                            1);
PresizeWindow.count = 6;
var Main = __typedjs(function  ()
                     {
                       PresizeWindow();
                       SetDefaultOptions();
                       pluginHelper.SetFlags(gddPluginFlagToolbarBack | gddPluginFlagToolbarForward,
                                             gddContentFlagHaveDetails | gddContentFlagManualLayout);
                       pluginHelper.about_text = MSG_ABOUT;
                       pluginHelper.max_content_items = 1000;
                       pluginHelper.onCommand = OnCommand;
                       pluginHelper.onDisplayStateChange = stateChange;
                       pluginHelper.onShowOptionsDlg = ShowOptionsDlg;
                       view.setInterval(timerRefresh,kUpdateIntervalMs);
                       view.setInterval(OnTimer,5000);
                       drawCalendar();
                       getUserInfo();
                     },
                     undefined,
                     "Main",
                     "gadgets/GoogleCalendar_cc/plugin.js",
                     2);
var curMonth = __new(Date,[]).getMonth();
var curYear = __new(Date,[]).getFullYear();
var curDate = __new(Date,[]).getDate();
var prevDate;
var OnTimer = __typedjs(function  ()
                        {
                          var cal = __new(Date,[]);
                          UpdateTimeMonthYear();
                          if (prevDate != cal.getDate())
                          {
                            prevDate = cal.getDate();
                            curDate = prevDate;
                            curMonth = cal.getMonth();
                            curYear = cal.getFullYear();
                            drawCalendar();
                          };
                          checkEventsForExpiry();
                        },
                        undefined,
                        "OnTimer",
                        "gadgets/GoogleCalendar_cc/plugin.js",
                        3);
var monthYearItem = null;
var UpdateTimeMonthYear = __typedjs(function  ()
                                    {
                                      var cal = __new(Date,[]);
                                      var hour = cal.getHours();
                                      var min = cal.getMinutes();
                                      var time;
                                      if (format24)
                                      {
                                        time = hour + ":" + ((min < 10) ? "0" + min : min);
                                      }
                                      else {
                                             hour %= 12;
                                             time = ((hour == 0) ? 12 : hour) + ":" + ((min < 10) ? "0" + min : min) + " " + ((cal.getHours() >= 12) ? MSG_PM : MSG_AM);
                                           };
                                      if (minimized)
                                      {
                                        time += " " + MONTHS_SHORT[curMonth] + " " + curDate + ", " + curYear;
                                      };
                                      view.caption = time;
                                      var monthYear = MONTHS_LONG[curMonth] + " " + curYear;
                                      if (monthYearItem && monthYearItem.heading != monthYear)
                                      monthYearItem.heading = monthYear;
                                    },
                                    undefined,
                                    "UpdateTimeMonthYear",
                                    "gadgets/GoogleCalendar_cc/plugin.js",
                                    4);
var SetDefaultOptions = __typedjs(function  ()
                                  {
                                    if (getEmail() == null)
                                    {
                                      options.PutValue("Email","");
                                    };
                                    if (getPW() == null)
                                    {
                                      options.PutValue("Passwd","");
                                    };
                                    if (options.GetValue("24") == "")
                                    {
                                      var d1 = __new(Date,[1970,12,29,13,0,0,0]);
                                      var localTime = d1.toLocaleTimeString();
                                      format24 = localTime.indexOf("13") >= 0 ? true : false;
                                      options.PutValue("24",format24 ? "true" : "false");
                                    };
                                    if (options.GetValue("european") == "")
                                    {
                                      options.PutValue("european","false");
                                    };
                                    if (options.GetValue("hidemonth") == "")
                                    {
                                      options.PutValue("hidemonth","false");
                                    };
                                    if (options.GetValue("showweek") == "")
                                    {
                                      options.PutValue("showweek","false");
                                    };
                                  },
                                  undefined,
                                  "SetDefaultOptions",
                                  "gadgets/GoogleCalendar_cc/plugin.js",
                                  5);
var AddCustomMenuItems = __typedjs(function  (menu)
                                   {
                                     menu.AddItem(MSG_MENU_REFESH_MENU,
                                                  0,
                                                  __typedjs(function  ()
                                                            {
                                                              getUserInfo();
                                                            },
                                                            arguments.callee,
                                                            "",
                                                            "gadgets/GoogleCalendar_cc/plugin.js",
                                                            0));
                                     menu.AddItem(MSG_MENU_GOTO_CALENDAR,0,goToGoogle);
                                   },
                                   undefined,
                                   "AddCustomMenuItems",
                                   "gadgets/GoogleCalendar_cc/plugin.js",
                                   6);
var goToGoogle = __typedjs(function  ()
                           {
                             showPage(kGoogleCalendarUrl);
                           },
                           undefined,
                           "goToGoogle",
                           "gadgets/GoogleCalendar_cc/plugin.js",
                           7);
var DoAuth = __typedjs(function  ()
                       {
                         var email = getEmail();
                         var pw = getPW();
                         if (email == "" && pw == "")
                         {
                           showErrorItem(MSG_GETTING_STARTED_1,
                                         enterUsernamePasswordText,
                                         gettingStartedSnippet,
                                         null);
                           return;
                         };
                         authToken = "";
                         if (++numAuthTriesSinceLastSuccess == 10)
                         {
                           showErrorItem(genericProblemText,
                                         errorUnknownText,
                                         errorUnknownSnippet,
                                         null);
                           numAuthTriesSinceLastSuccess = 0;
                           return;
                         };
                         authReq = __new(XMLHttpRequest,[]);
                         authReq.open("POST",kGoogleClientLoginUrl,true);
                         authReq.setRequestHeader("Content-Type",
                                                  "application/x-www-form-urlencoded; charset=UTF-8");
                         var data = "Email=" + encodeUrl(email) + "&Passwd=" + encodeUrl(pw) + "&source=" + encodeUrl(kClientLoginAppName) + "&service=cl&accountType=hosted_or_google";
                         authReq.onreadystatechange = onAuthResponse;
                         try
                         {
                           authReq.send(data);
                         }
                         catch (e) {
                                   };
                       },
                       undefined,
                       "DoAuth",
                       "gadgets/GoogleCalendar_cc/plugin.js",
                       8);
var onAuthResponse = __typedjs(function  ()
                               {
                                 if (! authReq || authReq.readyState != 4)
                                 {
                                   return;
                                 };
                                 if (authReq.status != 200)
                                 {
                                   if (authReq.status == 403)
                                   {
                                     handleAuthError(authReq.responseText);
                                     return;
                                   };
                                 };
                                 numAuthTriesSinceLastSuccess = 0;
                                 var tokens = authReq.responseText.split("\n");
                                 for (var t = 0; t < tokens.length; t++)
                                 {
                                   name = tokens[t].substring(0,tokens[t].indexOf("="));
                                   val = tokens[t].substring(tokens[t].indexOf("=") + 1);
                                   if (name.toLowerCase() == "auth")
                                   {
                                     authToken = val;
                                   }
                                   else if (name.toLowerCase() == "sid")
                                        {
                                          sid = val;
                                        }
                                        else if (name.toLowerCase() == "lsid")
                                             {
                                               lsid = val;
                                             };
                                 };
                                 if (authToken != "")
                                 {
                                   getUserInfo();
                                 };
                               },
                               undefined,
                               "onAuthResponse",
                               "gadgets/GoogleCalendar_cc/plugin.js",
                               9);
var handleAuthError = __typedjs(function  (response)
                                {
                                  var error,url;
                                  var tokens = response.split("\n");
                                  for (var t = 0; t < tokens.length; t++)
                                  {
                                    name = tokens[t].substring(0,tokens[t].indexOf("="));
                                    val = tokens[t].substring(tokens[t].indexOf("=") + 1);
                                    if (name.toLowerCase() == "error")
                                    {
                                      error = val;
                                    }
                                    else if (name.toLowerCase() == "url")
                                         {
                                           url = val;
                                         };
                                  };
                                  if (error == "BadAuthentication")
                                  {
                                    showErrorItem(genericProblemText,
                                                  errorBadAuthText,
                                                  errorBadAuthSnippet,
                                                  null);
                                    authToken = "";
                                    options.PutValue("Email","");
                                    options.PutValue("Passwd","");
                                  }
                                  else if (error == "NotVerified" || error == "TermsNotAgreed" || error == "Unknown" || error == "AccountDeleted" || error == "AccountDisabled")
                                       {
                                         showErrorItem(genericProblemText,
                                                       errorUnknownText,
                                                       errorUnknownSnippet,
                                                       null);
                                       }
                                       else if (error == "CaptchaRequired")
                                            {
                                              showErrorItem(genericProblemText,
                                                            errorCaptchaRequiredText,
                                                            errorCaptchaRequiredSnippet,
                                                            url);
                                            }
                                            else if (error == "ServiceUnavailable")
                                                 {
                                                   showErrorItem(genericProblemText,
                                                                 errorServiceUnavailableText,
                                                                 errorServiceUnavailableSnippet,
                                                                 null);
                                                 };
                                },
                                undefined,
                                "handleAuthError",
                                "gadgets/GoogleCalendar_cc/plugin.js",
                                10);
var countObject = __typedjs(function  (o)
                            {
                              var count = 0;
                              for (var i in o)
                              {
                                count++;
                              };
                              return count;
                            },
                            undefined,
                            "countObject",
                            "gadgets/GoogleCalendar_cc/plugin.js",
                            11);
var timerRefresh = __typedjs(function  ()
                             {
                               if (authToken)
                               {
                                 forceReloadFromServer();
                               };
                             },
                             undefined,
                             "timerRefresh",
                             "gadgets/GoogleCalendar_cc/plugin.js",
                             12);
var forceReloadFromServer = __typedjs(function  ()
                                      {
                                        loadEventsForCurrentDates(true);
                                      },
                                      undefined,
                                      "forceReloadFromServer",
                                      "gadgets/GoogleCalendar_cc/plugin.js",
                                      13);
var loadEventsForCurrentDates = __typedjs(function  (force)
                                          {
                                            if (authToken == null || authToken == "")
                                            {
                                              DoAuth();
                                              return;
                                            };
                                            var startDate = __new(Date,[curYear,curMonth,1]);
                                            var dow = (startDate.getDay() + europeanDate) % 7;
                                            var leftOver = (38 - dow);
                                            var startString = getDateIso8601(__new(Date,
                                                                                   [startDate.getTime() - dow * 86400000]));
                                            var endString = getDateIso8601(__new(Date,
                                                                                 [startDate.getTime() + leftOver * 86400000]));
                                            setEventStoreTime(startString,endString);
                                            for (var i = 0; i < userStore.length; ++i)
                                            {
                                              var user = userStore[i];
                                              if (getUserShow(user))
                                              {
                                                loadEventsFromServer(startString,
                                                                     endString,
                                                                     force,
                                                                     user);
                                              };
                                            };
                                          },
                                          undefined,
                                          "loadEventsForCurrentDates",
                                          "gadgets/GoogleCalendar_cc/plugin.js",
                                          14);
var getEmail = __typedjs(function  ()
                         {
                           return options.GetValue("Email");
                         },
                         undefined,
                         "getEmail",
                         "gadgets/GoogleCalendar_cc/plugin.js",
                         15);
var getPW = __typedjs(function  ()
                      {
                        return options.GetValue("Passwd");
                      },
                      undefined,
                      "getPW",
                      "gadgets/GoogleCalendar_cc/plugin.js",
                      16);
var cacheItems = {};
var cacheUpdated = {};
var loadEventsFromServer = __typedjs(function  (expandFrom,
                                                expandTo,
                                                force,
                                                user)
                                     {
                                       var cache = cacheItems[user.id + "/" + expandFrom + "/" + expandTo];
                                       if (cache && force !== true)
                                       {
                                         return;
                                       };
                                       var url = user.url + "?start-min=" + expandFrom + "&start-max=" + expandTo + "&max-results=400";
                                       if (user.session)
                                       {
                                         url += "&" + user.session;
                                       };
                                       var updated = cacheUpdated[user.id + "/" + expandFrom + "/" + expandTo];
                                       if (updated)
                                       {
                                         url += "&updated-min=" + to3339String(updated);
                                       };
                                       var xmlReq = __new(XMLHttpRequest,[]);
                                       xmlReq.open("GET",url,true);
                                       xmlReq.onreadystatechange = genReceive(xmlReq,
                                                                              expandFrom,
                                                                              expandTo,
                                                                              user);
                                       xmlReq.setRequestHeader("Content-Type",
                                                               "application/atom+xml; charset=UTF-8");
                                       xmlReq.setRequestHeader("Authorization",
                                                               "GoogleLogin auth=" + authToken);
                                       xmlReq.setRequestHeader("X-If-No-Redirect","1");
                                       xmlReq.setRequestHeader("Referer",
                                                               "http://gds.calendar." + kClientLoginAppName);
                                       xmlReq.setRequestHeader("If-Modified-Since",
                                                               "Sat, 1 Jan 2000 00:00:00 GMT");
                                       try
                                       {
                                         xmlReq.send();
                                       }
                                       catch (e) {
                                                 };
                                     },
                                     undefined,
                                     "loadEventsFromServer",
                                     "gadgets/GoogleCalendar_cc/plugin.js",
                                     17);
var genReceive = __typedjs(function  (xmlReq,start,end,user)
                           {
                             return __typedjs(function  ()
                                              {
                                                return OnReceivedData(xmlReq,start,end,user);
                                              },
                                              arguments.callee,
                                              "",
                                              "gadgets/GoogleCalendar_cc/plugin.js",
                                              0);
                           },
                           undefined,
                           "genReceive",
                           "gadgets/GoogleCalendar_cc/plugin.js",
                           18);
var getUserInfo = __typedjs(function  (opt_url)
                            {
                              if (authToken == null || authToken == "")
                              {
                                DoAuth();
                                return;
                              };
                              if (! opt_url)
                              {
                                opt_url = kCalendarUserFeedUrl;
                              };
                              var xmlReq = __new(XMLHttpRequest,[]);
                              xmlReq.open("GET",opt_url,true);
                              xmlReq.onreadystatechange = genUserReceive(xmlReq);
                              xmlReq.setRequestHeader("Content-Type",
                                                      "application/atom+xml; charset=UTF-8");
                              xmlReq.setRequestHeader("Authorization",
                                                      "GoogleLogin auth=" + authToken);
                              xmlReq.setRequestHeader("X-If-No-Redirect","1");
                              xmlReq.setRequestHeader("Referer",
                                                      "http://gds.calendar." + kClientLoginAppName);
                              xmlReq.setRequestHeader("If-Modified-Since",
                                                      "Sat, 1 Jan 2000 00:00:00 GMT");
                              try
                              {
                                xmlReq.send();
                              }
                              catch (e) {
                                        };
                            },
                            undefined,
                            "getUserInfo",
                            "gadgets/GoogleCalendar_cc/plugin.js",
                            19);
var userStore = [];
var UserItem = __typedjs(function  (title,
                                    id,
                                    url,
                                    color,
                                    accessLevel,
                                    selected,
                                    timezone,
                                    hidden,
                                    updated,
                                    overrideName)
                         {
                           __thisref(this,arguments.callee).title = title;
                           __thisref(this,arguments.callee).id = id;
                           __thisref(this,arguments.callee).url = url;
                           __thisref(this,arguments.callee).color = color;
                           __thisref(this,arguments.callee).accessLevel = accessLevel;
                           __thisref(this,arguments.callee).selected = selected;
                           __thisref(this,arguments.callee).timezone = timezone;
                           __thisref(this,arguments.callee).hidden = hidden;
                           __thisref(this,arguments.callee).updated = updated;
                           __thisref(this,arguments.callee).overrideName = overrideName;
                         },
                         undefined,
                         "UserItem",
                         "gadgets/GoogleCalendar_cc/plugin.js",
                         20);
var addUser = __typedjs(function  (elem,users)
                        {
                          var title = "",
                              id = "",
                              url = "",
                              email = "",
                              color = "",
                              accessLevel = "";
                          var selected = true,hidden = false,timezone = "",updated = null;
                          var overrideName = "";
                          for (var node = elem.firstChild; node != null; node = node.nextSibling)
                          {
                            if (node.nodeName == "id")
                            {
                              if (node.firstChild)
                              {
                                id = node.firstChild.nodeValue;
                              }
                              else {
                                     id = MSG_NO_TITLE;
                                   };
                            }
                            else if (node.nodeName == "title")
                                 {
                                   if (node.firstChild)
                                   {
                                     title = node.firstChild.nodeValue;
                                   }
                                   else {
                                          title = MSG_NO_TITLE;
                                        };
                                 }
                                 else if (node.nodeName == "link" && node.getAttribute("rel") == "alternate")
                                      {
                                        url = node.getAttribute("href");
                                      }
                                      else if (node.nodeName == "gCal:color")
                                           {
                                             color = node.getAttribute("value");
                                           }
                                           else if (node.nodeName == "gCal:accesslevel")
                                                {
                                                  accessLevel = node.getAttribute("value");
                                                }
                                                else if (node.nodeName == "gCal:selected")
                                                     {
                                                       selected = (node.getAttribute("value") != "false");
                                                     }
                                                     else if (node.nodeName == "gCal:timezone")
                                                          {
                                                            timezone = node.getAttribute("value");
                                                          }
                                                          else if (node.nodeName == "gCal:hidden")
                                                               {
                                                                 hidden = (node.getAttribute("value") != "false");
                                                               }
                                                               else if (node.nodeName == "updated" && node.firstChild)
                                                                    {
                                                                      updated = rfc3339StringToDate(node.firstChild.nodeValue);
                                                                    }
                                                                    else if (node.nodeName == "gCal:overridename")
                                                                         {
                                                                           overrideName = node.getAttribute("value");
                                                                         };
                          };
                          users.push(__new(UserItem,
                                           [title,
                                            id,
                                            url,
                                            color,
                                            accessLevel,
                                            selected,
                                            timezone,
                                            hidden,
                                            updated,
                                            overrideName]));
                        },
                        undefined,
                        "addUser",
                        "gadgets/GoogleCalendar_cc/plugin.js",
                        21);
var genUserReceive = __typedjs(function  (xmlReq)
                               {
                                 return __typedjs(function  ()
                                                  {
                                                    if (! xmlReq || ! (xmlReq.readyState == 4))
                                                    {
                                                      return;
                                                    };
                                                    if (xmlReq.status != 200)
                                                    {
                                                      if (xmlReq.status == 412)
                                                      {
                                                        var location = xmlReq.getResponseHeader("X-Redirect-Location");
                                                        getUserInfo(location);
                                                      }
                                                      else if (xmlReq.status == 401)
                                                           {
                                                             authToken = "";
                                                             DoAuth();
                                                           }
                                                           else {
                                                                };
                                                      return;
                                                    };
                                                    var responseText = xmlReq.responseText;
                                                    var doc = __new(DOMDocument,[]);
                                                    doc.loadXML(responseText);
                                                    var feed = doc.getElementsByTagName("feed");
                                                    if (! feed || feed.length == 0)
                                                    {
                                                      showErrorItem(genericProblemText,
                                                                    errorOccuredDescText,
                                                                    unexpectedErrorText,
                                                                    null);
                                                      return;
                                                    };
                                                    var elem = doc.getElementsByTagName("entry");
                                                    if (elem != null && elem.length > 0)
                                                    {
                                                      userStore = [];
                                                      for (var i = 0; i < elem.length; ++i)
                                                      {
                                                        addUser(elem[i],userStore);
                                                      };
                                                    };
                                                    forceReloadFromServer();
                                                  },
                                                  arguments.callee,
                                                  "",
                                                  "gadgets/GoogleCalendar_cc/plugin.js",
                                                  0);
                               },
                               undefined,
                               "genUserReceive",
                               "gadgets/GoogleCalendar_cc/plugin.js",
                               22);
var cachedEventStore = null;
var getEventStore = __typedjs(function  ()
                              {
                                if (cachedEventStore)
                                {
                                  return cachedEventStore;
                                };
                                var out = [];
                                for (var i = 0; i < userStore.length; ++i)
                                {
                                  if (! getUserShow(userStore[i]))
                                  {
                                    continue;
                                    ;;
                                  };
                                  var item = cacheItems[userStore[i].id + "/" + getEventStore.start + "/" + getEventStore.end];
                                  if (item && item.length > 0)
                                  {
                                    out = out.concat(item);
                                  };
                                };
                                cachedEventStore = out;
                                return out;
                              },
                              undefined,
                              "getEventStore",
                              "gadgets/GoogleCalendar_cc/plugin.js",
                              23);
var setEventStoreTime = __typedjs(function  (start,end)
                                  {
                                    getEventStore.start = start;
                                    getEventStore.end = end;
                                    cachedEventStore = null;
                                  },
                                  undefined,
                                  "setEventStoreTime",
                                  "gadgets/GoogleCalendar_cc/plugin.js",
                                  24);
var prevTime = __new(Date,[]).getTime() - 8 * 60 * 60 * 1000;
var checkEventsForExpiry = __typedjs(function  ()
                                     {
                                       var curTime = __new(Date,[]).getTime();
                                       if (curTime - prevTime < 58000)
                                       {
                                         return;
                                       };
                                       var eventStore = getEventStore();
                                       if (eventStore)
                                       {
                                         for (var i = 0; i < eventStore.length; ++i)
                                         {
                                           var e = eventStore[i];
                                           if (! e.startTime || ! e.endTime)
                                           {
                                             continue;
                                             ;;
                                           };
                                           var eventStartTime = e.startTime.getTime();
                                           var remindBeforeTime = ((e.reminder == "") ? 10 : e.reminder) * 60 * 1000;
                                           eventStartTime -= remindBeforeTime;
                                           if (eventStartTime > prevTime && eventStartTime <= curTime)
                                           {
                                             var item = __new(ContentItem,[]);
                                             item.layout = gddContentItemLayoutEmail;
                                             item.heading = e.title;
                                             item.source = e.getNiceTime();
                                             if (e.location != null)
                                             item.snippet = MSG_LOCATION + ": " + e.location;
                                             item.open_command = e.url;
                                             contentArea.AddContentItem(item,
                                                                        gddItemDisplayAsNotification);
                                           };
                                         };
                                       };
                                       prevTime = curTime;
                                     },
                                     undefined,
                                     "checkEventsForExpiry",
                                     "gadgets/GoogleCalendar_cc/plugin.js",
                                     25);
var getEventsForDate = __typedjs(function  (d,store)
                                 {
                                   var start = __new(Date,
                                                     [d.getFullYear(),
                                                      d.getMonth(),
                                                      d.getDate()]).getTime();
                                   var end = __new(Date,
                                                   [d.getFullYear(),
                                                    d.getMonth(),
                                                    d.getDate() + 1]).getTime();
                                   start = Math.floor(start / 1000);
                                   end = Math.floor(end / 1000);
                                   var toPurge = {};
                                   var purgeNeeded = false;
                                   var out = [];
                                   if (store)
                                   {
                                     for (var i = 0; i < store.length; ++i)
                                     {
                                       var e = store[i];
                                       if (! e.startTime || ! e.endTime)
                                       {
                                         continue;
                                         ;;
                                       };
                                       var safeStart = Math.floor(e.startTime.getTime() / 1000);
                                       var safeEnd = Math.floor(e.endTime.getTime() / 1000);
                                       if (end > safeStart && start < safeEnd)
                                       {
                                         out.push(e);
                                         if (e.originalId)
                                         {
                                           toPurge[safeStart + "_" + safeEnd] = 1;
                                           purgeNeeded = true;
                                         };
                                       }
                                       else if (safeStart == safeEnd && safeStart == start)
                                            {
                                              out.push(e);
                                              if (e.originalId)
                                              {
                                                toPurge[safeStart + "_" + safeEnd] = 1;
                                                purgeNeeded = true;
                                              };
                                            };
                                     };
                                   };
                                   if (purgeNeeded)
                                   {
                                     var outPurged = [];
                                     for (var i = 0; i < out.length; ++i)
                                     {
                                       var e = out[i];
                                       if (e.recur && ! e.originalId)
                                       {
                                         var safeStart = Math.floor(e.startTime.getTime() / 1000);
                                         var safeEnd = Math.floor(e.endTime.getTime() / 1000);
                                         if (toPurge[safeStart + "_" + safeEnd])
                                         {
                                           continue;
                                           ;;
                                         };
                                       };
                                       outPurged.push(e);
                                     };
                                     out = outPurged;
                                   };
                                   return out.sort(sortByDateFunc);
                                 },
                                 undefined,
                                 "getEventsForDate",
                                 "gadgets/GoogleCalendar_cc/plugin.js",
                                 26);
var genCalEvents = __typedjs(function  (elem,events,userId)
                             {
                               var title = "";
                               var url = "";
                               var location = "";
                               var reminder = "";
                               var startTime = "";
                               var endTime = "";
                               var isAllDay = false;
                               var attendees = "";
                               var recur = false;
                               var status = "";
                               var desc = "";
                               var originalId = "";
                               for (var node = elem.firstChild; node != null; node = node.nextSibling)
                               {
                                 if (node.nodeName == "title")
                                 {
                                   title = node.firstChild ? node.firstChild.nodeValue : MSG_NO_TITLE;
                                 }
                                 else if (node.nodeName == "link" && node.getAttribute("rel") == "alternate")
                                      {
                                        url = node.getAttribute("href");
                                      }
                                      else if (node.nodeName == "gd:where")
                                           {
                                             location = node.getAttribute("valueString");
                                           }
                                           else if (node.nodeName == "gd:reminder")
                                                {
                                                  reminder = node.getAttribute("minutes");
                                                }
                                                else if (node.nodeName == "gd:who")
                                                     {
                                                       attendees += node.getAttribute("valueString") + ", \n";
                                                     }
                                                     else if (node.nodeName == "gd:recurrence")
                                                          {
                                                            recur = true;
                                                          }
                                                          else if (node.nodeName == "gd:eventStatus")
                                                               {
                                                                 status = node.getAttribute("value");
                                                               }
                                                               else if (node.nodeName == "content" && node.firstChild)
                                                                    {
                                                                      desc = node.firstChild.nodeValue;
                                                                    }
                                                                    else if (node.nodeName == "gd:originalEvent")
                                                                         {
                                                                           originalId = node.getAttribute("id");
                                                                         };
                               };
                               if (status == "http://schemas.google.com/g/2005#event.canceled")
                               {
                                 return;
                               };
                               if (attendees.length > 3)
                               {
                                 attendees = attendees.substr(0,attendees.length - 3);
                               };
                               for (var node = elem.firstChild; node != null; node = node.nextSibling)
                               {
                                 if (node.nodeName == "gd:when" && title != null)
                                 {
                                   var startTimeStr = node.getAttribute("startTime");
                                   var endTimeStr = node.getAttribute("endTime");
                                   startTime = rfc3339StringToDate(startTimeStr);
                                   endTime = rfc3339StringToDate(endTimeStr);
                                   if (startTime == null || endTime == null)
                                   {
                                     continue;
                                     ;;
                                   };
                                   isAllDay = (startTimeStr.length <= 11);
                                   events.push(__new(CalEvent,
                                                     [title,
                                                      url,
                                                      location,
                                                      reminder,
                                                      startTime,
                                                      endTime,
                                                      isAllDay,
                                                      attendees,
                                                      recur,
                                                      desc,
                                                      userId,
                                                      originalId]));
                                 };
                               };
                             },
                             undefined,
                             "genCalEvents",
                             "gadgets/GoogleCalendar_cc/plugin.js",
                             27);
var CalEvent = __typedjs(function  (title,
                                    url,
                                    location,
                                    reminder,
                                    startTime,
                                    endTime,
                                    isAllDay,
                                    attendees,
                                    recur,
                                    desc,
                                    userId,
                                    originalId)
                         {
                           __thisref(this,arguments.callee).title = title;
                           __thisref(this,arguments.callee).url = url;
                           __thisref(this,arguments.callee).location = location;
                           __thisref(this,arguments.callee).reminder = reminder;
                           __thisref(this,arguments.callee).startTime = startTime;
                           __thisref(this,arguments.callee).endTime = endTime;
                           __thisref(this,arguments.callee).isAllDay = isAllDay;
                           __thisref(this,arguments.callee).attendees = attendees;
                           __thisref(this,arguments.callee).recur = recur;
                           __thisref(this,arguments.callee).desc = desc;
                           __thisref(this,arguments.callee).userId = userId;
                           __thisref(this,arguments.callee).originalId = originalId;
                         },
                         undefined,
                         "CalEvent",
                         "gadgets/GoogleCalendar_cc/plugin.js",
                         28);
CalEvent.prototype.getItem = __typedjs(function  ()
                                       {
                                         if (__thisref(this,arguments.callee).item)
                                         return __thisref(this,arguments.callee).item;
                                         __thisref(this,arguments.callee).item = __new(ContentItem,
                                                                                       []);
                                         __thisref(this,
                                                   arguments.callee).item.onOpenItem = genDetails(__thisref(this,
                                                                                                            arguments.callee).url);
                                         __thisref(this,
                                                   arguments.callee).item.layout = gddContentItemLayoutNowrapItems;
                                         __thisref(this,
                                                   arguments.callee).item.flags = gddContentItemFlagNoRemove;
                                         if (__thisref(this,arguments.callee).isAllDay)
                                         {
                                           __thisref(this,
                                                     arguments.callee).item.heading = __thisref(this,
                                                                                                arguments.callee).title;
                                         }
                                         else {
                                                __thisref(this,
                                                          arguments.callee).item.heading = FormatTimeShort(__thisref(this,
                                                                                                                     arguments.callee).startTime) + " " + __thisref(this,
                                                                                                                                                                    arguments.callee).title;
                                              };
                                         var blurb = GenNiceStartEnd(__thisref(this,
                                                                               arguments.callee).startTime,
                                                                     __thisref(this,
                                                                               arguments.callee).endTime,
                                                                     __thisref(this,
                                                                               arguments.callee).isAllDay) + "\n\n";
                                         if (__thisref(this,arguments.callee).location)
                                         {
                                           blurb += MSG_LOCATION + "\n" + __thisref(this,
                                                                                    arguments.callee).location + "\n\n";
                                         };
                                         if (__thisref(this,arguments.callee).attendees)
                                         {
                                           blurb += MSG_ATTENDEES + "\n" + __thisref(this,
                                                                                     arguments.callee).attendees + "\n\n";
                                         };
                                         __thisref(this,arguments.callee).item.snippet = blurb;
                                         __thisref(this,
                                                   arguments.callee).item.tooltip = blurb.replace(/\n\n/g,
                                                                                                  "<br/>").replace(/\n/g,
                                                                                                                   "").replace(/<br\/>/g,
                                                                                                                               "\n");
                                         __thisref(this,
                                                   arguments.callee).item.onDetailsView = genDetailsFunc(__thisref(this,
                                                                                                                   arguments.callee));
                                         __thisref(this,
                                                   arguments.callee).item.onDrawItem = genDrawItem(__thisref(this,
                                                                                                             arguments.callee));
                                         return __thisref(this,arguments.callee).item;
                                       },
                                       undefined,
                                       "CalEvent.prototype.getItem",
                                       "gadgets/GoogleCalendar_cc/plugin.js",
                                       29);
var genDetailsFunc = __typedjs(function  (c)
                               {
                                 return __typedjs(function  (item)
                                                  {
                                                    return onDetailsView(item,c);
                                                  },
                                                  arguments.callee,
                                                  "",
                                                  "gadgets/GoogleCalendar_cc/plugin.js",
                                                  0);
                               },
                               undefined,
                               "genDetailsFunc",
                               "gadgets/GoogleCalendar_cc/plugin.js",
                               30);
CalEvent.prototype.getNiceTime = __typedjs(function  ()
                                           {
                                             return GenNiceStartEnd(__thisref(this,
                                                                              arguments.callee).startTime,
                                                                    __thisref(this,
                                                                              arguments.callee).endTime,
                                                                    __thisref(this,
                                                                              arguments.callee).isAllDay);
                                           },
                                           undefined,
                                           "CalEvent.prototype.getNiceTime",
                                           "gadgets/GoogleCalendar_cc/plugin.js",
                                           31);
var SESSION_REGEX = /(gsessionid=[^&]*)/;
var OnReceivedData = __typedjs(function  (xmlReq,start,end,user)
                               {
                                 if (! xmlReq || ! (xmlReq.readyState == 4))
                                 {
                                   return;
                                 };
                                 if (xmlReq.status == 200)
                                 {
                                   processDoc(xmlReq.responseText,start,end,user);
                                 }
                                 else if (xmlReq.status == 412)
                                      {
                                        var location = xmlReq.getResponseHeader("X-Redirect-Location");
                                        var parts = SESSION_REGEX.exec(location);
                                        if (parts)
                                        {
                                          user.session = parts[1];
                                          loadEventsFromServer(start,end,true,user);
                                        }
                                        else {
                                             };
                                      }
                                      else if (xmlReq.status == 401)
                                           {
                                             authToken = "";
                                             loadEventsForCurrentDates();
                                           }
                                           else {
                                                };
                               },
                               undefined,
                               "OnReceivedData",
                               "gadgets/GoogleCalendar_cc/plugin.js",
                               32);
var processDoc = __typedjs(function  (responseText,start,end,user)
                           {
                             var doc = __new(DOMDocument,[]);
                             doc.loadXML(responseText);
                             var feed = doc.getElementsByTagName("feed");
                             if (! feed || feed.length == 0)
                             {
                               showErrorItem(genericProblemText,
                                             errorOccuredDescText,
                                             unexpectedErrorText,
                                             null);
                               return;
                             };
                             var updatedElem = doc.getElementsByTagName("updated");
                             var updatedTime = null;
                             if (updatedElem != null && updatedElem.length > 0)
                             {
                               updatedTime = rfc3339StringToDate(updatedElem[0].firstChild.nodeValue);
                             };
                             var cacheValue = cacheUpdated[user.id + "/" + start + "/" + end];
                             if (cacheValue)
                             {
                               if (sameTime(updatedTime,cacheValue))
                               {
                                 return;
                               }
                               else {
                                      delete cacheItems[user.id + "/" + start + "/" + end];
                                      delete cacheUpdated[user.id + "/" + start + "/" + end];
                                      cachedEventStore = null;
                                      loadEventsFromServer(start,end,true,user);
                                      return;
                                    };
                             };
                             var elem = doc.getElementsByTagName("entry");
                             if (elem != null && elem.length > 0)
                             {
                               var events = [];
                               for (var i = 0; i < elem.length; ++i)
                               {
                                 genCalEvents(elem[i],events,user.id);
                               };
                               if (countObject(cacheItems) > 200)
                               {
                                 cacheItems = {};
                                 cacheUpdated = {};
                               };
                               cacheItems[user.id + "/" + start + "/" + end] = events;
                               cacheUpdated[user.id + "/" + start + "/" + end] = updatedTime;
                               cachedEventStore = null;
                             };
                             drawCalendar();
                           },
                           undefined,
                           "processDoc",
                           "gadgets/GoogleCalendar_cc/plugin.js",
                           33);
var genDetails = __typedjs(function  (url)
                           {
                             return __typedjs(function  ()
                                              {
                                                loginAndGotoURL(url);
                                              },
                                              arguments.callee,
                                              "",
                                              "gadgets/GoogleCalendar_cc/plugin.js",
                                              0);
                           },
                           undefined,
                           "genDetails",
                           "gadgets/GoogleCalendar_cc/plugin.js",
                           34);
var DrawRounded = __typedjs(function  (graphics,
                                       x,
                                       y,
                                       width,
                                       height,
                                       color)
                            {
                              graphics.DrawRect(x + 1,y,width - 2,height,color,color);
                              graphics.DrawLine(x,y + 1,x,y + height - 1,color);
                              graphics.DrawLine(x + width - 1,
                                                y + 1,
                                                x + width - 1,
                                                y + height - 1,
                                                color);
                            },
                            undefined,
                            "DrawRounded",
                            "gadgets/GoogleCalendar_cc/plugin.js",
                            35);
var sortByDateFunc = __typedjs(function  (a,b)
                               {
                                 if (b.isAllDay != a.isAllDay)
                                 {
                                   return b.isAllDay ? 1 : - 1;
                                 };
                                 if (! b.isAllDay)
                                 {
                                   var startDiff = a.startTime.getTime() - b.startTime.getTime();
                                   if (startDiff)
                                   {
                                     return startDiff;
                                   };
                                 };
                                 return b.title.toLowerCase() > a.title.toLowerCase() ? - 1 : 1;
                               },
                               undefined,
                               "sortByDateFunc",
                               "gadgets/GoogleCalendar_cc/plugin.js",
                               36);
var sortByTitle = __typedjs(function  (a,b)
                            {
                              return b.title.toLowerCase() > a.title.toLowerCase() ? - 1 : 1;
                            },
                            undefined,
                            "sortByTitle",
                            "gadgets/GoogleCalendar_cc/plugin.js",
                            37);
var showErrorItem = __typedjs(function  (title,
                                         subtitle,
                                         snippet,
                                         url)
                              {
                                contentArea.RemoveAllContentItems();
                                PresizeWindow();
                                var h = __new(HelpContentItem,[title,subtitle,snippet,url]);
                                h.AddToSidebar();
                              },
                              undefined,
                              "showErrorItem",
                              "gadgets/GoogleCalendar_cc/plugin.js",
                              38);
var COLOR_MAP = [undefined,
                 ["#A32929","#CC3333"],
                 ["#B1365F","#DD4477"],
                 ["#7A367A","#994499"],
                 ["#5229A3","#6633CC"],
                 ["#29527A","#336699"],
                 ["#2952A3","#3366CC"],
                 ["#1B887A","#22AA99"],
                 ["#28754E","#329262"],
                 ["#0D7813","#109618"],
                 ["#528800","#66AA00"],
                 ["#88880E","#AAAA11"],
                 ["#AB8B00","#D6AE00"],
                 ["#BE6D00","#EE8800"],
                 ["#B1440E","#DD5511"],
                 ["#865A5A","#A87070"],
                 ["#705770","#8C6D8C"],
                 ["#4E5D6C","#627487"],
                 ["#5A6986","#7083A8"],
                 ["#4A716C","#5C8D87"],
                 ["#6E6E41","#898951"],
                 ["#8D6F47","#B08B59"]];
var genDrawItem = __typedjs(function  (c)
                            {
                              return __typedjs(function  (item,target,graphics,x,y,width,height)
                                               {
                                                 DrawItem(item,
                                                          target,
                                                          graphics,
                                                          x,
                                                          y,
                                                          width,
                                                          height,
                                                          c.isAllDay,
                                                          c.recur,
                                                          c.attendees,
                                                          c.reminder,
                                                          c.userId);
                                               },
                                               arguments.callee,
                                               "",
                                               "gadgets/GoogleCalendar_cc/plugin.js",
                                               0);
                            },
                            undefined,
                            "genDrawItem",
                            "gadgets/GoogleCalendar_cc/plugin.js",
                            39);
var getColorForUserId = __typedjs(function  (userId)
                                  {
                                    var u = getUserForId(userId);
                                    if (u)
                                    {
                                      for (var j = 1; j < COLOR_MAP.length; ++j)
                                      {
                                        if (u.color == COLOR_MAP[j][0])
                                        {
                                          return j;
                                        };
                                      };
                                    };
                                    return 6;
                                  },
                                  undefined,
                                  "getColorForUserId",
                                  "gadgets/GoogleCalendar_cc/plugin.js",
                                  40);
var getUserForId = __typedjs(function  (userId)
                             {
                               for (var i = 0; i < userStore.length; ++i)
                               {
                                 if (userStore[i].id == userId)
                                 {
                                   return userStore[i];
                                 };
                               };
                               return null;
                             },
                             undefined,
                             "getUserForId",
                             "gadgets/GoogleCalendar_cc/plugin.js",
                             41);
var DrawItem = __typedjs(function  (item,
                                    target,
                                    graphics,
                                    x,
                                    y,
                                    width,
                                    height,
                                    allDay,
                                    recur,
                                    attendees,
                                    reminder,
                                    userId)
                         {
                           var color = getColorForUserId(userId);
                           var textColor = COLOR_MAP[color][0];
                           var widthOffset = 3;
                           if (allDay)
                           {
                             DrawRounded(graphics,x,y,width,height + 2,COLOR_MAP[color][1]);
                             textColor = "#FFFFFF";
                           };
                           graphics.DrawText(x + widthOffset,
                                             y,
                                             width,
                                             height,
                                             item.heading,
                                             textColor,
                                             0,
                                             gddFontNormal);
                           var textX = x + widthOffset + graphics.GetTextWidth(item.heading,
                                                                               0,
                                                                               gddFontNormal) + 4;
                           if (reminder)
                           {
                             graphics.DrawImage(textX,y + 4,9,7,image_alarm[allDay],100);
                             textX += 12;
                           };
                           if (recur)
                           {
                             graphics.DrawImage(textX,y + 4,9,7,image_rep[allDay],100);
                             textX += 12;
                           };
                           if (attendees)
                           {
                             graphics.DrawImage(textX,y + 4,9,7,image_people[allDay],100);
                             textX += 12;
                           };
                         },
                         undefined,
                         "DrawItem",
                         "gadgets/GoogleCalendar_cc/plugin.js",
                         42);
var HelpContentItem = __typedjs(function  (title,
                                           subtitle,
                                           snippet,
                                           url)
                                {
                                  if (url == null || url == "")
                                  {
                                    url = kProductHomePageUri;
                                  };
                                  var c = __new(ContentItem,[]);
                                  c.layout = gddContentItemLayoutNews;
                                  c.heading = title;
                                  c.source = subtitle;
                                  c.snippet = snippet;
                                  c.tooltip = snippet;
                                  c.SetRect(0,0,getWindowWidth(),getWindowHeight());
                                  c.onOpenItem = genDetails(url);
                                  c.flags = gddContentItemFlagNoRemove | gddContentItemFlagHighlighted;
                                  __thisref(this,arguments.callee).item = c;
                                  HelpContentItem.prototype.AddToSidebar = AddToSidebar;
                                  var AddToSidebar = __typedjs(function  ()
                                                               {
                                                                 contentArea.AddContentItem(__thisref(this,
                                                                                                      arguments.callee).item,
                                                                                            gddItemDisplayInSidebar);
                                                               },
                                                               arguments.callee,
                                                               "AddToSidebar",
                                                               "gadgets/GoogleCalendar_cc/plugin.js",
                                                               0);
                                },
                                undefined,
                                "HelpContentItem",
                                "gadgets/GoogleCalendar_cc/plugin.js",
                                43);
var ShowOptionsDlg = __typedjs(function  (wnd)
                               {
                                 wnd.AddControl(gddWndCtrlClassLabel,0,"",MSG_SIGN_IN,10,10,260,25);
                                 wnd.AddControl(gddWndCtrlClassLabel,0,"",MSG_EMAIL,40,40,200,25);
                                 wnd.AddControl(gddWndCtrlClassEdit,
                                                0,
                                                "Email",
                                                getEmail(),
                                                40,
                                                55,
                                                200,
                                                24);
                                 wnd.AddControl(gddWndCtrlClassLabel,0,"",MSG_PW,40,85,200,15);
                                 wnd.AddControl(gddWndCtrlClassEdit,
                                                gddWndCtrlTypeEditPassword,
                                                "Passwd",
                                                getPW(),
                                                40,
                                                100,
                                                200,
                                                24);
                                 var ctl;
                                 ctl = wnd.AddControl(gddWndCtrlClassButton,
                                                      gddWndCtrlTypeButtonCheck,
                                                      "button24",
                                                      MSG_24,
                                                      20,
                                                      140,
                                                      250,
                                                      24);
                                 ctl.value = (options.GetValue("24") == "true") ? "1" : "0";
                                 ctl = wnd.AddControl(gddWndCtrlClassButton,
                                                      gddWndCtrlTypeButtonCheck,
                                                      "european",
                                                      MSG_BEGIN_MONDAY,
                                                      20,
                                                      160,
                                                      250,
                                                      24);
                                 ctl.value = (options.GetValue("european") == "true") ? "1" : "0";
                                 ctl = wnd.AddControl(gddWndCtrlClassButton,
                                                      gddWndCtrlTypeButtonCheck,
                                                      "hidemonth",
                                                      MSG_HIDE_CALENDAR,
                                                      20,
                                                      180,
                                                      250,
                                                      24);
                                 ctl.value = (options.GetValue("hidemonth") == "true") ? "1" : "0";
                                 ctl = wnd.AddControl(gddWndCtrlClassButton,
                                                      gddWndCtrlTypeButtonCheck,
                                                      "wn",
                                                      MSG_SHOW_WEEK_NUM,
                                                      20,
                                                      200,
                                                      250,
                                                      24);
                                 ctl.value = (options.GetValue("showweek") == "true") ? "1" : "0";
                                 if (userStore.length > 0)
                                 {
                                   var otherCalendars = false;
                                   var orderedStores = userStore.sort(sortByTitle);
                                   wnd.AddControl(gddWndCtrlClassLabel,
                                                  0,
                                                  "",
                                                  MSG_MY_CALENDARS,
                                                  10,
                                                  245,
                                                  250,
                                                  24);
                                   var calCount = 0;
                                   for (var i = 0; i < orderedStores.length; ++i)
                                   {
                                     var user = orderedStores[i];
                                     if (user.accessLevel != "owner")
                                     {
                                       otherCalendars = true;
                                       continue;
                                       ;;
                                     };
                                     ctl = wnd.AddControl(gddWndCtrlClassButton,
                                                          gddWndCtrlTypeButtonCheck,
                                                          "u" + i,
                                                          user.title,
                                                          20,
                                                          260 + 20 * calCount,
                                                          250,
                                                          24);
                                     ++calCount;
                                     ctl.value = getUserShow(user) ? "1" : "0";
                                   };
                                   if (otherCalendars)
                                   {
                                     wnd.AddControl(gddWndCtrlClassLabel,
                                                    0,
                                                    "",
                                                    MSG_OTHER_CALENDARS,
                                                    10,
                                                    265 + 20 * calCount,
                                                    250,
                                                    24);
                                     ++calCount;
                                     for (var i = 0; i < orderedStores.length; ++i)
                                     {
                                       var user = orderedStores[i];
                                       if (user.accessLevel == "owner")
                                       {
                                         continue;
                                         ;;
                                       };
                                       ctl = wnd.AddControl(gddWndCtrlClassButton,
                                                            gddWndCtrlTypeButtonCheck,
                                                            "u" + i,
                                                            user.title,
                                                            20,
                                                            260 + 20 * calCount,
                                                            250,
                                                            24);
                                       ++calCount;
                                       ctl.value = getUserShow(user) ? "1" : "0";
                                     };
                                   };
                                 };
                                 wnd.onClose = OptionsDlgClosed;
                               },
                               undefined,
                               "ShowOptionsDlg",
                               "gadgets/GoogleCalendar_cc/plugin.js",
                               44);
var getUserShow = __typedjs(function  (user)
                            {
                              if (user.show === true || user.show === false)
                              {
                                return user.show;
                              };
                              var show = options.GetValue("show-" + user.id);
                              if (show === "true" || show === "false")
                              {
                                return user.show = (show === "true");
                              };
                              show = ! user.hidden && user.selected;
                              setUserShow(user,show);
                              return show;
                            },
                            undefined,
                            "getUserShow",
                            "gadgets/GoogleCalendar_cc/plugin.js",
                            45);
var setUserShow = __typedjs(function  (user,value)
                            {
                              options.PutValue("show-" + user.id,value ? "true" : "false");
                              user.show = value;
                            },
                            undefined,
                            "setUserShow",
                            "gadgets/GoogleCalendar_cc/plugin.js",
                            46);
var OptionsDlgClosed = __typedjs(function  (wnd,code)
                                 {
                                   if (code != gddIdOK)
                                   {
                                     return;
                                   };
                                   var email = wnd.GetControl("Email").text;
                                   if (email == null || email.length == 0)
                                   {
                                     utils.alert(emailIsBlank);
                                     return false;
                                   };
                                   if (email.indexOf("@") == - 1)
                                   {
                                     email += "@gmail.com";
                                   };
                                   options.PutValue("Email",email);
                                   var passwd = wnd.GetControl("Passwd").text;
                                   if (passwd == null || passwd.length == 0)
                                   {
                                     utils.alert(passwordIsBlank);
                                     return false;
                                   };
                                   options.PutValue("Passwd",passwd);
                                   var ctl;
                                   ctl = wnd.GetControl("button24");
                                   options.PutValue("24",(ctl.value == true) ? "true" : "false");
                                   format24 = (ctl.value == true);
                                   ctl = wnd.GetControl("european");
                                   options.PutValue("european",
                                                    (ctl.value == true) ? "true" : "false");
                                   europeanDate = ctl.value ? 1 : 0;
                                   ctl = wnd.GetControl("hidemonth");
                                   options.PutValue("hidemonth",
                                                    (ctl.value == true) ? "true" : "false");
                                   hideMonthView = ctl.value ? 1 : 0;
                                   ctl = wnd.GetControl("wn");
                                   options.PutValue("showweek",
                                                    (ctl.value == true) ? "true" : "false");
                                   showWeekNum = ctl.value ? 1 : 0;
                                   if (userStore.length > 0)
                                   {
                                     var orderedStores = userStore.sort(sortByTitle);
                                     for (var i = 0; i < orderedStores.length; ++i)
                                     {
                                       ctl = wnd.GetControl("u" + i);
                                       var user = orderedStores[i];
                                       var oldVal = getUserShow(user);
                                       var newVal = (ctl.value == true);
                                       if (oldVal != newVal)
                                       {
                                         setUserShow(user,newVal);
                                       };
                                     };
                                   };
                                   authToken = "";
                                   numAuthTriesSinceLastSuccess = 0;
                                   forceReloadFromServer();
                                   drawCalendar();
                                 },
                                 undefined,
                                 "OptionsDlgClosed",
                                 "gadgets/GoogleCalendar_cc/plugin.js",
                                 47);
var encodeUrl = __typedjs(function  (sStr)
                          {
                            return escape(sStr).replace(/\+/g,"%2B").replace(/\"/g,
                                                                             "%22").replace(/\'/g,
                                                                                            "%27").replace(/\//g,
                                                                                                           "%2F");
                          },
                          undefined,
                          "encodeUrl",
                          "gadgets/GoogleCalendar_cc/plugin.js",
                          48);
var pad = __typedjs(function  (num)
                    {
                      return (num < 10 ? "0" : "") + num;
                    },
                    undefined,
                    "pad",
                    "gadgets/GoogleCalendar_cc/plugin.js",
                    49);
var getDateIso8601 = __typedjs(function  (date)
                               {
                                 return date.getFullYear() + "-" + pad(date.getMonth() + 1) + "-" + pad(date.getDate());
                               },
                               undefined,
                               "getDateIso8601",
                               "gadgets/GoogleCalendar_cc/plugin.js",
                               50);
var getDateYYYYMMDD = __typedjs(function  (date)
                                {
                                  return date.getFullYear() + pad(date.getMonth() + 1) + pad(date.getDate());
                                },
                                undefined,
                                "getDateYYYYMMDD",
                                "gadgets/GoogleCalendar_cc/plugin.js",
                                51);
var advanceDay = __typedjs(function  (d,offset)
                           {
                             return __new(Date,
                                          [d.getFullYear(),d.getMonth(),d.getDate() + offset]);
                           },
                           undefined,
                           "advanceDay",
                           "gadgets/GoogleCalendar_cc/plugin.js",
                           52);
var sameDay = __typedjs(function  (d1,d2)
                        {
                          return (d1.getFullYear() == d2.getFullYear() && d1.getMonth() == d2.getMonth() && d1.getDate() == d2.getDate());
                        },
                        undefined,
                        "sameDay",
                        "gadgets/GoogleCalendar_cc/plugin.js",
                        53);
var sameTime = __typedjs(function  (d1,d2)
                         {
                           return sameDay(d1,
                                          d2) && d1.getHours() == d2.getHours() && d1.getMinutes() == d2.getMinutes() && d1.getSeconds() == d2.getSeconds();
                         },
                         undefined,
                         "sameTime",
                         "gadgets/GoogleCalendar_cc/plugin.js",
                         54);
var dateIsToday = __typedjs(function  (date)
                            {
                              return sameDay(date,__new(Date,[]));
                            },
                            undefined,
                            "dateIsToday",
                            "gadgets/GoogleCalendar_cc/plugin.js",
                            55);
var showPage = __typedjs(function  (url)
                         {
                           var wshShell = __new(ActiveXObject,["WScript.Shell"]);
                           wshShell.run(url);
                           wshShell = null;
                         },
                         undefined,
                         "showPage",
                         "gadgets/GoogleCalendar_cc/plugin.js",
                         56);
var amp_re_ = /&/g;
var lt_re_ = /</g;
var gt_re_ = />/g;
var HtmlEscape = __typedjs(function  (str)
                           {
                             if (! str)
                             return "";
                             return str.replace(amp_re_,"&amp;").replace(lt_re_,
                                                                         "&lt;").replace(gt_re_,
                                                                                         "&gt;").replace(quote_re_,
                                                                                                         "&quot;");
                           },
                           undefined,
                           "HtmlEscape",
                           "gadgets/GoogleCalendar_cc/plugin.js",
                           57);
var quote_re_ = /\"/g;
var QuoteEscape = __typedjs(function  (str)
                            {
                              return HtmlEscape(str).replace(quote_re_,"&quot;");
                            },
                            undefined,
                            "QuoteEscape",
                            "gadgets/GoogleCalendar_cc/plugin.js",
                            58);
var EscapeHtmlString = __typedjs(function  (str)
                                 {
                                   var out = [];
                                   if (! str)
                                   return out;
                                   for (var i = 0; i < str.length; ++i)
                                   {
                                     var code = str.charCodeAt(i);
                                     if (code > 127)
                                     {
                                       out.push("&#",code,";");
                                     }
                                     else {
                                            out.push(str.charAt(i));
                                          };
                                   };
                                   return out.join("");
                                 },
                                 undefined,
                                 "EscapeHtmlString",
                                 "gadgets/GoogleCalendar_cc/plugin.js",
                                 59);
var onDetailsView = __typedjs(function  (item,c)
                              {
                                var color = COLOR_MAP[getColorForUserId(c.userId)][0];
                                var html = "<html><head>" + "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\"></head>" + "<body style=\"font-family: arial, sans-serif;\" TOPMARGIN=0 LEFTMARGIN=0>" + "<font size=-1><span style=\"font-size:120%; color:" + color + "\"><b>" + HtmlEscape(c.title) + "</b></span><br>";
                                if (userStore && userStore.length > 1)
                                {
                                  html += "<font size=-1><span style=\"color:" + color + "\">" + HtmlEscape(getUserForId(c.userId).title) + "</span><br>";
                                };
                                html += c.getNiceTime() + "<br><br>";
                                if (c.location)
                                {
                                  html += "<b>" + MSG_LOCATION + ":</b> " + HtmlEscape(c.location) + "<br>";
                                };
                                if (c.desc)
                                {
                                  html += "<b>" + MSG_DESCRIPTION + ":</b> " + HtmlEscape(c.desc) + "<br>";
                                };
                                if (c.reminder)
                                {
                                  html += "<b>" + MSG_REMINDER + ":</b> " + c.reminder + " " + MSG_REMINDER_MINUTES + "<br>";
                                };
                                if (c.recur)
                                {
                                  html += "<b>" + MSG_REPEATS + ":</b> " + MSG_REPEATS_YES + "<br>";
                                };
                                if (c.attendees)
                                {
                                  html += "<b>" + MSG_ATTENDEES + ":</b> " + HtmlEscape(c.attendees) + "<br>";
                                };
                                html += "<br><a target=\"_new\" href=\"" + QuoteEscape(c.url) + "\">" + MSG_EDIT_EVENT + " &raquo;</a>";
                                html += "<br><a target=\"_new\" href=\"" + QuoteEscape("http://www.google.com/calendar/render?date=" + getDateYYYYMMDD(__new(Date,
                                                                                                                                                             [curYear,
                                                                                                                                                              curMonth,
                                                                                                                                                              curDate]))) + "\">" + MSG_SHOW_IN_CALENDAR + " &raquo;</a>";
                                html += "<br><br></font></body></html>";
                                var control = __new(DetailsView,[]);
                                control.SetContent(__thisref(this,arguments.callee).title,
                                                   c.starTime,
                                                   EscapeHtmlString(html),
                                                   true,
                                                   item.layout);
                                control.html_content = true;
                                var details = {};
                                details.details_control = control;
                                return details;
                              },
                              undefined,
                              "onDetailsView",
                              "gadgets/GoogleCalendar_cc/plugin.js",
                              60);
var createOnDateUrl = __typedjs(function  (d)
                                {
                                  var zDate = getDateYYYYMMDD(d);
                                  var zDateEnd = getDateYYYYMMDD(advanceDay(d,1));
                                  return kCalendarEventUrl + "?action=TEMPLATE&dates=" + zDate + "/" + zDateEnd;
                                },
                                undefined,
                                "createOnDateUrl",
                                "gadgets/GoogleCalendar_cc/plugin.js",
                                61);
var NiceDate = __typedjs(function  (d,show_date,show_time)
                         {
                           var text = "";
                           if (show_date)
                           {
                             text += DOW[d.getDay()] + ", " + MONTHS_SHORT[d.getMonth()] + " " + d.getDate();
                           };
                           if (show_time)
                           {
                             if (show_date)
                             {
                               text += ", ";
                             };
                             var h = d.getHours();
                             if (! format24)
                             {
                               if (h > 12)
                               {
                                 h = h - 12;
                               }
                               else if (h == 0)
                                    {
                                      h = 12;
                                    };
                             }
                             else {
                                    if (h < 10)
                                    h = "0" + h;
                                  };
                             text += h;
                             var min = d.getMinutes();
                             if (min > 0 || format24)
                             {
                               text += ":" + (min < 10 ? "0" : "") + min;
                             };
                           };
                           return text;
                         },
                         undefined,
                         "NiceDate",
                         "gadgets/GoogleCalendar_cc/plugin.js",
                         62);
var GenNiceStartEnd = __typedjs(function  (start,end,allday)
                                {
                                  var text = "";
                                  var presentationEndDate = end;
                                  if (allday)
                                  {
                                    presentationEndDate = __new(Date,
                                                                [end.getFullYear(),
                                                                 end.getMonth(),
                                                                 end.getDate() - 1]);
                                  }
                                  else if (start.getTime() == end.getTime())
                                       {
                                         text = NiceDate(start,true,true);
                                         if (! format24)
                                         {
                                           text += (end.getHours() >= 12) ? MSG_PM : MSG_AM;
                                         };
                                         return text;
                                       };
                                  var difday = ! sameDay(start,presentationEndDate);
                                  text += NiceDate(start,true,! allday);
                                  if (allday)
                                  {
                                    if (difday)
                                    {
                                      text += "-" + NiceDate(presentationEndDate,difday,false);
                                    };
                                  }
                                  else {
                                         if (! format24 && (difday || (start.getHours() < 12 && end.getHours() >= 12)))
                                         {
                                           text += (start.getHours() < 12) ? MSG_AM : MSG_PM;
                                         };
                                         text += "-" + NiceDate(end,difday,true);
                                         if (! format24)
                                         {
                                           text += (end.getHours() >= 12) ? MSG_PM : MSG_AM;
                                         };
                                       };
                                  return text;
                                },
                                undefined,
                                "GenNiceStartEnd",
                                "gadgets/GoogleCalendar_cc/plugin.js",
                                63);
var to3339String = __typedjs(function  (d)
                             {
                               return d.getUTCFullYear() + "-" + pad(d.getUTCMonth()) + "-" + pad(d.getUTCDay()) + "T" + pad(d.getUTCHours()) + ":" + pad(d.getUTCMinutes()) + ":" + pad(d.getUTCSeconds()) + "Z";
                             },
                             undefined,
                             "to3339String",
                             "gadgets/GoogleCalendar_cc/plugin.js",
                             64);
var DATE_TIME_REGEX = /^(\d\d\d\d)-(\d\d)-(\d\d)T(\d\d):(\d\d):(\d\d)\.\d+(\+|-)(\d\d):(\d\d)$/;
var DATE_TIME_REGEX_Z = /^(\d\d\d\d)-(\d\d)-(\d\d)T(\d\d):(\d\d):(\d\d)\.\d+Z$/;
var DATE_REGEX = /^(\d\d\d\d)-(\d\d)-(\d\d)$/;
var rfc3339StringToDate = __typedjs(function  (rfc3339)
                                    {
                                      var parts = DATE_TIME_REGEX.exec(rfc3339);
                                      if (! parts)
                                      {
                                        parts = DATE_TIME_REGEX_Z.exec(rfc3339);
                                      };
                                      if (parts && parts.length > 0)
                                      {
                                        var d = __new(Date,[]);
                                        d.setUTCFullYear(parts[1],
                                                         parseInt(parts[2],10) - 1,
                                                         parts[3]);
                                        d.setUTCHours(parts[4]);
                                        d.setUTCMinutes(parts[5]);
                                        d.setUTCSeconds(parts[6]);
                                        var tzOffsetFeedMin = 0;
                                        if (parts.length > 7)
                                        {
                                          tzOffsetFeedMin = parseInt(parts[8],
                                                                     10) * 60 + parseInt(parts[9],
                                                                                         10);
                                          if (parts[7] != "-")
                                          {
                                            tzOffsetFeedMin = - tzOffsetFeedMin;
                                          };
                                        };
                                        return __new(Date,
                                                     [d.getTime() + tzOffsetFeedMin * 60 * 1000]);
                                      };
                                      parts = DATE_REGEX.exec(rfc3339);
                                      if (parts && parts.length > 0)
                                      {
                                        return __new(Date,
                                                     [parts[1],parseInt(parts[2],10) - 1,parts[3]]);
                                      };
                                      return null;
                                    },
                                    undefined,
                                    "rfc3339StringToDate",
                                    "gadgets/GoogleCalendar_cc/plugin.js",
                                    65);
var FormatTimeShort = __typedjs(function  (date)
                                {
                                  var hr = date.getHours(),mi = date.getMinutes();
                                  if (format24)
                                  {
                                    if (hr < 10)
                                    hr = "0" + hr;
                                    if (mi < 10)
                                    mi = "0" + mi;
                                    return hr + ":" + mi;
                                  }
                                  else {
                                         var ap = hr < 12 ? MSG_AM : MSG_PM;
                                         hr = (hr % 12) || 12;
                                         if (mi)
                                         {
                                           if (mi < 10)
                                           {
                                             mi = "0" + mi;
                                           };
                                           return hr + ":" + mi + ap;
                                         }
                                         else {
                                                return hr + ap;
                                              };
                                       };
                                },
                                undefined,
                                "FormatTimeShort",
                                "gadgets/GoogleCalendar_cc/plugin.js",
                                66);
var prevMonth = __typedjs(function  ()
                          {
                            OnCommand(gddCmdToolbarBack);
                          },
                          undefined,
                          "prevMonth",
                          "gadgets/GoogleCalendar_cc/plugin.js",
                          67);
var nextMonth = __typedjs(function  ()
                          {
                            OnCommand(gddCmdToolbarForward);
                          },
                          undefined,
                          "nextMonth",
                          "gadgets/GoogleCalendar_cc/plugin.js",
                          68);
var OnCommand = __typedjs(function  (cmd)
                          {
                            if (cmd == gddCmdToolbarBack || cmd == gddCmdToolbarForward)
                            {
                              curMonth += (cmd == gddCmdToolbarForward) ? 1 : - 1;
                              curYear += (curMonth < 0) ? - 1 : (curMonth >= 12 ? 1 : 0);
                              curMonth = (curMonth < 0) ? 11 : (curMonth % 12);
                              loadEventsForCurrentDates();
                              drawCalendar();
                            };
                          },
                          undefined,
                          "OnCommand",
                          "gadgets/GoogleCalendar_cc/plugin.js",
                          69);
var getWindowHeight = __typedjs(function  ()
                                {
                                  var height = pluginHelper.window_height;
                                  if (height < 10)
                                  {
                                    height = 100;
                                  };
                                  return height;
                                },
                                undefined,
                                "getWindowHeight",
                                "gadgets/GoogleCalendar_cc/plugin.js",
                                70);
var getWindowWidth = __typedjs(function  ()
                               {
                                 var width = pluginHelper.window_width;
                                 if (width < 10)
                                 {
                                   width = 100;
                                 };
                                 return width;
                               },
                               undefined,
                               "getWindowWidth",
                               "gadgets/GoogleCalendar_cc/plugin.js",
                               71);
var drawCalendar = __typedjs(function  ()
                             {
                               contentArea.RemoveAllContentItems();
                               PresizeWindow();
                               var fullWidth = getWindowWidth();
                               var itemWidth = fullWidth / (7 + (showWeekNum ? 1 : 0));
                               var x = 0,y = 0;
                               var eventStore = getEventStore();
                               if (! hideMonthView)
                               {
                                 AddItem(MSG_PREV_DATE,
                                         x,
                                         y,
                                         itemWidth,
                                         itemHeight,
                                         DrawHeader,
                                         prevMonth);
                                 monthYearItem = AddItem(MONTHS_LONG[curMonth] + " " + curYear,
                                                         x + itemWidth,
                                                         y,
                                                         fullWidth - 2 * itemWidth,
                                                         itemHeight,
                                                         DrawHeader,
                                                         goToGoogle,
                                                         kGoogleCalendarUrl);
                                 AddItem(MSG_NEXT_DATE,
                                         fullWidth - itemWidth,
                                         y,
                                         itemWidth,
                                         itemHeight,
                                         DrawHeader,
                                         nextMonth);
                                 y += itemHeight;
                                 if (showWeekNum)
                                 {
                                   x = itemWidth;
                                 };
                                 for (var i = 0; i < 7; ++i)
                                 {
                                   var drawFunc = drawLongShort(DOW_SHORT[(i + europeanDate) % 7],
                                                                DOW_TINY[(i + europeanDate) % 7],
                                                                gddFontNormal);
                                   AddStaticItem(DOW_SHORT[(i + europeanDate) % 7],
                                                 x + itemWidth * i,
                                                 y,
                                                 itemWidth,
                                                 itemHeight,
                                                 drawFunc);
                                 };
                                 AddStaticItem("",0,0,fullWidth,itemHeight * 2,DrawHeaderBkgndMain);
                                 y += itemHeight;
                                 var cal = __new(Date,[]);
                                 var actualDate = cal.getDate();
                                 var actualMonth = cal.getMonth();
                                 cal = __new(Date,[curYear,curMonth,1]);
                                 var dow = (cal.getDay() + 7 - europeanDate) % 7;
                                 x = 0;
                                 cal.setDate(1 - dow);
                                 for (var i = 0; i < 42; ++i)
                                 {
                                   var drawDate = cal.getDate();
                                   var drawMonth = cal.getMonth();
                                   var drawYear = cal.getFullYear();
                                   var events = getEventsForDate(cal,eventStore);
                                   var anyEvents = events.length > 0;
                                   var toolTip = "";
                                   if (anyEvents)
                                   {
                                     toolTip = NiceDate(cal,true,false) + "\n";
                                     for (var j = 0; j < events.length; j++)
                                     {
                                       var e = events[j];
                                       toolTip += "\n" + e.title + "\n" + e.getNiceTime() + "\n";
                                     };
                                   };
                                   if (i > 0 && i % 7 == 0)
                                   {
                                     if (cal.getFullYear() > curYear || cal.getMonth() > curMonth)
                                     break;
                                     x = 0;
                                     dow = 0;
                                     y += itemHeight;
                                   };
                                   if (showWeekNum && x == 0)
                                   {
                                     var weekNum = getWeek(drawYear,drawMonth,drawDate);
                                     AddStaticItem(weekNum,x,y,itemWidth,itemHeight,DrawItemCal);
                                     x += itemWidth;
                                   };
                                   AddItem(drawDate,
                                           x,
                                           y,
                                           itemWidth,
                                           itemHeight,
                                           (actualDate == drawDate && actualMonth == drawMonth) ? DrawItemCalWhite : (anyEvents) ? DrawItemCalBold : DrawItemCal,
                                           genSelectDate(drawYear,drawMonth,drawDate),
                                           createOnDateUrl(cal),
                                           toolTip);
                                   if (curDate == drawDate && curMonth == drawMonth)
                                   AddStaticItem("",
                                                 x - 2,
                                                 y - 2,
                                                 itemWidth + 5,
                                                 itemHeight + 5,
                                                 DrawSelectedDayBkgnd);
                                   if (actualDate == drawDate && actualMonth == drawMonth)
                                   AddStaticItem("",
                                                 x - 2,
                                                 y - 2,
                                                 itemWidth + 5,
                                                 itemHeight + 5,
                                                 DrawTodayBkgnd)
                                   else if (i % 7 == (europeanDate ? 5 : 0) || i % 7 == 6)
                                        {
                                          AddStaticItem("",
                                                        x - 2,
                                                        y - 2,
                                                        itemWidth + 5,
                                                        itemHeight + 5,
                                                        DrawWeekendBkgnd);
                                        };
                                   dow++;
                                   x += itemWidth;
                                   cal.setDate(drawDate + 1);
                                 };
                                 if (showWeekNum)
                                 {
                                   AddStaticItem("",
                                                 0,
                                                 itemHeight * 2,
                                                 itemWidth,
                                                 y - itemHeight,
                                                 DrawHeaderBkgndMain);
                                 };
                                 y += itemHeight + 4;
                               };
                               x = 0;
                               var arrowConst = 16;
                               if (! getPW())
                               {
                                 var lines = Math.ceil(500 / fullWidth);
                                 AddStaticItem(MSG_NO_ACCOUNT,
                                               x,
                                               y + 2,
                                               fullWidth,
                                               itemHeight * lines,
                                               DrawItemWrap);
                                 y += itemHeight * lines;
                                 AddItem(MSG_NO_ACCOUNT_SIGNUP,
                                         x,
                                         y + 2,
                                         fullWidth,
                                         itemHeight,
                                         DrawItemLink,
                                         goToGoogle);
                                 y += itemHeight;
                                 AddStaticItem(MSG_NO_ACCOUNT_LOGIN,
                                               x,
                                               y + 2,
                                               fullWidth,
                                               itemHeight * 3,
                                               DrawItemWrap,
                                               goToGoogle);
                                 UpdateTimeMonthYear();
                                 return;
                               };
                               var currentDate = __new(Date,[curYear,curMonth,curDate]);
                               var displaycontentItems = getEventsForDate(currentDate,eventStore);
                               AddItem(MSG_PREV_DATE,
                                       x,
                                       y + 2,
                                       arrowConst,
                                       itemHeight,
                                       DrawItemCalBold,
                                       gotoPrevDate);
                               var numItemsShown = 0;
                               if (displaycontentItems.length == 0)
                               {
                                 var dateText = NiceDate(currentDate,true,false);
                                 var drawFunc = drawLongShort(MSG_NO_EVENTS_ON + " " + dateText,
                                                              dateText,
                                                              gddFontBold);
                                 AddItem("",
                                         x + arrowConst,
                                         y + 2,
                                         fullWidth - 2 * arrowConst,
                                         itemHeight,
                                         drawFunc,
                                         genDetails(createOnDateUrl(currentDate)),
                                         null,
                                         MSG_CREATE);
                                 AddItem(MSG_NEXT_DATE,
                                         fullWidth - arrowConst,
                                         y + 2,
                                         arrowConst,
                                         itemHeight,
                                         DrawItemCalBold,
                                         gotoNextDate);
                                 y += itemHeight;
                               }
                               else {
                                      var dateText = NiceDate(currentDate,true,false);
                                      var drawFunc = drawLongShort(MSG_EVENTS_ON + " " + dateText,
                                                                   dateText,
                                                                   gddFontBold);
                                      AddItem(dateText,
                                              x + arrowConst,
                                              y + 2,
                                              fullWidth - arrowConst * 2,
                                              itemHeight,
                                              drawFunc,
                                              genDetails(createOnDateUrl(currentDate)),
                                              null,
                                              MSG_CREATE);
                                      AddItem(MSG_NEXT_DATE,
                                              fullWidth - arrowConst,
                                              y + 2,
                                              arrowConst,
                                              itemHeight,
                                              DrawItemCalBold,
                                              gotoNextDate);
                                      y += itemHeight + 4;
                                      numItemsShown = displaycontentItems.length;
                                      for (var i = 0; i < displaycontentItems.length; ++i)
                                      {
                                        var item = displaycontentItems[i].getItem();
                                        item.SetRect(x,y,fullWidth,itemHeight);
                                        contentArea.AddContentItem(item,gddItemDisplayInSidebar);
                                        y += itemHeight;
                                      };
                                    };
                               y += 4;
                               AddItem(MSG_CREATE,
                                       x,
                                       y + 2,
                                       fullWidth,
                                       itemHeight,
                                       DrawItemLink,
                                       quickAddPrompt);
                               y += itemHeight;
                               var isToday = dateIsToday(currentDate);
                               if (! isToday)
                               {
                                 AddItem(MSG_TODAY,
                                         x,
                                         y + 2,
                                         fullWidth,
                                         itemHeight,
                                         DrawItemLink,
                                         gotoToday);
                                 y += itemHeight;
                               };
                               AddItem(MSG_VIEW_CALENDAR,
                                       x,
                                       y + 2,
                                       fullWidth,
                                       itemHeight,
                                       DrawItemLink,
                                       goToGoogle);
                               y += itemHeight;
                               if (isToday && numItemsShown < 2)
                               {
                                 AddStaticItem("",x,y + 2,fullWidth,itemHeight);
                               };
                               UpdateTimeMonthYear();
                             },
                             undefined,
                             "drawCalendar",
                             "gadgets/GoogleCalendar_cc/plugin.js",
                             72);
var getQuickAddString = __typedjs(function  (text)
                                  {
                                    return "<atom:entry xmlns:atom=\'http://www.w3.org/2005/Atom\'>" + "<atom:category scheme=\'http://schemas.google.com/g/2005#kind\' " + "term=\'http://schemas.google.com/g/2005#event\'></atom:category>" + "<atom:content type=\'text\'>" + HtmlEscape(text) + "</atom:content>" + "<gCal:quickadd xmlns:gCal=\'http://schemas.google.com/gCal/2005\' " + "value=\'true\'>" + "</gCal:quickadd></atom:entry>";
                                  },
                                  undefined,
                                  "getQuickAddString",
                                  "gadgets/GoogleCalendar_cc/plugin.js",
                                  73);
var genCreateEventReceive = __typedjs(function  (xmlReq,text)
                                      {
                                        return __typedjs(function  ()
                                                         {
                                                           return onCreateEventReceive(xmlReq,text);
                                                         },
                                                         arguments.callee,
                                                         "",
                                                         "gadgets/GoogleCalendar_cc/plugin.js",
                                                         0);
                                      },
                                      undefined,
                                      "genCreateEventReceive",
                                      "gadgets/GoogleCalendar_cc/plugin.js",
                                      74);
var onCreateEventReceive = __typedjs(function  (xmlReq,text)
                                     {
                                       if (! xmlReq || ! (xmlReq.readyState == 4))
                                       {
                                         return;
                                       };
                                       if (xmlReq.status != 201)
                                       {
                                         showQuickAdd(text);
                                         return;
                                       };
                                       var doc = __new(DOMDocument,[]);
                                       var responseText = xmlReq.responseText;
                                       doc.loadXML(responseText);
                                       var elem = doc.getElementsByTagName("entry");
                                       if (elem != null && elem.length > 0)
                                       {
                                         var events = [];
                                         genCalEvents(elem[0],events,"");
                                         var text = MSG_EVENT_ADDED + " (" + events[0].title + ")\n" + events[0].getItem().snippet;
                                         utils.alert(text);
                                         forceReloadFromServer();
                                       };
                                     },
                                     undefined,
                                     "onCreateEventReceive",
                                     "gadgets/GoogleCalendar_cc/plugin.js",
                                     75);
var showQuickAdd = __typedjs(function  (text)
                             {
                               showPage(kCalendarEventUrl + "?action=TEMPLATE&ctext=" + encodeUrl(text));
                             },
                             undefined,
                             "showQuickAdd",
                             "gadgets/GoogleCalendar_cc/plugin.js",
                             76);
var promptShown = false;
var quickAddPrompt = __typedjs(function  ()
                               {
                                 if (promptShown)
                                 {
                                   return;
                                 };
                                 promptShown = true;
                                 var text = utils.prompt(MSG_QUICKADD,"");
                                 promptShown = false;
                                 if (text)
                                 {
                                   var xmlReq = __new(XMLHttpRequest,[]);
                                   xmlReq.open("POST",kCalendarCreateEventUrl,true);
                                   xmlReq.setRequestHeader("Content-Type",
                                                           "application/atom+xml; charset=UTF-8");
                                   xmlReq.setRequestHeader("Authorization",
                                                           "GoogleLogin auth=" + authToken);
                                   xmlReq.setRequestHeader("Referer",
                                                           "http://gds.calendar." + kClientLoginAppName);
                                   var data = getQuickAddString(text);
                                   xmlReq.onreadystatechange = genCreateEventReceive(xmlReq,text);
                                   try
                                   {
                                     xmlReq.send(data);
                                   }
                                   catch (e) {
                                             };
                                 };
                               },
                               undefined,
                               "quickAddPrompt",
                               "gadgets/GoogleCalendar_cc/plugin.js",
                               77);
var loginAndGotoURL = __typedjs(function  (url)
                                {
                                  if (getEmail().indexOf("@gmail.com") == - 1)
                                  {
                                    showPage(url);
                                    return;
                                  };
                                  var xmlReq = __new(XMLHttpRequest,[]);
                                  xmlReq.open("POST",kAccountsUrl + "/IssueAuthToken ",true);
                                  xmlReq.setRequestHeader("Content-Type",
                                                          "application/x-www-form-urlencoded; charset=UTF-8");
                                  var data = "SID=" + sid + "&LSID=" + lsid + "&service=gaia&Session=false";
                                  xmlReq.onreadystatechange = bind2(loginAndGotoURLReceive,
                                                                    xmlReq,
                                                                    url);
                                  try
                                  {
                                    xmlReq.send(data);
                                  }
                                  catch (e) {
                                            };
                                },
                                undefined,
                                "loginAndGotoURL",
                                "gadgets/GoogleCalendar_cc/plugin.js",
                                78);
var loginAndGotoURLReceive = __typedjs(function  (xmlReq,url)
                                       {
                                         if (! xmlReq || ! (xmlReq.readyState == 4))
                                         {
                                           return;
                                         };
                                         var newUrl = kAccountsUrl + "/TokenAuth?auth=" + xmlReq.responseText + "&service=cl&source=GDSCAL&continue=" + encodeUrl(url);
                                         showPage(newUrl);
                                       },
                                       undefined,
                                       "loginAndGotoURLReceive",
                                       "gadgets/GoogleCalendar_cc/plugin.js",
                                       79);
var bind2 = __typedjs(function  (func,arg1,arg2)
                      {
                        return __typedjs(function  ()
                                         {
                                           func(arg1,arg2);
                                         },
                                         arguments.callee,
                                         "",
                                         "gadgets/GoogleCalendar_cc/plugin.js",
                                         0);
                      },
                      undefined,
                      "bind2",
                      "gadgets/GoogleCalendar_cc/plugin.js",
                      80);
var getWeek = __typedjs(function  (year,month,day)
                        {
                          var when = __new(Date,[year,month,day]);
                          var newYear = __new(Date,[year,0,1]);
                          var modDay = newYear.getDay();
                          if (modDay == 0)
                          modDay = 6
                          else modDay--;
                          var daynum = ((Date.UTC(year,
                                                  when.getMonth(),
                                                  when.getDate(),
                                                  0,
                                                  0,
                                                  0) - Date.UTC(year,
                                                                0,
                                                                1,
                                                                0,
                                                                0,
                                                                0)) / 1000 / 60 / 60 / 24) + 1;
                          if (modDay < 4)
                          {
                            return Math.floor((daynum + modDay - 1) / 7) + 1;
                          }
                          else {
                                 var weeknum = Math.floor((daynum + modDay - 1) / 7);
                                 if (weeknum == 0)
                                 {
                                   year--;
                                   var prevNewYear = __new(Date,[year,0,1]);
                                   var prevmodDay = prevNewYear.getDay();
                                   if (prevmodDay == 0)
                                   prevmodDay = 6
                                   else prevmodDay--;
                                   if (prevmodDay < 4)
                                   weeknum = 53
                                   else weeknum = 52;
                                 };
                                 return weeknum;
                               };
                        },
                        undefined,
                        "getWeek",
                        "gadgets/GoogleCalendar_cc/plugin.js",
                        81);
var drawLongShort = __typedjs(function  (longText,
                                         shortText,
                                         fontType)
                              {
                                return __typedjs(function  (item,target,graphics,x,y,width,height)
                                                 {
                                                   var textWidth = graphics.GetTextWidth(longText,
                                                                                         0,
                                                                                         fontType);
                                                   var text = longText;
                                                   if (textWidth >= width)
                                                   {
                                                     text = shortText;
                                                   };
                                                   graphics.DrawText(x,
                                                                     y,
                                                                     width,
                                                                     height,
                                                                     text,
                                                                     gddColorNormalText,
                                                                     gddTextFlagCenter + gddTextFlagSingleLine + gddTextFlagVCenter,
                                                                     fontType);
                                                 },
                                                 arguments.callee,
                                                 "",
                                                 "gadgets/GoogleCalendar_cc/plugin.js",
                                                 0);
                              },
                              undefined,
                              "drawLongShort",
                              "gadgets/GoogleCalendar_cc/plugin.js",
                              82);
var selectDate = __typedjs(function  (year,month,day)
                           {
                             curYear = year;
                             var oldMonth = curMonth;
                             curMonth = month;
                             if (curMonth != oldMonth)
                             {
                               loadEventsForCurrentDates();
                             };
                             curDate = day;
                             drawCalendar();
                           },
                           undefined,
                           "selectDate",
                           "gadgets/GoogleCalendar_cc/plugin.js",
                           83);
var genSelectDate = __typedjs(function  (year,month,day)
                              {
                                return __typedjs(function  ()
                                                 {
                                                   selectDate(year,month,day);
                                                 },
                                                 arguments.callee,
                                                 "",
                                                 "gadgets/GoogleCalendar_cc/plugin.js",
                                                 0);
                              },
                              undefined,
                              "genSelectDate",
                              "gadgets/GoogleCalendar_cc/plugin.js",
                              84);
var AddStaticItem = __typedjs(function  (str,
                                         x,
                                         y,
                                         width,
                                         height,
                                         drawHandler)
                              {
                                var item = __new(ContentItem,[]);
                                item.heading = str;
                                item.flags = gddContentItemFlagStatic | gddContentItemFlagNoRemove;
                                item.SetRect(x,y,width,height);
                                if (drawHandler)
                                item.onDrawItem = drawHandler;
                                contentArea.AddContentItem(item,gddItemDisplayInSidebar);
                                return item;
                              },
                              undefined,
                              "AddStaticItem",
                              "gadgets/GoogleCalendar_cc/plugin.js",
                              85);
var gotoToday = __typedjs(function  ()
                          {
                            var d = __new(Date,[]);
                            selectDate(d.getFullYear(),d.getMonth(),d.getDate());
                          },
                          undefined,
                          "gotoToday",
                          "gadgets/GoogleCalendar_cc/plugin.js",
                          86);
var gotoPrevDate = __typedjs(function  ()
                             {
                               var d = __new(Date,[curYear,curMonth,curDate - 1]);
                               selectDate(d.getFullYear(),d.getMonth(),d.getDate());
                             },
                             undefined,
                             "gotoPrevDate",
                             "gadgets/GoogleCalendar_cc/plugin.js",
                             87);
var gotoNextDate = __typedjs(function  ()
                             {
                               var d = __new(Date,[curYear,curMonth,curDate + 1]);
                               selectDate(d.getFullYear(),d.getMonth(),d.getDate());
                             },
                             undefined,
                             "gotoNextDate",
                             "gadgets/GoogleCalendar_cc/plugin.js",
                             88);
var AddItem = __typedjs(function  (str,
                                   x,
                                   y,
                                   width,
                                   height,
                                   drawHandler,
                                   detailsHandler,
                                   url,
                                   tooltip)
                        {
                          var item = __new(ContentItem,[]);
                          item.heading = str;
                          item.snippet = str;
                          item.tooltip = tooltip ? tooltip : str;
                          item.flags = gddContentItemFlagNoRemove;
                          item.SetRect(x,y,width,height);
                          if (drawHandler)
                          item.onDrawItem = drawHandler;
                          if (detailsHandler)
                          item.onDetailsView = detailsHandler;
                          if (url)
                          {
                            item.onOpenItem = genDetails(url);
                          };
                          contentArea.AddContentItem(item,gddItemDisplayInSidebar);
                          return item;
                        },
                        undefined,
                        "AddItem",
                        "gadgets/GoogleCalendar_cc/plugin.js",
                        89);
var DrawHeader = __typedjs(function  (item,
                                      target,
                                      graphics,
                                      x,
                                      y,
                                      width,
                                      height)
                           {
                             graphics.DrawText(x,
                                               y,
                                               width,
                                               height,
                                               item.heading,
                                               gddColorNormalText,
                                               gddTextFlagSingleLine | gddTextFlagVCenter | gddTextFlagCenter,
                                               gddFontNormal);
                           },
                           undefined,
                           "DrawHeader",
                           "gadgets/GoogleCalendar_cc/plugin.js",
                           90);
var DrawHeaderBkgndMain = __typedjs(function  (item,
                                               target,
                                               graphics,
                                               x,
                                               y,
                                               width,
                                               height)
                                    {
                                      var curItemHeight = graphics.GetTextHeight("Z",
                                                                                 1000,
                                                                                 0,
                                                                                 gddFontNormal) + 5;
                                      if (curItemHeight != itemHeight)
                                      {
                                        itemHeight = curItemHeight;
                                        drawCalendar();
                                        return;
                                      };
                                      graphics.DrawRect(x - 2,
                                                        y - 2,
                                                        width + 6,
                                                        height + 5,
                                                        "#c3d9ff",
                                                        "#c3d9ff");
                                    },
                                    undefined,
                                    "DrawHeaderBkgndMain",
                                    "gadgets/GoogleCalendar_cc/plugin.js",
                                    91);
var DrawHeaderBkgnd = __typedjs(function  (item,
                                           target,
                                           graphics,
                                           x,
                                           y,
                                           width,
                                           height)
                                {
                                  graphics.DrawRect(x,y,width,height,"#c3d9ff","#c3d9ff");
                                  graphics.DrawText(x,
                                                    y,
                                                    width,
                                                    height,
                                                    item.heading,
                                                    gddColorNormalText,
                                                    gddTextFlagSingleLine | gddTextFlagVCenter | gddTextFlagCenter,
                                                    gddFontNormal);
                                },
                                undefined,
                                "DrawHeaderBkgnd",
                                "gadgets/GoogleCalendar_cc/plugin.js",
                                92);
var DrawTodayBkgnd = __typedjs(function  (item,
                                          target,
                                          graphics,
                                          x,
                                          y,
                                          width,
                                          height)
                               {
                                 graphics.DrawRect(x,y,width,height,"#557799","#557799");
                               },
                               undefined,
                               "DrawTodayBkgnd",
                               "gadgets/GoogleCalendar_cc/plugin.js",
                               93);
var DrawWeekendBkgnd = __typedjs(function  (item,
                                            target,
                                            graphics,
                                            x,
                                            y,
                                            width,
                                            height)
                                 {
                                   graphics.DrawRect(x,y,width,height,"#E8EEF7","#E8EEF7");
                                 },
                                 undefined,
                                 "DrawWeekendBkgnd",
                                 "gadgets/GoogleCalendar_cc/plugin.js",
                                 94);
var DrawSelectedDayBkgnd = __typedjs(function  (item,
                                                target,
                                                graphics,
                                                x,
                                                y,
                                                width,
                                                height)
                                     {
                                       graphics.DrawRect(x,y,width,height,"#808080",undefined);
                                     },
                                     undefined,
                                     "DrawSelectedDayBkgnd",
                                     "gadgets/GoogleCalendar_cc/plugin.js",
                                     95);
var DrawItemCalWhite = __typedjs(function  (item,
                                            target,
                                            graphics,
                                            x,
                                            y,
                                            width,
                                            height)
                                 {
                                   graphics.DrawText(x,
                                                     y,
                                                     width + 3,
                                                     height,
                                                     item.heading,
                                                     "#FFFFFF",
                                                     gddTextFlagCenter | gddTextFlagSingleLine | gddTextFlagVCenter,
                                                     gddFontNormal);
                                 },
                                 undefined,
                                 "DrawItemCalWhite",
                                 "gadgets/GoogleCalendar_cc/plugin.js",
                                 96);
var DrawItemCalBold = __typedjs(function  (item,
                                           target,
                                           graphics,
                                           x,
                                           y,
                                           width,
                                           height)
                                {
                                  graphics.DrawText(x,
                                                    y,
                                                    width + 3,
                                                    height,
                                                    item.heading,
                                                    gddColorNormalText,
                                                    gddTextFlagCenter | gddTextFlagSingleLine | gddTextFlagVCenter,
                                                    gddFontBold);
                                },
                                undefined,
                                "DrawItemCalBold",
                                "gadgets/GoogleCalendar_cc/plugin.js",
                                97);
var DrawItemCal = __typedjs(function  (item,
                                       target,
                                       graphics,
                                       x,
                                       y,
                                       width,
                                       height)
                            {
                              graphics.DrawText(x,
                                                y,
                                                width + 3,
                                                height,
                                                item.heading,
                                                gddColorNormalText,
                                                gddTextFlagCenter | gddTextFlagSingleLine | gddTextFlagVCenter,
                                                gddFontNormal);
                            },
                            undefined,
                            "DrawItemCal",
                            "gadgets/GoogleCalendar_cc/plugin.js",
                            98);
var DrawItemLink = __typedjs(function  (item,
                                        target,
                                        graphics,
                                        x,
                                        y,
                                        width,
                                        height)
                             {
                               graphics.DrawText(x,
                                                 y,
                                                 width,
                                                 height,
                                                 item.heading,
                                                 "#0000CC",
                                                 gddTextFlagSingleLine,
                                                 gddFontNormal);
                               var textWidth = graphics.GetTextWidth(item.heading,
                                                                     0,
                                                                     gddFontNormal);
                               graphics.DrawLine(x,y + height,x + textWidth,y + height,"#0000CC");
                             },
                             undefined,
                             "DrawItemLink",
                             "gadgets/GoogleCalendar_cc/plugin.js",
                             99);
var DrawItemWrap = __typedjs(function  (item,
                                        target,
                                        graphics,
                                        x,
                                        y,
                                        width,
                                        height)
                             {
                               graphics.DrawText(x,
                                                 y,
                                                 width,
                                                 height,
                                                 item.heading,
                                                 gddColorNormalText,
                                                 gddTextFlagWordBreak,
                                                 gddFontNormal);
                             },
                             undefined,
                             "DrawItemWrap",
                             "gadgets/GoogleCalendar_cc/plugin.js",
                             100);
