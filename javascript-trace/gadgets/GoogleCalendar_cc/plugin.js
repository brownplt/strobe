/*
 *  Google Calendar Sidebar plugin for Google Desktop
 *  Copyright (c) 2006, Google
 *  Copyright (c) 2006, Manas Tungare
 *  Copyright (c) 2006, Josh Stewart
 */
 
// CONSTANTS
var MONTHS_LONG = [MONTH_LONG_0, MONTH_LONG_1, MONTH_LONG_2, MONTH_LONG_3,
                   MONTH_LONG_4, MONTH_LONG_5, MONTH_LONG_6, MONTH_LONG_7,
                   MONTH_LONG_8, MONTH_LONG_9, MONTH_LONG_10, MONTH_LONG_11];
var MONTHS_SHORT = [MONTH_SHORT_0, MONTH_SHORT_1, MONTH_SHORT_2, MONTH_SHORT_3,
                   MONTH_SHORT_4, MONTH_SHORT_5, MONTH_SHORT_6, MONTH_SHORT_7,
                   MONTH_SHORT_8, MONTH_SHORT_9, MONTH_SHORT_10, MONTH_SHORT_11];
var DOW = [DOW_0, DOW_1, DOW_2, DOW_3, DOW_4, DOW_5, DOW_6];
var DOW_SHORT = DOW_SHORT_STR.split(',');
var DOW_TINY = DOW_TINY_STR.split(',');

var itemHeight = 16;
var europeanDate = options.GetValue("european") == "true" ? 1 : 0;
var format24 = options.GetValue("24") == "true";
var hideMonthView = options.GetValue("hidemonth") == "true";
var showWeekNum = options.GetValue("showweek") == "true";

var kCurrentVersion = "2.0.0.0";
var kClientLoginAppName = "GCAL-" + kCurrentVersion;

var kProductHomePageUri = "http://www.google.com/googlecalendar/gd_help_0_9_8.html";

var kCalendarUrl = 'http://www.google.com/calendar';
var kAutomatedCalendarUrl = 'http://desktop1.google.com/calendar';
var kAccountsUrl = 'https://www.google.com/accounts';

var kGoogleCalendarUrl = kCalendarUrl + '/render';
var kCalendarUserFeedUrl = kAutomatedCalendarUrl + '/feeds/default';
var kCalendarEventUrl = kCalendarUrl + '/event';
var kCalendarCreateEventUrl = kCalendarUrl + '/feeds/default/private/full';
var kGoogleClientLoginUrl = kAccountsUrl + '/ClientLogin';

var kUpdateIntervalMs = 1000 * 60 * 10; // refresh every 30 minutes

// Preload a few images for use later
var image_rep = {};
image_rep[false] = framework.graphics.LoadImage("repeat_dark.gif");
image_rep[true] = framework.graphics.LoadImage("repeat_white.gif");

var image_alarm = {};
image_alarm[false] = framework.graphics.LoadImage("alarm_dark.gif");
image_alarm[true] = framework.graphics.LoadImage("alarm_white.gif");

var image_people = {};
image_people[false] = framework.graphics.LoadImage("people_dark.gif");
image_people[true] = framework.graphics.LoadImage("people_white.gif");

// globals...
var authReq = null;
var authToken = "";
var lsid = "";
var sid = "";
var numAuthTriesSinceLastSuccess = 0;
var minimized = false;

// Commands relevant to calendars
// ------------------------------
// drawCalendar                 - make the calendar redraw based on cached data [or no data at all]
// loadEventsForCurrentDates    - load events for a specific range of dates
// forceReloadFromServer        - shorthand for LoadEventsForCurrentDates(true)
// loadEventsFromServer(expandFrom, expandTo, force, user) - force an event to load

// Add an an invisible item to the window to force it to be a specific height
function PresizeWindow() {
  if (PresizeWindow.count > 0) {
    var lines = 15;
    AddStaticItem("", 0, 0, 0, itemHeight * lines);
    PresizeWindow.count--;
  }
}

function stateChange(newState) {
  minimized = gddTileDisplayStateMinimized == newState;
  UpdateTimeMonthYear();
  drawCalendar();
}

PresizeWindow.count = 6;

function Main() {
  PresizeWindow();
  SetDefaultOptions();
  pluginHelper.SetFlags(gddPluginFlagToolbarBack | gddPluginFlagToolbarForward,
    gddContentFlagHaveDetails | gddContentFlagManualLayout);
  pluginHelper.about_text = MSG_ABOUT;

  // since we have more items than default
  pluginHelper.max_content_items = 1000;
  pluginHelper.onCommand = OnCommand;
  pluginHelper.onDisplayStateChange = stateChange;

  //pluginHelper.onAddCustomMenuItems = AddCustomMenuItems; //: for tracer
  pluginHelper.onShowOptionsDlg = ShowOptionsDlg;

  // Every so often load more events from the server
  view.setInterval(timerRefresh, kUpdateIntervalMs);

  // timer every 5 seconds
  view.setInterval(OnTimer, 5000);
  drawCalendar();
  getUserInfo();  
}

///////////////////////////////////////////////////////////////////////////////
// THE DATE IN THE UPPER LEFT
var curMonth = new Date().getMonth();
var curYear = new Date().getFullYear();
var curDate = new Date().getDate();
var prevDate;

function OnTimer() {
  var cal = new Date();
  UpdateTimeMonthYear();
  if (prevDate != cal.getDate()) {  // date changed?
    prevDate = cal.getDate();
    curDate = prevDate;
    curMonth = cal.getMonth();
    curYear = cal.getFullYear();
    drawCalendar();
  }
  
  checkEventsForExpiry();
}

var monthYearItem = null;
function UpdateTimeMonthYear() {
  var cal = new Date();
  var hour = cal.getHours();
  var min = cal.getMinutes();

  var time;
  if (format24) {
    time = hour + ":" + ((min < 10) ? "0" + min : min);
  } else {
    hour %= 12;
    time = ((hour == 0) ? 12 : hour) + ":" +
           ((min < 10) ? "0" + min : min) + " " +
           ((cal.getHours() >= 12) ? MSG_PM : MSG_AM);
  }

  if (minimized) {
    time += ' ' + MONTHS_SHORT[curMonth] + ' ' + curDate + ', ' + curYear;
  }
  view.caption = time;
  var monthYear = MONTHS_LONG[curMonth] + " " + curYear;
  if (monthYearItem && monthYearItem.heading != monthYear)   // optimize redraws
    monthYearItem.heading = monthYear;
}

///////////////////////////////////////////////////////////////////////////////

function SetDefaultOptions() {
  // Odd values may be in the options. Clear them out accordingly
  if (getEmail() == null) {
    options.PutValue("Email", "");
  }
  if (getPW() == null) {
    options.PutValue("Passwd", "");
  }
  if (options.GetValue("24") == "") {
    var d1 = new Date(1970, 12, 29, 13, 0, 0, 0);
    var localTime = d1.toLocaleTimeString();
    format24 = localTime.indexOf("13") >= 0 ? true : false;
    options.PutValue("24", format24 ? "true" : "false");
  }
  if (options.GetValue("european") == "") {
    options.PutValue("european", "false");
  }
  if (options.GetValue("hidemonth") == "") {
    options.PutValue("hidemonth", "false");
  }
  if (options.GetValue("showweek") == "") {
    options.PutValue("showweek", "false");
  }
}

function AddCustomMenuItems(menu) {
  menu.AddItem(MSG_MENU_REFESH_MENU, 0, function() {getUserInfo()});
  menu.AddItem(MSG_MENU_GOTO_CALENDAR, 0, goToGoogle);
}

function goToGoogle() {
  showPage(kGoogleCalendarUrl);
}

function DoAuth() {
  var email = getEmail();
  var pw = getPW();
  if (email == "" && pw == "") {
    showErrorItem(MSG_GETTING_STARTED_1, enterUsernamePasswordText, gettingStartedSnippet, null);
    return;
  }

  authToken = "";

  // To avoid accidentally DDOSing the calendar server, we hiccup after 10
  // unsuccessful logins
  if (++numAuthTriesSinceLastSuccess == 10) {
    showErrorItem(genericProblemText, errorUnknownText, errorUnknownSnippet, null);
    numAuthTriesSinceLastSuccess = 0;
    return;
  }
  authReq = new XMLHttpRequest();
  authReq.open("POST", kGoogleClientLoginUrl, true);
  authReq.setRequestHeader('Content-Type', 'application/x-www-form-urlencoded; charset=UTF-8');

  var data = "Email=" + encodeUrl(email) +
    "&Passwd=" + encodeUrl(pw) +
    "&source=" + encodeUrl(kClientLoginAppName) +
    "&service=cl&accountType=hosted_or_google";

  authReq.onreadystatechange = onAuthResponse;

  try {
    authReq.send(data);
  } catch(e) {
    // Just in case
  }
}

function onAuthResponse() {
  if (!authReq || authReq.readyState != 4) {
    return;
  }

  if (authReq.status != 200) { // 200 = HTTP Request OK.
    if (authReq.status == 403) { // 403 = Auth Failed
      handleAuthError(authReq.responseText);
      return;
    }
  }

  numAuthTriesSinceLastSuccess = 0;
  var tokens = authReq.responseText.split('\n');
  for (var t = 0; t < tokens.length; t++) {
    name = tokens[t].substring(0, tokens[t].indexOf('='));
    val = tokens[t].substring(tokens[t].indexOf('=') + 1);

    if (name.toLowerCase() == "auth") {
      authToken = val;
    } else if (name.toLowerCase() == "sid") {
      sid = val;
    } else if (name.toLowerCase() == "lsid") {
      lsid = val;
    }
  }

  if (authToken != "") {
    getUserInfo();
  }
}

function handleAuthError(response) {
  // Check the error we received, if it's a CaptchaRequired, then send user to Google.
  // Else just show an error item and shut up.
  var error, url;

  var tokens = response.split('\n');
  for (var t = 0; t < tokens.length; t++) {
    name = tokens[t].substring(0, tokens[t].indexOf('='));
    val = tokens[t].substring(tokens[t].indexOf('=') + 1);

    if (name.toLowerCase() == "error") {
      error = val;
    } else if (name.toLowerCase() == "url") {
      url = val;
    }
  }

  if (error == "BadAuthentication") {
    showErrorItem(genericProblemText, errorBadAuthText, errorBadAuthSnippet, null);

    // Wipe out the authentication
    authToken = "";
    options.PutValue("Email", "");
    options.PutValue("Passwd", "");    
  } else if (error == "NotVerified" ||
             error == "TermsNotAgreed" ||
             error == "Unknown" ||
             error == "AccountDeleted" ||
             error == "AccountDisabled") {
    showErrorItem(genericProblemText, errorUnknownText, errorUnknownSnippet, null);
  } else if (error == "CaptchaRequired") {
    showErrorItem(genericProblemText, errorCaptchaRequiredText,
      errorCaptchaRequiredSnippet, url);
  } else if (error == "ServiceUnavailable") {
    showErrorItem(genericProblemText, errorServiceUnavailableText,
      errorServiceUnavailableSnippet, null);
  }
}

// Count the number of items in an object
function countObject(o) {
  var count = 0;
  for(var i in o) {
    count++;
  }
  return count;
}

function timerRefresh() {
  if (authToken) {
    forceReloadFromServer();
  }
}

function forceReloadFromServer() {
  loadEventsForCurrentDates(true);
}

function loadEventsForCurrentDates(force) {
  // No AuthToken? look em up
  if (authToken == null || authToken == "") {
    DoAuth();
    return; // After auth succeeds, control will go here again
  }

  var startDate = new Date(curYear, curMonth, 1);
  var dow = (startDate.getDay() + europeanDate) % 7;
  var leftOver = (38 - dow);

  var startString =
    getDateIso8601(new Date(startDate.getTime() - dow * 86400000));
  var endString =
    getDateIso8601(new Date(startDate.getTime() + leftOver * 86400000));

  setEventStoreTime(startString, endString);

  for(var i = 0; i < userStore.length; ++i) {
    var user = userStore[i];
    if (getUserShow(user)) {
      loadEventsFromServer(startString, endString, force, user);
    }
  }
}

function getEmail() {
  return options.GetValue("Email");
}

function getPW() {
  return options.GetValue("Passwd");
}

// A useful cache mapping from the request to the array of events in the 
// response. Purged every time we do a timer-refresh or a forced ping
var cacheItems = {};

// Keep time of the last modified time for the cache entry, which allows
// us to be more efficient in loading events
var cacheUpdated = {};

function loadEventsFromServer(expandFrom, expandTo, force, user) {
  // If we have a cache available, then only refresh if we were forced to
  var cache = cacheItems[user.id + '/' + expandFrom + '/' + expandTo];
  if (cache && force !== true) {
    return;
  }

  var url = user.url + "?start-min=" + expandFrom + "&start-max=" + expandTo +
           "&max-results=400";
  if (user.session) {
    url += "&" + user.session;
  }

  // If we have a cache available. Then try to grab new events
  var updated = cacheUpdated[user.id + '/' + expandFrom + '/' + expandTo];
  if (updated) {
    url += '&updated-min=' + to3339String(updated);
  }
  
  var xmlReq = new XMLHttpRequest();
  xmlReq.open("GET", url, true);
  xmlReq.onreadystatechange = genReceive(xmlReq, expandFrom, expandTo, user);
  xmlReq.setRequestHeader('Content-Type','application/atom+xml; charset=UTF-8');
  xmlReq.setRequestHeader('Authorization', 'GoogleLogin auth=' + authToken);
  xmlReq.setRequestHeader('X-If-No-Redirect', '1');
  xmlReq.setRequestHeader('Referer', 'http://gds.calendar.' + kClientLoginAppName);

  // thwart the cache
  xmlReq.setRequestHeader("If-Modified-Since", "Sat, 1 Jan 2000 00:00:00 GMT");

  try {
    xmlReq.send();
  } catch(e) {
    // Just in case
  }
}

function genReceive(xmlReq, start, end, user) {
  return function() {
    return OnReceivedData(xmlReq, start, end, user);
  }
}

//-----------------------------------------------------------------------------
// FUNCTIONS RELATED TO DOWNLOADING USER INFO

function getUserInfo(opt_url) {
  // Credentials entered, but no AuthToken? DoAuth!
  if (authToken == null || authToken == "") {
    DoAuth();
    return; // After auth is done, control will be passed here again, if succeeded.
  }

  if (!opt_url) {
    opt_url = kCalendarUserFeedUrl;
  }

  var xmlReq = new XMLHttpRequest();
  xmlReq.open("GET", opt_url, true);
  xmlReq.onreadystatechange = genUserReceive(xmlReq);
  xmlReq.setRequestHeader('Content-Type','application/atom+xml; charset=UTF-8');
  xmlReq.setRequestHeader('Authorization', 'GoogleLogin auth=' + authToken);
  xmlReq.setRequestHeader('X-If-No-Redirect', '1');
  xmlReq.setRequestHeader('Referer', 'http://gds.calendar.' + kClientLoginAppName);
  
  // thwart the cache
  xmlReq.setRequestHeader("If-Modified-Since", "Sat, 1 Jan 2000 00:00:00 GMT");

  try {
    xmlReq.send();
  } catch(e) {
    // Just in case
  }
}

var userStore = [];

function UserItem(title, id, url, color, accessLevel, selected, timezone, 
                  hidden, updated, overrideName) {
  this.title = title;
  this.id = id;
  this.url = url;
  this.color = color;
  this.accessLevel = accessLevel;
  this.selected = selected;
  this.timezone = timezone;
  this.hidden = hidden;
  this.updated = updated;
  this.overrideName = overrideName;
}

function addUser(elem, users) {
  var title = "", id = "", url = "", email = "", color = "", accessLevel = "";
  var selected = true, hidden = false, timezone = "", updated = null;
  var overrideName = "";
      
  for (var node = elem.firstChild; node != null; node = node.nextSibling) {
    if (node.nodeName == "id") {
      if (node.firstChild) {
        id = node.firstChild.nodeValue;
      } else {
        id = MSG_NO_TITLE;
      }
    } else if (node.nodeName == "title") {
      if (node.firstChild) {
        title = node.firstChild.nodeValue;
      } else {
        title = MSG_NO_TITLE;
      }
    } else if (node.nodeName == "link" && 
               node.getAttribute("rel") == "alternate") {
      url = node.getAttribute("href");
    } else if (node.nodeName == "gCal:color") {
      color = node.getAttribute("value");
    } else if (node.nodeName == "gCal:accesslevel") {
      accessLevel = node.getAttribute("value");
    } else if (node.nodeName == "gCal:selected") {
      selected = (node.getAttribute("value") != "false");
    } else if (node.nodeName == "gCal:timezone") {
      timezone = node.getAttribute("value");
    } else if (node.nodeName == "gCal:hidden") {
      hidden = (node.getAttribute("value") != "false");
    } else if (node.nodeName == "updated" && node.firstChild) {
      updated = rfc3339StringToDate(node.firstChild.nodeValue);
    } else if (node.nodeName == "gCal:overridename") {
      overrideName = node.getAttribute("value");
    }

  }
  users.push(new UserItem(title, id, url, color, accessLevel, selected, 
    timezone, hidden, updated, overrideName));
}

function genUserReceive(xmlReq) {
  return function() {
    if (!xmlReq || !(xmlReq.readyState == 4)) { // completed OK?
      return;
    }
    if (xmlReq.status != 200) {
      if (xmlReq.status == 412) {
        // we've been redirected
        var location = xmlReq.getResponseHeader("X-Redirect-Location");
        getUserInfo(location);
      } else if (xmlReq.status == 401) {
        authToken = ""; // force authentication on next refresh.
        DoAuth();
      } else {
        // fail silently for now...
      }
      return;
    }

    var responseText = xmlReq.responseText;

    var doc = new DOMDocument();
    doc.loadXML(responseText);

    var feed = doc.getElementsByTagName('feed');
    if (!feed || feed.length == 0) {
      // no <feed> element
      showErrorItem(genericProblemText, errorOccuredDescText, 
        unexpectedErrorText, null);
      return;
    }

    var elem = doc.getElementsByTagName("entry");
    if (elem != null && elem.length > 0) {
      userStore = [];
      for(var i = 0; i < elem.length; ++i) {
        addUser(elem[i], userStore);
      }
    }
    forceReloadFromServer();
  }
}

//-----------------------------------------------------------------------------
// FUNCTIONS RELATED TO EVENTS

var cachedEventStore = null;
function getEventStore() {
  if (cachedEventStore) {
    return cachedEventStore;
  }

  var out = [];
  for(var i = 0; i < userStore.length; ++i) {
    // Don't show events from users that have been unchecked
    if (!getUserShow(userStore[i])) {
      continue;
    }
    var item = cacheItems[userStore[i].id + '/' + getEventStore.start + '/' + 
      getEventStore.end];
    if (item && item.length > 0) {
      out = out.concat(item);
    }
  }
  cachedEventStore = out;

  return out;
}

function setEventStoreTime(start, end) {
  getEventStore.start = start;
  getEventStore.end = end;
  cachedEventStore = null;
}

// If we just started, show the last 8 hours worth of notifications
var prevTime = new Date().getTime() - 8*60*60*1000;
function checkEventsForExpiry() {
  // get current time
  var curTime = new Date().getTime();

  // Only check notifications every minute, though use 58 seconds for padding
  if (curTime - prevTime < 58000) {
    return;
  }
  
  var eventStore = getEventStore();
  if (eventStore) {
    // go through each event and see if start time is before prevTime
    // and after curTime. That means event just started (or  reminder should
    // fire now)
    for(var i = 0; i < eventStore.length; ++i) {
      var e = eventStore[i];
      if (!e.startTime || !e.endTime) {
        continue;
      }
      
      var eventStartTime = e.startTime.getTime();
      var remindBeforeTime = ((e.reminder == "") ? 10 : e.reminder) * 60 * 1000;
      eventStartTime -= remindBeforeTime;
      if (eventStartTime > prevTime && eventStartTime <= curTime) {
        var item = new ContentItem();
        item.layout = gddContentItemLayoutEmail;
        item.heading = e.title;
        item.source = e.getNiceTime();
        if (e.location != null)
          item.snippet = MSG_LOCATION + ': ' + e.location;
        item.open_command = e.url;
        contentArea.AddContentItem(item, gddItemDisplayAsNotification);
      }
    }
  }

  prevTime = curTime;
}

// Given a date, return an array containing all events that occur on that date
function getEventsForDate(d, store) {
  var start = new Date(d.getFullYear(), d.getMonth(), d.getDate()).getTime();
  var end = new Date(d.getFullYear(), d.getMonth(), d.getDate() + 1).getTime();
  
  start = Math.floor(start / 1000);
  end = Math.floor(end / 1000);

  // Currently GData returns overridden events as seperate entries
  // We may need to murge them
  var toPurge = {};
  var purgeNeeded = false;

  var out = [];
  if (store) {
    for(var i = 0; i < store.length; ++i) {
      var e = store[i];
      if (!e.startTime || !e.endTime) {
        // noop
        continue;
      } 

      // for reasons unknown, JS sometimes moves the results
      // by a few ms. Truncate that...
      var safeStart = Math.floor(e.startTime.getTime() / 1000);
      var safeEnd = Math.floor(e.endTime.getTime() / 1000);

      if (end > safeStart && start < safeEnd) {
        out.push(e);
        if (e.originalId) {
          toPurge[safeStart + '_' + safeEnd] = 1;
          purgeNeeded = true;
        }
      } else if (safeStart == safeEnd &&
                 safeStart == start) {
        out.push(e);
        if (e.originalId) {
          toPurge[safeStart + '_' + safeEnd] = 1;
          purgeNeeded = true;
        }
      }
    }
  }

  if (purgeNeeded) {
    var outPurged = [];
    for(var i = 0; i < out.length; ++i) {
      var e = out[i];
      if (e.recur && !e.originalId) {
        var safeStart = Math.floor(e.startTime.getTime() / 1000);
        var safeEnd = Math.floor(e.endTime.getTime() / 1000);
        if (toPurge[safeStart + '_' + safeEnd]) {
          continue;
        }
      }
      outPurged.push(e);
    }
    out = outPurged;
  }  
  return out.sort(sortByDateFunc);
}

/**
  * @param elem {Object} the XML node to extract the event from
  * @param events {Array} the array of events to plop the new event into
  * @param userId {String} the user id
  */
function genCalEvents(elem, events, userId) {
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
      
  for (var node = elem.firstChild; node != null; node = node.nextSibling) {
    if (node.nodeName == "title") {
      title = node.firstChild ? node.firstChild.nodeValue : MSG_NO_TITLE;
    } else if (node.nodeName == "link" && 
               node.getAttribute("rel") == "alternate") {
      url = node.getAttribute("href");
    } else if (node.nodeName == "gd:where") {
      location = node.getAttribute("valueString");
    } else if (node.nodeName == "gd:reminder") {
      reminder = node.getAttribute("minutes");
    } else if (node.nodeName == "gd:who") {
      attendees += node.getAttribute("valueString") + ", \n";
    } else if (node.nodeName == "gd:recurrence") {
      recur = true;
    } else if (node.nodeName == "gd:eventStatus") {
      status = node.getAttribute("value");
    } else if (node.nodeName == "content" && node.firstChild) {
      desc = node.firstChild.nodeValue;
    } else if (node.nodeName == "gd:originalEvent") {
      originalId = node.getAttribute("id");
      
      // technically we should run through the array eventually and remove the
      // original. But it may not be in the array yet... sigh
    }
  }
  
  if (status == "http://schemas.google.com/g/2005#event.canceled") {
    return;
  }

  if (attendees.length > 3) { // Remove extra comma and space
    attendees = attendees.substr(0, attendees.length - 3);
  }

  for (var node = elem.firstChild; node != null; node = node.nextSibling) {
    // gd:when's will be multiple in case of recurrences.
    if (node.nodeName == "gd:when" && title != null) {
      var startTimeStr = node.getAttribute("startTime");
      var endTimeStr = node.getAttribute("endTime");

      startTime = rfc3339StringToDate(startTimeStr);
      endTime = rfc3339StringToDate(endTimeStr);

      if (startTime == null || endTime == null) {
        continue;
      }

      isAllDay = (startTimeStr.length <= 11);
      events.push(new CalEvent(title, url, location, reminder, startTime,
        endTime, isAllDay, attendees, recur, desc, userId, originalId));
      
    }
  }
}

function CalEvent(title, url, location, reminder, startTime, endTime, isAllDay, 
                  attendees, recur, desc, userId, originalId) {
  this.title = title;
  this.url = url;
  this.location = location;
  this.reminder = reminder;
  this.startTime = startTime;
  this.endTime = endTime;
  this.isAllDay = isAllDay;
  this.attendees = attendees;
  this.recur = recur;
  this.desc = desc;
  this.userId = userId;
  this.originalId = originalId;
}

CalEvent.prototype.getItem = function() {
  if (this.item)
    return this.item;

  this.item = new ContentItem();
  this.item.onOpenItem = genDetails(this.url);
  this.item.layout = gddContentItemLayoutNowrapItems;
  this.item.flags = gddContentItemFlagNoRemove;

  if (this.isAllDay) {
    this.item.heading = this.title;
  } else {
    this.item.heading = FormatTimeShort(this.startTime) + ' ' + this.title;
  }

  var blurb = GenNiceStartEnd(this.startTime, this.endTime, this.isAllDay) +
              "\n\n";
  if (this.location) {
    blurb += MSG_LOCATION + "\n" + this.location + "\n\n";
  }
  if (this.attendees) {
    blurb += MSG_ATTENDEES + "\n" + this.attendees + "\n\n";
  }

  this.item.snippet = blurb;
  this.item.tooltip = blurb.replace(/\n\n/g, '<br/>').replace(/\n/g, '').
    replace(/<br\/>/g, '\n');
  this.item.onDetailsView = genDetailsFunc(this);
  this.item.onDrawItem = genDrawItem(this);
  return this.item;
}

function genDetailsFunc(c) {
  return function(item) {
    return onDetailsView(item, c);
  }
}

CalEvent.prototype.getNiceTime = function() {
  return GenNiceStartEnd(this.startTime, this.endTime, this.isAllDay);
}

var SESSION_REGEX = /(gsessionid=[^&]*)/;

/**
 * @param xmlReq {Object} the XMLHttpRequest object associated with this call
 * @param start {String} start time of the range that we are downloading
 * @param end {String} end time
 * @param user {UserItem} the user object
 */
function OnReceivedData(xmlReq, start, end, user) {
  if (!xmlReq || !(xmlReq.readyState == 4)) { // completed OK?
    return;
  }

  if (xmlReq.status == 200) {
    processDoc(xmlReq.responseText, start, end, user)
  } else if (xmlReq.status == 412) {
    // we've been redirected
    var location = xmlReq.getResponseHeader("X-Redirect-Location");
    var parts = SESSION_REGEX.exec(location);
    if (parts) {
      user.session = parts[1];
      loadEventsFromServer(start, end, true, user);
    } else {
      // Something very strange happened... fail silently for now
    }
  } else if (xmlReq.status == 401) {
    authToken = ""; // force authentication on next refresh.
    loadEventsForCurrentDates();
  } else {
    // fail silently for now
  }
}


/**
 * @param responseText {String} the XML blob to parse
 * @param start {String} start time of the range that we are downloading
 * @param end {String} end time
 * @param user {UserItem} the user object
 */
function processDoc(responseText, start, end, user) {
  // Parse and add each calendar event to our array.
  var doc = new DOMDocument();
  doc.loadXML(responseText);

  var feed = doc.getElementsByTagName('feed');
  if (!feed || feed.length == 0) {
    // no <feed> element
    showErrorItem(genericProblemText, errorOccuredDescText, 
                  unexpectedErrorText, null);
    return;
  }

  var updatedElem = doc.getElementsByTagName("updated");
  var updatedTime = null;
  if (updatedElem != null && updatedElem.length > 0) {
    updatedTime = rfc3339StringToDate(updatedElem[0].firstChild.nodeValue);
  }
  
  // If we have a cache value. Then we have 2 choices
  var cacheValue = cacheUpdated[user.id + '/' + start + '/' + end];
  if (cacheValue) {
    if (sameTime(updatedTime, cacheValue)) {
      // Nothing changed. Continue as planned.
      return;
    } else {
      // Something changed. Wipe the cache and reload
      delete cacheItems[user.id + '/' + start + '/' + end];
      delete cacheUpdated[user.id + '/' + start + '/' + end];
      cachedEventStore = null;
      
      // Try again
      loadEventsFromServer(start, end, true, user);
      
      // We're done. This is slightly recursive, but we'll proceed the next time
      return;
    }
  }

  var elem = doc.getElementsByTagName("entry");
  if (elem != null && elem.length > 0) {
    var events = [];
    for(var i = 0; i < elem.length; ++i) {
      genCalEvents(elem[i], events, user.id);
    }
    
    // Purge if it ever hits 200
    if (countObject(cacheItems) > 200) {
      cacheItems = {};
      cacheUpdated = {};
    }
    cacheItems[user.id + '/' + start + '/' + end] = events;
    cacheUpdated[user.id + '/' + start + '/' + end] = updatedTime;
    cachedEventStore = null;
  }

  drawCalendar();
}

//-----------------------------------------------------------------------------
// FUNCTIONS RELATED TO DRAWING

function genDetails(url) {
  return function() {
    loginAndGotoURL(url);
  }
}

function DrawRounded(graphics, x, y, width, height, color) {
  graphics.DrawRect(x+1, y, width-2, height, color, color);
  graphics.DrawLine(x, y + 1, x, y + height - 1, color);
  graphics.DrawLine(x + width - 1, y + 1, x + width - 1, y + height - 1, color);
}

function sortByDateFunc(a, b) {
  // All day events come first
  if (b.isAllDay != a.isAllDay) {
    return b.isAllDay ? 1 : -1;
  }
  
  // Only sort timed events by start time
  if (!b.isAllDay) {
    var startDiff = a.startTime.getTime() - b.startTime.getTime();
    if (startDiff) {
      return startDiff;
    }
  }

  // Fall back on alphabetical
  return b.title.toLowerCase() > a.title.toLowerCase() ? -1 : 1;
}

function sortByTitle(a, b) {
  return b.title.toLowerCase() > a.title.toLowerCase() ? -1 : 1;
}

function showErrorItem(title, subtitle, snippet, url) {
  contentArea.RemoveAllContentItems();
  PresizeWindow();
  var h = new HelpContentItem(title, subtitle, snippet, url);
  h.AddToSidebar();
}

var COLOR_MAP = [
  undefined , // no color 0 //: added explicit 'undefined'
  ['#A32929', '#CC3333'],
  ['#B1365F', '#DD4477'],
  ['#7A367A', '#994499'],
  ['#5229A3', '#6633CC'],
  ['#29527A', '#336699'],
  ['#2952A3', '#3366CC'],
  ['#1B887A', '#22AA99'],
  ['#28754E', '#329262'],
  ['#0D7813', '#109618'],
  ['#528800', '#66AA00'],
  ['#88880E', '#AAAA11'],
  ['#AB8B00', '#D6AE00'],
  ['#BE6D00', '#EE8800'],
  ['#B1440E', '#DD5511'],
  ['#865A5A', '#A87070'],
  ['#705770', '#8C6D8C'],
  ['#4E5D6C', '#627487'],
  ['#5A6986', '#7083A8'],
  ['#4A716C', '#5C8D87'],
  ['#6E6E41', '#898951'],
  ['#8D6F47', '#B08B59']
];

function genDrawItem(c) {
  return function(item, target, graphics, x, y, width, height) {
    DrawItem(item, target, graphics, x, y, width, height, 
      c.isAllDay, c.recur, c.attendees, c.reminder, c.userId)
  }
}

function getColorForUserId(userId) {
  var u = getUserForId(userId);

  if (u) {
    for(var j = 1; j < COLOR_MAP.length; ++j) {
      if (u.color == COLOR_MAP[j][0]) {
        return j;
      }
    }
  }
  
  return 6;
}

function getUserForId(userId) {
  for(var i = 0; i < userStore.length; ++i) {
    if (userStore[i].id == userId) {
      return userStore[i];
    }
  }
  return null;
}


function DrawItem(item, target, graphics, x, y, width, height, allDay, recur, 
                  attendees, reminder, userId) {
                  
  var color = getColorForUserId(userId);
  var textColor = COLOR_MAP[color][0];
  var widthOffset = 3;
  if (allDay) {
    DrawRounded(graphics, x, y, width, height + 2, COLOR_MAP[color][1]);
    textColor = '#FFFFFF';
  }
  graphics.DrawText(x + widthOffset, y, width, height, item.heading, textColor, 0,
    gddFontNormal);

  var textX = x + widthOffset + graphics.GetTextWidth(item.heading, 0, gddFontNormal) + 4;
  if (reminder) {
    graphics.DrawImage(textX, y + 4, 9, 7, image_alarm[allDay], 100)
    textX += 12;
  }
  if (recur) {
    graphics.DrawImage(textX, y + 4, 9, 7, image_rep[allDay], 100)
    textX += 12;
  }
  if (attendees) {
    graphics.DrawImage(textX, y + 4, 9, 7, image_people[allDay], 100)
    textX += 12;
  }
}

function HelpContentItem(title, subtitle, snippet, url) {
  if (url == null || url == "") {
    url = kProductHomePageUri;
  }

  var c = new ContentItem();
  c.layout = gddContentItemLayoutNews;
  c.heading = title;
  c.source = subtitle;
  c.snippet = snippet;
  c.tooltip = snippet;
  c.SetRect(0, 0, getWindowWidth(), getWindowHeight());
  c.onOpenItem = genDetails(url);
  c.flags = gddContentItemFlagNoRemove | gddContentItemFlagHighlighted;
  this.item = c;

  // Methods
  HelpContentItem.prototype.AddToSidebar = AddToSidebar;
  function AddToSidebar() {
    contentArea.AddContentItem(this.item, gddItemDisplayInSidebar);
  }
}

// ---- Options Dialog ----

function ShowOptionsDlg(wnd) {
  wnd.AddControl(gddWndCtrlClassLabel, 0, "", MSG_SIGN_IN, 10, 10, 260, 25);

  wnd.AddControl(gddWndCtrlClassLabel, 0, "", MSG_EMAIL, 40, 40, 200, 25);
  wnd.AddControl(gddWndCtrlClassEdit, 0, "Email", getEmail(),
      40, 55, 200, 24);

  wnd.AddControl(gddWndCtrlClassLabel, 0, "", MSG_PW, 40, 85, 200, 15);
  wnd.AddControl(gddWndCtrlClassEdit, gddWndCtrlTypeEditPassword, "Passwd", getPW(),
      40, 100, 200, 24);

  var ctl;
  ctl = wnd.AddControl(gddWndCtrlClassButton, gddWndCtrlTypeButtonCheck, "button24",
    MSG_24, 20, 140, 250, 24);
  ctl.value = (options.GetValue("24") == "true") ? "1" : "0";

  ctl = wnd.AddControl(gddWndCtrlClassButton, gddWndCtrlTypeButtonCheck, "european",
    MSG_BEGIN_MONDAY, 20, 160, 250, 24);
  ctl.value = (options.GetValue("european") == "true") ? "1" : "0";

  ctl = wnd.AddControl(gddWndCtrlClassButton, gddWndCtrlTypeButtonCheck, "hidemonth",
    MSG_HIDE_CALENDAR, 20, 180, 250, 24);
  ctl.value = (options.GetValue("hidemonth") == "true") ? "1" : "0";

  ctl = wnd.AddControl(gddWndCtrlClassButton, gddWndCtrlTypeButtonCheck, "wn",
    MSG_SHOW_WEEK_NUM, 20, 200, 250, 24);
  ctl.value = (options.GetValue("showweek") == "true") ? "1" : "0";

  // If we have users, then add them
  if (userStore.length > 0) {
    var otherCalendars = false;
    var orderedStores = userStore.sort(sortByTitle);
    wnd.AddControl(gddWndCtrlClassLabel, 0, "", MSG_MY_CALENDARS, 10, 245, 250, 24);

    var calCount = 0;
    for(var i = 0; i < orderedStores.length; ++i) {
      var user = orderedStores[i];
      if (user.accessLevel != "owner") {
        otherCalendars = true;
        continue;
      }
      ctl = wnd.AddControl(gddWndCtrlClassButton, gddWndCtrlTypeButtonCheck, "u" + i,
        user.title, 20, 260 + 20 * calCount, 250, 24);
      ++calCount;
      ctl.value = getUserShow(user) ? "1" : "0";
    }
    if (otherCalendars) {
      wnd.AddControl(gddWndCtrlClassLabel, 0, "", MSG_OTHER_CALENDARS, 10, 265 + 20 * calCount, 250, 24);
      ++calCount;
      for(var i = 0; i < orderedStores.length; ++i) {
        var user = orderedStores[i];
        if (user.accessLevel == "owner") {
          continue;
        }
        ctl = wnd.AddControl(gddWndCtrlClassButton, gddWndCtrlTypeButtonCheck, "u" + i,
          user.title, 20, 260 + 20 * calCount, 250, 24);
        ++calCount;
        ctl.value = getUserShow(user) ? "1" : "0";
      }
    }
  }
  
  wnd.onClose = OptionsDlgClosed;
}

function getUserShow(user) {
  // Return the value if available
  if (user.show === true || user.show === false) {
    return user.show;
  }

  // Try to load it from options  
  var show = options.GetValue('show-' + user.id);
  if (show === "true" || show === "false") {
    return user.show = (show === "true");
  }

  // Must be first time that we've seen it, save the state to options
  show = !user.hidden && user.selected;
  setUserShow(user, show);
  return show;
}

function setUserShow(user, value) {
  options.PutValue('show-' + user.id, value ? "true" : "false");
  user.show = value;
}

function OptionsDlgClosed(wnd, code) {
  if (code != gddIdOK) {
    return;
  }

  var email = wnd.GetControl("Email").text;
  if (email == null || email.length == 0) {
    utils.alert(emailIsBlank);
    return false;
  }

  if (email.indexOf("@") == -1) {
    email += '@gmail.com';
  }
  options.PutValue("Email", email);

  var passwd = wnd.GetControl("Passwd").text;
  if (passwd == null || passwd.length == 0) {
    utils.alert(passwordIsBlank);
    return false;
  }
  options.PutValue("Passwd", passwd);

  var ctl;
  ctl = wnd.GetControl("button24");
  options.PutValue("24", (ctl.value == true) ? "true" : "false");
  format24 = (ctl.value == true);

  ctl = wnd.GetControl("european");
  options.PutValue("european", (ctl.value == true) ? "true" : "false");
  europeanDate = ctl.value ? 1 : 0;

  ctl = wnd.GetControl("hidemonth");
  options.PutValue("hidemonth", (ctl.value == true) ? "true" : "false");
  hideMonthView = ctl.value ? 1 : 0;

  ctl = wnd.GetControl("wn");
  options.PutValue("showweek", (ctl.value == true) ? "true" : "false");
  showWeekNum = ctl.value ? 1 : 0;

  // If we have users, check if we changed the checkbox
  if (userStore.length > 0) {
    var orderedStores = userStore.sort(sortByTitle);
    for(var i = 0; i < orderedStores.length; ++i) {
      ctl = wnd.GetControl("u" + i);

      var user = orderedStores[i];
      var oldVal = getUserShow(user);
      var newVal = (ctl.value == true);
      if (oldVal != newVal) {
        setUserShow(user, newVal);
      }
    }
  }

  authToken = ''; // reset auth token.
  numAuthTriesSinceLastSuccess = 0;
  forceReloadFromServer();
  drawCalendar();
}

//////////////////////////////////////////////////////////////////////////////
// HELPER FUNCTIONS

/* From http://www.rgagnon.com/jsdetails/js-0096.html */
function encodeUrl(sStr) {
  return escape(sStr).replace(/\+/g, "%2B").replace(/\"/g, "%22").
                      replace(/\'/g, "%27").replace(/\//g, "%2F");
}

function pad(num) {
  return (num < 10 ? '0' : '') + num;
}

function getDateIso8601(date) {
  return date.getFullYear() + "-" + pad(date.getMonth() + 1) +
    "-" + pad(date.getDate());
}

function getDateYYYYMMDD(date) {
  return date.getFullYear() + pad(date.getMonth() + 1) + pad(date.getDate());
}

function advanceDay(d, offset) {
  return new Date(d.getFullYear(), d.getMonth(), d.getDate() + offset);
}

function sameDay(d1, d2) {
  return (d1.getFullYear() == d2.getFullYear() && d1.getMonth() == d2.getMonth() && 
          d1.getDate() == d2.getDate());
}

function sameTime(d1, d2) {
  return sameDay(d1, d2) && d1.getHours() == d2.getHours() && 
    d1.getMinutes() == d2.getMinutes() && d1.getSeconds() == d2.getSeconds();
}

function dateIsToday(date) {
  return sameDay(date, new Date());
}

function showPage(url) {
  var wshShell = new ActiveXObject("WScript.Shell");
  wshShell.run(url);
  wshShell = null;
}

// Do html escaping
var amp_re_ = /&/g;
var lt_re_ = /</g;
var gt_re_ = />/g;

function HtmlEscape(str) {
  if (!str) return "";
  return str.replace(amp_re_, "&amp;").replace(lt_re_, "&lt;").
    replace(gt_re_, "&gt;").replace(quote_re_, "&quot;");
}

// Escape double quote '"' characters in addition to '&', '<', '>' so that a
// string can be included in an HTML tag attribute value within double quotes.
var quote_re_ = /\"/g;
function QuoteEscape(str) {
  return HtmlEscape(str).replace(quote_re_, /* SAFE */ "&quot;");
}

// Escape non-latin HTML
function EscapeHtmlString(str) {
  var out = [];
  if (!str) return out;
  for(var i = 0; i < str.length; ++i) {
    var code = str.charCodeAt(i);
    if (code > 127) {
      out.push("&#", code, ";");
    } else {
      out.push(str.charAt(i));
    }
  }
  return out.join('');
}

// This function is called when the user clicks on the item for details view.
function onDetailsView(item, c) {
  var color = COLOR_MAP[getColorForUserId(c.userId)][0];

  var html = '<html><head>' + 
    '<meta http-equiv="Content-Type" content="text/html; charset=UTF-8"></head>' +
    '<body style="font-family: arial, sans-serif;" TOPMARGIN=0 LEFTMARGIN=0>' +
    '<font size=-1><span style="font-size:120%; color:' +
    color + '"><b>' + HtmlEscape(c.title) + '</b></span><br>';

  // If they have more than 2 or more calendars loaded, show the calendar name
  if (userStore && userStore.length > 1) {
    html += '<font size=-1><span style="color:' +
      color + '">' + HtmlEscape(getUserForId(c.userId).title) +
      '</span><br>';
  }

  html += c.getNiceTime() + '<br><br>';
    
  if (c.location) {
    html += '<b>' + MSG_LOCATION + ':</b> ' + HtmlEscape(c.location) + '<br>';
  }
  if (c.desc) {
    html += '<b>' + MSG_DESCRIPTION + ':</b> ' + HtmlEscape(c.desc) + '<br>';
  }
  if (c.reminder) {
    html += '<b>' + MSG_REMINDER + ':</b> ' + c.reminder + ' ' +
    MSG_REMINDER_MINUTES + '<br>';
  }
  if (c.recur) {
    html += '<b>' + MSG_REPEATS + ':</b> ' + MSG_REPEATS_YES + '<br>';
  }
  if (c.attendees) {
    html += '<b>' + MSG_ATTENDEES + ':</b> ' + HtmlEscape(c.attendees) + '<br>';
  }
  
  html += '<br><a target="_new" href="' + QuoteEscape(c.url) + 
    '">' + MSG_EDIT_EVENT + ' &raquo;</a>';
    
  html += '<br><a target="_new" href="' + 
    QuoteEscape('http://www.google.com/calendar/render?date=' + 
    getDateYYYYMMDD(new Date(curYear, curMonth, curDate))) + 
    '">' + MSG_SHOW_IN_CALENDAR + ' &raquo;</a>';
  
  html += '<br><br></font></body></html>';
  var control = new DetailsView();
  control.SetContent(this.title, c.starTime, EscapeHtmlString(html), 
    true, item.layout);
  control.html_content = true;
  var details = {};
  details.details_control = control;
  return details;
}

function createOnDateUrl(d) {
  var zDate = getDateYYYYMMDD(d);
  var zDateEnd = getDateYYYYMMDD(advanceDay(d, 1));
  return kCalendarEventUrl + '?action=TEMPLATE&dates=' + zDate + '/' + zDateEnd;
}

//////////////////////////////////////////////////////////////////////////////
// DATE FORMATTING FUNCTIONS

// Call this to format the date nicely
function NiceDate(d, show_date, show_time) {
  var text = '';

  if (show_date) {
    text += DOW[d.getDay()] + ', ' +
            MONTHS_SHORT[d.getMonth()] + ' ' + d.getDate();
  }

  if (show_time) {
    if (show_date) {
      text += ', ';
    }

    var h = d.getHours();

    if (!format24) {
      if (h > 12) {
        h = h - 12;
      }
      else if (h == 0) {
        h = 12;
      }
    } else {
      // 24-hour time should always be 5 characters "hh:mm"
      if (h < 10) h = '0' + h;
    }

    text += h;
    var min = d.getMinutes();
    if (min > 0 || format24) {
      text += ":" + (min < 10 ? '0' : '') + min;
    }
  }

  return text;
}

// Given an event, generate a human readable string describing the time
function GenNiceStartEnd(start, end, allday) {
  var text = '';

  // Determine if the start + end are on different days
  var presentationEndDate = end;
  if (allday) {
    presentationEndDate = new Date(end.getFullYear(), end.getMonth(), end.getDate() - 1);
  } else if (start.getTime() == end.getTime()) {
    text = NiceDate(start, true, true);
    if (!format24) {
      text += (end.getHours() >= 12) ? MSG_PM : MSG_AM;
    }
    return text;
  }
  var difday = !sameDay(start, presentationEndDate);

  text += NiceDate(start, true, !allday);

  if (allday) {
    if (difday) {
      text += '-' + NiceDate(presentationEndDate, difday, false);
    }
  }
  else {
    // If different days or spans noon, then show AM/PM
    if (!format24 && (difday || (start.getHours() < 12 && end.getHours() >= 12))) {
      text += (start.getHours() < 12) ? MSG_AM : MSG_PM;
    }

    text += '-' + NiceDate(end, difday, true);
    if (!format24) {
      text += (end.getHours() >= 12) ? MSG_PM : MSG_AM;
    }
  }


  return text;
}

/** Convert a javascript date time to 3339 style [drop the ms and in UTZ]
  * ie - 2006-04-28T09:00:00Z
  */
function to3339String(d) {
  return d.getUTCFullYear() + '-' + pad(d.getUTCMonth()) + '-' + 
         pad(d.getUTCDay()) + 'T' + pad(d.getUTCHours()) + ':' + 
         pad(d.getUTCMinutes()) + ':' + pad(d.getUTCSeconds()) + 'Z';
}

var DATE_TIME_REGEX = /^(\d\d\d\d)-(\d\d)-(\d\d)T(\d\d):(\d\d):(\d\d)\.\d+(\+|-)(\d\d):(\d\d)$/;
var DATE_TIME_REGEX_Z = /^(\d\d\d\d)-(\d\d)-(\d\d)T(\d\d):(\d\d):(\d\d)\.\d+Z$/;
var DATE_REGEX = /^(\d\d\d\d)-(\d\d)-(\d\d)$/;

/* Convert the incoming date into a javascript date
 * we accept the following 3 forms:
 * 2006-04-28T09:00:00.000-07:00
 * 2006-04-28T09:00:00.000Z
 * 2006-04-19
 */
function rfc3339StringToDate(rfc3339) {
  var parts = DATE_TIME_REGEX.exec(rfc3339);
  
  // Try out the Z version
  if (!parts) {
    parts = DATE_TIME_REGEX_Z.exec(rfc3339);
  }
  
  if (parts && parts.length > 0) {
    var d = new Date();
    d.setUTCFullYear(parts[1], parseInt(parts[2], 10) - 1, parts[3]);
    d.setUTCHours(parts[4]);
    d.setUTCMinutes(parts[5]);
    d.setUTCSeconds(parts[6]);

    var tzOffsetFeedMin = 0;
    if (parts.length > 7) {
      tzOffsetFeedMin = parseInt(parts[8],10) * 60 + parseInt(parts[9],10);
      if (parts[7] != "-") { // This is supposed to be backwards.
        tzOffsetFeedMin = -tzOffsetFeedMin;
      }
    }
    return new Date(d.getTime() + tzOffsetFeedMin * 60 * 1000);
  }

  parts = DATE_REGEX.exec(rfc3339);
  if (parts && parts.length > 0) {
    return new Date(parts[1], parseInt(parts[2],10) - 1, parts[3]);
  }
  return null;
}

function FormatTimeShort(date) {
  var hr = date.getHours(),
      mi = date.getMinutes();
  if (format24) {
    // there isn't a "short" version of 24 hour time
    // 24-hour time should always be 5-characters "hh:mm"
    if (hr < 10) hr = '0' + hr;
    if (mi < 10) mi = '0' + mi;
    return hr + ':' + mi;
  } else {
    var ap = hr < 12 ? MSG_AM : MSG_PM;
    hr = (hr % 12) || 12;
    if (mi) {
      if (mi < 10) { mi = '0' + mi; }
      return hr + ':' + mi + ap;
    } else {
      return hr + ap;
    }
  }
}

//////////////////////////////////////////////////////////////////////////////
// CALENDAR DRAWING FUNCTIONS

function prevMonth() {
  OnCommand(gddCmdToolbarBack);
}

function nextMonth() {
  OnCommand(gddCmdToolbarForward);
}

function OnCommand(cmd) {
  if (cmd == gddCmdToolbarBack || cmd == gddCmdToolbarForward) {
    curMonth += (cmd == gddCmdToolbarForward) ? 1 : -1;
    curYear += (curMonth < 0) ? -1 : (curMonth >= 12 ? 1 : 0);
    curMonth = (curMonth < 0) ? 11 : (curMonth % 12);
    loadEventsForCurrentDates();
    drawCalendar();
  }
}

function getWindowHeight() {
  var height = pluginHelper.window_height;
  if (height < 10) {
    // Default width. It sometimes returns 0... sigh
    height = 100; 
  }
  return height;
}

function getWindowWidth() {
  var width = pluginHelper.window_width;
  if (width < 10) {
    // Default width. It sometimes returns 0... sigh
    width = 100; 
  }
  return width;
}

// Large scary function which draws calendars on the screen
function drawCalendar() {
  contentArea.RemoveAllContentItems();   // clear current UI elements
  PresizeWindow();

  var fullWidth = getWindowWidth();
  var itemWidth = fullWidth / (7 + (showWeekNum ? 1 : 0));
  var x = 0, y = 0;

  var eventStore = getEventStore();
  if (!hideMonthView) {
    AddItem(MSG_PREV_DATE, x, y, itemWidth, itemHeight, DrawHeader, prevMonth);

    monthYearItem = AddItem(MONTHS_LONG[curMonth] + ' ' + curYear, 
      x + itemWidth, y, fullWidth - 2 * itemWidth, itemHeight, DrawHeader, goToGoogle, 
      kGoogleCalendarUrl);

    AddItem(MSG_NEXT_DATE, fullWidth - itemWidth, y,
      itemWidth, itemHeight, DrawHeader, nextMonth);

    y += itemHeight;
    
    if (showWeekNum) {
      x = itemWidth;
    }

    for (var i = 0; i < 7; ++i) {
      var drawFunc = drawLongShort(DOW_SHORT[(i+ europeanDate)%7], 
        DOW_TINY[(i+ europeanDate)%7], gddFontNormal);
    
      AddStaticItem(DOW_SHORT[(i+ europeanDate)%7], x + itemWidth * i, y, itemWidth, 
        itemHeight, drawFunc);
    }
    AddStaticItem('', 0, 0, fullWidth, itemHeight*2, DrawHeaderBkgndMain);
    y += itemHeight;

    var cal = new Date();
    var actualDate = cal.getDate();
    var actualMonth = cal.getMonth();
    cal = new Date(curYear, curMonth, 1);

    var dow = (cal.getDay() + 7 - europeanDate) % 7;  
    x = 0;
    cal.setDate(1 - dow);

    for (var i = 0; i < 42; ++i) {  // add each day of the month
      var drawDate = cal.getDate();
      var drawMonth = cal.getMonth();
      var drawYear = cal.getFullYear();
      
      // Generate the appropriate tooltip for this date
      var events = getEventsForDate(cal, eventStore);
      var anyEvents = events.length > 0;
      var toolTip = '';
      if (anyEvents) {
        toolTip = NiceDate(cal, true, false) + '\n';
        for(var j = 0; j < events.length; j++) {
          var e = events[j];
          toolTip += '\n' + e.title + '\n' + e.getNiceTime() + '\n';
        }
      }

      // Determine if we have hit the end of the week
      if (i > 0 && i % 7 == 0) {
        if (cal.getFullYear() > curYear || cal.getMonth() > curMonth) break;
        x = 0;
        dow = 0;
        y += itemHeight;
      }
      
      if (showWeekNum && x == 0) {
        var weekNum = getWeek(drawYear, drawMonth, drawDate);
        AddStaticItem(weekNum, x, y, itemWidth, itemHeight, DrawItemCal);
        x += itemWidth;
      }
      
      // Draw the item
      AddItem(drawDate, x, y, itemWidth, itemHeight, 
        (actualDate == drawDate && actualMonth == drawMonth) ? DrawItemCalWhite : 
        (anyEvents) ? DrawItemCalBold : DrawItemCal,
        genSelectDate(drawYear, drawMonth, drawDate),
        createOnDateUrl(cal),
        toolTip
      );
      // show a border around selected date
      if (curDate == drawDate && curMonth == drawMonth)
        AddStaticItem("", x - 2, y - 2, itemWidth + 5, itemHeight + 5, DrawSelectedDayBkgnd);
      // show a highlight item behind today
      if (actualDate == drawDate && actualMonth == drawMonth)
        AddStaticItem("", x - 2, y - 2, itemWidth + 5, itemHeight + 5, DrawTodayBkgnd);
      else if (i % 7 == (europeanDate ? 5 : 0) || i  % 7 == 6) {
        AddStaticItem("", x - 2, y - 2, itemWidth + 5, itemHeight + 5, DrawWeekendBkgnd);
      }
      dow++;
      x += itemWidth;
      cal.setDate(drawDate + 1);
    }

    if (showWeekNum) {
      AddStaticItem('', 0, itemHeight*2, itemWidth, y - itemHeight, DrawHeaderBkgndMain);
    }

    y += itemHeight + 4;
  }
  x = 0;

  var arrowConst = 16;

  if (!getPW()) {
    // Hack to estimate how many lines the text will be.
    var lines = Math.ceil(500 / fullWidth);
    AddStaticItem(MSG_NO_ACCOUNT, x, y + 2, fullWidth, itemHeight * lines, DrawItemWrap);
    y += itemHeight * lines;

    AddItem(MSG_NO_ACCOUNT_SIGNUP, x, y + 2, fullWidth, itemHeight, DrawItemLink, goToGoogle);
    y += itemHeight;

    AddStaticItem(MSG_NO_ACCOUNT_LOGIN, x, y + 2, fullWidth, itemHeight * 3, 
      DrawItemWrap, goToGoogle);
      UpdateTimeMonthYear();
    return;
  }

  // Done drawing the mini-calendar. Time to draw the events on the bottom
  var currentDate = new Date(curYear, curMonth, curDate);
  var displaycontentItems = getEventsForDate(currentDate, eventStore);

  AddItem(MSG_PREV_DATE, x, y + 2, arrowConst, itemHeight, DrawItemCalBold, gotoPrevDate);
  var numItemsShown = 0; //: added semicolon
  if (displaycontentItems.length == 0) {
    var dateText = NiceDate(currentDate, true, false)
    var drawFunc = drawLongShort(MSG_NO_EVENTS_ON + ' ' + dateText, dateText, gddFontBold);

    AddItem('', x + arrowConst, y + 2, fullWidth - 2*arrowConst, itemHeight, 
      drawFunc, genDetails(createOnDateUrl(currentDate)), null, MSG_CREATE);
    AddItem(MSG_NEXT_DATE, fullWidth-arrowConst, y + 2, arrowConst, itemHeight, 
      DrawItemCalBold, gotoNextDate);
        
    y += itemHeight;
  } else {
    var dateText = NiceDate(currentDate, true, false)
    var drawFunc = drawLongShort(MSG_EVENTS_ON + ' ' + dateText, dateText, gddFontBold);

    AddItem(dateText, x + arrowConst, y + 2, fullWidth - arrowConst*2, itemHeight, 
      drawFunc, genDetails(createOnDateUrl(currentDate)), null, MSG_CREATE);
    AddItem(MSG_NEXT_DATE, fullWidth-arrowConst, y + 2, arrowConst, itemHeight, 
      DrawItemCalBold, gotoNextDate);

    y += itemHeight + 4;

    numItemsShown = displaycontentItems.length;
    for(var i = 0; i < displaycontentItems.length; ++i) {
      var item = displaycontentItems[i].getItem();
      item.SetRect(x, y, fullWidth, itemHeight);
      contentArea.AddContentItem(item, gddItemDisplayInSidebar);
      y += itemHeight;
    }
  }
  y += 4;
    
  // Show a 'Create Event' link
  AddItem(MSG_CREATE, x, y + 2, fullWidth, itemHeight, DrawItemLink, 
          quickAddPrompt);
  y += itemHeight;

  // Show a 'Jump to today' link if they aren't currently looking at today
  var isToday = dateIsToday(currentDate);
  if (!isToday) {
    AddItem(MSG_TODAY, x, y + 2, fullWidth, itemHeight, DrawItemLink, gotoToday);
    y += itemHeight;
  }

  // Add a link back to the calendar
  AddItem(MSG_VIEW_CALENDAR, x, y + 2, fullWidth, itemHeight, DrawItemLink, goToGoogle);
  y += itemHeight;

  if (isToday && numItemsShown < 2) {
    AddStaticItem('', x, y + 2, fullWidth, itemHeight);
  }
  UpdateTimeMonthYear();
}

// This incantation allows us to quickadd an event
function getQuickAddString(text) {
  return "<atom:entry xmlns:atom='http://www.w3.org/2005/Atom'>" + 
    "<atom:category scheme='http://schemas.google.com/g/2005#kind' " +
      "term='http://schemas.google.com/g/2005#event'></atom:category>" + 
    "<atom:content type='text'>" + HtmlEscape(text) + "</atom:content>" +
    "<gCal:quickadd xmlns:gCal='http://schemas.google.com/gCal/2005' " +
      "value='true'>" +
    "</gCal:quickadd></atom:entry>";
}

function genCreateEventReceive(xmlReq, text) {
  return function() {
    return onCreateEventReceive(xmlReq, text);
  }
}

function onCreateEventReceive(xmlReq, text) {
  if (!xmlReq || !(xmlReq.readyState == 4)) {
    return;
  }
  
  if (xmlReq.status != 201) {
    // just send them to the quickadd page to let them disambiguate
    showQuickAdd(text);
    return;
  }

  var doc = new DOMDocument();
  var responseText = xmlReq.responseText;
  doc.loadXML(responseText);

  var elem = doc.getElementsByTagName("entry");
  if (elem != null && elem.length > 0) {
    var events = [];
    genCalEvents(elem[0], events, "");
    
    // Tell the user what we added
    var text = MSG_EVENT_ADDED + ' (' + events[0].title + ')\n' + 
      events[0].getItem().snippet;
    utils.alert(text);
    
    // reload events from server to reflect the new one
    forceReloadFromServer();
  }
}

function showQuickAdd(text) {
  showPage(kCalendarEventUrl + '?action=TEMPLATE&ctext=' + encodeUrl(text));
}

var promptShown = false;

function quickAddPrompt() {
  if (promptShown) {
    return;
  }
  promptShown = true;
  var text = utils.prompt(MSG_QUICKADD,'');
  promptShown = false;
  if (text) {
    var xmlReq = new XMLHttpRequest();
    xmlReq.open("POST", kCalendarCreateEventUrl, true);
    xmlReq.setRequestHeader('Content-Type', 'application/atom+xml; charset=UTF-8');
    xmlReq.setRequestHeader('Authorization', 'GoogleLogin auth=' + authToken);
    xmlReq.setRequestHeader('Referer', 'http://gds.calendar.' + kClientLoginAppName);
    
    var data = getQuickAddString(text);
    xmlReq.onreadystatechange = genCreateEventReceive(xmlReq, text);

    try {
      xmlReq.send(data);
    } catch(e) {
      // Just in case
    }
  }
}

/** Open a browser window where the user is logged in, and
  * then go to a URL.
  */
function loginAndGotoURL(url) {

  // NOTE! This currently DOES NOT WORK for GAFYD users [long story]
  // In the meantime, we limit it to gmail users.
  if (getEmail().indexOf('@gmail.com') == -1) {
    showPage(url);
    return;
  }

  var xmlReq = new XMLHttpRequest();
  xmlReq.open("POST", kAccountsUrl + '/IssueAuthToken ', true);
  xmlReq.setRequestHeader('Content-Type', 'application/x-www-form-urlencoded; charset=UTF-8');
  var data = 'SID=' + sid + '&LSID=' + lsid + '&service=gaia&Session=false';
  xmlReq.onreadystatechange = bind2(loginAndGotoURLReceive,xmlReq,url);

  try {
    xmlReq.send(data);
  } catch(e) {
    // Just in case
  }
}

/** We've got the token, now open the new page with it */
function loginAndGotoURLReceive(xmlReq, url) {
  if (!xmlReq || !(xmlReq.readyState == 4)) {
    return;
  }

  var newUrl = kAccountsUrl + '/TokenAuth?auth='  + xmlReq.responseText +
    '&service=cl&source=GDSCAL&continue=' + encodeUrl(url);
  showPage(newUrl);
}

function bind2(func, arg1, arg2) {
  return function() {
    func(arg1, arg2);
  }
}

// Thanks to http://www.irt.org/script/914.htm
function getWeek(year,month,day) {
  var when = new Date(year,month,day);
  var newYear = new Date(year,0,1);
  var modDay = newYear.getDay();
  if (modDay == 0) modDay=6; else modDay--;

  var daynum = ((Date.UTC(year,when.getMonth(),when.getDate(),0,0,0) -
                Date.UTC(year,0,1,0,0,0)) /1000/60/60/24) + 1;

  if (modDay < 4 ) {
    return Math.floor((daynum+modDay-1)/7)+1;
  }
  else {
    var weeknum = Math.floor((daynum+modDay-1)/7);
    if (weeknum == 0) {
      year--;
      var prevNewYear = new Date(year,0,1);
      var prevmodDay = prevNewYear.getDay();
      if (prevmodDay == 0) prevmodDay = 6; else prevmodDay--;
      if (prevmodDay < 4) weeknum = 53; else weeknum = 52;
    }
    return weeknum;
  }
}

// Given 2 text strings. Generate a closure to draw the longer one if we have
// enough space, or the shorter one if we don't
function drawLongShort(longText, shortText, fontType) {
  return function(item, target, graphics, x, y, width, height) {
    var textWidth = graphics.GetTextWidth(longText, 0, fontType);
    var text = longText;
    if (textWidth >= width) {
      text = shortText;
    }
    graphics.DrawText(x, y, width, height, text, gddColorNormalText, 
      gddTextFlagCenter + gddTextFlagSingleLine + gddTextFlagVCenter, fontType);
  }
}

// Cause the given y/m/d to become active
function selectDate(year, month, day) {
  curYear = year;
  var oldMonth = curMonth;
  curMonth = month;

  if (curMonth != oldMonth) {
    loadEventsForCurrentDates();   
  }
  
  curDate = day;
  drawCalendar(); 
}

// Generate a closure which causes the given y/m/d to become active.
// Useful for handing to details handlers
function genSelectDate(year, month, day) {
  return function() {
    selectDate(year, month, day);
  }
}

function AddStaticItem(str, x, y, width, height, drawHandler) {
  var item = new ContentItem();
  item.heading = str;
  item.flags = gddContentItemFlagStatic | gddContentItemFlagNoRemove;  // dont take input
  item.SetRect(x, y, width, height);
  if (drawHandler)
    item.onDrawItem = drawHandler;
  contentArea.AddContentItem(item, gddItemDisplayInSidebar);
  return item;
}

function gotoToday() {
  var d = new Date();
  selectDate(d.getFullYear(), d.getMonth(), d.getDate());
}

function gotoPrevDate() {
  var d = new Date(curYear, curMonth, curDate - 1);
  selectDate(d.getFullYear(), d.getMonth(), d.getDate());
}

function gotoNextDate() {
  var d = new Date(curYear, curMonth, curDate + 1);
  selectDate(d.getFullYear(), d.getMonth(), d.getDate());
}

function AddItem(str, x, y, width, height, drawHandler, detailsHandler, url, tooltip) {
  var item = new ContentItem();
  item.heading = str;
  item.snippet = str;
  item.tooltip = tooltip ? tooltip : str;
  item.flags = gddContentItemFlagNoRemove;
  item.SetRect(x, y, width, height);
  if (drawHandler)
    item.onDrawItem = drawHandler;
  if (detailsHandler)
    item.onDetailsView = detailsHandler;
  if (url) {
    item.onOpenItem = genDetails(url);
  }
    
  contentArea.AddContentItem(item, gddItemDisplayInSidebar);
  return item;
}

///////////////////////////////////////////////////////////////////////////////
// Many drawing helper functions
function DrawHeader(item, target, graphics, x, y, width, height) {
  graphics.DrawText(x, y, width, height, item.heading, gddColorNormalText, 
    gddTextFlagSingleLine|gddTextFlagVCenter|gddTextFlagCenter, gddFontNormal);
}

function DrawHeaderBkgndMain(item, target, graphics, x, y, width, height) {
  var curItemHeight = graphics.GetTextHeight("Z", 1000, 0, gddFontNormal) + 5;
  if (curItemHeight != itemHeight) {
    itemHeight = curItemHeight;
    drawCalendar();
    return;
  } 

  graphics.DrawRect(x-2, y-2, width+6, height+5, '#c3d9ff', '#c3d9ff');
}

function DrawHeaderBkgnd(item, target, graphics, x, y, width, height) {
  graphics.DrawRect(x, y, width, height, '#c3d9ff', '#c3d9ff');
  graphics.DrawText(x, y, width, height, item.heading, gddColorNormalText, 
    gddTextFlagSingleLine|gddTextFlagVCenter|gddTextFlagCenter, gddFontNormal);
}

function DrawTodayBkgnd(item, target, graphics, x, y, width, height) {
  graphics.DrawRect(x, y, width, height, '#557799', '#557799');
}

function DrawWeekendBkgnd(item, target, graphics, x, y, width, height) {
  graphics.DrawRect(x, y, width, height, "#E8EEF7", "#E8EEF7");
}

function DrawSelectedDayBkgnd(item, target, graphics, x, y, width, height) {
  graphics.DrawRect(x, y, width, height, "#808080", undefined);
}

function DrawItemCalWhite(item, target, graphics, x, y, width, height) {
  graphics.DrawText(x, y, width + 3, height, item.heading, '#FFFFFF', 
    gddTextFlagCenter|gddTextFlagSingleLine|gddTextFlagVCenter, gddFontNormal);
}

function DrawItemCalBold(item, target, graphics, x, y, width, height) {
  graphics.DrawText(x, y, width + 3, height, item.heading, gddColorNormalText, 
    gddTextFlagCenter|gddTextFlagSingleLine|gddTextFlagVCenter, gddFontBold);
}

function DrawItemCal(item, target, graphics, x, y, width, height) {
  graphics.DrawText(x, y, width + 3, height, item.heading, gddColorNormalText, 
    gddTextFlagCenter|gddTextFlagSingleLine|gddTextFlagVCenter, gddFontNormal);
}

// Draw an underlined text item
function DrawItemLink(item, target, graphics, x, y, width, height) {
  graphics.DrawText(x, y, width, height, item.heading, '#0000CC', 
    gddTextFlagSingleLine, gddFontNormal);
  var textWidth = graphics.GetTextWidth(item.heading, 0, gddFontNormal);
  graphics.DrawLine(x, y + height, x + textWidth, y + height, '#0000CC');
}

function DrawItemWrap(item, target, graphics, x, y, width, height) {
  graphics.DrawText(x, y, width, height, item.heading, gddColorNormalText, 
    gddTextFlagWordBreak, gddFontNormal);
}
