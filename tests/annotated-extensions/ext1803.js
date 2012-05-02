/*:: type proconCommonType = {Ext with
  updateButtonElements : -> Undef,
  authenticateUser : -> Bool
};

type proconCommonImport = {Ext with
  common : proconCommonType
};

type publicObjType = {Ext with
  updatePrefs : -> Undef,
  getBlockedURI : -> nsIURI,
  setWhitelistSessionPage : Str -> Undef,
  setWhitelistSessionDomain : Str -> Undef,
};

type publicObjImport = {Ext with
  publicObj : publicObjType
};

type prefsObjType = {Ext with
  getPrefByType : Str -> (Num + Str + Bool + Null),
  setPrefByType : Unsafe
};

type prefsObjImport = {Ext with
  preferences : prefsObjType
};


type addonListenerType = {Ext with
  onUninstalling : Unsafe,
  onDisabling : Ext -> Undef
};

type proconType = {Ext with
  onLoad : -> Undef,
  onMenuItemCommand : (nsIDOMEvent + Undef) * Ext -> Undef,
  onToolbarButtonCommand : nsIDOMEvent -> Undef,
  onStatusbarButtonCommand : (nsIDOMEvent + Undef) -> Undef,
  openAbout : (nsIDOMEvent + Undef) -> Undef,
  onNotificationPopupShowing : nsIDOMEvent -> Undef,
  allowPage : (nsIDOMEvent + Undef) -> Undef,
  allowDomain : (nsIDOMEvent + Undef) -> Undef,
  addBlacklistSite : Unsafe,
  addBlacklistWord : Unsafe,
  addWhitelistSite : Unsafe,
  addProfanitylistWord : Unsafe,
  onFirefoxLoad : Unsafe,
  onFirefoxUnload : nsIDOMEvent -> Undef,
  contentListener : nsIDOMEvent -> Undef,
  configProtectionListener : nsIDOMEvent -> Undef,
  addonProtectionListener : addonListenerType,
  common : proconCommonType
};

*/
var Cc = Components.classes;
var Ci = Components.interfaces;
var Cu = Components.utils;

Cu.import("resource://gre/modules/XPCOMUtils.jsm");

var nsIDOMWindowIface = Ci.nsIDOMWindow;
var nsIHttpChannelIface = Ci.nsIHttpChannel;
var nsIInterfaceRequestorIface = Ci.nsIInterfaceRequestor;
var nsIChannelIface = Ci.nsIChannel;

var AddonManager = /*: {Ext with addAddonListener : addonListenerType -> Undef, getAddonByID : Str * (Ext -> Undef) -> Undef } */null;

/**
 * ComponentCollector Constructor.
 * @constructor
 */


/*:: type ComponentCollector = rec c . {AnObject with 
  _xpcom_categories : Array<Ext>,
  observerService : nsIObserverService,
  QueryInterface : QueryInterfaceType,
  initializeComponent : [this('c)] -> Undef,
  deinitializeComponent : [this('c)] -> Undef,
  observe : [this('c)] nsISupports * Str * Str -> Undef,
  observeRequest : [this('c)] nsISupports -> Undef,
  getInterfaceForRequest : [this('c)] nsILoadGroup * nsIJSIID -> (nsISupports + Null),
  getDomWindowForRequest : [this('c)] nsIHttpChannel -> nsIDOMWindow,
 } ;

type ComponentCollectorConstructor = {Ext with prototype : ComponentCollector };

type xpcomUtils = {Ext with
  generateNSGetFactory : Array<ComponentCollectorConstructor> -> Ext,
  generateNSGetModule : Array<ComponentCollectorConstructor> -> Ext
};*/
var XPCOMUtils = /*: xpcomUtils */null;

/* BEGIN resource://procon/preferences.jsm */
// NEAL: I needed to modify portions of this to work properly with the type system.
/*-----------------------------------------------------
  Copyright (c) 2011 Hunter Paolini.  All Rights Reserved.
  -----------------------------------------------------*/

var EXPORTED_SYMBOLS = /*: Array<Str> */["preferences"];

var preferences = /*: prefsObjType */
{
    getPrefByType : function(name)
    {
        var pref = Cc["@mozilla.org/preferences-service;1"].getService(Ci.nsIPrefBranch);
        switch (pref.getPrefType(name))
        {
            case pref.PREF_BOOL :
                return pref.getBoolPref(name);
            case pref.PREF_INT :
                return pref.getIntPref(name);
            case pref.PREF_STRING :
                try
                {
                    return pref.getComplexValue(name, Ci.nsIPrefLocalizedString).data;
                }
                catch(e)
                {
                    return pref.getComplexValue(name, Ci.nsISupportsString).data;
                }
        }
        return null;
    },

    setPrefByType : /*: cheat @Unsafe */function(name, value)
    {
        var pref = Cc["@mozilla.org/preferences-service;1"].getService(Ci.nsIPrefBranch);
        switch (pref.getPrefType(name))
        {
            case pref.PREF_BOOL :
                var value2 = /^true$/i.test(value);
                pref.setBoolPref(name, value2);
                break;
            case pref.PREF_INT :
                pref.setIntPref(name, parseInt(value, 10));
                break;
            case pref.PREF_STRING :
                var str = Cc["@mozilla.org/supports-string;1"].createInstance(Ci.nsISupportsString);
                str.data = value;
                pref.setComplexValue(name, Ci.nsISupportsString, str);
                break;
            default :
                break;
        }
    }
};
/* END resource://procon/preferences.jsm */

/* BEGIN resource://procon/houseKeeping.jsm */
/*-----------------------------------------------------
  Copyright (c) 2011 Hunter Paolini.  All Rights Reserved.
  -----------------------------------------------------*/

var EXPORTED_SYMBOLS = ["houseKeeping"];

let currentVersion = "3.3";

/*: cheat @Unsafe */function houseKeeping()
{
    let preferences = (/*:cheat prefsObjImport*/(Components.utils.import("resource://procon/preferences.jsm", null))).preferences;
    let PrefsS = Components.classes["@mozilla.org/preferences-service;1"].getService(Components.interfaces.nsIPrefService);
    let Prefs_old = PrefsS.getBranch("procon.");
    let Prefs = PrefsS.getBranch("extensions.procon.");

    let prefNames = /*: cheat {Ext with /^("-*- code -*-"|"__proto__")/ :! Str} */
    {
        // booleans
        "whitelist.enabled" : "enableWhiteList",
        "blacklist.enabled" : "enabled",
        "blacklist.advanced.limitNetAccess" : "bat",
        "blacklist.advanced.renderDelay" : "pDelay",
        "blacklist.advanced.examineMeta" : "exMeta",
        "blacklist.advanced.showDetails" : "reason",
        "blacklist.advanced.customWarning" : "customWarn",      
        "blacklist.advanced.redirect" : "pcust",      
        "profanitylist.enabled" : "filteron",
        "misc.showStatusButton" : "pstatus",
        "misc.showMenuButton" : "htm",
        //"blacklist.words.enabled" : "",
        //"blacklist.sites.enabled" : "" 

        // strings
        "profanitylist.placeholder" : "customCens",
        "whitelist.sites" : "whiteList",
        "blacklist.sites" : "urlregex",
        "blacklist.advanced.customWarningMsg" : "WarningMsg",
        "blacklist.words" : "wordregex",
        "profanitylist.words" : "wordlist",
        "blacklist.advanced.redirectURL" : "psit",
        //"general.password" : "password"
    };

    for (var newName in prefNames)
    {
        let oldName = prefNames[/*: cheat realisticStr */newName];
        if (Prefs_old.prefHasUserValue(oldName))
        {
            var oldValue = preferences.getPrefByType("procon." + oldName);

            if (oldName == "htm")
                oldValue = !oldValue;

            preferences.setPrefByType("extensions.procon." + newName, '' + oldValue);
            Prefs_old.clearUserPref(oldName);
        }
    }

    if (Prefs_old.prefHasUserValue("password"))
    {
        Prefs.setCharPref("general.password", Prefs_old.getCharPref("password"));
        Prefs_old.clearUserPref("password");
    }

    if (Prefs_old.prefHasUserValue("action"))
    {
        switch (Prefs_old.getIntPref("action"))
        {
            case 0 :
                Prefs.setBoolPref("blacklist.words.enabled", true);
                Prefs.setBoolPref("blacklist.sites.enabled", false);
                break;
            case 1 :
                Prefs.setBoolPref("blacklist.words.enabled", false);
                Prefs.setBoolPref("blacklist.sites.enabled", true);
                break;
            case 2 :
                Prefs.setBoolPref("blacklist.words.enabled", true);
                Prefs.setBoolPref("blacklist.sites.enabled", true);
                break;
        }
        Prefs_old.clearUserPref("action");
    }

    if (Prefs_old.prefHasUserValue("authenticated"))
        Prefs_old.clearUserPref("authenticated");

    if (Prefs_old.prefHasUserValue("addons"))
        Prefs_old.clearUserPref("addons");

    Prefs.setCharPref("currentVersion", currentVersion);

    (/*: cheat publicObjImport*/Components.utils.import("resource://procon/filter.jsm", null)).publicObj.updatePrefs();
}
/* END resource://procon/houseKeeping.jsm */

/* BEGIN chrome://procon/content/third_party/md5.js */
/*
 * A JavaScript implementation of the RSA Data Security, Inc. MD5 Message
 * Digest Algorithm, as defined in RFC 1321.
 * Version 2.1 Copyright (C) Paul Johnston 1999 - 2002.
 * Other contributors: Greg Holt, Andrew Kepert, Ydnar, Lostinet
 * Distributed under the BSD License
 * See http://pajhome.org.uk/crypt/md5 for more info.
 */

/*
 * Configurable variables. You may need to tweak these to be compatible with
 * the server-side, but the defaults work in most cases.
 */
var hexcase = 0;  /* hex output format. 0 - lowercase; 1 - uppercase        */
var chrsz   = 8;  /* bits per input character. 8 - ASCII; 16 - Unicode      */

/*
 * Add integers, wrapping at 2^32. This uses 16-bit operations internally
 * to work around bugs in some JS interpreters.
 */
/*: Num * Num -> Num */function safe_add(x, y)
{
  var lsw = (x & 0xFFFF) + (y & 0xFFFF);
  var msw = (x >> 16) + (y >> 16) + (lsw >> 16);
  return (msw << 16) | (lsw & 0xFFFF);
}

/*
 * Bitwise rotate a 32-bit number to the left.
 */
/*: Num * Num -> Num */function bit_rol(num, cnt)
{
  return (num << cnt) | (num >>> (32 - cnt));
}

/*
 * These functions implement the four basic operations the algorithm uses.
 */
/*: Num * Num * Num * Num * Num * Num -> Num */function md5_cmn(q, a, b, x, s, t)
{
  return safe_add(bit_rol(safe_add(safe_add(a, q), safe_add(x, t)), s),b);
}
/*: Num * Num * Num * Num * Num * Num * Num -> Num */function md5_ff(a, b, c, d, x, s, t)
{
  return md5_cmn((b & c) | ((~b) & d), a, b, x, s, t);
}
/*: Num * Num * Num * Num * Num * Num * Num -> Num */function md5_gg(a, b, c, d, x, s, t)
{
  return md5_cmn((b & d) | (c & (~d)), a, b, x, s, t);
}
/*: Num * Num * Num * Num * Num * Num * Num -> Num */function md5_hh(a, b, c, d, x, s, t)
{
  return md5_cmn(b ^ c ^ d, a, b, x, s, t);
}
/*: Num * Num * Num * Num * Num * Num * Num -> Num */function md5_ii(a, b, c, d, x, s, t)
{
  return md5_cmn(c ^ (b | (~d)), a, b, x, s, t);
}

/*
 * Calculate the MD5 of an array of little-endian words, and a bit length
 */
/*: Array<Num> * Num -> Array<Num>*/function core_md5(x, len)
{
  /* append padding */
  x[len >> 5] |= 0x80 << ((len) % 32);
  x[(((len + 64) >>> 9) << 4) + 14] = len;

  var a =  1732584193;
  var b = -271733879;
  var c = -1732584194;
  var d =  271733878;

  for(var i = 0; i < x.length; i += 16)
  {
    var olda = a;
    var oldb = b;
    var oldc = c;
    var oldd = d;

    a = md5_ff(a, b, c, d, x[i+ 0], 7 , -680876936);
    d = md5_ff(d, a, b, c, x[i+ 1], 12, -389564586);
    c = md5_ff(c, d, a, b, x[i+ 2], 17,  606105819);
    b = md5_ff(b, c, d, a, x[i+ 3], 22, -1044525330);
    a = md5_ff(a, b, c, d, x[i+ 4], 7 , -176418897);
    d = md5_ff(d, a, b, c, x[i+ 5], 12,  1200080426);
    c = md5_ff(c, d, a, b, x[i+ 6], 17, -1473231341);
    b = md5_ff(b, c, d, a, x[i+ 7], 22, -45705983);
    a = md5_ff(a, b, c, d, x[i+ 8], 7 ,  1770035416);
    d = md5_ff(d, a, b, c, x[i+ 9], 12, -1958414417);
    c = md5_ff(c, d, a, b, x[i+10], 17, -42063);
    b = md5_ff(b, c, d, a, x[i+11], 22, -1990404162);
    a = md5_ff(a, b, c, d, x[i+12], 7 ,  1804603682);
    d = md5_ff(d, a, b, c, x[i+13], 12, -40341101);
    c = md5_ff(c, d, a, b, x[i+14], 17, -1502002290);
    b = md5_ff(b, c, d, a, x[i+15], 22,  1236535329);

    a = md5_gg(a, b, c, d, x[i+ 1], 5 , -165796510);
    d = md5_gg(d, a, b, c, x[i+ 6], 9 , -1069501632);
    c = md5_gg(c, d, a, b, x[i+11], 14,  643717713);
    b = md5_gg(b, c, d, a, x[i+ 0], 20, -373897302);
    a = md5_gg(a, b, c, d, x[i+ 5], 5 , -701558691);
    d = md5_gg(d, a, b, c, x[i+10], 9 ,  38016083);
    c = md5_gg(c, d, a, b, x[i+15], 14, -660478335);
    b = md5_gg(b, c, d, a, x[i+ 4], 20, -405537848);
    a = md5_gg(a, b, c, d, x[i+ 9], 5 ,  568446438);
    d = md5_gg(d, a, b, c, x[i+14], 9 , -1019803690);
    c = md5_gg(c, d, a, b, x[i+ 3], 14, -187363961);
    b = md5_gg(b, c, d, a, x[i+ 8], 20,  1163531501);
    a = md5_gg(a, b, c, d, x[i+13], 5 , -1444681467);
    d = md5_gg(d, a, b, c, x[i+ 2], 9 , -51403784);
    c = md5_gg(c, d, a, b, x[i+ 7], 14,  1735328473);
    b = md5_gg(b, c, d, a, x[i+12], 20, -1926607734);

    a = md5_hh(a, b, c, d, x[i+ 5], 4 , -378558);
    d = md5_hh(d, a, b, c, x[i+ 8], 11, -2022574463);
    c = md5_hh(c, d, a, b, x[i+11], 16,  1839030562);
    b = md5_hh(b, c, d, a, x[i+14], 23, -35309556);
    a = md5_hh(a, b, c, d, x[i+ 1], 4 , -1530992060);
    d = md5_hh(d, a, b, c, x[i+ 4], 11,  1272893353);
    c = md5_hh(c, d, a, b, x[i+ 7], 16, -155497632);
    b = md5_hh(b, c, d, a, x[i+10], 23, -1094730640);
    a = md5_hh(a, b, c, d, x[i+13], 4 ,  681279174);
    d = md5_hh(d, a, b, c, x[i+ 0], 11, -358537222);
    c = md5_hh(c, d, a, b, x[i+ 3], 16, -722521979);
    b = md5_hh(b, c, d, a, x[i+ 6], 23,  76029189);
    a = md5_hh(a, b, c, d, x[i+ 9], 4 , -640364487);
    d = md5_hh(d, a, b, c, x[i+12], 11, -421815835);
    c = md5_hh(c, d, a, b, x[i+15], 16,  530742520);
    b = md5_hh(b, c, d, a, x[i+ 2], 23, -995338651);

    a = md5_ii(a, b, c, d, x[i+ 0], 6 , -198630844);
    d = md5_ii(d, a, b, c, x[i+ 7], 10,  1126891415);
    c = md5_ii(c, d, a, b, x[i+14], 15, -1416354905);
    b = md5_ii(b, c, d, a, x[i+ 5], 21, -57434055);
    a = md5_ii(a, b, c, d, x[i+12], 6 ,  1700485571);
    d = md5_ii(d, a, b, c, x[i+ 3], 10, -1894986606);
    c = md5_ii(c, d, a, b, x[i+10], 15, -1051523);
    b = md5_ii(b, c, d, a, x[i+ 1], 21, -2054922799);
    a = md5_ii(a, b, c, d, x[i+ 8], 6 ,  1873313359);
    d = md5_ii(d, a, b, c, x[i+15], 10, -30611744);
    c = md5_ii(c, d, a, b, x[i+ 6], 15, -1560198380);
    b = md5_ii(b, c, d, a, x[i+13], 21,  1309151649);
    a = md5_ii(a, b, c, d, x[i+ 4], 6 , -145523070);
    d = md5_ii(d, a, b, c, x[i+11], 10, -1120210379);
    c = md5_ii(c, d, a, b, x[i+ 2], 15,  718787259);
    b = md5_ii(b, c, d, a, x[i+ 9], 21, -343485551);

    a = safe_add(a, olda);
    b = safe_add(b, oldb);
    c = safe_add(c, oldc);
    d = safe_add(d, oldd);
  }
  return [a, b, c, d];

}

/*
 * Convert a string to an array of little-endian words
 * If chrsz is ASCII, characters >255 have their hi-byte silently ignored.
 */
/*: Str -> Array<Num> */function str2binl(str)
{
  var bin = /*:cheat Array<Num>*/Array();
  var mask = (1 << chrsz) - 1;
  for(var i = 0; i < str.length * chrsz; i += chrsz)
    bin[i>>5] |= (str.charCodeAt(i / chrsz) & mask) << (i%32);
  return bin;
}

/*
 * Convert an array of little-endian words to a hex string.
 */
/*: Array<Num> -> Str */function binl2hex(binarray)
{
  var hex_tab = hexcase ? "0123456789ABCDEF" : "0123456789abcdef";
  var str = /*:Str*/"";
  for(var i = 0; i < binarray.length * 4; i++)
  {
    str += hex_tab.charAt((binarray[i>>2] >> ((i%4)*8+4)) & 0xF) +
           hex_tab.charAt((binarray[i>>2] >> ((i%4)*8  )) & 0xF);
  }
  return str;
}

/*
 * These are the functions you'll usually want to call
 * They take string arguments and return either hex or base-64 encoded strings
 */
/*: Str -> Str*/function hex_md5(s){ return binl2hex(core_md5(str2binl(s), s.length * chrsz));}

/* END chrome://procon/content/third_party/md5.js */
/* BEGIN resource://procon/filter.jsm */
/*-----------------------------------------------------
  Copyright (c) 2011 Hunter Paolini.  All Rights Reserved.
  -----------------------------------------------------*/

/**
 * @fileOverview Code to examine output of Web documents
 */
var EXPORTED_SYMBOLS = ["contentListener", "scanURL", "publicObj", "formatList", "LIST_TYPE"];

const Cc = Components.classes;
const Ci = Components.interfaces;

const nsITimerIface = Ci.nsITimer;
const nsIDOMXPathResultIface = Ci.nsIDOMXPathResult;
const nsIDOMHTMLStyleElementIface = Ci.nsIDOMHTMLStyleElement;
const nsIDOMHTMLScriptElementIface = Ci.nsIDOMHTMLScriptElement;
const nsIWebNavigationIface = Ci.nsIWebNavigation;

/**
 * Preference branch
 */
let PrefsS = Cc["@mozilla.org/preferences-service;1"].getService(Ci.nsIPrefService);
var Prefs = PrefsS.getBranch("extensions.procon.");

/**
 * String bundle
 */
const stringBundleService = Cc["@mozilla.org/intl/stringbundle;1"].getService(Ci.nsIStringBundleService);
let stringBundle = stringBundleService.createBundle("chrome://procon/locale/overlay.properties");

/**
 * Schemes ignored by the filter
 */
let ignoredSchemes =
{
    resource   : true,
    about      : true,
    chrome     : true,
    data       : true,
    javascript : true
};

/**
 * Latest blocked URI
 */
let blockedURI = /*: nsIURI */null;

/*:: 
type numDict = {Ext with /^("-*- code -*-"|"__proto__")/ :! Num};

type cacheObjType = {Ext with
  blacklist : numDict,
  whitelist : numDict
};*/
/**
 * Cache of visited addresses
 */
var cacheObj = /*: [cacheObjType] -> cacheObjType */function()
{
    this.blacklist = /*: cheat numDict */
    {
    };
    
    this.whitelist = /*: cheat numDict */
    {
    };
};

/**
 * Types of lists allowed
 */
const LIST_TYPE =
{
    SITES : 0,
    WORDS : 1
};

/**
 * Prepare strings containing unicode characters
 */
/*: Str * Num -> Str */function formatList(str, type)
{
    let list = /*:Str*/"";

    // convert unicode characters
    for (let i = 0, len = str.length; i < len; i++)
    {
        let character = str.charCodeAt(i);
        if (character > 127)
        {
            let hexVal = Number(character).toString(16);
            list += "\\u" + ("000" + hexVal).match(/.{4}$/)[0];
            continue;
        }
        list += str.charAt(i);
    }

    // remove trailing newlines
    list = "\n" + list + "\n";
    let array = list.split(/[\r\n]+/);
    array.pop();
    array.shift();

    list = (type === LIST_TYPE.WORDS)
        ? "(?:^|\\b|\\s)(?:" + array.join("|") + ")(?:$|\\b|\\s)"
        : array.join("|");

    return list;
}

/*:: type blacklistObjInst = {Ext with
  enabled : Bool,
  sites : RegExp,
  words : RegExp,
  sites_enabled : Bool,
  words_enabled : Bool,
  advanced_limitNetAccess : Bool,
  advanced_showDetails : Bool,
  advanced_customWarning : Bool,
  advanced_customWarningMsg : Str,
  advanced_redirect : Bool,
  advanced_redirectURL : Str,
  advanced_examineMeta : Bool
}; */
/**
 * Blacklist preferences object
 */
var blacklistObj = /*: [blacklistObjInst] -> blacklistObjInst */function()
{
    this.enabled = Prefs.getBoolPref("blacklist.enabled");

    let subscriptions_sitesA = /*: Array<Ext>*/[], subscriptions_wordsA = /*: Array<Ext>*/[];

    if (Prefs.getBoolPref("subscriptions.enabled"))
    {
        let subscriptions_sitesObjJ = Prefs.getComplexValue("subscriptions.blacklist.sites", Ci.nsISupportsString).data;
        let subscriptions_wordsObjJ = Prefs.getComplexValue("subscriptions.blacklist.words", Ci.nsISupportsString).data;

        var subscriptions_sitesObj = JSON.parse(subscriptions_sitesObjJ);
        var subscriptions_wordsObj = JSON.parse(subscriptions_wordsObjJ);

        for (var i in subscriptions_sitesObj)
            subscriptions_sitesA.push(subscriptions_sitesObj[/*:cheat realisticStr*/i]);

        for (var i in subscriptions_wordsObj)
            subscriptions_wordsA.push(subscriptions_wordsObj[/*:cheat realisticStr*/i]);
 
        var subscriptions_sites = (subscriptions_sitesA.length)
            ? "|" + subscriptions_sitesA.join("|")
            : "";
        var subscriptions_words = (subscriptions_wordsA.length)
            ? "|" + subscriptions_wordsA.join("|")
            : "";
    }

    let sites = Prefs.getComplexValue("blacklist.sites", Ci.nsISupportsString).data;
    let words = Prefs.getComplexValue("blacklist.words", Ci.nsISupportsString).data;
  
    this.sites = new RegExp(formatList(sites, LIST_TYPE.SITES) + subscriptions_sites, "gi");
    this.words = new RegExp(formatList(words, LIST_TYPE.WORDS) + subscriptions_words, "gi");
    this.sites_enabled = Prefs.getBoolPref("blacklist.sites.enabled");
    this.words_enabled = Prefs.getBoolPref("blacklist.words.enabled");

// NEAL: WE DON'T SUPPORT DELETE
//    delete sites;
//    delete words;
//    delete subscriptions_sites;
//    delete subscriptions_words;

    //advanced preferences
    this.advanced_limitNetAccess   = Prefs.getBoolPref("blacklist.advanced.limitNetAccess");
    this.advanced_showDetails      = Prefs.getBoolPref("blacklist.advanced.showDetails");
    this.advanced_customWarning    = Prefs.getBoolPref("blacklist.advanced.customWarning");
    this.advanced_customWarningMsg = Prefs.getComplexValue("blacklist.advanced.customWarningMsg", Ci.nsISupportsString).data;
    this.advanced_redirect         = Prefs.getBoolPref("blacklist.advanced.redirect");
    this.advanced_redirectURL      = Prefs.getComplexValue("blacklist.advanced.redirectURL", Ci.nsISupportsString).data;
    this.advanced_examineMeta      = Prefs.getBoolPref("blacklist.advanced.examineMeta");
};

/*:: type constructor whitelistConstructor = [whitelistObjInst] -> whitelistObjInst
and prototype whitelist = {AnObject with
  session : {AnObject with
    pages : numDict,
    domains : numDict
  }
}
and instance whitelistObjInst = {Ext with
  enabled : Bool,
  sites : RegExp,
}; */
/**
 * Whitelist preferences object
 */
var whitelistObj = /*: whitelistConstructor */function()
{
    this.enabled = Prefs.getBoolPref("whitelist.enabled");
    let subscriptions_sitesA = /*: Array<Ext>*/[];
 
    if (Prefs.getBoolPref("subscriptions.enabled"))
    {
        let subscriptions_sitesObjP = Prefs.getComplexValue("subscriptions.whitelist.sites", Ci.nsISupportsString).data;
        var subscriptions_sitesObj = JSON.parse(subscriptions_sitesObjP);

        for (var i in subscriptions_sitesObj)
            subscriptions_sitesA.push(subscriptions_sitesObj[/*:cheat realisticStr*/i]);
 
        var subscriptions_sites = (subscriptions_sitesA.length)
            ? "|" + subscriptions_sitesA.join("|")
            : "";
    }
 
    let sites = Prefs.getComplexValue("whitelist.sites", Ci.nsISupportsString).data;

    if (this.enabled && Prefs.getBoolPref("blacklist.advanced.redirect"))
    {
        this.sites = new RegExp(formatList(Prefs.getComplexValue("blacklist.advanced.redirectURL", Ci.nsISupportsString).data
                        + "\n" + sites, LIST_TYPE.SITES) + subscriptions_sites, "gi");
    }
    else if (Prefs.getBoolPref("blacklist.advanced.redirect"))
    {
        // BUGFIX: http://proconlatte.com/bugs/view.php?id=7
        this.sites = new RegExp(formatList(Prefs.getComplexValue("blacklist.advanced.redirectURL", Ci.nsISupportsString).data, LIST_TYPE.SITES), "gi");
        this.enabled = true;
    }
    else
    {
        this.sites = new RegExp(formatList(sites, LIST_TYPE.SITES) + subscriptions_sites, "gi");
    }

// NEAL: WE DON'T SUPPORT DELETE
//    delete sites;
//    delete subscriptions_sites;
};

// establish session access for blocked sites 
whitelistObj.prototype.session = /*:cheat {AnObject with pages:numDict, domains:numDict}*/
{
    pages : /*: numDict */
    {
    },

    domains : /*: numDict */
    {
    }
};

/*:: type profanitylistObjInst = {Ext with
  enabled : Bool,
  words : RegExp,
  placeholder : Str
}; */
/**
 * Profanity list preferences object
 */
var profanitylistObj = /*: [profanitylistObjInst] -> profanitylistObjInst */function()
{
    this.enabled = Prefs.getBoolPref("profanitylist.enabled");
    let subscriptions_wordsA = /*:Array<Ext>*/[];

    if (Prefs.getBoolPref("subscriptions.enabled"))
    {
        let subscriptions_wordsObjJ = Prefs.getComplexValue("subscriptions.profanitylist.words", Ci.nsISupportsString).data;
        var subscriptions_wordsObj = JSON.parse(subscriptions_wordsObjJ);

        for (var i in subscriptions_wordsObj)
            subscriptions_wordsA.push(subscriptions_wordsObj[/*:cheat realisticStr*/i]);
 
        var subscriptions_words = (subscriptions_wordsA.length)
            ? "|" + subscriptions_wordsA.join("|")
            : "";
    }

    let words = Prefs.getComplexValue("profanitylist.words", Ci.nsISupportsString).data;

    this.words = new RegExp(formatList(words, LIST_TYPE.WORDS) + subscriptions_words, "gi");

// NEAL: WE DON'T SUPPORT DELETES
//    delete words;
//    delete subscriptions_words;

    // regex might strip the first space sometimes
    this.placeholder = " " + Prefs.getComplexValue("profanitylist.placeholder", Ci.nsISupportsString).data;
};

let blacklist     = /*: blacklistObjInst */new blacklistObj();
let whitelist     = /*: whitelistObjInst*/new whitelistObj();
let profanitylist = /*: profanitylistObjInst */new profanitylistObj();
let cache         = /*: cacheObjType*/new cacheObj();

/**
 * Public functions
 */
var publicObj = /*: publicObjType */
{
    updatePrefs : function()
    {
        blacklist     = new blacklistObj();
        whitelist     = new whitelistObj();
        profanitylist = new profanitylistObj();
        cache         = new cacheObj();
    },

    getBlockedURI : function()
    {
        return blockedURI;
    },

    setWhitelistSessionPage : function(url)
    {
        whitelist.session.pages[/*: cheat realisticStr */decodeURIComponent(url)] = 1;
    },

    setWhitelistSessionDomain : function(url)
    {
        whitelist.session.domains[/*: cheat realisticStr */decodeURIComponent(url)] = 1;
    }
};

/**
 * Check whether the whitelist contains the URI parameters
 */
/*: Str * realisticStr -> Bool */function inWhitelist(host, spec)
{
    if (typeof whitelist.session.domains[host] !== "undefined" || typeof whitelist.session.pages[spec] !== "undefined")
    {
        return true;
    }
    else if (whitelist.enabled)
    {
        let match = -1;

        //get or store from cache
        if (typeof cache.whitelist[spec] !== "undefined")
        {
            match = cache.whitelist[spec];
        }
        else
        {
            match = spec.search(whitelist.sites);

            //prevent bypass of whitelist keywords via parameters
            if (match !== -1)
            {
                let prematchStr = spec.substr(0, match);
                if (prematchStr.indexOf("?") !== -1 || prematchStr.indexOf("#") !== -1)
                    match = -1;
            }
            cache.whitelist[spec] = match;
        }

        if (match !== -1)
            return true;

    }

    return false;
}

/*:: type notificationBox = {Ext with
  removeAllNotifications : Bool -> Undef,
    appendNotification : Str * Ext * Str * Num * Array<Ext> -> Undef,
  PRIORITY_WARNING_HIGH : Num
};

type specialBrowser = {Ext with
  getNotificationBox : nsIDOMHTMLDocument -> notificationBox
}; */
/**
 * Returns browser window
 */
/*: -> specialBrowser */function getBrowser()
{
    let wm = Cc["@mozilla.org/appshell/window-mediator;1"].getService(Ci.nsIWindowMediator);
    return (/*:cheat specialBrowser*/(wm.getMostRecentWindow("navigator:browser").getBrowser()));
}

/**
 * Notifies the user of blocked page
 */
/*: nsIWebNavigation * nsIURI * Str * Bool -> Undef */function filteredNode(doc, URI, msg, isContent)
{
    let win = getBrowser();
    //let win = doc.defaultView;
    let contentBrwsr = /*:cheat nsIDOMHTMLDocument*/(win.getBrowserForDocument(doc));

    if (isContent)
    {
        //stop loading
        if (contentBrwsr !== null)
            contentBrwsr.stop();

        let docEl = (/*:cheat Mutable<nsIDOMHTMLElement> */(doc.documentElement));
        if (docEl !== null)
        {
            let newEl = docEl.cloneNode(false);
            newEl.innerHTML = '';
            docEl.parentNode.replaceChild(newEl, docEl);
        }

        if (blacklist.advanced_redirect)
        {
            //var contentBrwsr = gBrowser.getBrowserForDocument(doc.defaultView.top.document);
            contentBrwsr.loadURI(blacklist.advanced_redirectURL, nsIWebNavigationIface.LOAD_FLAGS_NONE, null, null, null); // BUG? THIS HAD ONLY ONE ARG
            return; //no need for notification box
        }
    }
    else if (blacklist.advanced_redirect)
    {
        doc.loadURI(blacklist.advanced_redirectURL, nsIWebNavigationIface.LOAD_FLAGS_NONE, null, null, null);
        return;
    }

    let notificationBox = win.getNotificationBox(contentBrwsr);
    notificationBox.removeAllNotifications(false); //remove open notification boxes

    blockedURI = URI;

    let msg2 = (blacklist.advanced_customWarning)
        ? blacklist.advanced_customWarningMsg + " " + msg
        : stringBundle.GetStringFromName("unavailablePage") + " " + msg;
    let button = /*: Array<Ext>*/[{
        label: stringBundle.GetStringFromName("options"),
        accessKey: null,
        popup: "procon-notification-popup",
        callback: null,
    }];

    notificationBox.appendNotification(msg2, null, "chrome://global/skin/icons/blacklist_favicon.png", notificationBox.PRIORITY_WARNING_HIGH, button);  
}

/*:: type constructor filterConstructor = [filterInst] nsIWebNavigation * nsIDOMXPathResult -> filterInst
and prototype filterProto = {AnObject with
  traceDoc : [filterInst] Num -> Undef,
  filter : filterInst -> Undef
}
and instance filterInst = {Ext with
  i : Num,
  t : Str,
  pause : Num,
  nodesPerBatch : Num,
  doc : nsIWebNavigation,
  elements : nsIDOMXPathResult
}; */
/**
 * Content filter
 */
var contentFilter = /*: filterConstructor */function(doc, elements)
{
    this.i = 0;
    this.t = "";
    this.pause = 3;
    this.nodesPerBatch = 20;
    this.doc = doc;
    this.elements = elements;
};

contentFilter.prototype.traceDoc = /*: [filterInst] Num -> Undef */function(ms)
{
    var timer = Cc["@mozilla.org/timer;1"].createInstance(nsITimerIface), _that = this;
    timer.initWithCallback(
        (/*: filterInst -> (nsITimer-> Undef)*/function(_that)
        {
            return function(ignored) /* BUG: IT'S SUPPOSED TO ACCEPT A SINGLE ARG */
            {
                _that.filter(_that);
            };
        })(_that), ms, nsITimerIface.TYPE_ONE_SHOT);
};

contentFilter.prototype.filter = function(_that)
{
    var el = /*: Mutable<nsIDOMCharacterData> */null, data = /*: Str */"", loopCount = 0;

    while ((el=/*: cheat Mutable<nsIDOMCharacterData> */_that.elements.snapshotItem(_that.i++)) && (loopCount <= _that.nodesPerBatch))
    {
        var pn = el.parentNode;

        if (pn === null || pn.nodeType === 9 || pn.nodeType === 11 
            || (pn.nodeType === 1 && !(pn instanceof nsIDOMHTMLScriptElementIface || pn instanceof nsIDOMHTMLStyleElementIface)))
        {
            data += el.data + " ";
        }

        loopCount++;
    }

    _that.t += data + " ";

    if (el != null)
    {
        _that.i--;
        _that.traceDoc(_that.pause);
    }
    else
    {
        var match = _that.t.search(blacklist.words);
        if (match !== -1)
        {
            let msg = (blacklist.advanced_showDetails)
                ? stringBundle.GetStringFromName("contentMatched") + " \"" + _that.t.substr(match,20) + "\\u2026\""
                : "";
            filteredNode(_that.doc, (/*: cheat nsIURI */(_that.doc.baseURIObject)), msg, true);
        }
    }
};

/**
 * Profanity filter
 */
var profanityFilter = /*: filterConstructor */function(doc, elements)
{
    this.i = 0;
    this.pause = 3;
    this.nodesPerBatch = 20;
    this.doc = doc;
    this.elements = elements;
};

profanityFilter.prototype.traceDoc = /*: [filterInst] Num -> Undef */function(ms)
{
    var timer = Cc["@mozilla.org/timer;1"].createInstance(nsITimerIface), _that = this;
    timer.initWithCallback((/*: filterInst -> (nsITimer-> Undef)*/function(_that)
        {
            return function(ignored) /* BUG: IT'S SUPPOSED TO ACCEPT A SINGLE ARG */
            {
                _that.filter(_that);
            };
        })(_that), ms, nsITimerIface.TYPE_ONE_SHOT);
};

profanityFilter.prototype.filter = function(_that)
{
    var el = /*: Mutable<nsIDOMCharacterData> */null, data = /*: Str*/"", loopCount = 0;

    while ((el=/*: cheat Mutable<nsIDOMCharacterData> */_that.elements.snapshotItem(_that.i++)) && (loopCount <= _that.nodesPerBatch))
    {
        var pn = el.parentNode;
        if (pn === null || pn.nodeType === 9 || pn.nodeType === 11
            || (pn.nodeType === 1 && !(pn instanceof nsIDOMHTMLScriptElementIface || pn instanceof nsIDOMHTMLStyleElementIface)))
        {
            data = el.data;
            if (data !== null)
            {
                // FIX for bug, but I don't like it since I'm still
                // not sure why replace() causes the cursor to move
                // BUG: http://proconlatte.com/bugs/view.php?id=6
                var match = data.search(profanitylist.words);
                if (match !== -1)
                    el.data = data.replace(profanitylist.words, profanitylist.placeholder);
            }
        }

        loopCount++;
    }

    if (el != null)
    {
        _that.i--;
        _that.traceDoc(_that.pause);
    }
};

/**
 * URL filtering
 */
/*: nsIHttpAuthenticableChannel * nsIURI -> Undef */function scanURL(aSubject, uri)
{
    if (blacklist.enabled && blacklist.sites_enabled && typeof ignoredSchemes[uri.scheme] === "undefined")
    {
        let URI_spec = /*: cheat realisticStr */decodeURIComponent(uri.spec);
        let URI_host = decodeURIComponent(uri.host);

        if (inWhitelist(URI_host, URI_spec))
            return;

        if (blacklist.advanced_limitNetAccess)
        {
            aSubject.cancel(/*: cheat Num*/Components.results.NS_ERROR_FAILURE); // Cancel the request
            let webNav = aSubject.notificationCallbacks.getInterface(nsIWebNavigationIface);
            let msg = (blacklist.advanced_showDetails)
                ? stringBundle.GetStringFromName("internetBlockEnabled")
                : "";
            filteredNode(webNav, uri, msg, false);
            return; // internet-wide block has been enabled in advanced prefs
        }

        let match = -1;

        //get or store result from cache
        if (typeof cache.blacklist[URI_spec] !== "undefined")
        {
            match = cache.blacklist[URI_spec];
        }
        else
        {
            match = URI_spec.search(blacklist.sites);
            cache.blacklist[URI_spec] = match;
        }

        if (match !== -1)
        {
            aSubject.cancel(/*: cheat Num*/Components.results.NS_ERROR_FAILURE); // Cancel the request
            let webNav = aSubject.notificationCallbacks.getInterface(nsIWebNavigationIface);
            let msg = (blacklist.advanced_showDetails)
                ? stringBundle.GetStringFromName("addressMatched") + " \"" + URI_spec.substr(match,20) + "\\u2026\""
                : "";
            filteredNode(webNav, uri, msg, false);
            return;
        }
    }
}

/**
 * Content listener
 */
/*: nsIDOMEvent -> Undef */function contentListener(event)
{
    let doc = /*: cheat nsIDOMHTMLDocument*/(event.target);
    let body = doc.body;

    if (!body || body.childElementCount === 0 || (blacklist.enabled === false && profanitylist.enabled === false))
        return;

    let elementsS = /*: cheat nsIDOMXPathResult */(doc.evaluate('//text()[normalize-space()]',
        body,
        null,
        nsIDOMXPathResultIface.ORDERED_NODE_SNAPSHOT_TYPE,
        null));

    let URI = (/*:cheat nsIURI*/(doc.baseURIObject));
    let URI_host = decodeURIComponent(URI.host);
    let URI_spec = /*: cheat realisticStr */decodeURIComponent(URI.spec);

    let scanContentAllowed = (blacklist.enabled 
        && blacklist.words_enabled 
        && typeof ignoredSchemes[URI.scheme] === "undefined" 
        && !(inWhitelist(URI_host, URI_spec)));

    if (scanContentAllowed)
    {
        // check meta tags
        if (blacklist.advanced_examineMeta)
        {
            let elementsS = /*: cheat nsIDOMXPathResult */(doc.evaluate('/html/head/meta[@name="description"]/@content',
                body,
                null,
                nsIDOMXPathResultIface.STRING_TYPE,
                null));
            var elements = elementsS.stringValue;
            var match = elements.search(blacklist.words);
            
            if (match !== -1)
            {
                let msg = (blacklist.advanced_showDetails)
                    ? stringBundle.GetStringFromName("metaTagMatched") + " \"" + elements.substr(match,20) + "\\u2026\""
                    : "";
                filteredNode(/*: cheat nsIWebNavigation*/doc, URI, msg, true);
                return;
            }
        }

        let cf = new contentFilter(/*: cheat nsIWebNavigation*/doc, elementsS);
        cf.traceDoc(0);
    }

    if (profanitylist.enabled)
    {
        let pf = new profanityFilter(/*: cheat nsIWebNavigation*/doc, elementsS);
        pf.traceDoc(0);
    }

    let timer = Cc["@mozilla.org/timer;1"].createInstance(nsITimerIface);
    timer.initWithCallback(
        /*: nsITimer-> Undef */function(ignored) /* BUG: FUNCTION NEEDS ONE ATTRIBUTE */
        {
/*
            body.addEventListener("DOMNodeInserted", 
            function(event)
            {
                let elements = doc.evaluate('descendant-or-self::text()[normalize-space()]',
                    event.target,
                    null,
                    nsIDOMXPathResultIface.ORDERED_NODE_SNAPSHOT_TYPE,
                    null);

                if (scanContentAllowed)
                {
                    let cf = new contentFilter(doc, elements);
                    cf.traceDoc(0);
                }

                if (profanitylist.enabled)
                {
                    let pf = new profanityFilter(doc, elements);
                    pf.traceDoc(0);
                }
            }, false);
*/
        }, 100, nsITimerIface.TYPE_ONE_SHOT);
}

/* END resource://procon/filter.jsm */
/* BEGIN resource://procon/subscriptions.jsm */
/*-----------------------------------------------------
  Copyright (c) 2011 Hunter Paolini.  All Rights Reserved.
  -----------------------------------------------------*/

var EXPORTED_SYMBOLS = ["subscriptions"];

const Cc = Components.classes;
const Ci = Components.interfaces;

let PrefsS = Cc["@mozilla.org/preferences-service;1"].getService(Ci.nsIPrefService);
var Prefs = PrefsS.getBranch("extensions.procon.subscriptions.");

/*:: type jsObjectInst = {Ext with
  regexReady : Bool,
  list : Str
};

type jsObjectWrapper = {Ext with
  /^("-*- code -*-"|"__proto__")/ :! jsObjectInst
};

type constructor subscriptionsConstructor = [subscriptionsInst] -> subscriptionsInst
and prototype subscriptionsProto = {AnObject with
  getFromURL : [subscriptionsInst] Str -> Bool,
  isValid : [subscriptionsInst] Str * Str -> Bool,
  save : Unsafe,
  update : Unsafe
}
and instance subscriptionsInst = {Ext with
  jsObject: {Ext with /^("-*- code -*-"|"__proto__")/ :! jsObjectWrapper}
};

*/
var subscriptions = /*: subscriptionsConstructor */ function()
{
    this.jsObject = /*: cheat {Ext with /^("-*- code -*-"|"__proto__")/ :! jsObjectWrapper} */
    {
    };
};

subscriptions.prototype.getFromURL = /*: [subscriptionsInst] Str -> Bool */function(url)
{
    var req = Cc["@mozilla.org/xmlextras/xmlhttprequest;1"].createInstance(Ci.nsIXMLHttpRequest);
    try
    {
        req.open('GET', url, false);
        req.overrideMimeType("text/plain");
        req.send(null);
        if(req.status == 200 || req.status == 0)
        {
            let jsStr = req.responseText;

            if (!this.isValid(jsStr, url))
            {
                dump("Invalid subscription: "+url+"\n");
            }
            else
            {
                return true;
            }
        }
        else
        {
            dump("Error loading subscription: "+url+"\n");
        }
    }
    catch (e)
    {
        return false;
    }

    return false;
};
  
subscriptions.prototype.isValid = /*: [subscriptionsInst] Str * Str -> Bool */function(jsStr, url)
{
    if (!jsStr)
        return false;
        
    var linebreak = /*: Str */"\n";
    if (/(\r\n?|\n\r?)/.test(jsStr))
        linebreak = RegExp.$1;
    
    var arr = jsStr.split(linebreak);

    if (!arr)
        return false;

    if(!(new RegExp("procon latte", "gi")).test(arr[0]))
        return false;

    let jsObject = /*: cheat {Ext with /^("-*- code -*-"|"__proto__")/ :! jsObjectInst} */
    {
    };

    var listRe = new RegExp("^(?:profanitylist\\.words|blacklist\\.(?:sites|words)|whitelist\\.sites)", "i");

    for (var i = 1, len = arr.length; i < len; i++)
    {
        var element = arr[i];

        if (element.length < 3 || /^\s*#/.test(element)) // NEAL: REGEX ENDED WITH /m, not / 
            continue;

        if (listRe.test(element))
        {
            var name = /*: cheat realisticStr */(element.match(listRe)[0]);
            var index = element.indexOf("=");

            if (index <= 0)
                continue;

            var value = /*: Array<Str> */[];
            var firstVal = element.substring(index + 1, element.length).replace(/^\s+|\s+$/g, "");
                
            if (firstVal.length > 0)
                value.push(firstVal);
            
            while (arr[++i] && arr[i].length > 0 && !(listRe.test(arr[i])))
                value.push(arr[i]);

            i--;

            let list = value.join("\n");
            
            jsObject[name] = /*: jsObjectInst */null;
            
            // check if list is already formatted as regex pattern
            if (/^\/.*\/$/g.test(list))
            {
                list = list.replace(/^\/|\/$/g, "");
                jsObject[name].regexReady = true;
            }
            
            jsObject[name].list = list;
        }
    }
    
    this.jsObject[/*: cheat realisticStr */encodeURIComponent(url)] = jsObject;
    return true;
};

subscriptions.prototype.save = /*: cheat @Unsafe */function(urls)
{
    Components.utils.import("resource://procon/filter.jsm");
 
    let blacklist_sitesJ = Prefs.getComplexValue("blacklist.sites", Ci.nsISupportsString).data;
    let blacklist_wordsJ = Prefs.getComplexValue("blacklist.words", Ci.nsISupportsString).data;
    let whitelist_sitesJ = Prefs.getComplexValue("whitelist.sites", Ci.nsISupportsString).data;
    let profanitylist_wordsJ = Prefs.getComplexValue("profanitylist.words", Ci.nsISupportsString).data;

    var e = /*:Ext*/null;
    try
    {
        var blacklist_sites = JSON.parse(blacklist_sitesJ);
        var blacklist_words = JSON.parse(blacklist_wordsJ);
        var whitelist_sites = JSON.parse(whitelist_sitesJ);
        var profanitylist_words = JSON.parse(profanitylist_wordsJ);
    }
    catch (e)
    {
        dump("Error saving subscriptions: " + e);
        return;
    }
 
    for (var anyI in this.jsObject)
    {
        var i = /*:cheat realisticStr */anyI;
        let obj = this.jsObject[i];     
        
        if (obj.hasOwnProperty("blacklist.sites"))
        {
            let bsObj = obj["blacklist.sites"];
            blacklist_sites[i] = (bsObj.hasOwnProperty("regexReady") && bsObj.regexReady)
                ? bsObj.list
                : formatList(bsObj.list, LIST_TYPE.SITES);
        }
        
        if (obj.hasOwnProperty("blacklist.words"))
        {
            let bwObj = obj["blacklist.words"];
            blacklist_words[i] = (bwObj.hasOwnProperty("regexReady") && bwObj.regexReady)
                ? bwObj.list
                : formatList(bwObj.list, LIST_TYPE.WORDS);
        }
        
        if (obj.hasOwnProperty("whitelist.sites"))
        {
            let wsObj = obj["whitelist.sites"];
            whitelist_sites[i] = (wsObj.hasOwnProperty("regexReady") && wsObj.regexReady)
                ? wsObj.list
                : formatList(wsObj.list, LIST_TYPE.SITES);
        }
        
        if (obj.hasOwnProperty("profanitylist.words"))
        {
            let pwObj = obj["profanitylist.words"];
            profanitylist_words[i] = (pwObj.hasOwnProperty("regexReady") && pwObj.regexReady)
                ? pwObj.list
                : formatList(pwObj.list, LIST_TYPE.WORDS);
        }
    }
    
    // remove old subscriptions
    let len = urls.length;
    for (var a in blacklist_sites)
    {
        let found = /*: Bool*/false;
        for (var j = 0; j < len; j++)
        {
            if (i == urls[j])
                found = true;
        }

// NEAL: WE DON'T SUPPORT DELETE
//        if (!found)
//            delete blacklist_sites[i];
    }

    for (var a in blacklist_words)
    {
        let found = /*:Bool*/false;
        for (var j = 0; j < len; j++)
        {
            if (i == urls[j])
                found = true;
        }

// NEAL: WE DON'T SUPPORT DELETE
//        if (!found)
//            delete blacklist_words[i];
    }

    for (var a in whitelist_sites)
    {
        let found = /*:Bool*/false;
        for (var j = 0; j < len; j++) {
            if (i == urls[j])
                found = true;
        }

// NEAL: WE DON'T SUPPORT DELETE
//        if (!found)
//            delete whitelist_sites[i];
    }
    
    for (var a in profanitylist_words)
    {
        let found = /*:Bool*/false;
        for (var j = 0; j < len; j++) {
            if (i == urls[j])
                found = true;
        }

// NEAL: WE DON'T SUPPORT DELETE
//        if (!found)
//            delete profanitylist_words[i];
    }

    // save as complex values
    let blacklist_sites_complex = Cc["@mozilla.org/supports-string;1"].createInstance(Ci.nsISupportsString);
    let blacklist_words_complex = Cc["@mozilla.org/supports-string;1"].createInstance(Ci.nsISupportsString);
    let whitelist_sites_complex = Cc["@mozilla.org/supports-string;1"].createInstance(Ci.nsISupportsString);
    let profanity_words_complex = Cc["@mozilla.org/supports-string;1"].createInstance(Ci.nsISupportsString);
    
    blacklist_sites_complex.data = JSON.stringify(blacklist_sites);
    blacklist_words_complex.data = JSON.stringify(blacklist_words);
    whitelist_sites_complex.data = JSON.stringify(whitelist_sites);
    profanity_words_complex.data = JSON.stringify(profanitylist_words);
    
    Prefs.setComplexValue("blacklist.sites", Ci.nsISupportsString, blacklist_sites_complex);
    Prefs.setComplexValue("blacklist.words", Ci.nsISupportsString, blacklist_words_complex);
    Prefs.setComplexValue("whitelist.sites", Ci.nsISupportsString, whitelist_sites_complex);
    Prefs.setComplexValue("profanitylist.words", Ci.nsISupportsString, profanity_words_complex);
};

subscriptions.prototype.update = /*: cheat @Unsafe */ function()
{
    try
    {
        let urls = /*: cheat Array<Str> */(JSON.parse(Prefs.getComplexValue("urls", Ci.nsISupportsString).data));

        for (var i = 0, len = urls.length; i < len; i++)
            this.getFromURL(decodeURIComponent(urls[i]));

        this.save(urls);
        
        let date = new Date();
        let time = date.getTime() / 1000;
        Prefs.setIntPref("lastUpdateTime", time);
    }
    catch(e)
    {
        return false;
    }

    //dump("ProCon: Subscriptions updated successfully...");
    return true;
};
/* END resource://procon/subscriptions.jsm */
/* BEGIN chrome://procon/content/common.js */
/*-----------------------------------------------------
  Copyright (c) 2011 Hunter Paolini.  All Rights Reserved.
  -----------------------------------------------------*/

let Cc = Components.classes;
let Ci = Components.interfaces;
let Cu = Components.utils;

const common = /*: proconCommonType */
{
    updateButtonElements : function()
    {
        let PrefsS = Cc["@mozilla.org/preferences-service;1"].getService(Ci.nsIPrefService);
        var Prefs = PrefsS.getBranch("extensions.procon.");

        var wm = Cc["@mozilla.org/appshell/window-mediator;1"].getService(Ci.nsIWindowMediator);
        var browserWindow = wm.getMostRecentWindow("navigator:browser");

        if (Prefs.getBoolPref("blacklist.enabled") || Prefs.getBoolPref("profanitylist.enabled"))
        {
            browserWindow.document.getElementById("procon-status-img").setAttribute("src", "chrome://procon/skin/images/security_small.png");
        }
        else
        {
            browserWindow.document.getElementById("procon-status-img").setAttribute("src", "chrome://procon/skin/images/security_small_gray.png");
        }

        (/*:cheat Mutable<nsIDOMXULElement>*/(browserWindow.document.getElementById("procon-status"))).hidden = !Prefs.getBoolPref("misc.showStatusButton");
        (/*:cheat Mutable<nsIDOMXULElement>*/(browserWindow.document.getElementById("procon-menu-button"))).hidden = !Prefs.getBoolPref("misc.showMenuButton");
    },

    authenticateUser : function()
    {
        let PrefsS = Cc["@mozilla.org/preferences-service;1"].getService(Ci.nsIPrefService);
        var Prefs = PrefsS.getBranch("extensions.procon.");

        if (Prefs.prefHasUserValue("general.password"))
        {
            var prompts = Cc["@mozilla.org/embedcomp/prompt-service;1"].getService(Ci.nsIPromptService);

            var wm = Cc["@mozilla.org/appshell/window-mediator;1"].getService(Ci.nsIWindowMediator);
            var browserWindow = wm.getMostRecentWindow("navigator:browser");
            var strings = /*:cheat nsIDOMXULStringBundleElement*/(browserWindow.document.getElementById("procon-strings"));

            var password = /*: Outparam<Str> */
            {
                value : ""
            },
            check = /*: Outparam<Bool> */
            {
                value : false
            }; //XXX: need to pass an object for the checkbox, even if hidden

            var password_result = prompts.promptPassword(null,
                strings.getString("passwordPromptTitle"),
                strings.getString("passwordPrompt"),
                password,
                '',
                check);

            if (!password_result)
                return false;

            if ((/*:cheat Ext*/Cu.import("chrome://procon/content/third_party/md5.js", null)).hex_md5(password.value) != Prefs.getCharPref("general.password"))
            {
                prompts.alert(null,
                    strings.getString("passwordPromptTitle"),
                    strings.getString("passwordPromptWrong"));
                return false;
            }
        }
        return true;
    }
};
/* END chrome://procon/content/common.js */


/*: ComponentCollectorConstructor */  
function ComponentCollectorService()
{
    Cu.import("resource://procon/filter.jsm");
}

ComponentCollectorService.prototype = /*:ComponentCollector*/{
    classID : Components.ID('{93909B7A-FCD8-11DF-A754-F892DFD72085}'),
    classDescription : 'ProCon Latte Content Filter Initializer',
    contractID : '@corvineum.org/startup;1',
    _xpcom_categories : [
        {
            category : 'profile-after-change'
        }],
    initializeComponent : /*:[ComponentCollector] -> Undef*/function()
    {
        this.observerService = Cc['@mozilla.org/observer-service;1'].getService(Ci.nsIObserverService);
        this.observerService.addObserver(this, 'xpcom-shutdown', false);
        this.observerService.addObserver(this, 'http-on-modify-request', false);
    },
    observerService : null,

    deinitializeComponent : /*: [ComponentCollector] -> Undef*/function()
    {
        this.observerService.removeObserver(this, 'http-on-modify-request');
        this.observerService.removeObserver(this, 'xpcom-shutdown');
    },

    observe : /*: [ComponentCollector] nsISupports * Str * Str -> Undef */function(aSubject, aTopic, aData)
    {
        if (aTopic == 'http-on-modify-request')
        {
            this.observeRequest(aSubject);
        }
        else if (aTopic == 'profile-after-change')
        {
            this.initializeComponent();
        }
        else if (aTopic == 'xpcom-shutdown')
        {
            this.deinitializeComponent();
        }
    },

    observeRequest : /*:[ComponentCollector] nsISupports -> Undef*/function(aSubj)
    {
        if (!(aSubj instanceof nsIHttpChannelIface))
            return;
      var aSubject = aSubj.QueryInterface(nsIHttpChannelIface);
      
        if (aSubject.loadFlags & nsIChannelIface.LOAD_INITIAL_DOCUMENT_URI)
        {
            var httpChannel = aSubject.QueryInterface(nsIHttpChannelIface);
            var win = this.getDomWindowForRequest(httpChannel);
            if (!win || !("wrappedJSObject" in win))
            {
                // This is not a request that originated from a DOM node (might be
                // a cert validation request, or a phishing protection request,
                // for instance). So we ignore it.
                return;
            }

            scanURL(/*: cheat nsIHttpAuthenticableChannel*/aSubject, httpChannel.URI);
        }
    },

    getInterfaceForRequest :
    /*: [ComponentCollector] nsILoadGroup * nsIJSIID -> (nsISupports + Null)*/
    function(request, iface)
    {
        /**
         * @param {nsISupports} supports The instance to retrieve the interface from.
         * @return {nsISupports} The associated interface instance, or null.
         */
         /*: nsISupports -> (nsISupports + Null)*/
        function getIface(supports)
        {
            if (supports == null)
                return null;

            if (!(supports instanceof nsIInterfaceRequestorIface))
                return null;

            var callbacks = supports.QueryInterface(nsIInterfaceRequestorIface);

            try
            {
                return callbacks.getInterface(iface);
            }
            catch (e)
            {
                return null;
            }

            return null; // BAD TYPECHECKER
        }

        var obj = getIface(request.notificationCallbacks);
        if (!obj && request.loadGroup != null)
        {
            // If we were unable to get the interface from the request
            // itself, try to get one from the request's load group (required
            // for XHRs).
            obj = getIface(request.loadGroup.groupObserver);
        }

        return obj;
    },

    getDomWindowForRequest : /*: [ComponentCollector] nsIHttpChannel -> nsIDOMWindow */function(request)
    {
        return /*:cheat nsIDOMWindow*/(this.getInterfaceForRequest(request, nsIDOMWindowIface));
    },

    // nsISupports interface implementation
    QueryInterface: /*:cheat QueryInterfaceType*/XPCOMUtils.generateQI([Ci.nsIObserver])
};

// XPCOMUtils.generateNSGetFactory was introduced in Mozilla 2 (Firefox 4).
// XPCOMUtils.generateNSGetModule is for Mozilla 1.9.x (Firefox 3).
if (XPCOMUtils.generateNSGetFactory)
{
    var NSGetFactory = XPCOMUtils.generateNSGetFactory([ComponentCollectorService]);
}
else
{
    var NSGetModule = XPCOMUtils.generateNSGetModule([ComponentCollectorService]);
}
/*-----------------------------------------------------
  Copyright (c) 2011 Hunter Paolini.  All Rights Reserved.
  -----------------------------------------------------*/

var procon = /*: proconType */null;
procon =
{
    onFirefoxLoad : /*: cheat @Unsafe */null,
    onFirefoxUnload : null,
    contentListener : null,
    configProtectionListener : null,
    addonProtectionListener : null,
    common : null,

    onLoad : function()
    {
        procon.common = (/*: cheat proconCommonImport*/Cu.import("chrome://procon/content/common.js", null)).common;
        procon.common.updateButtonElements();
    },

    onMenuItemCommand : function(e, args)
    {
        window.openDialog("chrome://procon/content/preferences.xul",
            "procon-preferences",
            "chrome,titlebar,toolbar,centerscreen,resizable,modal",
            "mainPane",
            args);
    },

    onToolbarButtonCommand : function(e)
    {
        procon.onMenuItemCommand(e, null);
    },

    onStatusbarButtonCommand : function(e)
    {
        procon.onMenuItemCommand(e, null);
    },

    openAbout : function(e)
    {
        Cu.import("resource://gre/modules/AddonManager.jsm");
        AddonManager.getAddonByID("{9D6218B8-03C7-4b91-AA43-680B305DD35C}",
            function(addon)
            {
                openDialog("chrome://mozapps/content/extensions/about.xul", "", "chrome,centerscreen,modal", addon);
            });
    },

    onNotificationPopupShowing : function(e)
    {
        var domainSessionMenuItem = document.getElementById("procon-notification-popup-domain-session");
        var domainWhitelistMenuItem = document.getElementById("procon-notification-popup-domain-whitelist");

        var URI = (/*: cheat publicObjImport*/Cu.import("resource://procon/filter.jsm", null)).publicObj.getBlockedURI();
        var stringBundle = (/*:cheat nsIDOMXULStringBundleElement*/document.getElementById("procon-strings"));

        domainSessionMenuItem.setAttribute("label", stringBundle.getFormattedString("domainAllowTemp", [URI.host]));
        domainWhitelistMenuItem.setAttribute("label", stringBundle.getFormattedString("domainAddWhitelist", [URI.host]));
    },

    allowPage : function(e)
    {
        if (!procon.common.authenticateUser())
            return;

        var err = /*:Ext*/null;
        try
        {
            //var URI = window.content.document.baseURIObject;alert(URI.spec);
            var publicObj = (/*:cheat publicObjImport*/Cu.import("resource://procon/filter.jsm", null)).publicObj;
            var URI = publicObj.getBlockedURI();
            publicObj.setWhitelistSessionPage(URI.spec);
            (/*: cheat Ext*/(window)).content.location = URI.spec;
        }
        catch(err)
        {
            dump("Procon publicObj: " + err);
        }
    },

    allowDomain : function(e)
    {
        if (!procon.common.authenticateUser())
            return;

        var err = /*:Ext*/null;
        try
        {
            var publicObj = (/*:cheat publicObjImport*/Cu.import("resource://procon/filter.jsm", null)).publicObj;
            var URI = publicObj.getBlockedURI();
            publicObj.setWhitelistSessionDomain(URI.host);
            (/*: cheat Ext*/(window)).content.location = URI.spec;
        }
        catch(err)
        {
            dump("Procon publicObj: " + err);
        }
    },

    addBlacklistSite : /*: cheat @Unsafe */function(el)
    {
        if (!procon.common.authenticateUser())
            return;

        var prompts = Cc["@mozilla.org/embedcomp/prompt-service;1"].getService(Ci.nsIPromptService);
        var check = /*: Outparam<Bool> */
        {
            value : false
        };
        var input = /*: Outparam<Str> */
        {
            value : (/*:cheat nsIURI*/window.content.document.baseURIObject).host || ""
        };
        var result = prompts.prompt(null, el.label, (/*:cheat nsIDOMXULStringBundleElement*/document.getElementById("procon-strings")).getString("siteBlockPrompt"), input, '', check);

        if (!result)
            return;

        var prefs = Cc["@mozilla.org/preferences-service;1"].getService(Ci.nsIPrefService);

        var value = prefs.getComplexValue("extensions.procon.blacklist.sites", Ci.nsISupportsString).data;
        var complex_value = Cc["@mozilla.org/supports-string;1"].createInstance(Ci.nsISupportsString);
        complex_value.data = value + "\n" + input.value;
        prefs.setComplexValue("extensions.procon.blacklist.sites", Ci.nsISupportsString, complex_value);

        var e = /*:Ext*/null;
        try
        {
            (/*:cheat publicObjImport*/Cu.import("resource://procon/filter.jsm", null)).publicObj.updatePrefs();
        }
        catch(e)
        {
            dump("Procon publicObj: " + e);
        }
    },

    addBlacklistWord : /*: cheat @Unsafe */function(el)
    {
        if (!procon.common.authenticateUser())
            return;

        var prompts = Cc["@mozilla.org/embedcomp/prompt-service;1"].getService(Ci.nsIPromptService);

        var check = /*: Outparam<Bool> */
        {
            value : false
        };
        var input = /*: Outparam<Str> */
        {
            value : ('' + window.content.document.getSelection()) || ""
        };
        var result = prompts.prompt(null, el.label, (/*:cheat nsIDOMXULStringBundleElement*/document.getElementById("procon-strings")).getString("wordBlockPrompt"), input, '', check);
 
        if (!result)
            return;
 
        var prefs = Cc["@mozilla.org/preferences-service;1"].getService(Ci.nsIPrefService);
 
        var value = prefs.getComplexValue("extensions.procon.blacklist.words", Ci.nsISupportsString).data;
        var complex_value = Cc["@mozilla.org/supports-string;1"].createInstance(Ci.nsISupportsString);
        complex_value.data = value + "\n" + input.value;
        prefs.setComplexValue("extensions.procon.blacklist.words", Ci.nsISupportsString, complex_value);
 
        var e = /*:Ext*/null;
        try
        {
            (/*:cheat publicObjImport*/Cu.import("resource://procon/filter.jsm", null)).publicObj.updatePrefs();
        }
        catch(e)
        {
            dump("Procon publicObj: " + e);
        }
    },

    addWhitelistSite : /*: cheat @Unsafe */function(el)
    {
        if (!procon.common.authenticateUser())
            return;

        var prompts = Cc["@mozilla.org/embedcomp/prompt-service;1"].getService(Ci.nsIPromptService);
        var check = /*: Outparam<Bool> */
        {
            value : false
        };
        var input = /*: Outparam<Str> */
        {
            value : (/*:cheat nsIURI*/window.content.document.baseURIObject).host || ""
        };
        var result = prompts.prompt(null, el.label, (/*:cheat nsIDOMXULStringBundleElement*/document.getElementById("procon-strings")).getString("siteTrustPrompt"), input, '', check);

        if (!result)
            return;

        var prefs = Cc["@mozilla.org/preferences-service;1"].getService(Ci.nsIPrefService);

        var value = prefs.getComplexValue("extensions.procon.whitelist.sites", Ci.nsISupportsString).data;
        var complex_value = Cc["@mozilla.org/supports-string;1"].createInstance(Ci.nsISupportsString);
        complex_value.data = value + "\n" + input.value;
        prefs.setComplexValue("extensions.procon.whitelist.sites", Ci.nsISupportsString, complex_value);

        var e = /*:Ext*/null;
        try
        {
            (/*:cheat publicObjImport*/Cu.import("resource://procon/filter.jsm", null)).publicObj.updatePrefs();
        }
        catch(e)
        {
            dump("Procon publicObj: " + e);
        }
    },

    addProfanitylistWord : /*: cheat @Unsafe */function(el)
    {
        if (!procon.common.authenticateUser())
            return;

        var prompts = Cc["@mozilla.org/embedcomp/prompt-service;1"].getService(Ci.nsIPromptService);
        var check = /*: Outparam<Bool> */
        {
            value : false
        };
        var input = /*: Outparam<Str> */
        {
            value : ('' + window.content.document.getSelection()) || ""
        };
        var result = prompts.prompt(null, el.label, (/*:cheat nsIDOMXULStringBundleElement*/document.getElementById("procon-strings")).getString("wordCensorPrompt"), input, '', check);

        if (!result)
            return;

        var prefs = Cc["@mozilla.org/preferences-service;1"].getService(Ci.nsIPrefService);

        var value = prefs.getComplexValue("extensions.procon.profanitylist.words", Ci.nsISupportsString).data;
        var complex_value = Cc["@mozilla.org/supports-string;1"].createInstance(Ci.nsISupportsString);
        complex_value.data = value + "\n" + input.value;
        prefs.setComplexValue("extensions.procon.profanitylist.words", Ci.nsISupportsString, complex_value);

        var e = /*:Ext*/null;
        try
        {
            (/*:cheat publicObjImport*/Cu.import("resource://procon/filter.jsm", null)).publicObj.updatePrefs();
        }
        catch(e)
        {
            dump("Procon publicObj: " + e);
        }
    }
};

window.addEventListener("load", procon.onLoad, false);

/*-----------------------------------------------------
  Copyright (c) 2011 Hunter Paolini.  All Rights Reserved.
  -----------------------------------------------------*/

procon.onFirefoxLoad = /*: cheat @Unsafe */function(event)
{
    var PrefsS = Cc["@mozilla.org/preferences-service;1"].getService(Ci.nsIPrefService);
    var Prefs = PrefsS.getBranch("extensions.procon.");

    // clean unecessary prefs from previous versions
    if (Prefs.getCharPref("currentVersion") < "3.0")
    {
        (/*: cheat Unsafe*/(Cu.import("resource://procon/houseKeeping.jsm", null).houseKeeping()));
        (/*: cheat proconCommonImport*/Cu.import("chrome://procon/content/common.js", null)).common.updateButtonElements();
    }

    // update subscriptions every 72 hours
    if (Prefs.getBoolPref("subscriptions.enabled"))
    {
        var date = new Date();
        var time = date.getTime() / 1000;
        var lastUpdateTime = Prefs.getIntPref("subscriptions.lastUpdateTime");

        if (((time - lastUpdateTime) / 3600) > 72)
        {
            (/*: cheat subscriptionsInst */(new (Cu.import("resource://procon/subscriptions.jsm", null).subscriptions))).update();
            (/*: cheat publicObjImport*/Cu.import("resource://procon/filter.jsm", null)).publicObj.updatePrefs();
        }
    }

    (/*: cheat nsIDOMDocument*/document).addEventListener("DOMContentLoaded", procon.contentListener, false);
    (/*: cheat nsIDOMDocument*/document).addEventListener("DOMContentLoaded", procon.configProtectionListener, false);

    if (Prefs.prefHasUserValue("general.password"))
        document.getElementById("helpSafeMode").disabled = true;

    try
    {
        Cu.import("resource://gre/modules/AddonManager.jsm");
        AddonManager.addAddonListener(procon.addonProtectionListener);
    }
    catch(ex)
    {
    }
};

procon.onFirefoxUnload = function(event)
{
    (/*: cheat nsIDOMDocument*/document).removeEventListener("DOMContentLoaded", procon.contentListener, false);
};

/*:: type contentListenerObj = {Ext with
  contentListener : nsIDOMEvent -> Undef
}; */
procon.contentListener = (/*: cheat contentListenerObj */Cu.import("resource://procon/filter.jsm", null)).contentListener;

procon.configProtectionListener = function(event)
{
    var loc1 = (/*:cheat nsIDOMDocument*/event.target).location;

    if (!loc1)
        return;

    var loc = loc1.href.toLowerCase();
    if (((loc == "about:config" || loc == "chrome://global/content/config.xul") && !(/*: cheat proconCommonImport*/Cu.import("chrome://procon/content/common.js", null)).common.authenticateUser()) || loc.indexOf("://procon/") != - 1)
        (/*:cheat Ext*/(event)).target.location = "about:blank";
};

procon.addonProtectionListener =
{
    onUninstalling : /*: cheat @Unsafe */function(addon)
    {
        if (addon.id == "{9D6218B8-03C7-4b91-AA43-680B305DD35C}" && !(/*: cheat proconCommonImport*/Cu.import("chrome://procon/content/common.js", null)).common.authenticateUser())
        {
            AddonManager.getAddonByID("{9D6218B8-03C7-4b91-AA43-680B305DD35C}", function(addon) { addon.cancelUninstall(); });
        }
        else
        {
            var Prefs = Cc["@mozilla.org/preferences-service;1"].getService(Ci.nsIPrefService);
            Prefs.clearUserPref("nglayout.initialpaint.delay");
        }
    },

    onDisabling : function(addon)
    {
        if (addon.id == "{9D6218B8-03C7-4b91-AA43-680B305DD35C}" && !(/*: cheat proconCommonImport*/Cu.import("chrome://procon/content/common.js", null)).common.authenticateUser())
            AddonManager.getAddonByID("{9D6218B8-03C7-4b91-AA43-680B305DD35C}", function(addon) { addon.userDisabled = false; });
    }
};

(/*:cheat Ext*/((/*:cheat nsIDOMWindow*/window).addEventListener("load", procon.onFirefoxLoad, false)));
(/*:cheat nsIDOMWindow*/window).addEventListener("unload", procon.onFirefoxUnload, false);

/*: nsIDOMEvent -> Undef */function(event) { procon.onMenuItemCommand(event, null); };
/*: nsIDOMEvent -> Undef */function(event) { procon.onToolbarButtonCommand(event); };
/*: nsIDOMEvent -> Undef */function(event) { procon.onNotificationPopupShowing(event); };
/*: nsIDOMEvent -> Undef */function(event) { procon.allowPage(); };
/*: nsIDOMEvent -> Undef */function(event) { procon.allowDomain(); };
/*: [nsIDOMXULLabeledControlElement] Ext -> Undef */function(event) { /*: cheat Ext*/(procon.addWhitelistSite(this)); };
/*: nsIDOMEvent -> Undef */function(event) { procon.onMenuItemCommand(event); };
/*: nsIDOMEvent -> Undef */function(event) { procon.openAbout(); };
/*: nsIDOMEvent -> Undef */function(event) { procon.onStatusbarButtonCommand(); };
/*: [nsIDOMXULLabeledControlElement] Ext -> Undef */function(event) { /*: cheat Ext*/(procon.addBlacklistSite(this)); };
/*: [nsIDOMXULLabeledControlElement] Ext -> Undef */function(event) { /*: cheat Ext*/(procon.addBlacklistWord(this)); };
/*: [nsIDOMXULLabeledControlElement] Ext -> Undef */function(event) { /*: cheat Ext*/(procon.addWhitelistSite(this)); };
/*: [nsIDOMXULLabeledControlElement] Ext -> Undef */function(event) { /*: cheat Ext*/(procon.addProfanitylistWord(this)); };
/*: [nsIDOMXULLabeledControlElement] Ext -> Undef */function(event) { /*: cheat Ext*/(procon.addBlacklistSite(this)); };
/*: [nsIDOMXULLabeledControlElement] Ext -> Undef */function(event) { /*: cheat Ext*/(procon.addBlacklistWord(this)); };
/*: [nsIDOMXULLabeledControlElement] Ext -> Undef */function(event) { /*: cheat Ext*/(procon.addWhitelistSite(this)); };
/*: [nsIDOMXULLabeledControlElement] Ext -> Undef */function(event) { /*: cheat Ext*/(procon.addProfanitylistWord(this)); };
/*: nsIDOMEvent -> Undef */function(event) { procon.onStatusbarButtonCommand(); };
