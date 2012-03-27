var Cc = Components.classes;
var Ci = Components.interfaces;
var Cu = Components.utils;

Cu.import("resource://gre/modules/XPCOMUtils.jsm");

var nsIDOMWindowIface = Ci.nsIDOMWindow;
var nsIHttpChannelIface = Ci.nsIHttpChannel;
var nsIInterfaceRequestorIface = Ci.nsIInterfaceRequestor;
var nsIChannelIface = Ci.nsIChannel;

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
 } ; */
/*: {Ext with prototype : ComponentCollector } */  
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
    observerService : /*: cheat nsIObserverService*/undefined,

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

    observeRequest : /*:[ComponentCollector] nsISupports -> Undef*/function(aSubject)
    {
        if (!(aSubject instanceof nsIHttpChannelIface))
            return;

        if ((/*:cheat nsIHttpChannel*/aSubject).loadFlags & /*cheat nsIChannel*/nsIChannelIface.LOAD_INITIAL_DOCUMENT_URI)
        {
            var httpChannel = aSubject.QueryInterface(nsIHttpChannelIface);
            var win = this.getDomWindowForRequest(httpChannel);
            // if (!win || !win.wrappedJSObject)
            // {
            //     // This is not a request that originated from a DOM node (might be
            //     // a cert validation request, or a phishing protection request,
            //     // for instance). So we ignore it.
            //     return;
            // }

            // scanURL(aSubject, httpChannel.URI);
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

// // XPCOMUtils.generateNSGetFactory was introduced in Mozilla 2 (Firefox 4).
// // XPCOMUtils.generateNSGetModule is for Mozilla 1.9.x (Firefox 3).
// if (XPCOMUtils.generateNSGetFactory)
// {
//     var NSGetFactory = XPCOMUtils.generateNSGetFactory([ComponentCollectorService]);
// }
// else
// {
//     var NSGetModule = XPCOMUtils.generateNSGetModule([ComponentCollectorService]);
// }
// /*-----------------------------------------------------
//   Copyright (c) 2011 Hunter Paolini.  All Rights Reserved.
//   -----------------------------------------------------*/

// var procon =
// {
//     onLoad : function()
//     {
//         procon.common = Cu.import("chrome://procon/content/common.js", null).common;
//         procon.common.updateButtonElements();
//     },

//     onMenuItemCommand : function(e, args)
//     {
//         window.openDialog("chrome://procon/content/preferences.xul",
//             "procon-preferences",
//             "chrome,titlebar,toolbar,centerscreen,resizable,modal",
//             "mainPane",
//             args);
//     },

//     onToolbarButtonCommand : function(e)
//     {
//         procon.onMenuItemCommand(e, null);
//     },

//     onStatusbarButtonCommand : function(e)
//     {
//         procon.onMenuItemCommand(e, null);
//     },

//     openAbout : function(e)
//     {
//         Cu.import("resource://gre/modules/AddonManager.jsm");
//         AddonManager.getAddonByID("{9D6218B8-03C7-4b91-AA43-680B305DD35C}",
//             function(addon)
//             {
//                 openDialog("chrome://mozapps/content/extensions/about.xul", "", "chrome,centerscreen,modal", addon);
//             });
//     },

//     onNotificationPopupShowing : function(e)
//     {
//         var domainSessionMenuItem = document.getElementById("procon-notification-popup-domain-session");
//         var domainWhitelistMenuItem = document.getElementById("procon-notification-popup-domain-whitelist");

//         var URI = Cu.import("resource://procon/filter.jsm", null).publicObj.getBlockedURI();
//         var stringBundle = document.getElementById("procon-strings");

//         domainSessionMenuItem.setAttribute("label", stringBundle.getFormattedString("domainAllowTemp", [URI.host]));
//         domainWhitelistMenuItem.setAttribute("label", stringBundle.getFormattedString("domainAddWhitelist", [URI.host]));
//     },

//     allowPage : function(e)
//     {
//         if (!procon.common.authenticateUser())
//             return;

//         try
//         {
//             //var URI = window.content.document.baseURIObject;alert(URI.spec);
//             var publicObj = Cu.import("resource://procon/filter.jsm", null).publicObj;
//             var URI = publicObj.getBlockedURI();
//             publicObj.setWhitelistSessionPage(URI.spec);
//             window.content.location = URI.spec;
//         }
//         catch(err)
//         {
//             dump("Procon publicObj: " + err);
//         }
//     },

//     allowDomain : function(e)
//     {
//         if (!procon.common.authenticateUser())
//             return;

//         try
//         {
//             var publicObj = Cu.import("resource://procon/filter.jsm", null).publicObj;
//             var URI = publicObj.getBlockedURI();
//             publicObj.setWhitelistSessionDomain(URI.host);
//             window.content.location = URI.spec;
//         }
//         catch(err)
//         {
//             dump("Procon publicObj: " + err);
//         }
//     },

//     addBlacklistSite : function(el)
//     {
//         if (!procon.common.authenticateUser())
//             return;

//         var prompts = Cc["@mozilla.org/embedcomp/prompt-service;1"].getService(Ci.nsIPromptService);
//         var check =
//         {
//             value : false
//         };
//         var input =
//         {
//             value : window.content.document.baseURIObject.host || ""
//         };
//         var result = prompts.prompt(null, el.label, document.getElementById("procon-strings").getString("siteBlockPrompt"), input, null, check);

//         if (!result)
//             return;

//         var prefs = Cc["@mozilla.org/preferences-service;1"].getService(Ci.nsIPrefService);

//         var value = prefs.getComplexValue("extensions.procon.blacklist.sites", Ci.nsISupportsString).data;
//         var complex_value = Cc["@mozilla.org/supports-string;1"].createInstance(Ci.nsISupportsString);
//         complex_value.data = value + "\n" + input.value;
//         prefs.setComplexValue("extensions.procon.blacklist.sites", Ci.nsISupportsString, complex_value);

//         try
//         {
//             Cu.import("resource://procon/filter.jsm", null).publicObj.updatePrefs();
//         }
//         catch(e)
//         {
//             dump("Procon publicObj: " + e);
//         }
//     },

//     addBlacklistWord : function(el)
//     {
//         if (!procon.common.authenticateUser())
//             return;

//         var prompts = Cc["@mozilla.org/embedcomp/prompt-service;1"].getService(Ci.nsIPromptService);

//         var check =
//         {
//             value : false
//         };
//         var input =
//         {
//             value : window.content.document.getSelection() || ""
//         };
//         var result = prompts.prompt(null, el.label, document.getElementById("procon-strings").getString("wordBlockPrompt"), input, null, check);
 
//         if (!result)
//             return;
 
//         var prefs = Cc["@mozilla.org/preferences-service;1"].getService(Ci.nsIPrefService);
 
//         var value = prefs.getComplexValue("extensions.procon.blacklist.words", Ci.nsISupportsString).data;
//         var complex_value = Cc["@mozilla.org/supports-string;1"].createInstance(Ci.nsISupportsString);
//         complex_value.data = value + "\n" + input.value;
//         prefs.setComplexValue("extensions.procon.blacklist.words", Ci.nsISupportsString, complex_value);

//         try
//         {
//             Cu.import("resource://procon/filter.jsm", null).publicObj.updatePrefs();
//         }
//         catch(e)
//         {
//             dump("Procon publicObj: " + e);
//         }
//     },

//     addWhitelistSite : function(el)
//     {
//         if (!procon.common.authenticateUser())
//             return;

//         var prompts = Cc["@mozilla.org/embedcomp/prompt-service;1"].getService(Ci.nsIPromptService);
//         var check =
//         {
//             value : false
//         };
//         var input =
//         {
//             value : window.content.document.baseURIObject.host || ""
//         };
//         var result = prompts.prompt(null, el.label, document.getElementById("procon-strings").getString("siteTrustPrompt"), input, null, check);

//         if (!result)
//             return;

//         var prefs = Cc["@mozilla.org/preferences-service;1"].getService(Ci.nsIPrefService);

//         var value = prefs.getComplexValue("extensions.procon.whitelist.sites", Ci.nsISupportsString).data;
//         var complex_value = Cc["@mozilla.org/supports-string;1"].createInstance(Ci.nsISupportsString);
//         complex_value.data = value + "\n" + input.value;
//         prefs.setComplexValue("extensions.procon.whitelist.sites", Ci.nsISupportsString, complex_value);

//         try
//         {
//             Cu.import("resource://procon/filter.jsm", null).publicObj.updatePrefs();
//         }
//         catch(e)
//         {
//             dump("Procon publicObj: " + e);
//         }
//     },

//     addProfanitylistWord : function(el)
//     {
//         if (!procon.common.authenticateUser())
//             return;

//         var prompts = Cc["@mozilla.org/embedcomp/prompt-service;1"].getService(Ci.nsIPromptService);
//         var check =
//         {
//             value : false
//         };
//         var input =
//         {
//             value : window.content.document.getSelection() || ""
//         };
//         var result = prompts.prompt(null, el.label, document.getElementById("procon-strings").getString("wordCensorPrompt"), input, null, check);

//         if (!result)
//             return;

//         var prefs = Cc["@mozilla.org/preferences-service;1"].getService(Ci.nsIPrefService);

//         var value = prefs.getComplexValue("extensions.procon.profanitylist.words", Ci.nsISupportsString).data;
//         var complex_value = Cc["@mozilla.org/supports-string;1"].createInstance(Ci.nsISupportsString);
//         complex_value.data = value + "\n" + input.value;
//         prefs.setComplexValue("extensions.procon.profanitylist.words", Ci.nsISupportsString, complex_value);

//         try
//         {
//             Cu.import("resource://procon/filter.jsm", null).publicObj.updatePrefs();
//         }
//         catch(e)
//         {
//             dump("Procon publicObj: " + e);
//         }
//     }
// };

// window.addEventListener("load", procon.onLoad, false);

// /*-----------------------------------------------------
//   Copyright (c) 2011 Hunter Paolini.  All Rights Reserved.
//   -----------------------------------------------------*/

// procon.onFirefoxLoad = function(event)
// {
//     var Prefs = Cc["@mozilla.org/preferences-service;1"].getService(Ci.nsIPrefService);
//     Prefs = Prefs.getBranch("extensions.procon.");

//     // clean unecessary prefs from previous versions
//     if (Prefs.getCharPref("currentVersion") < "3.0")
//     {
//         Cu.import("resource://procon/houseKeeping.jsm", null).houseKeeping();
//         Cu.import("chrome://procon/content/common.js", null).common.updateButtonElements();
//     }

//     // update subscriptions every 72 hours
//     if (Prefs.getBoolPref("subscriptions.enabled"))
//     {
//         var date = new Date();
//         var time = date.getTime() / 1000;
//         var lastUpdateTime = Prefs.getIntPref("subscriptions.lastUpdateTime");

//         if (((time - lastUpdateTime) / 3600) > 72)
//         {
//             (new (Cu.import("resource://procon/subscriptions.jsm", null).subscriptions)).update();
//             Cu.import("resource://procon/filter.jsm", null).publicObj.updatePrefs();
//         }
//     }

//     document.addEventListener("DOMContentLoaded", procon.contentListener, false);
//     document.addEventListener("DOMContentLoaded", procon.configProtectionListener, false);

//     if (Prefs.prefHasUserValue("general.password"))
//         document.getElementById("helpSafeMode").disabled = true;

//     try
//     {
//         Cu.import("resource://gre/modules/AddonManager.jsm");
//         AddonManager.addAddonListener(procon.addonProtectionListener);
//     }
//     catch(ex)
//     {
//     }
// };

// procon.onFirefoxUnload = function(event)
// {
//     document.removeEventListener("DOMContentLoaded", procon.contentListener, false);
// };

// procon.contentListener = Cu.import("resource://procon/filter.jsm", null).contentListener;

// procon.configProtectionListener = function(event)
// {
//     var loc = event.target.location;

//     if (!loc)
//         return;

//     loc = loc.href.toLowerCase();
//     if (((loc == "about:config" || loc == "chrome://global/content/config.xul") && !Cu.import("chrome://procon/content/common.js", null).common.authenticateUser()) || loc.indexOf("://procon/") != - 1)
//         event.target.location = "about:blank";
// };

// procon.addonProtectionListener =
// {
//     onUninstalling : function(addon)
//     {
//         if (addon.id == "{9D6218B8-03C7-4b91-AA43-680B305DD35C}" && !Cu.import("chrome://procon/content/common.js", null).common.authenticateUser())
//         {
//             AddonManager.getAddonByID("{9D6218B8-03C7-4b91-AA43-680B305DD35C}", function(addon) { addon.cancelUninstall(); });
//         }
//         else
//         {
//             var Prefs = Cc["@mozilla.org/preferences-service;1"].getService(Ci.nsIPrefService);
//             Prefs.clearUserPref("nglayout.initialpaint.delay");
//         }
//     },

//     onDisabling : function(addon)
//     {
//         if (addon.id == "{9D6218B8-03C7-4b91-AA43-680B305DD35C}" && !Cu.import("chrome://procon/content/common.js", null).common.authenticateUser())
//             AddonManager.getAddonByID("{9D6218B8-03C7-4b91-AA43-680B305DD35C}", function(addon) { addon.userDisabled = false; });
//     }
// };

// window.addEventListener("load", procon.onFirefoxLoad, false);
// window.addEventListener("unload", procon.onFirefoxUnload, false);

// function(event) { procon.onMenuItemCommand(event, null); };
// function(event) { procon.onToolbarButtonCommand(event); };
// function(event) { procon.onNotificationPopupShowing(event); };
// function(event) { procon.allowPage(); };
// function(event) { procon.allowDomain(); };
// function(event) { procon.addWhitelistSite(this); };
// function(event) { procon.onMenuItemCommand(event); };
// function(event) { procon.openAbout(); };
// function(event) { procon.onStatusbarButtonCommand(); };
// function(event) { procon.addBlacklistSite(this); };
// function(event) { procon.addBlacklistWord(this); };
// function(event) { procon.addWhitelistSite(this); };
// function(event) { procon.addProfanitylistWord(this); };
// function(event) { procon.addBlacklistSite(this); };
// function(event) { procon.addBlacklistWord(this); };
// function(event) { procon.addWhitelistSite(this); };
// function(event) { procon.addProfanitylistWord(this); };
// function(event) { procon.onStatusbarButtonCommand(); };
