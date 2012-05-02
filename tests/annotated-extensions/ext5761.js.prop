var Cc = Components.classes;
var Ci = Components.interfaces;

var gBrowser = /*:{nsIDOMXULElement with
                 mCurrentBrowser : Ext,
                 getBrowserForDocument : nsIDOMDocument -> Ext }*/null;

var gContextMenu = /*:{AnObject with
                     onTextInput : Bool }*/ null;

var gSanitizePromptDialog = /*: {AnObject with
                              updatePrefs : -> Undef,
                              selectedTimespan :  Ext }*/ null;


// ------------------------- sanitize.js ---------------------------------------


/*:: type cacheObjType = {AnObject with
  time : Num,
  id : Str,
  text : Str,
  title : Str,
  }; */

/*:: type aNode = rec a . {nsIDOMHTMLElement with
  form : nsIDOMHTMLFormElement,
  tacacheDoc : nsIDOMDocument,
  tacacheID : Str,
  tacacheSubmit : Bool,
  tacacheOnSave : Bool,
  tacacheBackup : Str,
  tacacheWhitelist : {AnObject with match : Bool, time : Num},
  textContent : Str,
  waiting : Ext,
  value : Str,
  }; */

/*:: type CacheUtilType = rec u . {AnObject with
  gPref : nsIPrefBranch,
  prefService : nsIPrefService,
  nsISS : nsISupportsString_IID,
  prefString : Str,
  isDoc : Str,
  getPref : [this('u)] -> Array<cacheObjType>,
  setPref : Unsafe,
  clearCacheByRange : Unsafe,
  savePrefFile : Unsafe,
  }; */

/*:: type poType = rec p . {AnObject with
  _branch : nsIPrefBranch2,
  register : [this('p)] -> Undef,
  unregister : [this('p)] -> Undef,
  observe : [this('p)] nsISupports * Str * Str -> Undef,
  QueryInterface : QueryInterfaceType
  }; */

/*:: type tacType = rec t . {AnObject with
  _PBS : nsIPrivateBrowsingService,
  inPrivateBrowsing : [this('t)] -> True,
  cache : Array<cacheObjType>,
  maxTextSaved : Num,
  isSaving : Ext,
  writeInterval : Num,
  whitelist : Array<Str>,
  whitelistTime : Num,
  clearSetting : Num,
  _neverClear : Num,
  _restartClear : Num,
  _timeClear : Num,
  restartClearTime : Num,
  clearOldPrefByTime : Unsafe,
  pref : nsIPrefBranch,
  prefObserver : poType,
  getTopDoc : nsIDOMDocument -> nsIDOMDocument,
  getIndex : [this('t)] Str -> Num,
  getID : aNode -> Str,
  getWhitelist : [this('t)] -> Undef,
  updateSetting : [this('t)] -> Undef,
  writeToPref : Unsafe,
  prepareToSave : Unsafe,
  checkWhitelist : [this('t)] aNode -> Bool,
  beforeWrite : [this('t)] aNode -> Undef,
  saveToFile : Unsafe,
  prefToCache : [this('t)] -> Undef,
  cacheToPref : Unsafe,
  prepareCache : Unsafe,
  onInput : Unsafe,
  onChange : Unsafe,
  onKeypress : Unsafe,
  cacheUpdate : Unsafe,
  init : [this(Window)] -> Undef,
  exit : [Window] -> Undef,
  }; */

/*:: type tacUIType = rec t . {AnObject with
  WM : nsIWindowMediator,
  pref : nsIPrefBranch,
  checkReloadButton : [this('t)] -> Undef,
  checkStatusButton : [this('t)] -> Undef,
  checkUI: [this('t)] -> Undef,
  }; */

/*:: type tacCacheWindowType = rec t . {AnObject with
  copyButton : nsIDOMHTMLButtonElement,
  cache : Array<cacheObjType>,
  init : [this('t)] -> Undef,
  getTime : [this('t)] Date -> Str,
  mouseScroll : [this(nsIDOMEventTarget)] nsIDOMEvent -> Undef,
  exit : [this(Window)] -> Undef,
  setCopyButton : [this('t)] -> Undef,
  copyAndClose : [this('t)] -> Undef,
  pasteClipboard : [this('t)] -> Undef,
  confirm : [this('t)] Str * Str -> Bool,
  clearThis : Unsafe,
  clearAll : Unsafe,
  onContextMenuShowing : [this('t)] -> Undef,
  }; */

/*:: type tacPrefType = rec t . {AnObject with
  pref : nsIPrefBranch,
  init : [this('t)] -> Undef,
  }; */

var textareaCacheSanitize = /*: {Ext with
                              init : [this(Window)] -> Undef,
                              exit : Unsafe,
                              onDialogAccept : Unsafe,
                              } */ undefined;
var textareaCacheUtil = /*:CacheUtilType*/null;
var textareaCache = /*:tacType*/null;
var tacacheUI = /*:tacUIType*/null;
var cacheWindow = /*:tacCacheWindowType */null;
var tacachePref  = /*:tacPrefType */null;

textareaCacheSanitize =  {
    init : function () {
        /*: cheat False */window.addEventListener("dialogaccept", textareaCacheSanitize.onDialogAccept, false);

        if ( textareaCacheUtil.gPref.getBoolPref("extensions.tacache.clearWithSanitize") ) {
            let item = document.querySelector("#itemList > [preference$='formdata']");
            if ( textareaCacheUtil.getPref().length > 0 && item.disabled ) {
                item.disabled = false;
                item.checked = textareaCacheUtil.gPref.getBoolPref("privacy.cpd.formdata");
            }
        }
    },

    exit : /*: cheat Unsafe */function () {
        window.removeEventListener("dialogaccept", textareaCacheSanitize.onDialogAccept, false);
    },

    onDialogAccept : /*: cheat Unsafe */function () {
        gSanitizePromptDialog.updatePrefs();
        var s = /*: cheat Ext */(new Sanitizer());
        s.prefDomain = "privacy.cpd.";
        s.range = ((/*: cheat Ext */Sanitizer).getClearRange(gSanitizePromptDialog.selectedTimespan));

        if ( textareaCacheUtil.gPref.getBoolPref("privacy.cpd.formdata") &&
             textareaCacheUtil.gPref.getBoolPref("extensions.tacache.clearWithSanitize") ) {
            textareaCacheUtil.clearCacheByRange(/*: cheat Array<Num> */(s.range));
        }
    },

};

window.addEventListener("load", textareaCacheSanitize.init, false);
/*: cheat False */window.addEventListener("unload", textareaCacheSanitize.exit, false);

// -------------------------- common.js ----------------------------------------

// Using JSON
if (typeof(JSON) == "undefined") {
    Components.utils.import("resource://gre/modules/JSON.jsm");
    JSON.parse = JSON.fromString;
    JSON.stringify = JSON.toString;
}

// Define Common Utilities
textareaCacheUtil = {
    gPref : Components.classes["@mozilla.org/preferences-service;1"].getService(Components.interfaces.nsIPrefBranch),
    prefService : Components.classes["@mozilla.org/preferences-service;1"].getService(Components.interfaces.nsIPrefService),
    nsISS : Components.interfaces.nsISupportsString,
    prefString : "extensions.tacache.cache",
    isDoc : "<D>",

    getPref : /*:[this(CacheUtilType)] -> Array<cacheObjType>*/ function () {
        let cache = /*:Array<cacheObjType>*/[];
        let string = this.gPref.getComplexValue(this.prefString, this.nsISS).data;
        try {
            cache = /*: cheat Array<cacheObjType> */(JSON.parse(string));
        }
        catch (e) {}
        return cache;
    },

    // ** PASSES **
    setPref : /*: cheat Unsafe */ function (aCache) {
        var string = JSON.stringify(aCache);
        var supportsString = Components.classes["@mozilla.org/supports-string;1"].createInstance(this.nsISS);
        supportsString.data = string;

        // use setComplexValue to save unicode char
        this.gPref.setComplexValue(this.prefString, this.nsISS, supportsString);
    },

    // range: array contains 2 elements
    //   range[0]: start time
    //   range[1]: end time
    // if range == null, clear all items
    //
    // ** PASSES **
    clearCacheByRange : /*: cheat Unsafe */function (range) {
        let starttime = range ? range[0] : 0;
        let endtime   = range ? range[1] : Date.now() * 1000;
        let changed   = /*:Bool*/false;
        let cache     = this.getPref();

        for ( let i = cache.length - 1; i >= 0; i-- ) {
            let time = Math.abs( parseInt(cache[i].time) ) * 1000;
            if ( time >= starttime && time <= endtime ) {
                cache.splice(i, 1);
                changed = true;
            }
        }
        if (changed) {
            this.setPref(cache);
        }
    },

    // ** PASSES **
    savePrefFile : /*: cheat @Unsafe */ function () {
        this.prefService.savePrefFile(null);
    }
};

// --------------------------- tacache.js --------------------------------------


textareaCache = {

    // Private Browsing Service for Firefox 3.1 above
    _PBS : null,

    cache         : /*: Array<cacheObjType> */[],
    maxTextSaved  : 0,    // size of the cache

    isSaving      : null,
    saveInterval  : 60000, // interval for save profile file, 1 minute by default
    writeInterval : 5000,  // interval for writing into the cache, 5 seconds by default

    whitelist     : /*: Array<Str> */[],
    whitelistTime : 0,     // the time the whitelist is built

    clearSetting  : -1,    // could be _neverClear, _restartClear or _timeClear
    _neverClear   : 0,     // never clear old data unless the cache is full
    _restartClear : 1,     // clear old data in the next session
    _timeClear    : 2,     // clear old data after one or several days

    restartClearTime : 5,  // for clearSetting == _restartClear, clear old data 5 minutes after restarting Firefox

    pref : Components.classes["@mozilla.org/preferences-service;1"].getService(Components.interfaces.nsIPrefBranch),

    prefObserver : /*: poType */ {
        _branch : /*: nsIPrefBranch2 */null,
        register: /*: [poType] -> Undef */ function () {
            this._branch = Components.classes["@mozilla.org/preferences-service;1"].
                getService(Components.interfaces.nsIPrefService).getBranch("extensions.tacache.").QueryInterface(Components.interfaces.nsIPrefBranch2);
            this._branch.addObserver("", this, false);
        },

        unregister: /*: [poType] -> Undef */ function () {
            if (!this._branch)
                return;
            this._branch.removeObserver("", this);
        },

        observe: /*: [poType] nsISupports * Str * Str -> Undef */ function (aSubject, aTopic, aData) {
            if (aTopic == "nsPref:changed") {
                switch (aData) {
                case "whitelist" :
                    textareaCache.getWhitelist();
                    break;
                case "cache" :
                case "statusButton" :
                    tacacheUI.checkUI();
                    break;
                default :
                    textareaCache.updateSetting();
                    break;
                }
            }
        },

        QueryInterface: /*: cheat QueryInterfaceType */ XPCOMUtils.generateQI([Ci.nsIObserver])

    },

    /********** for Debug *******************/
    // msg : function (msg) {
    //   var d = new Date();
    //   Application.console.log( "Textarea Cache ("+d.toString() +") :" + msg );
    // },
    /*****************************************/

    // ORIGINALLY A GETTER
    inPrivateBrowsing : /*: [tacType] -> True */ function () {
        if (this._PBS)
            return ( this._PBS.privateBrowsingEnabled );
        else
            // BAD - used to be false
            return true;
    },

    saveToFile : /*: cheat Unsafe */ function () {
        this.isSaving = null;
        textareaCacheUtil.savePrefFile();
    },

    prefToCache : /*: [tacType] -> Undef */ function () {
        this.cache = textareaCacheUtil.getPref();
    },

    cacheToPref : /*: cheat Unsafe */ function () {
        textareaCacheUtil.setPref(this.cache);
    },

    setOldPref : function () {
        for ( var i = 0; i < this.cache.length; i++ )
            this.cache[i].time = - this.cache[i].time;
        this.cacheToPref();
    },

    clearOldPref : function () {
        for ( var i = this.cache.length - 1; i >= 0; i-- ) {
            if ( this.cache[i].time < 0 )
                this.cache.splice(i, 1);
        }
        this.cacheToPref();
    },

    clearOldPrefByTime : /*: cheat Unsafe */function () {
        if ( this.clearSetting != this._timeClear )
            return;
        var days = this.pref.getIntPref("extensions.tacache.clearTimeSpan");
        days = ( days < 1 ) ? 1 : days;
        // time limit removed since 0.8.6
        // days = ( days > 7 ) ? 7 : days; // one week maximum

        var endtime = Date.now() - days * 86400 * 1000; // 1 day = 86400 sec
        var range = [ 0, endtime * 1000 ];

        textareaCacheUtil.clearCacheByRange(range);
        this.prefToCache();
    },

    getTopDoc : /*: nsIDOMDocument -> nsIDOMDocument */ function (node) {
        let doc = node.ownerDocument ? node.ownerDocument : node;
        while ( doc.defaultView && doc.defaultView.parent.document != doc ) {
            doc = doc.defaultView.parent.document;
        }
        return doc;
    },

    getIndex :  function (id) {
        for ( var i = 0; i < this.cache.length; i++ ) {
            if ( id == this.cache[i].id )
                return i;
        }
        return -1;
    },

    getID : /*: aNode -> Str */ function (node) {
        let nodeName = node.nodeName.toLowerCase();
        let browser = gBrowser.getBrowserForDocument(node.tacacheDoc);
        let docShellID = browser.docShell.historyID;
        let index = browser.sessionHistory.index;

        let nodeID = node.id;
        if ( node.id == "" && "form" in node ) {
            try {
                nodeID = node.form.id;
            }
            catch (e) {}
        }

        var node2 = /*: nsIDOMHTMLElement */node;
        // XPath takes too much time, but offsetTop is not ideal
        if ( nodeID == "" ) {
            let offsetTop = node2.offsetTop;
            let count = 0;
            while ( "offsetParent" in node2 && node2.offsetParent && count++ < 3 ) { // for saving time, we iterate only 3 levels
                (/*: cheat Mutable<nsIDOMHTMLElement> */node2) = /*: cheat nsIDOMHTMLElement */(node2.offsetParent);
                offsetTop += node2.offsetTop;
            }
            nodeID = ''+offsetTop;
        }

        return docShellID + "-" + index + "#" + ( nodeName == "textarea" ? nodeID : textareaCacheUtil.isDoc );
    },

    writeToPref :  /*: cheat Unsafe */function (node) {
        this.prefToCache();

        if ( !node.tacacheID ) {
            (/*: cheat Mutable<aNode> */node).tacacheDoc = this.getTopDoc(/*: cheat nsIDOMDocument */node);
            (/*: cheat Mutable<aNode> */node).tacacheID = this.getID(node);
        }
        let tacacheID = node.tacacheID;
        let title = node.tacacheDoc.title;


        var text = /*: Str */"";
        if ( node.nodeName.toLowerCase() == "textarea" ) { //textarea
            text = (/*: cheat nsIDOMHTMLTextAreaElement */node).value;
        }
        else if ( node.nodeName.toLowerCase() == "html" ) { // WYSIWYG Editor
            text = (/*: cheat nsIDOMHTMLDocument */(node.ownerDocument)).body.innerHTML;
        }
        else // element.contentEditable == true
            text = node.innerHTML;

        let time = Date.now();
        var ix = this.getIndex(tacacheID);

        if ( text == "" ) {
            // the content in the input area has become empty after pressing Enter,
            // keep the old content and set a new id
            if ( node.tacacheSubmit ) {
                (/*: cheat Mutable<aNode> */node).tacacheSubmit = false;
                window.clearTimeout(node.waiting);
                text = node.tacacheBackup;
                tacacheID += time;
            }
            else
                return;
        }

        if ( ix != -1 ) {
            // not changed or empty, don't save
            if ( text == this.cache[ix].text )
                return;
            this.cache.splice(ix, 1);
        }

        var temp = /*: cacheObjType */{ title : title, text : text, id : tacacheID, time : time };
        if ( this.cache.unshift( temp ) > this.maxTextSaved )
            this.cache.pop();
        this.cacheToPref();
    },

    // Check white list
    // return value: true -- match
    //               false -- not match
    //
    checkWhitelist : /*: [tacType] aNode -> Bool */ function (node) {
        if ( this.whitelist == null )
            return false;

        // get the real URL of the page
        var docURL = this.getTopDoc(/*: cheat nsIDOMDocument */node).location.toString();

        var keyword = /*: Str */'';

        for ( var i = 0; i < this.whitelist.length; i++ ) {
            keyword = this.whitelist[i];
            if ( keyword != "" && (new RegExp(keyword)).test(docURL) ) {
                return true;
            }
        }
        return false;
    },

    beforeWrite : /*: [tacType] aNode -> Undef */ /* cheat Unsafe */ function (node) {
        (/*: cheat Mutable<aNode> */node).tacacheOnSave = false;

        // don't do anything if broswering in private mode and the user wants to disable Textarea Cache
        if ( this.inPrivateBrowsing() )
            return;

        // in whitelist
        if ( !node.tacacheWhitelist || ( node.tacacheWhitelist.time < this.whitelistTime ) ) {
            (/*: cheat Mutable<aNode> */node).tacacheWhitelist = { match : this.checkWhitelist(node), time : Date.now() };
        }
        if ( node.tacacheWhitelist.match )
            return;

        // the node is hidden
        if ( node.style.display == "none" || node.scrollHeight == 0 || node.scrollWidth == 0 )
            return;

        this.writeToPref(node);
    },

    prepareToSave : /*: cheat Unsafe */function () {
        if ( !this.isSaving ) {
            this.isSaving = window.setTimeout( function () {textareaCache.saveToFile();}, /* this.saveInterval */ 50);
        }
    },

    onInput : /*: cheat Unsafe */ function (e) {
        var node = (/*: cheat aNode */(e.target));

        // Only for textarea node
        if ( node.nodeName.toLowerCase() != "textarea" )
            return;

        if ( node.value == "" )
            return;

        (/*: cheat Mutable<aNode> */node).tacacheBackup = node.value;

        if (!node.tacacheOnSave) {
            (/*: cheat Mutable<aNode> */node).tacacheOnSave = true;
            (/*: cheat Mutable<aNode> */node).waiting = window.setTimeout( function () {textareaCache.beforeWrite(node);}, textareaCache.writeInterval );
        }
        textareaCache.prepareToSave();
    },

    onChange : /*: cheat Unsafe */ function (e) {
        var node = (/*: cheat aNode */ (e.target));

        // Only for textarea node
        if ( node.nodeName.toLowerCase() != "textarea" )
            return;

        if ( node.tacacheOnSave )
            window.clearTimeout(node.waiting);
        textareaCache.beforeWrite(node);
        textareaCache.prepareToSave();
    },

    onKeypress : /*: cheat Unsafe */ function (e) {
        var node = (/*: cheat aNode */e.target);
        var nodeName = node.nodeName.toLowerCase();

        if ( nodeName == "textarea" && node.value == "" && e.keyCode == 13 ) {
            (/*: cheat Mutable<aNode> */node).tacacheSubmit = true;
            textareaCache.beforeWrite(node);
            return;
        }

        // ths node is a WYSIWYG editor or an editable node?
        if ( (/*: cheat nsIDOMHTMLDocument */node.ownerDocument).designMode != "on" && !node.isContentEditable )
            return;

        if ( node.textContent == "" && e.keyCode == 13 ) {
            (/*: cheat Mutable<aNode> */ node).tacacheSubmit = true;
            textareaCache.beforeWrite(node);
            return;
        }

        (/*: cheat Mutable<aNode> */node).tacacheBackup = ( nodeName == "html" ) ?
            (/*: cheat nsIDOMHTMLDocument */ node.ownerDocument).body.innerHTML : node.innerHTML;

        if (!node.tacacheOnSave) {
            (/*: cheat Mutable<aNode> */node).tacacheOnSave = true;
            (/*: cheat Mutable<aNode> */node).waiting = window.setTimeout( function () {textareaCache.beforeWrite(node);}, textareaCache.writeInterval );
        }
        textareaCache.prepareToSave();
    },

    // Update cache from 0.7 to 0.7.1
    cacheUpdate : /*: cheat Unsafe */function () {
        if ( this.cache.length > 0 && !("time" in this.cache[0]) ) {

            for (var i = 0; i < this.cache.length; i++) {
                var item = this.cache[i];
                let temp = item.id.split("@");
                item.id = temp[0];
                item.time = parseInt(temp[1]);
                if ( item.id[0] == "-" ) {
                    item.time = -item.time;
                    item.id = item.id.slice(1);
                }
                if ( item.id.indexOf("D") )
                    item.id = item.id + textareaCacheUtil.isDoc;
            }

            // No forEach support, so replaced by above
            // Array.forEach( this.cache, function (item) {
            //   let temp = item.id.split("@");
            //   item.id = temp[0];
            //   item.time = parseInt(temp[1]);
            //   if ( item.id[0] == "-" ) {
            //     item.time = -item.time;
            //     item.id = item.id.slice(1);
            //   }
            //   if ( item.id.indexOf("D") )
            //     item.id = item.id + textareaCacheUtil.isDoc;
            // });
        }

        this.cacheToPref();
        this.saveToFile();

    },

    // Get white list
    // PASSES
    getWhitelist : /*: [tacType] -> Undef */  function () {
        var list = this.pref.getCharPref("extensions.tacache.whitelist");
        let temp = /*: Array<Str> */[];

        // still editing
        if ( list.indexOf("\n") >= 0 )
            return;

        if ( list == "" ) {
            temp = null;
        }
        else {
            temp = list.split(" ").sort();
        }

        if ( temp.toString() != this.whitelist.toString() ) { // update whitelist
            this.whitelist = temp;
            this.whitelistTime = Date.now();
        }
    },

    // PASSES
    updateSetting : /*: [tacType] -> Undef */ function () {
        this.clearSetting     = this.pref.getIntPref("extensions.tacache.clearCache");
        this.maxTextSaved     = this.pref.getIntPref("extensions.tacache.maxTextSaved");
        this.writeInterval    = this.pref.getIntPref("extensions.tacache.interval") * 1000;
        this.saveInterval     = this.pref.getIntPref("extensions.tacache.saveInterval") * 1000;
        this.restartClearTime = this.pref.getIntPref("extensions.tacache.restartClearInMin");
    },

    prepareCache : /*: cheat Unsafe */function () {
        this.prefToCache();
        this.cacheUpdate();

        if ( this.maxTextSaved <= 0 )
            this.maxTextSaved = 20;

        if ( this.cache.length > this.maxTextSaved ) {
            this.cache.splice( this.maxTextSaved, this.cache.length - this.maxTextSaved );
            this.cacheToPref();
        }

        tacacheUI.checkStatusButton();

        let startup = this.pref.getIntPref("browser.startup.page");
        var sessionRestore = /*:Bool */false;
        try {
            var ss = Cc["@mozilla.org/browser/sessionstartup;1"].getService(Ci.nsISessionStartup);
            sessionRestore = ss.doRestore();
        }
        catch(e) { }
        var numWin = tacacheUI.numberOfWindows();
        let _this = this;
        // restartClearTime
        //   > 0 : clear old data after a time interval (in minute)
        //   = 0 : keep clear old data during this session, but clear them this session is closed
        //   < 0 : never clear old data unless the cache is full
        //
        // When a new session starts, we may get two kind of data in the cache.
        //   1. "very" old data: they were marked as "old" in the last session, and we will kill them now.
        //   2. normal data: the data saved in the last session. we need to mark them as "old" in this session.
        //
        // If user sets |browser.startup.page| to 3 (always save session and show tabs in last session when starting Firefox)
        //   always clear old data
        //
        if ( this.clearSetting == this._restartClear &&
             ( startup == 3 || !sessionRestore ) && numWin == 1 ) {
            // clear the "very" old data
            if ( this.restartClearTime >= 0 )
                this.clearOldPref();
            // then mark the data left in the last session as "old"
            this.setOldPref();
            // clear these old data session after a time interval
            if ( this.restartClearTime > 0 )
                window.setTimeout( function () { _this.clearOldPref(); }, this.restartClearTime * 60 * 1000 );
        }

        // Check the cache every hour
        // If an item is older than a span of time (one day, three days or one week, etc.), delete the item.
        //
        this.clearOldPrefByTime();
        var interval = 1000 * 60 * 30; // 30 minutes
        window.setInterval( function () { _this.clearOldPrefByTime(); }, interval );
    },

    init : function () {
        textareaCache.prefObserver.register();
        try {
            textareaCache._PBS = Components.classes["@mozilla.org/privatebrowsing;1"]
                .getService(Components.interfaces.nsIPrivateBrowsingService);
            textareaCache.updateSetting();
            textareaCache.getWhitelist();
        }
        catch (e) {}

        /*: cheat False */textareaCache.prepareCache();

        let contentMenu = document.getElementById("contentAreaContextMenu");
        if ( contentMenu ) {
            /*: cheat False */gBrowser.addEventListener("keypress", textareaCache.onKeypress, false);
            /*: cheat False */gBrowser.addEventListener("change", textareaCache.onChange, false);
            /*: cheat False */gBrowser.addEventListener("input", textareaCache.onInput, false);
            contentMenu.addEventListener("popupshowing", tacacheUI.onContentMenu, false);
        }
    },

    exit : function () {
        if ( textareaCache.clearSetting == textareaCache._restartClear && textareaCache.restartClearTime == 0 )
            textareaCache.clearOldPref();
        textareaCache.prefObserver.unregister();

        let contentMenu = document.getElementById("contentAreaContextMenu");
        if ( contentMenu ) {
            /*: cheat False */gBrowser.removeEventListener("keypress", textareaCache.onKeypress, false);
            /*: cheat False */gBrowser.removeEventListener("change", textareaCache.onChange, false);
            /*: cheat False */gBrowser.removeEventListener("input", textareaCache.onInput, false);
            contentMenu.removeEventListener("popupshowing", tacacheUI.onContentMenu, false);
        }
    }

};

window.addEventListener("load", textareaCache.init, false);
window.addEventListener("unload", textareaCache.exit, false);

// -------------------------- ui.js --------------------------------------------

tacacheUI = {
    WM : Components.classes['@mozilla.org/appshell/window-mediator;1'].getService(Components.interfaces.nsIWindowMediator),
    pref : Components.classes["@mozilla.org/preferences-service;1"].getService(Components.interfaces.nsIPrefBranch),

    numberOfWindows : function (all) {
        var enumerator = this.WM.getEnumerator("navigator:browser");
        var count = 0;
        while ( enumerator.hasMoreElements() ) {
            count++;
            if (!all && count == 2)
                break;
            enumerator.getNext();
        }
        return count;
    },

    checkReloadButton : /*: [tacUIType] -> Undef */function () {
        var optionsURL = "chrome://tacache/content/cacheWindow.xul";
        var windows = this.WM.getEnumerator(""+null);   // while (windows.hasMoreElements()) {
        var win = /*: cheat nsIDOMWindow */(windows.getNext());
        if (win.document.documentURI == optionsURL) {
            if ( /*: cheat nsIDOMXULMenuListElement */ (win.document.getElementById("cacheMenu")).selectedIndex == 0 ) {
                (/*: cheat tacCacheWindowType */win.document.defaultView.cacheWindow).init();
            }
            else {
                (/*: cheat Mutable<nsIDOMXULControlElement> */win.document.getElementById("reloadButton")).disabled = false;
                (/*: cheat Mutable<nsIDOMXULControlElement> */win.document.getElementById("clearThis")).disabled = true;
                (/*: cheat Mutable<nsIDOMXULControlElement> */win.document.getElementById("clearAll")).disabled = true;
            }
            return;
        }
    },

    checkStatusButton : /*: [tacUIType] -> Undef */ function () {
        var aHidden = ( this.pref.getBoolPref("extensions.tacache.statusButton") == false ||
                        textareaCacheUtil.getPref().length == 0 );

        var enumerator = this.WM.getEnumerator("navigator:browser");
        while ( enumerator.hasMoreElements() ) {
            var win = (/*: cheat nsIDOMWindow */enumerator.getNext());
            var aButton = win.document.getElementById("tacacheStatusButton");
            if ( aButton )
                aButton.setAttribute("hidden",  ""+aHidden);
        }
    },

    checkUI : /*: [tacUIType] -> Undef */ function () {
        this.checkReloadButton();
        this.checkStatusButton();
    },

    openWindow : function ( aURL, aFeature, aArg ) {
        aArg = aArg || null;

        var windows = this.WM.getEnumerator(null);
        while (windows.hasMoreElements()) {
            var win = windows.getNext();
            if (win.document.documentURI == aURL) {
                win.focus();
                return;
            }
        }
        if ( aArg == null )
            window.open(aURL, "", "chrome, centerscreen" + aFeature);
        else
            openDialog(aURL, "", "chrome, centerscreen" + aFeature, aArg);
    },

    openCacheWindow : function (opener) {
        opener = null || opener;
        this.openWindow("chrome://tacache/content/cacheWindow.xul", ", resizable", opener);
    },

    openOptionWindow : function (addWhitelist) {
        try {
            var arg = addWhitelist ? gBrowser.mCurrentBrowser.contentDocument.location.host : null;
        }
        catch (e) {
            var arg = null;
        }

        this.openWindow("chrome://tacache/content/pref-tacache.xul", "", arg);
    },

    onContentMenu : function (e) {
        var tacacheContextMenuOpenCache = document.getElementById("tacacheContextOpenCache");
        if ( gContextMenu.onTextInput &&
             document.popupNode.localName.toLowerCase() != "input" &&
             textareaCacheUtil.getPref().length > 0 )
            tacacheContextMenuOpenCache.hidden = false;
        else
            tacacheContextMenuOpenCache.hidden = true;
    }
};

// ------------------------------- xul script calls ----------------------------

// tacachePref calls, which for some reason were not extracted automatically.
function(event) { tacachePref.init(); };
function(event) { tacachePref.uninit('close'); };
function(event) { tacachePref.uninit(true); };
function(event) { tacachePref.uninit(false); };

function(event) { tacacheUI.openCacheWindow(); };
function(event) { tacacheUI.openOptionWindow(true); event.stopPropagation(); };
function(event) { tacacheUI.openOptionWindow(false); event.stopPropagation(); };
function(event) { tacacheUI.openOptionWindow(true); event.stopPropagation(); };
function(event) { tacacheUI.openCacheWindow(); event.stopPropagation(); };
function(event) { tacacheUI.openCacheWindow(); };
function(event) { tacacheUI.openCacheWindow({ document : document, node : document.popupNode }); };


// ____________________________ cacheWindow.js _________________________________

cacheWindow = {
    cache : /*:Array<cacheObjType>*/[],
    type : null,
    _Text : 0,
    _Doc : 1,
    copyButton : null,
    opener : null,

    init : /*: [tacCacheWindowType] -> Undef */  function () {
        if ( window.arguments ) {
            this.opener = window.arguments[0];
        }
        let copyAndClose = (/*: cheat nsIDOMHTMLButtonElement */document.getElementById("copyButton"));
        let closeAndPaste = (/*: cheat nsIDOMHTMLButtonElement */document.getElementById("pasteButton"));
        if (this.opener) {
            (/*: cheat Mutable<nsIDOMHTMLButtonElement> */copyAndClose).hidden = true;
            this.copyButton = closeAndPaste;
        }
        else {
            (/*: cheat Mutable<nsIDOMHTMLButtonElement> */closeAndPaste).hidden = true;
            this.copyButton = copyAndClose;
        }

        this.type = this._Text;
        let popup = document.getElementById("cacheMenuPopup");
        let cacheMenu = /*: cheat nsIDOMXULMenuListElement */(document.getElementById("cacheMenu"));
        while (popup.hasChildNodes()) {
            popup.removeChild(popup.lastChild);
        }

        this.cache = textareaCacheUtil.getPref();

        for ( let i = 0; i < this.cache.length; i++ ) {
            let m = popup.appendChild(document.createElement("menuitem"));
            m.value = this.cache[i].text;
            m.deckType = ( this.cache[i].id.indexOf(textareaCacheUtil.isDoc) > 0 ) ? this._Doc : this._Text;
            m.id = "cacheTitle" + i;
            let time = cacheWindow.getTime( new Date("test") );
            m.label = time + this.cache[i].title;
            m.setAttribute("anonid", this.cache[i].id);
            m.setAttribute("oncommand", "cacheWindow.getContent(this);");
        }

        (/*: cheat Mutable<nsIDOMXULMenuListElement> */cacheMenu).disabled =
            (/*: cheat Mutable<nsIDOMXULControlElement> */document.getElementById("clearAll")).disabled =
            (/*: cheat Mutable<nsIDOMXULControlElement> */document.getElementById("clearThis")).disabled =
            ( this.cache.length == 0 );
        if ( this.cache.length == 0 ) {
            popup.appendChild(document.createElement("menuitem")).label = "";
            document.getElementById("textContent").value = "";
        }
        else {
            this.getContent( document.getElementById("cacheTitle0") );
        }

        (/*: cheat Mutable<nsIDOMXULMenuListElement> */cacheMenu).selectedIndex = 0;
        this.setCopyButton();
        this.copyButton.focus();

        cacheMenu.addEventListener("DOMMouseScroll", cacheWindow.mouseScroll, false);
        window.addEventListener("unload", cacheWindow.exit, false);
    },

    exit : function () {
        document.getElementById("cacheMenu").removeEventListener("DOMMouseScroll", (/*: cheat Ext */cacheWindow.mouseScroll), false);
    },

    mouseScroll : function (e) {
        let list = document.getElementById("cacheMenu");
        let count = list.itemCount;
        let selected = list.selectedIndex;

        if ( /*: cheat Ext */e.detail > 0 )
            selected += ( selected < count - 1 ) ? 1 : 0;
        else
            selected -= ( selected > 0 ) ? 1 : 0;

        list.selectedIndex = selected;
        cacheWindow.getContent(list.selectedItem);
    },

    digiFormat : function (num) {
        let s = num.toString();
        if ( s.length < 2 )
            s = "0"+ s;
        return s;
    },

    getTime : /*: [tacCacheWindowType] Date -> Str */ function (date) {
        let y = date.getFullYear();
        let m = this.digiFormat(date.getMonth()+1);
        let d = this.digiFormat(date.getDate());
        let h = this.digiFormat(date.getHours());
        let n = this.digiFormat(date.getMinutes());
        return y + "-" + m + "-" + d + " " + h + ":" + n + "  ";
    },

    getContent : function (node) {
        this.type =  node.deckType;
        document.getElementById("contentDeck").setAttribute("selectedIndex", this.type);
        if ( this.type == this._Text )
            document.getElementById("textContent").value = node.value;
        else {
            document.getElementById("textContent").value = node.value;
            document.getElementById("docContent").contentDocument.body.innerHTML = node.value;
        }
        document.getElementById("cacheMenu").setAttribute("anonid", node.getAttribute("anonid"));
        this.setCopyButton();
    },

    setCopyButton : function () {
        (/*: cheat Mutable<nsIDOMHTMLButtonElement> */this.copyButton).disabled = ( this.cache.length == 0 );
    },

    copyAndClose : function () {
        let nodeID = ( this.type == this._Text ) ? "textContent" : "docContent";
        let node = document.getElementById(nodeID);

        node.focus();
        goDoCommand("cmd_selectAll");
        goDoCommand("cmd_copy");
        window.close();
    },

    pasteClipboard : function () {
        // opener = { document, node }
        //   document = document of Firefox window
        //   node = the text input node
        //   ** note : document != node.ownerDocument **
        this.opener.node.focus();
        let doc = this.opener.document;
        try {
            var controller = doc.commandDispatcher.getControllerForCommand("cmd_paste");
            if (controller && controller.isCommandEnabled("cmd_paste"))
                controller.doCommand("cmd_paste");
        }
        catch (e) {
        }
    },

    confirm : function (aTitle, aMsg) {
        var prompts = Components.classes["@mozilla.org/embedcomp/prompt-service;1"]
            .getService(Components.interfaces.nsIPromptService);
        var button = prompts.confirmEx((/*: cheat nsIDOMWindow */window),
                                       aTitle,
                                       aMsg,
                                       (prompts.BUTTON_TITLE_YES * prompts.BUTTON_POS_0)
                                       + (prompts.BUTTON_TITLE_NO * prompts.BUTTON_POS_1),
                                       ''+null, ''+null, ''+null, ''+null, {value : /*: Bool*/false});
        return (button == 0);
    },

    clearAll : /*: cheat Unsafe */ function () {

        textareaCacheUtil.gPref.clearUserPref("extensions.tacache.cache");
        textareaCacheUtil.savePrefFile();

        window.close();
    },

    clearThis : /*: cheat Unsafe */function () {
        let index = document.getElementById("cacheMenu").getAttribute("anonid");
        for ( let i = 0; i< this.cache.length; i++ ) {
            if ( this.cache[i].id == index ) {
                this.cache.splice(i, 1);
                break;
            }
        }

        textareaCacheUtil.setPref(this.cache);
        textareaCacheUtil.savePrefFile();


        if ( this.cache.length == 0 )
            window.close();
        else
            this.init();
    },

    onContextMenuShowing : function () {
        let focusedWindow = document.commandDispatcher.focusedWindow;
        let selection = (/*: cheat Str */ focusedWindow.getSelection().toString());

        (/*: cheat Mutable<nsIDOMXULSelectControlElement> */ document.getElementById("contextCopy")).disabled = ( selection.length == 0 ) ? true : false;
    }

};

// ---------------------------- pref-tacache.js --------------------------------

tacachePref = {
    pref : Components.classes["@mozilla.org/preferences-service;1"].getService(Components.interfaces.nsIPrefBranch),

    init : /* [tacPrefType] -> Undef */ function () {
        var textbox = document.getElementById("tacache-whitelist");
        var newItem = /*:Str */null+"";
        if ( window.arguments ) {
            newItem = ""+window.arguments[0];
        }

        // whitelist
        var str = ""+textbox.value;
        textbox.value = str.replace(/ /g, "\n").replace(/%20/g, " ");

        if ( newItem && newItem != "" ) {
            newItem = newItem.replace(/%20/g, " ");
            textbox.value = newItem + "\n" + textbox.value;

            setTimeout( function () {
                textbox.focus();
                textbox.selectionStart = 0;
                textbox.selectionEnd = newItem.length;
            }, 0 );
        }

        var restartClearTime = document.getElementById("restartClearTime");
        var userTimeButton = document.getElementById("restartClearUserSetting");
        var time = this.pref.getIntPref((/*: cheat nsIDOMElement*/restartClearTime).getAttribute("preference"));

        //if ( time < 0 )
        //  time = 0;
        if ( time != 0 && time != 5 ) {
            userTimeButton.hidden = false;
            userTimeButton.value = time;
            restartClearTime.selectedItem = userTimeButton;
        }
        else
            userTimeButton.hidden = true;
        restartClearTime.value = time;

        var fiveDays = document.getElementById("tppimeClearSpan.fiveDays");
        var oneWeek  = document.getElementById("timeClearSpan.oneWeek");
        var cacheSize = this.pref.getIntPref("extensions.tacache.maxTextSaved");
        // fiveDays.hidden = cacheSize < 50;
        // oneWeek.hidden  = cacheSize < 99;
    },

    uninit: function (save) {
        if ( save == true ||
             ( document.documentElement.instantApply && save == "close" ) ) {
            var textbox = document.getElementById("tacache-whitelist");
            var str = ""+textbox.value;
            str = str.replace(/ /g, "%20").replace(/\n/g, " ").replace(/^\s+/g, '').replace(/\s+$/g, '');
            this.pref.setCharPref( textbox.getAttribute("preference"), str );
        }
    }
};

/* -------------------- cacheWindow.xul ---------------------------- */

function(event) { /*: cheat False */cacheWindow.clearThis();};

function(event) { if (cacheWindow.confirm((/*: cheat Str */(this.label)), (/*: cheat Str */(this.getAttribute('value')))))  /*: cheat False */cacheWindow.clearThis();};

function(event) { if (cacheWindow.confirm((/*: cheat Str */(this.label)), (/*: cheat Str */(this.getAttribute('value'))))) /*: cheat False */cacheWindow.clearAll();};

function(event) { cacheWindow.init(); /* this.disabled='true'*/};

function(event) { cacheWindow.copyAndClose(); };

function(event) { cacheWindow.copyAndClose(); cacheWindow.pasteClipboard(); };
