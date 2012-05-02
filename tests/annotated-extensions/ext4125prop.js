/*::

type HitcherType =
 typlambda o :: * .
   typlambda t :: * .
     typlambda a :: * .
       typlambda r :: * .
         'o * Str -> ([this('t)] 'a -> 'r);

type HitchType = forall o : forall t : forall a : forall r :
       'o * Str -> ([this('t)] 'a -> 'r);

type keyMapType =  {Ext with
  /^("-*- code -*-"|"__proto__")/ :! Str};

type poType = rec p . {AnObject with
    types : Ext,
    register : [this('p)] -> Undef,
    unregister : [this('p)] -> Undef,
    observe : [this('p)] nsISupports * Str * Str -> Undef,
    private_branch : nsIPrefBranch2,
    QueryInterface : QueryInterfaceType,
};

type mockEvent = #{originalTarget :^ nsIDOMEventTarget};

type extEL = ([this(Any)] nsIDOMEvent -> Bool) + ([this(Any)] nsIDOMEvent -> Undef);

type gBrowserEL = ([this(Any)] nsIDOMEvent -> Undef);

type windowEL = ([this(Any)] nsIDOMEvent -> Undef) +
  ([this(Any)] (nsIDOMEvent + Num) * (Bool + Undef) -> Undef);

type nsIDOMWindowEL = ([this(Any)] nsIDOMEvent -> Undef);

type GBrowser = { Ext with
  tabContainer : Window,
  contentDocument : nsIDOMDocument,
  addTab : Str -> Ext,
  selectedTab : Ext,
  addEventListener : Str * gBrowserEL  * Bool -> Undef,
  removeEventListener : Str * gBrowserEL  * Bool -> Undef,
  selectedBrowser : {Ext with
    contentDocument : nsIDOMDocument
  }
};

// ----------------- Color ---------------------

type keywordType =  {Ext with
  /^("-*- code -*-"|"__proto__")/ :! Str};

type funcType = {Ext with
  /^("-*- code -*-"|"__proto__")/ :! RegExp};

type constructor ColorConstructor = [ColorInst] (Str + Num + Undef) * (Num + Undef) * (Num + Undef) * (Num + Undef) -> ColorInst
and prototype Color = {AnObject with
  dummy : Str
}
and instance ColorInst = {Ext with
  hex : [ColorInst] ColorInst -> Str,
  toString : [ColorInst] ColorInst -> Str,
  rgb : [ColorInst] ColorInst -> Str,
  rgba : [ColorInst] -> Str,
  invert : [ColorInst] -> ColorInst,
  blend : [ColorInst] ColorInst * Num -> Array<ColorInst>,
  alpha : Num,
  red : Num,
  green : Num,
  blue : Num,
};

// ---------------- CacheObj -----------------------

type constructor CacheObjConstructor = {[CacheObjInst] nsIDOMHTMLTextAreaElement -> CacheObjInst with
  get : Unsafe,
  make : Unsafe,
}
and prototype CacheObj = {AnObject with
  addGumDrop : Unsafe,
  adjust : [CacheObjInst] -> Undef,
  edit : @Unsafe,
  fade : [CacheObjInst] Num * Num -> Undef,
  fadeStep : [CacheObjInst] Array<ColorInst> * Array<ColorInst> * Num * Num -> ( -> Undef),
  getNodeIdentifier : [CacheObjInst] nsIDOMHTMLTextAreaElement -> Str,
  getStyle : [CacheObjInst] nsIDOMHTMLTextAreaElement * Str -> Str,
  hasChanged : [CacheObjInst] -> Bool,
  hitched_keypress : Unsafe,
  initFromExistingFile : Unsafe,
  keypress : Unsafe,
  onClick : Unsafe,
  onContext : Unsafe,
  read : [CacheObjInst] -> Str,
  setExtension :  Unsafe,
  toString : [CacheObjInst] -> Str,
  update : [CacheObjInst] -> Bool,
  write : @Unsafe,
}
and instance CacheObjInst = {Ext with
  base_filename : Str,
  button : nsIDOMHTMLElement,
  button_fade_timer : Num,
  dummy : Str,
  edit_count : Num,
  extension : Str,
  file : nsIFile,
  gumdrop_url : Str,
  gumdrop_width : Str,
  gumdrop_height : Str,
  initial_background : Str,
  initial_color : Str,
  is_focused : Bool,
  is_listening : Bool,
  mouseout : [CacheObjInst] nsIDOMEvent -> Undef,
  mouseover : Unsafe,
  node : nsIDOMHTMLTextAreaElement,
  node_id : Str,
  private_is_watching : Bool,
  size : Num,
  timestamp : Num,
  uid : Str,
  uuid : Num,
};

// ----------------- Monitor -------------------------------

type constructor MonitorConstructor = [MonitorInst] -> MonitorInst
and prototype Monitor = {AnObject with
  hitched_destroy : -> Undef,
  hitched_restart : [MonitorInst] -> Undef,
  destroy : [Any] -> Undef,
  restart : [Any] -> Undef,
  hitched_registerPage : Unsafe,
  registerPage : Unsafe,
  hitched_findnodes : [MonitorInst] nsIDOMDocument -> Array<nsIDOMHTMLTextAreaElement>,
  findnodes : [MonitorInst] nsIDOMDocument -> Array<nsIDOMHTMLTextAreaElement>,
  hitched_watcher : Unsafe,
  watcher : Unsafe,
  hitched_incrementLock : [MonitorInst] -> Undef,
  incrementLock : [MonitorInst] -> Undef,
  hitched_decrementLock : [MonitorInst] -> Undef,
  decrementLock : [MonitorInst] -> Undef,
  hitched_isLocked : [MonitorInst] -> Bool,
  isLocked : [MonitorInst] -> Bool,
  hitched_handleSubtreeModified : Unsafe,
  handleSubtreeModified : Unsafe,
  hitched_startPage : Unsafe,
  startPage : Unsafe,
  hitched_stopPage : [MonitorInst] mockEvent -> Undef,
  stopPage : [this(Any)] mockEvent -> Undef,
  isXUL : [MonitorInst] nsIDOMDocument -> Bool,
  isHTML : [MonitorInst] nsIDOMDocument -> Bool,
}
and instance MonitorInst = {Ext with
  dummy : Str,
  _lock_count : Num,
  id : Num,
  defaultView : Ext,
};

// --------------------- Monitor -----------------------------

type constructor ItsAllTextConstructor = [iatInstance] -> Undef
and prototype ItsAllText = {AnObject with
  splitterMouseDown : [iatInstance] Ext -> Undef,
  init : Unsafe,
  initScripts : [iatInstance] -> Undef,
  localeFormat : [iatInstance] Str * Array<Str> -> Str,
  localeString : [iatInstance] Str -> Str,
  hitch : HitchType,
  listen : ([iatInstance] Ext * Str * extEL * Bool-> Undef) &
           ([iatInstance] GBrowser * Str * gBrowserEL * Bool -> Undef) &
           ([iatInstance] Window * Str * windowEL * Bool -> Undef) &
           ([iatInstance] nsIDOMWindow * Str * nsIDOMWindowEL * Bool -> Undef),
  unlisten : ([iatInstance] Ext * Str * extEL * Bool -> Undef) &
             ([iatInstance] GBrowser * Str * gBrowserEL * Bool -> Undef) &
             ([iatInstance] Window * Str * windowEL * Bool -> Undef) &
             ([iatInstance] nsIDOMWindow * Str * nsIDOMWindowEL * Bool -> Undef),
  hashString : [iatInstance] Str -> Str,
  eventToKeyprint : [iatInstance] nsIDOMKeyEvent -> Str,
  keyprintToString : [iatInstance] Str -> Str,
  cleanEditDir : Unsafe,
  menuNewExtEdit : Unsafe,
  menuExtEdit : Unsafe,
  rebuildMenu : Unsafe,
  getLocale : [iatInstance] -> nsIStringBundle,
  logString : [iatInstance] Any ... -> Str,
  log : [iatInstance] Any ... -> Undef,
  debuglog : [iatInstance] Any ... -> Undef,
  debug : [iatInstance] Any ... -> Undef,
  XHTMLNS : Str,
}
and instance iatInstance = {Ext with
  monitor : MonitorInst,
  factoryFile : (Str + Undef) -> nsILocalFile,
  getEditDir : Unsafe,
  MYSTRING : Str,
  preferences : {AnObject with
    debug : Bool,
    private_get : Str -> Ext,
    private_set : Unsafe,
    extensions : Str,
    editor : Str,
    tracker_id : Str,
    fade_time : Num,
    gumdrop_position : Str,
    charset : Str,
  },
  preference_observer : poType,
  private_is_darwin : Bool,
  private_current_uid : Str,
  keyMap : keyMapType,
  CacheObj : CacheObjConstructor,
  README : Str,
  XULNS : Str,
  getCharset : [iatInstance] -> Str,
  getRefresh : [iatInstance] -> Num,
  getTrackerId : Unsafe,
  isDarwin : [iatInstance] -> Bool,
  getEditor : Unsafe,
  getDebug : [iatInstance] -> Bool,
  getDisableGumdrops : [iatInstance] -> Bool,
  getExtensions : [iatInstance] -> Array<Str>,
  openPreferences : [iatInstance] (Bool + Undef) -> Undef,
  appendExtensions : Unsafe,
  getFromTracker : Unsafe,
  addToTracker : Unsafe,
  getContainingBlockOffset : [iatInstance] nsIDOMHTMLElement * nsIDOMElement -> Array<Num>,
  marshalKeyEvent : [iatInstance] nsIDOMKeyEvent -> Str,
  keyMarshalToString : [iatInstance] Str -> Str,
  onEditNode : Unsafe,
  onContextMenu : Unsafe,
  openReadme : [iatInstance] -> Undef,
  splitterMouseDown :^ [iatInstance] Ext -> Undef,
};

*/

var gBrowser = /*:GBrowser*/null;
/*: -> GBrowser */ function getBrowser() { return gBrowser; }
var Firebug = /*:Ext*/null;

var HTMLDocument = /*:Ext*/null;


var ItsAllText = /*: ItsAllTextConstructor */ function() {};

var itsalltext = /*: iatInstance */ new ItsAllText();

var Monitor = /*: MonitorConstructor */ function() {};


/* --------------------------- ItsAllText ------------------------ */

var ItsAllText = /*: ItsAllTextConstructor */ function () {
    /**
     * This data is all private, which prevents security problems and it
     * prevents clutter and collection.
     * @type Object
     */
    var that = this,
        loadthings;

    /**
     * A factory method to make an nsILocalFile object.
     * @param {String} path A path to initialize the object with (optional).
     * @returns {nsILocalFile}
     */
    that.factoryFile =  function (path) {
        var file = Components.
            classes["@mozilla.org/file/local;1"].
            createInstance(Components.interfaces.nsILocalFile);
        if (typeof(path) == 'string' && path !== '') {
            file.initWithPath(""+path);
        }
        return file;
    };

    /**
     * Returns the directory where we put files to edit.
     * @returns nsILocalFile The location where we should write editable files.
     */
    that.getEditDir = /*: cheat Unsafe */function () {
        /* Where is the directory that we use. */

        var fobj = Components.classes["@mozilla.org/file/directory_service;1"].
            getService(Components.interfaces.nsIProperties).
            get("ProfD", Components.interfaces.nsILocalFile);
        fobj.append(that.MYSTRING);
        if (!fobj.exists()) {
            fobj.create(Components.interfaces.nsIFile.DIRECTORY_TYPE,
                        parseInt('0700', 8));
        }
        if (!fobj.isDirectory()) {
            that.error(that.localeFormat('problem_making_directory', [fobj.path]));
        }
        return fobj;

    };

    /**
     * Dictionary for storing the preferences in.
     * @type Hash
     */
    that.preferences = {
        debug: false,
        extensions: '',
        editor: '',
        tracker_id: '',
        fade_time : 0,
        gumdrop_position : '',
        charset : '',


        /**
         * Fetches the current value of the preference.
         * @private
         * @param {String} aData The name of the pref to fetch.
         * @returns {Object} The value of the preference.
         */

        private_get: function (aData) {
            var po = that.preference_observer,
                real_type = po.types[aData],
                type = real_type === 'Float' ? 'Char' : real_type,
                retval = /*: Ext*/'';
            retval = po.private_branch[/*: cheat /get(Char|Bool|Int)Pref/ */'get' + type + 'Pref'](aData);
            return real_type === 'Float' ? parseFloat(retval) : retval;
        },

        /**
         * Sets the current preference.
         * @param {String} aData The name of the pref to change.
         * @param {Object} value The value to set.
         */

        private_set: /*: cheat Unsafe */function (aData, value) {
            var po = that.preference_observer,
                real_type = po.types[aData],
                type = real_type === 'Float' ? 'Char' : real_type;
            if (real_type === 'Float') {
                value = '' + parseFloat(value);
            }
            po.private_branch[/*: cheat /set(Char|Bool|Int)Pref/ */'set' + type + 'Pref'](aData, /*: cheat Num */value);
        },

    };

    /**
     * A Preference Observer.
     */
    that.preference_observer = {
         private_branch : /*: nsIPrefBranch2 */ null,
        /**
         * Dictionary of types (well, really the method needed to get/set the
         * type.
         * @type Hash
         */
        types : /*: Ext */ {
            charset:            'Char',
            editor:             'Char',
            refresh:            'Int',
            debug:              'Bool',
            gumdrop_position:   'Char',
            fade_time:          'Float',
            extensions:         'Char',
            hotkey:             'Char',
            tracker_id:         'Char',
        },

        /**
         * Register the observer.
         */
        register: /*: [poType] -> Undef */ function () {

            var prefService = Components.
                classes["@mozilla.org/preferences-service;1"].
                getService(Components.interfaces.nsIPrefService),
                type;
            this.private_branch = prefService.getBranch("extensions." + that.MYSTRING + ".").QueryInterface(Components.interfaces.nsIPrefBranch2);
            this.private_branch.addObserver("", this, false);
            /* setup the preferences */
            for (type in this.types) {
                if (this.types.hasOwnProperty(type)) {
                    // TODO
                    /*: cheat False */(that.preferences[type] = that.preferences.private_get(type));
                }
            }
        },

        /**
         * Unregister the observer. Not currently used, but may be
         * useful in the future.
         */
        unregister: /*: [poType] -> Undef */ function () {
            if (!this.private_branch) {
                return;
            }
            this.private_branch.removeObserver("", this);
        },

        /**
         * Observation callback.
         * @param {String} aSubject The nsIPrefBranch we're observing (after appropriate QI)e
         * @param {String} aData The name of the pref that's been changed (relative to the aSubject).
         * @param {String} aTopic The string defined by NS_PREFBRANCH_PREFCHANGE_TOPIC_ID
         */
        observe: /*: [poType] nsISupports * Str * Str -> Undef */ function (aSubject, aTopic, aData) {
            if (aTopic != "nsPref:changed") {
                return;
            }
            if (that.preferences) {
                /*: cheat False */(that.preferences[aData] = that.preferences.private_get(aData));
                if (aData == 'refresh') {
                    that.monitor.restart();
                }
            }
        },

        QueryInterface: /*:cheat QueryInterfaceType*/XPCOMUtils.generateQI([Ci.nsIObserver])
    };

    /**
     * A Preference Option: What character set should the file use?
     * @returns {String} the charset to be used.
     */
    that.getCharset = function () {
        return that.preferences.charset;
    };

    /**
     * A Preference Option: How often should we search for new content?
     * @returns {int} The number of seconds between checking for new content.
     */
    that.getRefresh = function () {
        var refresh = that.preferences.refresh;
        if (!refresh || refresh < 1) {
            //disabled-debug -- that.debug('Invalid refresh:', refresh);
            refresh = 1;
        }
        return 1000 * refresh;
    };

    that.getTrackerId = /*: cheat Unsafe */ function () {
        var id = that.preferences.tracker_id;
        if (!id) {
            id = [that.MYSTRING,
                  Math.floor(Math.random()*999999)/*.toString()*/,//NEAL
                  Math.round(new Date().getTime()),
                 ].join(':');
            id = that.hashString(id);
            that.preferences.private_set('tracker_id', id);
        }
        return id;
    };


    /**
     * Returns true if the system is running Mac OS X.
     * @returns {boolean} Is this a Mac OS X system?
     */
    that.isDarwin = function () {
        /* more help:
         http://developer.mozilla.org/en/docs/Code_snippets:Miscellaneous#Operating_system_detection
        */

        var is_darwin = that.private_is_darwin;
        if (typeof(is_darwin) == 'undefined') {
            is_darwin = /^Darwin/i.test(Components.classes["@mozilla.org/xre/app-info;1"].getService(Components.interfaces.nsIXULRuntime).OS);
            that.private_is_darwin = is_darwin;
        }
        return is_darwin;
    };

    /**
     * A Preference Option: What editor should we use?
     *
     * Note: On some platforms, this can return an
     * NS_ERROR_FILE_INVALID_PATH exception and possibly others.
     *
     * For a complete list of exceptions, see:
     * http://lxr.mozilla.org/seamonkey/source/xpcom/base/nsError.h#262
     * @returns {nsILocalFile} A file object of the editor.
     */
    that.getEditor = /*: cheat Unsafe */function () {
        var editor = that.preferences.editor,
            retval = /*: nsILocalFile */null;

        if (editor === '' && that.isDarwin()) {
            editor = '/Application/TextEdit.app';
            that.preferences.private_set('editor', editor);
        }

        if (editor !== '') {
            retval = that.factoryFile(editor);
        }
        return retval;
    };

    /**
     * A Preference Option: should we display debugging info?
     * @returns {bool}
     */
    that.getDebug = function () {
        return that.preferences.debug;
    };

    /**
     * A Preference Option: Are the edit gumdrops disabled?
     * @returns {bool}
     */
    that.getDisableGumdrops = function () {
        return that.preferences.gumdrop_position === 'none';
    };

    /**
     * A Preference Option: The list of extensions
     * @returns Array
     */
    that.getExtensions = function () {
        var string = that.preferences.extensions.replace(/[\n\t ]+/g, ''),
            extensions = string.split(',');
        if (extensions.length === 0) {
            return ['.txt'];
        } else {
            return extensions;
        }
    };

    /**
     * Open the preferences dialog box.
     * @param{boolean} wait The function won't return until the preference is set.
     * @private
     * Borrowed from http://wiki.mozilla.org/XUL:Windows
     * and utilityOverlay.js's openPreferences()
     */
    that.openPreferences = function (wait) {
        wait = typeof(wait) == 'boolean' ? wait : false;
        var paneID = that.MYSTRING + '_preferences',
            psvc = Components.classes["@mozilla.org/preferences-service;1"].getService(Components.interfaces.nsIPrefBranch),
            instantApply = psvc.getBoolPref("browser.preferences.instantApply"/*, false*/) && !wait,
            features = "chrome,titlebar,toolbar,centerscreen" + (instantApply ? ",dialog" : ",modal"),
            xpcom_wm = Components.classes["@mozilla.org/appshell/window-mediator;1"],
            wm = xpcom_wm.getService(Components.interfaces.nsIWindowMediator),
            win = wm.getMostRecentWindow("Browser:Preferences"),
            pane = /*: nsIDOMElement */null;

        if (win) {
            win.focus();
            if (paneID) {
                pane = win.document.getElementById(paneID);
                /*: cheat False */(win.document.documentElement.showPane(pane));
            }
        } else {
            openDialog('chrome://itsalltext/content/preferences.xul',
                       "preferences", features, paneID, wait);
        }
    };

    /**
     * A Preference Option: Append an extension
     * @returns Array
     */
    that.appendExtensions = /*: cheat Unsafe */function (ext) {
        var current = that.getExtensions(),
            value = /*: Str */'',
            i = /*: Num */0;
        ext = ext.replace(/[\n\t ]+/g, '');
        for (i = 0; i < current.length; i++) {
            if (ext == current[i]) {
                return; // Don't add a duplicate.
            }
        }

        value = that.preferences.extensions;
        if (value.replace(/[\t\n ]+/g) === '') {
            value = ext;
        } else {
            value = (/*: Array<Str>*/[value, ',', ext]).join('');
        }
        that.preferences.private_set('extensions', value);
    };

    // @todo [wish] Profiling and optimization.

    /*:: type TrackerObjType = {AnObject with
                /^("-*- code -*-"|"__proto__")/ :! CacheObjInst};
    */

    that.getFromTracker = /*: cheat Unsafe */function (id) {
        var tracker = /*: TrackerObjType */null, doc = /*: nsIDOMDocument */null;
        if (typeof gBrowser !== 'undefined') {
            doc = gBrowser.contentDocument;
        } else {
            // We must be in a XUL window, fall back to simpler method.
            doc = /*: cheat nsIDOMDocument */(window.document);
        }
        tracker = /*: cheat TrackerObjType */(doc.getUserData(that.getTrackerId()));
        if (!tracker) {
            tracker = /*: cheat TrackerObjType */{};
            doc.setUserData(that.getTrackerId(), /*: cheat nsIVariant */tracker, null);
        }
        return tracker[id];
    };


    that.addToTracker = /*: cheat Unsafe */function (id, cobj) {
        var tracker = /*: TrackerObjType */null, doc = /*: nsIDOMDocument */null;
        if (typeof gBrowser !== 'undefined') {
            doc = gBrowser.contentDocument;
        } else {
            // We must be in a XUL window, fall back to simpler method.
            doc = /*: cheat nsIDOMDocument */window.document;
        }
        tracker = /*: cheat TrackerObjType */doc.getUserData(that.getTrackerId());
        if (!tracker) {
            tracker = /*: cheat TrackerObjType */{};
        }
        tracker[id] = cobj;
        doc.setUserData(that.getTrackerId(), /*: cheat nsIVariant */tracker, null);
        that.debug("addToTracker:", id, cobj, tracker);
    };

    // @todo [wish] Refresh textarea on editor quit.
    // @todo [9] IDEA: support for input elements as well?
    // @todo [5] Minimum size for textareas.
    // @todo [5] Mark textareas somehow as 'in editor'.

    /**
     * Returns the offset from the containing block.
     * @param {Object} node A DOM element.
     * @param {Object} container If unset, then this will use the offsetParent of node. Pass in null to go all the way to the root.
     * @return {Array} The X & Y page offsets
     */
    that.getContainingBlockOffset = function (node, container) {
        if (typeof(container) == 'undefined') {
            container = node.offsetParent;
        }
        var pos = /*: Array<Num> */[node.offsetLeft, node.offsetTop],
            pnode = /*: cheat nsIDOMHTMLElement */node.offsetParent;
        while (pnode && (container === null || pnode != container)) {
            pos[/* Num* */0] += pnode.offsetLeft || 0;
            pos[/* Num*/1] += pnode.offsetTop  || 0;
            pos[/* Num*/0] -= pnode.scrollLeft || 0;
            pos[/* Num*/1] -= pnode.scrollTop  || 0;
            pnode = /*: cheat nsIDOMHTMLElement */ pnode.offsetParent;
        }
        return pos;
    };


    /**
     * marshals a keypress event.
     */
    that.marshalKeyEvent = function (event) {
        var marshal = /*: Array<Num> */[event.altKey  ? 1 : 0,
                       event.ctrlKey ? 1 : 0,
                       event.metaKey ? 1 : 0,
                       event.shiftKey ? 1 : 0,
                       event.charCode,
                       event.keyCode];
        return marshal.join(':');
    };

    that.keyMap = /*: cheat keyMapType */ {
        '8'   : 'Backspace',
        '9'   : 'Tab',
        '13'  : 'Enter',
        '19'  : 'Break',
        '27'  : 'Escape',
        '33'  : 'PgUp',
        '34'  : 'PgDn',
        '35'  : 'End',
        '36'  : 'Home',
        '37'  : 'Left',
        '38'  : 'Up',
        '39'  : 'Right',
        '40'  : 'Down',
        '45'  : 'Insert',
        '46'  : 'Delete',
        '112' : 'F1',
        '113' : 'F2',
        '114' : 'F3',
        '115' : 'F4',
        '116' : 'F5',
        '117' : 'F6',
        '118' : 'F7',
        '119' : 'F8',
        '120' : 'F9',
        '121' : 'F10',
        '122' : 'F11',
        '144' : 'Num Lock',
        '145' : 'Scroll Lock',
        ''  : '<none>'
    };

    /**
     * Converts a marshalled key event into a string.
     */
    that.keyMarshalToString = function (km) {
        var e = km.split(':'),
            out = /*: Array<Str> */[],
            c = parseInt(e[5], 10);
        if (e[0] === '1') {
            out.push('alt');
        }
        if (e[1] === '1') {
            out.push('ctrl');
        }
        if (e[2] === '1') {
            out.push('meta');
        }
        if (e[3] === '1') {
            out.push('shift');
        }
        if (e[4] === '0') {
            // TODO
            if (that.keyMap.hasOwnProperty(c)) {
                out.push(/*: cheat Str */(that.keyMap[c]));
            } else {
                out.push('code:' + c);
            }
        } else {
            out.push(String.fromCharCode(e[4]).toUpperCase());
        }
        return out.join(' ');
    };

    /**
     * Open the editor for a selected node.
     * This is used by the XUL.
     * @param {Object} node The textarea to get.
     */
    that.onEditNode = /*: cheat Unsafe */ function (node) {
        var cobj = that.CacheObj.make(node);
        if (cobj) {
            cobj.edit();
        }
        return;
    };

    /**
     * Triggered when the context menu is shown.
     * @param {Object} event The event passed in by the event handler.
     */
    that.onContextMenu = /*: cheat Unsafe */function (event) {
        var tid = /*:Ext*/null, node = /*:nsIDOMElement*/null, tag = /*: Ext*/null, is_disabled = /*:Bool*/false, cobj = /*: CacheObjInst*/null, menu = /*:Ext*/null, cstyle = /*: nsIDOMCSSStyleDeclaration */null, doc = /*:nsIDOMDocument*/null;
        if (event.target) {
            tid = (/*: cheat nsIDOMXULElement */(event.target)).id;
            if (tid == "itsalltext-context-popup" ||
                tid == "contentAreaContextMenu") {
                node = /*: cheat nsIDOMElement */(document.popupNode);
                tag = node.nodeName.toLowerCase();
                doc = node.ownerDocument;
                cstyle = doc.defaultView.getComputedStyle(node, '');
                is_disabled = (!(tag == 'textarea' ||
                                 tag == 'textbox') ||
                               node.style.display == 'none' ||
                               (cstyle && ((""+cstyle.display == 'none') ||
                                           (""+cstyle.visibility == 'hidden')) ||
                                /*:cheat Bool*/(node.getAttribute('readonly')) ||
                                /*:cheat Bool*/(node.getAttribute('disabled'))
                               ));
                if (tid == "itsalltext-context-popup") {
                    cobj = that.CacheObj.get(''+node);
                    that.rebuildMenu(cobj.uid,
                                     'itsalltext-context-popup',
                                     is_disabled);
                } else {
                    // tid == "contentAreaContextMenu"
                    menu = document.getElementById("itsalltext-contextmenu");
                    menu.setAttribute('hidden', is_disabled);
                }

            }
       }
        return true;
    };


    that.openReadme = function () {
        try {
            var browser = getBrowser();
            browser.selectedTab = browser.addTab(that.README);
        } catch (e) {
            //disabled-debug -- that.debug("failed to openReadme:", e);
        }
    };

    // Needed to pu - was inline in the call to listen below
    var test1 = /*: [Any] nsIDOMEvent -> Undef */function (event) {
        //disabled-debug -- that.debug('!!load', event);

        if (typeof(gBrowser) === 'undefined') {
            /*: cheat False */that.monitor.registerPage(event);
        } else {
            // Add a callback to be run every time a document loads.
            // note that this includes frames/iframes within the document

            /*: cheat False */that.listen(gBrowser, "load",
                        that.monitor.registerPage, true);
        }

        // Start watching the preferences.
        that.preference_observer.register();

        // Setup the context menu whenever it is shown.
        var contentAreaContextMenu = document.getElementById("contentAreaContextMenu");
        if (contentAreaContextMenu) {
            // that.listen(contentAreaContextMenu,
            //             'popupshowing',
            //             /*: HitcherType![iatInstance][Any][nsIDOMEvent][Bool] */
            //             /*: cheat [Any] nsIDOMEvent -> Bool */(that.hitch(that, 'onContextMenu')),
            //             false);
        }
    };


    // Do the startup when things are loaded.
    // TODONOW: move to separate function
//    that.listen(window, 'load', test1, false);

    //    TODONOW: move to separate function
    // that.listen(window, 'unload', /*: [Any] nsIDOMEvent -> Undef */ function (event) {
    //     if (typeof(gBrowser) === 'undefined') {
    //         that.monitor.stopPage(event);
    //     }
    //     var doc = event.originalTarget;
    //     //disabled-debug -- that.debug("pageunload(): A page has been unloaded", doc && doc.location);
    //     that.preference_observer.unregister();
    //     that.monitor.destroy();
    // }, false);

    /* Start your engines! */
    /*: cheat False */this.init();
};

ItsAllText.prototype.init = /*: cheat Unsafe */function () {
    /**
     * A serial for tracking ids
     * @type Integer
     */
    this.serial_id = 0;

    /**
     * A constant, a string used for things like the preferences.
     * @type String
     */
    this.MYSTRING = 'itsalltext';

    /**
     * A constant, the version number.  Set by the Makefile.
     * @type String
     */
    this.VERSION = '1.6.3';

    /**
     * A constant, the url to the readme.
     * @type String
     */
    this.README = 'chrome://itsalltext/locale/readme.xhtml';

    /* The XHTML Namespace */
    this.XHTMLNS = "http://www.w3.org/1999/xhtml";

    /* The XUL Namespace */
    this.XULNS   = "http://www.mozilla.org/keymaster/gatekeeper/there.is.only.xul";

    /* The monitor */
    this.monitor = null;

    /* For debugging */
    this.thread_id = Math.round(new Date().getTime() * Math.random());

    /* Clean the edit directory whenever we create a new window. */
    (/*: cheat False */(this.cleanEditDir()));

    /* Load the various bits needed to make this work. */
    this.initScripts();

    /* Start the monitor */
    var itsalltext = this;
    setTimeout(function () {
        itsalltext.monitor = /*: MonitorInst */new Monitor();
    }, 1);
};

/* Load the various bits needed to make this work. */
ItsAllText.prototype.initScripts = function() {
    var loader = Components.classes["@mozilla.org/moz/jssubscript-loader;1"].getService(Components.interfaces.mozIJSSubScriptLoader);
    loader.loadSubScript('chrome://itsalltext/content/Color.js', this);
    loader.loadSubScript('chrome://itsalltext/content/monitor.js', this);
    loader.loadSubScript('chrome://itsalltext/content/cacheobj.js', this);
};

/**
 * Formats a locale string, replacing $N with the arguments in arr.
 * @param {String} name Locale property name
 * @param {Array} arr Array of strings to replace in the string.
 * @returns String
 */
ItsAllText.prototype.localeFormat = function (name, arr) {
    // Liam:  last parameter was arr.length - removed becuase this is a BUG.
    return this.getLocale().formatStringFromName(name, arr);
};
/**
 * Returns the locale string matching name.
 * @param {String} name Locale property name
 * @returns String
 */
ItsAllText.prototype.localeString = function (name) {
    return this.getLocale().GetStringFromName(name);
};

/**
 * Create an error message from given arguments.
 * @param {Object} message One or more objects to be made into strings...
 */
ItsAllText.prototype.logString = function () {
    var arguments = /*: Ext*/null;
    var args = /*: cheat Array<Ext>*/Array.prototype.slice.apply(arguments, [0]),
    i=/*:Num*/0;
    for (i = 0; i < args.length; i++) {
        var e = /*:Ext*/null;
        try {
            args[i] = "" + args[i];
        } catch (e) {
            Components.utils.reportError(e);
            args[i] = 'toStringFailed';
        }
    }
    args.unshift(this.MYSTRING + ' [' + this.thread_id + ']:');
    return args.join(' ');
};

/**
 * This is a handy debug message.  I'll remove it or disable it when
 * I release this.
 * @param {Object} message One or more objects can be passed in to display.
 */
ItsAllText.prototype.log = function () {
    var arguments = /*: Ext*/null;
    const consoleService = Components.classes["@mozilla.org/consoleservice;1"];
    var message = /*: cheat Str */(this.logString.apply(this, arguments)),
    obj = consoleService.getService(Components.interfaces.nsIConsoleService);
    try {
        // idiom: Convert arguments to an array for easy handling.
        obj.logStringMessage(message);
    } catch (e) {
        Components.utils.reportError(message);
    }
};

/**
 * Uses log iff debugging is turned on.  Used for messages that need to
 * globally logged (firebug only logs locally).
 * @param {Object} message One or more objects can be passed in to display.
 */
ItsAllText.prototype.debuglog = function () {
    var arguments = /*: Ext*/null;
    if (this.preferences.debug) {
        /*: cheat Undef */this.log.apply(this, arguments);
    }
};

/**
 * Displays debug information, if debugging is turned on.
 * Requires Firebug.
 * @param {Object} message One or more objects can be passed in to display.
 */
ItsAllText.prototype.debug = function () {
    var arguments = /*: Ext*/null;
    if (this.preferences && this.preferences.debug) {
        var message = /*: cheat Str */(this.logString.apply(this, arguments));
        window.dump(message + '\n');
        try {
            Firebug.Console.logFormatted(arguments);
        } catch (e) {
        }
    }
};

/**
 * This wraps the call to object.method to ensure that 'this' is correct.
 * This is borrowed from GreaseMonkey (though the concept has been around)
 * @method hitch
 * @param {Object} object
 * @param {String} method The method on object to call
 * @returns {Function} A wrapped call to object.method() which passes the arguments.
 */
ItsAllText.prototype.hitch = /*: cheat HitchType */function (object, method) {
    if (! object[method]) {
        throw "method '" + method + "' does not exist on object '" + object + "'";
    }

    var arguments = null;
    var staticArgs = Array.prototype.splice.call(arguments, 2, arguments.length);

    return function () {
        // make a copy of staticArgs (don't modify it because it gets reused for
        // every invocation).
        var args = staticArgs.concat(),
            i=0;

        // add all the new arguments
        for (i = 0; i < arguments.length; i++) {
            args.push(arguments[i]);
        }

        // invoke the original function with the correct this object and
        // the combined list of static and dynamic arguments.
        return  object[method].apply(object, args);
    };
};

/**
 * @method listen
 * @param source {HTMLElement} The element to listen for events on.
 * @param event {String} The name of the event to listen for.
 * @param listener {Function} The function to run when the event is triggered.
 * @param opt_capture {Boolean} Should the event be captured?
 */
ItsAllText.prototype.listen = function (source, event, listener, opt_capture) {
    opt_capture = !!opt_capture;
    this.unlisten(source, event, listener, opt_capture);
    // this.debug("listen(%o, %o, -, %o)", source, event, opt_capture);
    if (source) {
//        source.addEventListener(event, listener, opt_capture);
    }
};

/**
 * Creates a mostly unique hash of a string
 * Most of this code is from:
 *    http://developer.mozilla.org/en/docs/nsICryptoHash
 * @param {String} some_string The string to hash.
 * @returns {String} a hashed string.
 */
ItsAllText.prototype.hashString = function (some_string) {
    var converter = Components.classes["@mozilla.org/intl/scriptableunicodeconverter"].createInstance(Components.interfaces.nsIScriptableUnicodeConverter),
        result = /*:Outparam<Num>*/null,
        data = /*:Array<Num>*/[],
        ch = /*:nsICryptoHash*/null,
        hash = /*:Str*/'',
        toHexString = /*: Num -> Str */null,
        retval = /*:Array<Str>*/[],
        i = /*:Num*/0;
    converter.charset = "UTF-8";

    /* result is the result of the hashing.  It's not yet a string,
     * that'll be in retval.
     * result.value will contain the array length
     */
    // result = {};

    /* data is an array of bytes */
    data = converter.convertToByteArray(some_string, result);
    ch   = Components.classes["@mozilla.org/security/hash;1"].createInstance(Components.interfaces.nsICryptoHash);

    ch.init(ch.MD5);
    // Liam: had additional parameter data.length. Removed because this is a BUG.
    ch.update(data);

    hash = ch.finish(true);

    // return the two-digit hexadecimal code for a byte
    toHexString = function (charCode) {
        return ("0" + charCode.toString(36)).slice(-2);
    };

    // convert the binary hash data to a hex string.
    for (i = 0; i < hash.length; ++i) {
        if (hash.hasOwnProperty(i)) {
            retval[i] = toHexString(hash.charCodeAt(i));
        }
    }

    return (retval.join(""));
};

/**
 * @method unlisten
 * @param source {HTMLElement} The element with the event
 * @param event {String} The name of the event.
 * @param listener {Function} The function that was to be run when the event is triggered.
 * @param opt_capture {Boolean} Should the event be captured?
 */
ItsAllText.prototype.unlisten = function (source, event, listener, opt_capture) {
    opt_capture = !!opt_capture;
    //disabled-debug -- this.debug("unlisten(%o, %o, -, %o)", source, event, opt_capture);
    try {
        /*: cheat @False */(source && source.removeEventListener(event, listener, opt_capture));
    } catch (err) {
        //disabled-debug -- this.debug("didn't unlisten: %o", err);
    }
};

/**
 * Convert an event into a key fingerprint, aka keyprint.
 * @param {Event} event
 * @returns {String} keyprint
 */
ItsAllText.prototype.eventToKeyprint = function (event) {
    return [ event.ctrlKey,
             event.altKey,
             event.metaKey,
             event.shiftKey,
             event.keyCode,
             event.charCode ].join(':');
};

/**
 * Convert a keyprint to a string suitable for human display.
 * @param {String} keyprint
 * @return {String}
 */
ItsAllText.prototype.keyprintToString = function (keyprint) {
    var split = keyprint.split(':'),
        string = /*:Array<Str>*/[];
    if (split[0] === 'true') {
        string.push('Ctrl');
    }
    if (split[1] === 'true') {
        string.push('Alt');
    }
    if (split[2] === 'true') {
        string.push('Meta');
    }
    if (split[3] === 'true') {
        string.push('Shift');
    }
    if (split[4] === '0') {
        string.push(String.fromCharCode(split[5]));
    } else {
        string.push('keyCode=', split[4]);
    }
    return string.join(' ');
};


/**
 * Cleans out the edit directory, deleting all old files.
 */
ItsAllText.prototype.cleanEditDir = /*: cheat Unsafe */ function (force) {
    var e = /*:Ext*/null;
    force = typeof(force) === 'boolean'?force:false;
    var last_week = Date.now() - (1000 * 60 * 60 * 24 * 7),
        fobj = this.getEditDir(),
        entries = fobj.directoryEntries,
        entry = /*: nsIFile */null;
    while (entries.hasMoreElements()) {
        entry = /*: cheat nsIFile */(entries.getNext());
        entry.QueryInterface(Components.interfaces.nsIFile);
        if (force || !entry.exists() || entry.lastModifiedTime < last_week) {
            try {
                entry.remove(false);
            } catch (e) {
                this.log('unable to remove', entry, 'because:', e);
            }
        }
    }
};


/**
 * The command that is called when picking a new extension.
 * @param {Event} event
 */
/*:: rec type ParamsType = {AnObject with out : OutType}
     and type OutType = {Ext with extension : Str}; */
ItsAllText.prototype.menuNewExtEdit = /*: cheat Unsafe*/ function (event) {
    var that = this,
        uid = this.private_current_uid,
        cobj = /*: CacheObjInst */(that.CacheObj.get(uid)),
        params = /*:ParamsType*/{out: null},
        ext = /*:Str*/"";
    (/*: cheat False */window.openDialog("chrome://itsalltext/content/newextension.xul", "", "chrome, dialog, modal, resizable=yes"/*,params*/).focus());
    if (params.out) {
        ext = params.out.extension.replace(/[\n\t ]+/g, '');
        if (params.out.do_save) {
            that.appendExtensions(ext);
        }
        cobj.edit(ext);
    }
};

/**
 * The command that is called when selecting an existing extension.
 * @param {Event} event
 * @param {string} ext
 * @param {boolean} clobber
 */
ItsAllText.prototype.menuExtEdit = /*: cheat Unsafe */function (ext, clobber, event) {
    var uid = this.private_current_uid,
    cobj = /*: CacheObjInst */(this.CacheObj.get(uid));
    if (ext !== null) {
        ext = typeof(ext) === 'string'?ext:(/*:cheat nsIDOMElement*/event.target).getAttribute('label');
    }
    //disabled-debug -- this.debug('menuExtEdit:', uid, ext, clobber);
    cobj.edit(ext, clobber);
};

/**
 * Rebuilds the option menu, to reflect the current list of extensions.
 * @private
 * @param {String} uid The UID to show in the option menu.
 */
ItsAllText.prototype.rebuildMenu =  /*: cheat Unsafe */function (uid, menu_id, is_disabled) {
    menu_id = typeof(menu_id) == 'string'?menu_id:'itsalltext-optionmenu';
    is_disabled = (typeof(is_disabled) === 'undefined' || !is_disabled) ? false : (is_disabled && true);
    var i = 0,
        that = this,
        exts = that.getExtensions(),
        menu = (/*: cheat nsIDOMDocument */document).getElementById(""+menu_id),
        items = menu.childNodes,
        items_length = items.length - 1, // We ignore the preferences item
        node = /*:nsIDOMElement*/null,
        magic_stop_node = /*:nsIDOMElement*/null,
        magic_start = /*: Num */0,
        magic_stop = /*: Num */0,
        cobj = that.CacheObj.get(uid);
    that.private_current_uid = uid;

    // Find the beginning and end of the magic replacement parts.
    for (i = 0; i < items_length; i++) {
        node = /*: cheat nsIDOMElement */items[i];
        if (node.nodeName.toLowerCase() == 'menuseparator') {
            if (magic_start === null) {
                magic_start = i;
            } else if (magic_stop === null) {
                magic_stop = i;
                magic_stop_node = node;
            }
        } else if (node.nodeName.toLowerCase() == 'menuitem') {
            node.setAttribute('disabled', is_disabled?'true':'false');
        }
    }

    // Remove old magic bits
    for (i = magic_stop - 1; i > magic_start; i--) {
        menu.removeChild(items[i]);
    }

    if (cobj.edit_count <= 0 && cobj.file && cobj.file.exists()) {
        node = (/*: cheat nsIDOMDocument */document).createElementNS(""+that.XULNS, 'menuitem');
        node.setAttribute('label', that.localeFormat('edit_existing', [cobj.extension]));
//        that.listen(node, 'command', /*: cheat [Any] nsIDOMEvent -> Undef */(that.hitch(that, 'menuExtEdit', null, false)), false);
        node.setAttribute('disabled', is_disabled?'true':'false');
        menu.insertBefore(node, magic_stop_node);
    }

    // Insert the new magic bits
    for (i = 0; i < exts.length; i++) {
        node = (/*: cheat nsIDOMDocument */document).createElementNS(""+that.XULNS, 'menuitem');
        node.setAttribute('label', that.localeFormat('edit_ext', [exts[i]]));
//        that.listen(node, 'command', /*: cheat [Any] nsIDOMEvent -> Undef */(that.hitch(that, 'menuExtEdit', exts[i], true)), false);
        node.setAttribute('disabled', is_disabled?'true':'false');
        menu.insertBefore(node, magic_stop_node);
    }
    return menu;
};

/**
 * Returns the locale object for translation.
ppp */
ItsAllText.prototype.getLocale = function () {
    var string_bundle = Components.classes["@mozilla.org/intl/stringbundle;1"],
        obj = string_bundle.getService(Components.interfaces.nsIStringBundleService);
    /**
     * A localization bundle.  Use it like so:
     * itsalltext.locale.getStringFromName('blah');
     */
    return obj.createBundle("chrome://itsalltext/locale/itsalltext.properties");
};

var itsalltext = /*: iatInstance */ new ItsAllText();

function(event) { /*: cheat False */itsalltext.onEdit1Node(/*: cheat nsIDOMHTMLTextAreaElement */(document.popupNode)); };
/*: nsIDOMEvent -> Undef*/function(event) { /*: cheat False */itsalltext.menuNewExtEdit(event); };
/*: nsIDOMEvent -> Undef*/function(event) { /*: cheat False */itsalltext.menuExtEdit('.txt', false, event); };
function(event) { itsalltext.openReadme(); };
function(event) { itsalltext.openPreferences(); };
/*: nsIDOMEvent -> Undef*/function(event) { /*: cheat False */itsalltext.menuNewExtEdit(event); };
/*: nsIDOMEvent -> Undef*/function(event) { /*: cheat False */itsalltext.menuExtEdit('.txt', false, event); }; // NEAL: PARAMETERS IN WRONG ORDER
function(event) { itsalltext.openReadme(); };
function(event) { itsalltext.openPreferences(); };
function(event) { itsalltext.openReadme(); };
function(event) { itsalltext.openPreferences(); };


/* ---------------------------- Monitor --------------------- */

var Monitor = /*: MonitorConstructor */function () {
    //     var hitch_re = /^hitched_/,
    //     method;
    //     itsalltext.debug('Monitor', itsalltext);

    //     for (method in this) {
    //         if (hitch_re.test(method)) {

    //             //disabled-debug -- itsalltext.debug('hitching ', method, ' -> ', method.replace(hitch_re, ''));
    //             this[method.replace(hitch_re, '')] = itsalltext.hitch(this, method);
    //         }
    //     }
};

Monitor.prototype.hitched_destroy = function () {
    /*: cheat False */(delete itsalltext);
};

Monitor.prototype.hitched_restart = /*: [MonitorInst] -> Undef */function () {
    var rate = itsalltext.getRefresh(),
        id   = this.id;
    if (id) {
        window.clearInterval(id);
    }
    // BUG
    this.id = window.setInterval((/*: cheat [Any] -> Undef */(this.watcher)), rate);
};

/**
 * Gets a page ready to be used by IAT.
 * This is called as an event handler.
 */
Monitor.prototype.hitched_registerPage = /*: cheat Unsafe */function (event) {
    var doc = /*:nsIDOMEventTarget*/null, appContent = /*:Ext*/null;
    if (event.originalTarget instanceof HTMLDocument) {
        doc = event.originalTarget;
        //disabled-debug -- itsalltext.debug('registerPage: ', doc && doc.location);

        /* appContent is the browser chrome. */
        appContent = document.getElementById("appcontent");
        // itsalltext.listen(appContent, 'DOMContentLoaded', this.startPage, true);
        // itsalltext.listen(document, 'unload', this.stopPage, true);
        // itsalltext.listen(gBrowser.tabContainer, 'TabSelect', /*: cheat [Any] nsIDOMEvent -> Undef */(this.watcher), true);
        this.startPage({originalTarget: doc});
        //disabled-debug -- itsalltext.debug('RegisterPage: END');
    }
};

/* Finds all nodes under a doc; includes iframes and frames. */
Monitor.prototype.hitched_findnodes = /*: [MonitorInst] nsIDOMDocument -> Array<nsIDOMHTMLTextAreaElement> */ function (doc) {
    if (!doc) {
        return /*:Array<nsIDOMHTMLTextAreaElement>*/[];
    }
    var is_html = this.isHTML(doc),
        is_xul  = this.isXUL(doc),
        i = /*:Num*/0,
        tmp = /*:nsIDOMNodeList*/null,
        nodes = /*:Array<nsIDOMHTMLTextAreaElement>*/[],
        iframes = /*:nsIDOMNodeList*/null,
        frames = /*:nsIDOMNodeList*/null;
    if (is_html) {
        /* HTML */
        tmp = (/*: cheat nsIDOMDocument */doc).getElementsByTagName('textarea');
        for (i = 0; i < tmp.length; i++) {
            nodes.push(/*: cheat nsIDOMHTMLTextAreaElement */(tmp[i]));
        }

        /* Now that we got the nodes in this document,
             * look for other documents. */
        iframes = doc.getElementsByTagName('iframe');
        for (i = 0; i < iframes.length; i++) {
            /*: cheat @False */(nodes.push.apply(nodes, (this.findnodes(iframes[i].contentDocument))));
        }

        frames = doc.getElementsByTagName('frame');
        for (i = 0; i < frames.length; i++) {
            /*: cheat @False */(nodes.push.apply(nodes, (this.findnodes(frames[i].contentDocument))));
        }
    } else if (is_xul) {
        /* XUL */
        tmp = (/*: cheat nsIDOMDocument */doc).getElementsByTagName('textbox');
        for (i = 0; i < tmp.length; i++) {
            nodes.push(/*: cheat nsIDOMHTMLTextAreaElement */(tmp[i]));
        }
    } else {
        this.stopPage({originalTarget: doc});
        return /*:Array<nsIDOMHTMLTextAreaElement>*/[];
    }
    return nodes;
};

/**
 * This is called repeatedly and regularly to trigger updates for the
 * cache objects in the page.
 */
Monitor.prototype.hitched_watcher = /*: cheat Unsafe */ function (offset, init) {
    //    If it's a special number or it's an event, then we need to init.
    if (offset.type && offset.type === 'TabSelect') {
        init = true;
    }
    var rate = itsalltext.getRefresh(),
        now = Date.now(),
        doc = /*:nsIDOMDocument*/null,
        nodes = /*:Array<nsIDOMHTMLTextAreaElement>*/[],
        i = /*:Num*/0,
        cobj = /*: CacheObjInst */null,
        node = /*:nsIDOMHTMLTextAreaElement*/null;

    if (!init && now - this.last_watcher_call < Math.round(rate * 0.9)) {
        //disabled-debug -- itsalltext.debug('watcher(', offset, '/', (now - this.last_watcher_call), ') -- skipping catchup refresh');
        return;
    }
    this.last_watcher_call = now;

    if (typeof(gBrowser) === 'undefined') {
        /* If we're in chrome. */
        doc = /*:cheat nsIDOMDocument*/document;
    } else {
        /* If we're in a tabbed browser. */
        doc = gBrowser.selectedBrowser.contentDocument;
    }
    //disabled-debug -- itsalltext.debug('watcher: ', offset, init, doc && doc.location);
    nodes = this.findnodes(doc);
    /* Now that we have the nodes, walk through and either make or
     * get the cache objects and update them. */
    for (i = 0; i < nodes.length; i++) {
        node = nodes[i];
        if (init) {
            cobj = itsalltext.CacheObj.make(node, this.isHTML(doc));
        } else {
            cobj = itsalltext.CacheObj.get(''+node);
        }
        if (cobj) {
            cobj.update();
        }
    }
};

Monitor.prototype._lock_count = 0;

Monitor.prototype.hitched_incrementLock = /*: [MonitorInst] -> Undef */ function () {
    this._lock_count ++;
};
Monitor.prototype.hitched_decrementLock = /*: [MonitorInst] -> Undef */ function () {
    this._lock_count --;
};
Monitor.prototype.hitched_isLocked = /*: [MonitorInst] -> Bool */ function () {
    return this._lock_count > 0;
};

Monitor.prototype.hitched_handleSubtreeModified = /*: cheat Unsafe */ function (event) {
    var has_textareas = /*:Bool*/ false;
    if (this.isLocked()) {
        return;
    }
    has_textareas = (/*:cheat nsIDOMDocument */event.originalTarget).getElementsByTagName('textarea').length > 0;
    if (has_textareas) {
        //disabled-debug -- itsalltext.debug('handleSubtreeModified: %o', event.target);
        try {
            // Ignore events while adding the gumdrops.
            this.incrementLock();
            this.watcher(0, true);
        } catch (e) {
            this.decrementLock();
        }
        this.decrementLock();
     }
};

Monitor.prototype.hitched_startPage = /*: cheat Unsafe */function (event, force) {
    var doc = /*: cheat nsIDOMDocument */(event.originalTarget),
        unsafeWin = /*: nsIDOMWindow */null;
    //disabled-debug -- itsalltext.debug('startPage', doc && doc.location, force);
    if (!(force || this.isHTML(doc))) {
        this.stopPage(event);
        return;
    }

    unsafeWin = (/*: cheat nsIDOMWindow */(doc.defaultView.wrappedJSObject));
    if (unsafeWin) {
//        itsalltext.listen(unsafeWin, 'pagehide', this.stopPage);
    }

    // Listen for the subtree being modified.
//    itsalltext.listen(unsafeWin, 'DOMSubtreeModified', this.handleSubtreeModified);

    // Kick off a watcher now...
    this.incrementLock();
    this.watcher(0, true);
    this.decrementLock();

    // Set up the future ones
     this.restart();
};

Monitor.prototype.hitched_stopPage = function (event) {
    var doc = event.originalTarget,
    unsafeWin = /*: nsIDOMWindow */null;;
    //disabled-debug -- itsalltext.debug('stopPage', doc && doc.location);

    unsafeWin = /*: cheat nsIDOMWindow */(doc.defaultView.wrappedJSObject);
    if (unsafeWin && itsalltext) {
//        itsalltext.unlisten(unsafeWin, 'pagehide', this.stopPage);
    }
};

Monitor.prototype.isXUL = function (doc) {
    var contentType = doc && doc.contentType,
             is_xul = (contentType == 'application/vnd.mozilla.xul+xml'),
             is_my_readme = /*: Bool*/false;
    try {
        is_my_readme = (window.location && window.location.href == itsalltext.README);
    } catch (e) {
        is_my_readme = false;
    }
    return is_xul && !is_my_readme;
};

Monitor.prototype.isHTML = function (doc) {
    var contentType = /* Str */""+null,
        location = /*: nsIDOMLocation */null,
        is_html = /*: Bool*/false,
        is_usable = /*: Bool */false,
        is_my_readme = /*: Bool */false;
    /* Check that this is a document we want to play with. */
    contentType = doc.contentType;
    location = doc.location;
    is_html = (contentType == 'text/html' ||
               contentType == 'text/xhtml' ||
               contentType == 'application/xhtml+xml');
    is_usable = is_html &&
                location &&
                location.protocol !== 'about:' &&
                location.protocol !== 'chrome:';
    try {
        is_my_readme = location && location.href == itsalltext.README;
        /*
         * Avoiding this error.... I hope.
         * uncaught exception: [Exception... "Component returned failure code: 0x80004003 (NS_ERROR_INVALID_POINTER) [nsIDOMLocation.href]"  nsresult: "0x80004003 (NS_ERROR_INVALID_POINTER)"  location: "JS frame :: chrome://itsalltext/chrome/itsalltext.js :: anonymous :: line 634"  data: no]
         * Line 0
         */
    } catch (e) {
        is_my_readme = false;
        is_usable = false;
    }
    return is_usable && !is_my_readme;
};


/* ----------------------- Color ---------------------------------- */


var Color = /*: ColorConstructor */null;

// Liam: Had to add dummy arguments
Color = function (a1, a2, a3, a4) {
    this.alpha = /*: Num */0;
    this.red = /*: Num */0;
    this.green = /*: Num */0;
    this.blue = /*: Num */0;

    var keyword = /*: keywordType */ null,
        func = /*: funcType */null,
        clamp = /*: Num * Num * Num -> Num */null,
        alphaBlend = /*: Num * Num * Num -> Num */null,
        value = /*: Str */""+null,
        components = /*: Array<Str> */[],
        pattern = /*: Str */""+null,
        key = /*: Str */""+null,
        base = /*: Num */0,
        m = /*: Num */0,
        r = /*: Num */0,
        g = /*: Num */0,
        b = /*: Num */0,
        a = /*: Num */0;

    // CSS 2.1 Colour Keywords
    keyword = /*: cheat keywordType */{
        maroon   : "#800000",
        red      : "#ff0000",
        orange   : "#ffA500",
        yellow   : "#ffff00",
        olive    : "#808000",
        purple   : "#800080",
        fuchsia  : "#ff00ff",
        white    : "#ffffff",
        lime     : "#00ff00",
        green    : "#008000",
        navy     : "#000080",
        blue     : "#0000ff",
        aqua     : "#00ffff",
        teal     : "#008080",
        black    : "#000000",
        silver   : "#c0c0c0",
        gray     : "#808080",
    };

    // CSS Functional Notations and Hex Patterns
    func = /*: cheat funcType */{
        rgb   : /^rgb\(\s*(\d{1,3})\s*,\s*(\d{1,3})\s*,\s*(\d{1,3})\s*\);?$/,
        "rgb%"  : /^rgb\(\s*(\d{1,3})%\s*,\s*(\d{1,3})%\s*,\s*(\d{1,3})%\s*\);?$/,
        rgba  : /^rgba\(\s*(\d{1,3})\s*,\s*(\d{1,3})\s*,\s*(\d{1,3})\s*,\s*((?:\d+(?:\.\d+)?)|(?:\.\d+))\s*\);?$/,
        "rgba%" : /^rgba\(\s*(\d{1,3})%\s*,\s*(\d{1,3})%\s*,\s*(\d{1,3})%\s*,\s*((?:\d+(?:\.\d+)?)|(?:\.\d+))\s*\);?$/,
        hex3  : /^#([0-9A-Fa-f])([0-9A-Fa-f])([0-9A-Fa-f]);?$/,
        hex6  : /^#([0-9A-Fa-f]{2})([0-9A-Fa-f]{2})([0-9A-Fa-f]{2});?$/
    };

    /**
     * Clamp the value between the low value and the high value
     * @private
     */
    clamp = function (value, low, high) {
        if (value < low) {
            value = low;
        }
        else if (value > high) {
            value = high;
        }
        return value;
    };

    /**
     * @private
     */
    alphaBlend = function (forground, background, alpha) {
        return Math.round(background * (1.0 - alpha) + forground * (alpha));
    };

    /*
     * Return the colour in hexadecimal notation: #RRGGBB. e.g. #FF9933
     * @param bg - Optional parameter used for calculating the colour if an alpha value less than 1.0 has been specified.
     *             If not specified, the alpha value will be ignored.
     */
    this.hex = function (bg) {
        var r = /*: Num */0,
            g = /*: Num */0,
            b = /*: Num */0,
            strHexR = /*: Str */""+null,
            strHexG = /*: Str */""+null,
            strHexB = /*: Str */""+null;
        if (bg) {
            r = alphaBlend(this.red, bg.red, this.alpha);
            g = alphaBlend(this.green, bg.green, this.alpha);
            b = alphaBlend(this.blue, bg.blue, this.alpha);
        } else {
            r = this.red;
            g = this.green;
            b = this.blue;
        }

        strHexR = r.toString(16).toUpperCase();
        strHexG = g.toString(16).toUpperCase();
        strHexB = b.toString(16).toUpperCase();

        if (strHexR.length < 2) {
            strHexR = "0" + strHexR;
        }
        if (strHexG.length < 2) {
            strHexG = "0" + strHexG;
        }
        if (strHexB.length < 2) {
            strHexB = "0" + strHexB;
        }

        return "#" + strHexR + strHexG + strHexB;
    };

    /**
     * Return the colour in CSS rgb() functional notation, using integers 0-255: rgb(255, 255 255);
     * @param bg - Optional parameter used for calculating the colour if an alpha value less than 1.0 has been specified.
     *             If not specified, the alpha value will be ignored.
     */
    this.rgb = function (bg) {
        var r = /*: Num */0,
            g = /*: Num */0,
            b = /*: Num */0;
        if (bg) {
            r = alphaBlend(this.red, bg.red, this.alpha);
            g = alphaBlend(this.green, bg.green, this.alpha);
            b = alphaBlend(this.blue, bg.blue, this.alpha);
        } else {
            r = this.red;
            g = this.green;
            b = this.blue;
        }

        return "rgb(" + r + ", " + g + ", " + b + ")";
    };

    /**
     * Return the colour in CSS rgba() functional notation, using integers 0-255 for color components: rgb(255, 255 255, 1.0);
     * @param bg - Optional parameter used for calculating the colour if an alpha value less than 1.0 has been specified.
     *             If not specified, and there is an alpha value, black will be used as the background colour.
     */
    this.rgba = function () {
        return "rgba(" + this.red + ", " + this.green + ", " + this.blue + ", " + this.alpha + ")";
    };

    /**
     * Returns a Color object with the values inverted. Ignores alpha.
     */
    this.invert = function () {
        return /*: ColorInst */new Color(
            "rgb(" +
                (255 - this.red) + ", " +
                (255 - this.green) + ", " +
                (255 - this.blue) + ")");
    };

    /**
     * Blend this colour with the colour specified and return a pallet with all the steps in between.
     * @param color - The colour to blend with
     * @param steps - The number of steps to take to reach the color.
     */
    this.blend = function (color, steps) {
        var pallet = /*: Array<ColorInst>*/[],
            r = /*: Num */0,
            g = /*: Num */0,
            b = /*: Num */0,
            i = /*: Num */0,
            step = {
            red   : (alphaBlend(color.red, this.red, color.alpha) - this.red) / steps,
            green : (alphaBlend(color.green, this.green, color.alpha) - this.green) / steps,
            blue  : (alphaBlend(color.blue,  this.blue,  color.alpha) - this.blue) / steps
        };
        for (i = 0; i < steps + 1; i++) {
            r = Math.round(this.red   + (step.red * i));
            g = Math.round(this.green + (step.green * i));
            b = Math.round(this.blue  + (step.blue * i));
            pallet.push(new Color(r, g, b));
        }
        return pallet;
    };

    /**
     * Constructor function
     */
    this.toString = this.hex;

    if (window.arguments.length >= 3) {
        /* r, g, b or r, g, b, a */
        r = /*: cheat Num */(window.arguments[0]);
        g = /*: cheat Num */(window.arguments[1]);
        b = /*: cheat Num */(window.arguments[2]);
        a = /*: cheat Num */(window.arguments[3]);

        this.red   = (!isNaN(r)) ? clamp(r, 0, 255) : 0;
        this.green = (!isNaN(g)) ? clamp(g, 0, 255) : 0;
        this.blue  = (!isNaN(b)) ? clamp(b, 0, 255) : 0;
        this.alpha = (!isNaN(a)) ? clamp(a, 0.0, 1.0) : 1.0;
    } else if (window.arguments.length == 1) {
        /* CSS Colour keyword or value */
        value = (/*: cheat Str */keyword[arguments[0]]) ?
            (/*: cheat Str */keyword[arguments[0]]) :
            /*: cheat Str */arguments[0];
        for (key in func) {
            if (func[key].test(value)) {
                pattern = key;
            }
        }

        components = value.match(/*: cheat RegExp */func[pattern]);
        base = 10;
        m = 1; // Multiplier for percentage values

        switch (pattern) {
        case "rgb%":
        case "rgba%":
            m = 2.55;
            base = 10;
            break;
        case "rgb":
        case "rgba":
            base = 10;
            break;
        case "hex3":
            components[1] = components[1] + "" + components[1];
            components[2] = components[2] + "" + components[2];
            components[3] = components[3] + "" + components[3];
            base = 16;
            break;
        case "hex6":
            base = 16;
            break;
        default:
            components = ["0", "255", "255", "255", "1.0"];
        }

        this.red   = clamp(Math.round(parseInt(components[1], base) * m), 0, 255);
        this.green = clamp(Math.round(parseInt(components[2], base) * m), 0, 255);
        this.blue  = clamp(Math.round(parseInt(components[3], base) * m), 0, 255);

        if (typeof(components[4]) === 'undefined' || isNaN(components[4])) {
            this.alpha = 1;
        } else {
            this.alpha = clamp(parseFloat("0" + components[4]), 0.0, 1.0);
        }
    }
};

/* -------------------------- CacheObj --------------------------- */

/**
 * A Cache object is used to manage the node and the file behind it.
 * @constructor
 * @param {Object} node A DOM Node to watch.
 */
var CacheObj = /*: Constructing<CacheObjConstructor> */ function (node) {
    var that = this,
        hitch_re = /^hitched_/,
        doc = node.ownerDocument,
        host = /*: Str*/""+null,
        hash = /*: Str */""+null,
        extension = /*: Str */""+null;

    this.uuid = Math.floor(Math.random()*2000);
    //disabled-debug -- itsalltext.debug('CacheObject ', this.uuid, node);

    // TODO: hitch
    // for (method in this) {
    //     if (hitch_re.test(method)) {
    //
    //         //disabled-debug -- itsalltext.debug('CacheObj ', this.uuid, 'hitching ', method, ' -> ', method.replace(hitch_re, ''));
    //         this[method.replace(hitch_re, '')] = itsalltext.hitch(this, method);
    //     }
    // }

    /* Gumdrop Image URL */
    that.gumdrop_url    = 'chrome://itsalltext/locale/gumdrop.png';
    /* Gumdrop Image Width */
    that.gumdrop_width  = itsalltext.localeString('gumdrop.width');
    /* Gumdrop Image Height */
    that.gumdrop_height = itsalltext.localeString('gumdrop.height');

    that.timestamp = 0;
    that.size = 0;
    that.node = node;
    that.button = /*: nsIDOMHTMLElement */null;
    that.initial_background = '';
    that.private_is_watching = /*: Bool */false;
    that.button_fade_timer = 0;
    that.is_focused = /*: Bool */false;
    that.is_listening = /*: Bool */false;

    that.node_id = that.getNodeIdentifier(node);

    /* This is a unique identifier for use on the web page to prevent the
     * web page from knowing what it's connected to.
     * @type String
     */
    that.uid = itsalltext.hashString([ doc.location.toString(),
                                       Math.random(),
                                       that.node_id ].join(':'));
    // @todo [security] Add a serial to the uid hash.

    node.setUserData(itsalltext.MYSTRING + '_UID', /*: cheat nsIVariant */(that.uid), null);
    /*: cheat False */itsalltext.addToTracker(/*: cheat realisticStr */(that.uid), that);

    /* Figure out where we will store the file.  While the filename can
     * change, the directory that the file is stored in should not!
     */
    host = window.escape(doc.location.hostname);
    hash = itsalltext.hashString([ doc.location.protocol,
           doc.location.port,
           doc.location.search ? doc.location.search : '?',
           doc.location.pathname,
           that.node_id].join(':'));
    that.base_filename = [host, hash.slice(0, 10)].join('.');
    /* The current extension.
     * @type String
     */
    that.extension = /*: Str */""+ null;

    /* Stores an nsILocalFile pointing to the current filename.
     * @type nsILocalFile
     */
    that.file = /*: nsIFile */null;

    /* The number of edits done on this object.
     * @type number
     */
    that.edit_count = 0;

    /* Set the default extension and create the nsIFile object. */
    extension = node.getAttribute('itsalltext-extension');
    if (typeof(extension) != 'string' || !extension.match(/^[.a-z0-9]+$/i)) {
        extension = itsalltext.getExtensions()[0];
    }
    /*: cheat False */that.setExtension(extension);

    /*: cheat False */that.initFromExistingFile();

    /**
     * A callback for when the textarea/textbox or button has
     * the mouse waved over it.
     * @param {Event} event The event object.
     */
    that.mouseover = /*: cheat Unsafe */function (event) {
        if (event.type === 'focus') {
            that.is_focused = true;
        }
        if (that.button_fade_timer) {
            clearTimeout(that.button_fade_timer);
        }
        var style = that.button?that.button.style:null;
        if (style) {
            style.setProperty('opacity', '0.7',   'important');
            style.setProperty('display', 'block', 'important');
        }

        // Refresh the Textarea.
        that.update();
        that.addGumDrop();
    };

    // /**
    //  * A callback for when the textarea/textbox or button has
    //  * the mouse waved over it and the moved off.
    //  * @param {Event} event The event object.
    //  */
    that.mouseout = function (event) {
        //disabled-debug -- itsalltext.debug("mouseout: %o", event, event.target, that.is_focused);
        if (that.button_fade_timer) {
            clearTimeout(that.button_fade_timer);
        }
        if (that.is_focused && event.type !== 'blur') {
            /* we're focused, don't fade until we're blurred. */
            return;
        }
        that.is_focused = false;

        var style = that.button?that.button.style:null,
            f = /*: -> Undef*/null,
            cur  = 0.7,
            dest = 0,
            fps  = 12,
            num_frames = (itsalltext.preferences.fade_time * fps),
            increment = (cur - dest) / num_frames,
            wait = (1 / fps) / 1000;
        if (style) {
            f = function () {
                cur -= increment;
                if (cur > dest) {
                    style.setProperty('opacity', cur.toString(), 'important');
                    that.button_fade_timer = setTimeout(f, wait);
                } else {
                    style.setProperty('display', 'none', 'important');
                }
            };
            f();
        }
    };
};

/**
 * Destroys the object, unallocating as much as possible to prevent leaks.
 */
CacheObj.prototype.destroy = function () {
    //disabled-debug -- itsalltext.debug('destroying', this.node_id, this.uid);
    var node = this.node,
        doc  = this.node.ownerDocument,
        html = doc.getElementsByTagName('html')[0];

    //node.removeAttribute(itsalltext.MYSTRING + '_UID');
    //html.removeAttribute(itsalltext.MYSTRING + '_id_serial');

    /*: cheat False*/delete this.node;
    /*: cheat False */delete this.button;
    /*: cheat False */delete this.file;
    this.file = this.node = this.button = null;
};

/**
 * Set the extension for the file to ext.
 * @param {String} ext The extension.  Must include the dot.  Example: .txt
 */
// PASSES
CacheObj.prototype.setExtension = /*: cheat Unsafe */function (ext) {
    if (ext == this.extension && this.file) {
        return; /* It's already set.  No problem. */
    }

    /* Create the nsIFile object */
    var file = itsalltext.factoryFile();
    file.initWithFile(itsalltext.getEditDir());
    file.append([this.base_filename, ext].join(''));

    this.extension = ext;
    this.file = file;
    if (file.exists()) {
        this.timestamp = file.lastModifiedTime;
        this.size      = file.fileSize;
    }
};

/**
 * This function looks for an existing file and starts to monitor
 * if the file exists already.  It also deletes all existing files for
 * this cache object.
 */
// PASSES
CacheObj.prototype.initFromExistingFile = /*: cheat Unsafe */function () {
    var base = this.base_filename,
        fobj = itsalltext.getEditDir(),
        entries = fobj.directoryEntries,
        ext = /*: Str */""+null,
        tmpfiles = /(\.bak|.tmp|~)$/,
        entry = /*: nsIFile */null;
    while (entries.hasMoreElements()) {
        entry = /*: cheat nsIFile */(entries.getNext());
        entry.QueryInterface(Components.interfaces.nsIFile);
        if (entry.leafName.indexOf(base) === 0) {
            // startswith
            if (ext === null && !entry.leafName.match(tmpfiles)) {
                ext = entry.leafName.slice(base.length);
                continue;
            }
            try {
                entry.remove(false);
            } catch (e) {
                //disabled-debug -- itsalltext.debug('unable to remove', entry, 'because:', e);
            }
        }
    }
    if (ext !== null) {
        this.setExtension(ext);
        this.private_is_watching = true;
    }
};

/**
 * Returns a unique identifier for the node, within the document.
 * @returns {String} the unique identifier.
 */
// PASSES
CacheObj.prototype.getNodeIdentifier = function (node) {
    var id  = node.getAttribute('id'),
        name = /*: Str */""+null,
        doc = /*: nsIDOMElement */null,
        attr = /*: Str */""+null,
        serial1 = /*: nsIVariant */null,
        serial2 = 0;
    if (!id) {
        name = node.getAttribute('name');
        doc = node.ownerDocument.getElementsByTagName('html')[0];
        attr = itsalltext.MYSTRING + '_id_serial';

        /* Get a serial that's unique to this document */
        serial1 = doc.getUserData(attr);
        if (serial1) {
            serial2 = parseInt(serial1, 10) + 1;
        } else {
            serial2 = 1;
        }
        id = [itsalltext.MYSTRING, 'generated_id', name, serial2].join('_');
        doc.setUserData(attr, /*: cheat nsIVariant */(serial2), null);
        node.setAttribute('id', id);
    }
    return id;
};

/**
 * Convert to this object to a useful string.
 * @returns {String} A string representation of this object.
 */
CacheObj.prototype.toString = function () {
    return [ "CacheObj",
             " uid=", this.uid,
             " timestamp=", this.timestamp,
             " size=", this.size].join('');
};

/**
 * Write out the contents of the node.
 *
 * @param {boolean} clobber Should an existing file be clobbered?
 */
CacheObj.prototype.write = /*: cheat @Unsafe */ function (clobber) {
    clobber = typeof(clobber) === 'boolean'?clobber:true;
    var foStream, conv, text;

    if (clobber) {
        foStream = Components.
            classes["@mozilla.org/network/file-output-stream;1"].
            createInstance(Components.interfaces.nsIFileOutputStream);

        /* write, create, truncate */
        foStream.init(this.file, 0x02 | 0x08 | 0x20,
                      parseInt('0600', 8), 0);

        /* We convert to chasret */
        conv = Components.
            classes["@mozilla.org/intl/scriptableunicodeconverter"].
            createInstance(Components.interfaces.nsIScriptableUnicodeConverter);
        conv.charset = itsalltext.getCharset();

        text = conv.ConvertFromUnicode(this.node.value);
        foStream.write(text, text.length);
        foStream.close();

        /* Reset Timestamp and filesize, to prevent a spurious refresh */
        this.timestamp = this.file.lastModifiedTime;
        this.size      = this.file.fileSize;
    } else {
        this.timestamp = this.size = null; // force refresh of textarea
    }

    return this.file.path;
};

/**
 * Fetches the computed CSS attribute for a specific node
 * @param {DOM} node The DOM node to get the information for.
 * @param {String} attr The CSS-style attribute to fetch (not DOM name).
 * @returns attribute
 */
CacheObj.prototype.getStyle = function (node, attr) {
    var view  = node ? node.ownerDocument.defaultView : null,
        style = view.getComputedStyle(node, '');
    return  style.getPropertyCSSValue(attr).cssText;
};

// @todo [9] IDEA: Pass in the line number to the editor, arbitrary command?
// @todo [9] IDEA: Allow the user to pick an alternative editor?
// @todo [9] IDEA: A different editor per extension?
/**
 * Edit a textarea as a file.
 * @param {String} extension The extension of the file to edit.
 * @param {boolean} clobber Should an existing file be clobbered?
 */
 CacheObj.prototype.edit = /*: cheat @Unsafe */ function (extension, clobber) {
     itsalltext.debug(this.uuid, 'edit(', extension, ', ', clobber, ')', this.uid);
     extension = typeof(extension) === 'string'?extension:this.extension;
     this.setExtension(extension);

     var filename = this.write(clobber),
     program = null,
     command,
     process,
     args,
     result,
     ec,
     params,
     procutil;
     procutil = Components.classes["@mozilla.org/process/util;1"];
     this.initial_background = this.node.style.backgroundColor;
     this.initial_color      = this.node.style.color;


     try {
         program = itsalltext.getEditor();

         // checks
         if (program === null) {
             throw {name: "Editor is not set."};
         }

         if (!program.exists()) {
             throw {name: "NS_ERROR_FILE_NOT_FOUND"};
         }

         if (itsalltext.isDarwin() &&
             program.isDirectory() &&
             program.leafName.match(/\.app$/i)) {
             // OS-X .app bundles should be run with open.
             args = ['-a', program.path, filename];
             program = itsalltext.factoryFile('/usr/bin/open');
         } else {
             /* Mac check because of
              * https://bugzilla.mozilla.org/show_bug.cgi?id=322865 */
             if (!(itsalltext.isDarwin() || program.isExecutable())) {
                 throw {name: "NS_ERROR_FILE_ACCESS_DENIED"};
             }
             args = [filename];
         }

         // Create an observer
         var observer          = {
             observe: function (subject, topic, data) {
                 // Topic moved as last argument to callbacks since we don't need it (we already know what it is)
                 if (topic==='process-finished') {
                     if (typeof(subject.exitValue) != 'undefined' && subject.exitValue != 0) {
                         var prompts = Components.classes["@mozilla.org/embedcomp/prompt-service;1"]
                             .getService(Components.interfaces.nsIPromptService);
                         prompts.alert(null, "Editor exited with status of " + subject.exitValue,
                                       "I ran this command: " + program.path + " " + (args.join(' ')) + "\n\n...and it exited with a status of " + subject.exitValue + ".");
                     }
                     itsalltext.debug("Process exited successfully: ", subject, data);
                 }
                 else if (topic === 'process-failed') {
                     itsalltext.debug("Process exited unsuccessfully: ", subject, data);
                 } else {
                     itsalltext.debug("Observer had a hard time: ", subject, topic, data);
                 }
             }
         };

         // create an nsIProcess
         process = procutil.createInstance(Components.interfaces.nsIProcess);
         process.init(program);

         // Run the process.
         if (typeof process.runwAsync == 'undefined') {
             // FF < 4.0
             process.runAsync(args, args.length, observer, false);
         } else {
             // FF >= 4.0 - Wide character support.
             process.runwAsync(args, args.length, observer, false);
         }

         this.private_is_watching = true;
         this.edit_count++;
     } catch (e) {
         itsalltext.debug("Caught error launching editor: ", e);
         params = { out: null,
                    exists: program ? program.exists() : false,
                    path: itsalltext.preferences.editor,
                    exception: e.name };
         window.openDialog('chrome://itsalltext/content/badeditor.xul',
                           null,
                           "chrome, titlebar, toolbar, centerscreen, modal",
                           params);
         if (params.out !== null && params.out.do_preferences) {
             itsalltext.openPreferences(true);
             this.edit(extension);
         }
     }
 };

/**
 * Delete the file from disk.
 */
// PASSES
CacheObj.prototype.remove = function () {
    if (this.file.exists()) {
        try {
            this.file.remove();
        } catch (e) {
            //disabled-debug -- itsalltext.debug('remove(', this.file.path, '): ', e);
            return false;
        }
    }
    return true;
};

/**
 * Read the file from disk.
 */
// PASSES
CacheObj.prototype.read = function () {
    /* read file, reset ts & size */
    var DEFAULT_REPLACEMENT_CHARACTER = 65533,
    buffer = /*: Array<Str> */[],
    fis = /*: nsIFileInputStream */null,
    istream = /*: nsIConverterInputStream */null,
    str = /*: Outparam<Str>*/null;

    try {
        fis = Components.classes["@mozilla.org/network/file-input-stream;1"].
            createInstance(Components.interfaces.nsIFileInputStream);
        fis.init(this.file, 0x01, parseInt('00400', 8), 0);
        // MODE_RDONLY | PERM_IRUSR

        istream = Components.classes["@mozilla.org/intl/converter-input-stream;1"].
            createInstance(Components.interfaces.nsIConverterInputStream);
        istream.init(fis, itsalltext.getCharset(), 4096, DEFAULT_REPLACEMENT_CHARACTER);

        str = /*: Outparam<Str> */null;
        while (istream.readString(4096, str) !== 0) {
            buffer.push(str.value);
        }

        istream.close();
        fis.close();

        this.timestamp = this.file.lastModifiedTime;
        this.size      = this.file.fileSize;

        return buffer.join('');
    } catch (e) {
        return "";
    }
    return "";
};

/**
 * Has the file object changed?
 * @returns {boolean} returns true if the file has changed on disk.
 */
// PASSES
CacheObj.prototype.hasChanged = function () {
    /* Check exists.  Check ts and size. */
    return this.private_is_watching &&
           this.file &&
           this.file.exists() &&
           this.file.isReadable() &&
           (this.file.lastModifiedTime != this.timestamp ||
            this.file.fileSize         != this.size);
};

/**
 * Part of the fading technique.
 * @param {Object} pallet A Color blend pallet object.
 * @param {int}    step   Size of a step.
 * @param {delay}  delay  Delay in microseconds.
 */
CacheObj.prototype.fadeStep = function (background_pallet, color_pallet, step, delay) {
    var that = this;
    return function () {
        if (step < background_pallet.length) {
            that.node.style.backgroundColor = background_pallet[step].hex();
            that.node.style.color = color_pallet[step].hex();
            step++;
            setTimeout(that.fadeStep(background_pallet, color_pallet, step, delay), delay);
        } else {
            that.node.style.backgroundColor = that.initial_background;
            that.node.style.color = that.initial_color;
        }
    };
};

/**
 * Node fade technique.
 * @param {int} steps  Number of steps in the transition.
 * @param {int} delay  How long to wait between delay (microseconds).
 */
CacheObj.prototype.fade = function (steps, delay) {
    var color         = this.getStyle(this.node, 'color');
    var color_stop    = /* ColorInst */new Color(color),
    color_start       = /* ColorInst */new Color('black'),
    color_pallet      = color_start.blend(color_stop, steps),

    background        = this.getStyle(this.node, 'background-color'),
    background_stop   = /*  ColorInst */new Color(background),
    background_start  = /*  ColorInst */new Color('yellow'),
    background_pallet = background_start.blend(background_stop, steps);
    setTimeout(this.fadeStep(background_pallet, color_pallet, 0, delay), delay);
};

/**
 * Update the node from the file.
 * @returns {boolean} Returns true ifthe file changed.
 */
// PASSES
CacheObj.prototype.update = function () {
    var value = /*: Str */""+null;
    if (this.hasChanged()) {
        value = this.read();
        if (value !== null) {
            this.fade(20, 100);
            (/*: cheat Mutable<nsIDOMHTMLTextAreaElement> */(this.node)).value = ""+value;
            var event = (/*: cheat nsIDOMDocument */document).createEvent("HTMLEvents");
            event.initEvent('change', true, false);
            this.node.dispatchEvent(event);

            return true;
        }
    }
    return false; // If we fall through, we
};

/**
 * Capture keypresses to do the hotkey edit.
 */
// PASSES
CacheObj.prototype.hitched_keypress = /*: cheat Unsafe */function (event) {
    itsalltext.debug(this.uuid, 'keypress()', event);
    var km = itsalltext.marshalKeyEvent(event),
             cobj = /*: CacheObjInst */ null;
    if (km === itsalltext.preferences.hotkey) {
        cobj = CacheObj.get(/*: cheat nsIDOMHTMLTextAreaElement */(event.target));
        cobj.edit();
        event.stopPropagation();
    }
    return false;
};

/**
 * The function to execute when a gumdrop is clicked.
 * @param {Object} event The event that triggered this.
 */
// PASSES
CacheObj.prototype.onClick = /*: cheat Unsafe */function (event) {
    //disabled-debug -- itsalltext.debug('onClick()', event);
    var cobj = CacheObj.get(/*: cheat nsIDOMHTMLTextAreaElement */(event.target));
    cobj.edit();
    event.stopPropagation();
    return false;
};

/**
 * The function to execute when a gumdrop is right clicked (context)
 * @param {Object} event The event that triggered this.
 */
// PASSES
CacheObj.prototype.onContext = /*: cheat Unsafe */function (event) {
    /* This took forever to fix; roughly 80+ man hours were spent
     * over 5 months trying to make this stupid thing work.
     * The documentation is completely wrong and useless.
     *
     * Excuse me while I scream.
     *
     * See Mozilla bugs:
     * https://bugzilla.mozilla.org/show_bug.cgi?id=287357
     * https://bugzilla.mozilla.org/show_bug.cgi?id=291083
     *
     * This is actually fixed in FF3 by replacing it with something
     * sane....openPopup()
     */
    var cobj = CacheObj.get(/*: cheat nsIDOMHTMLTextAreaElement */(event.target)),
        popup = /*: cheat nsIPopupBoxObject */itsalltext.rebuildMenu(cobj.uid);

    // TODO: I had to add event as the last parameter because
    // there WAS no event there before, which is wrong. ???
    popup.openPopup(cobj.button, 'end_before',
                    0, 0,
                    true, false, event);

    event.stopPropagation();
    event.preventDefault();
    return false;
};


/**
 * Add the gumdrop to a textarea.
 * @param {Object} cache_object The Cache Object that contains the node.
 */
// PASSES
CacheObj.prototype.addGumDrop = /*: cheat Unsafe */function () {
    var cache_object = this,
        node = /*: nsIDOMHTMLTextAreaElement */null,
        doc = /*: nsIDOMDocument */null,
        gumdrop = /*: nsIDOMHTMLElement */null,
        parent = /*: nsIDOMNode */null,
        nextSibling = /*: nsIDOMNode */null;

    try {
        itsalltext.monitor.incrementLock();

        if (cache_object.button !== null) {
            cache_object.adjust();
            itsalltext.monitor.decrementLock();
            return; /*already done*/
        }

        // Add the textarea mouseovers even if the button is disabled
        node = cache_object.node;
        itsalltext.debug('addGumDrop', cache_object.uuid, node);
        if (!cache_object.is_listening) {
            // itsalltext.listen(node, "mouseover", itsalltext.hitch(cache_object, "mouseover"), false);
            // itsalltext.listen(node, "mouseout",  itsalltext.hitch(cache_object, "mouseout"),  false);
            // itsalltext.listen(node, "focus",     itsalltext.hitch(cache_object, "mouseover"), false);
            // itsalltext.listen(node, "blur",      itsalltext.hitch(cache_object, "mouseout"),  false);
            // itsalltext.listen(node, "keypress",  cache_object.keypress,  false);
            cache_object.is_listening = true;
        }
        if (itsalltext.getDisableGumdrops()) {
            itsalltext.monitor.decrementLock();
            return;
        }
        itsalltext.debug('addGumDrop()', cache_object);

        doc = node.ownerDocument;
        if (!node.parentNode) {
            itsalltext.monitor.decrementLock();
            return;
        }

        gumdrop = (/*: cheat nsIDOMHTMLElement */(doc.createElementNS(itsalltext.XHTMLNS, "img")));
        gumdrop.setAttribute('src', this.gumdrop_url);

        if (itsalltext.getDebug()) {
            gumdrop.setAttribute('title', cache_object.node_id);
        } else {
            gumdrop.setAttribute('title', itsalltext.localeString('program_name'));
        }
        cache_object.button = gumdrop; // Store it for easy finding in the future.

        // Image Attributes
        gumdrop.style.setProperty('cursor',   'pointer',  'important');
        gumdrop.style.setProperty('display',  'none',     'important');
        gumdrop.style.setProperty('position', 'absolute', 'important');
        gumdrop.style.setProperty('padding',  '0',        'important');
        gumdrop.style.setProperty('margin',   '0',        'important');
        gumdrop.style.setProperty('border',   'none',     'important');
        gumdrop.style.setProperty('zIndex',   '32768',    'important');

        gumdrop.style.setProperty('width',  this.gumdrop_width + 'px', 'important');
        gumdrop.style.setProperty('height', this.gumdrop_height + 'px', 'important');

        gumdrop.setUserData(itsalltext.MYSTRING + '_UID', /*: cheat nsIVariant */(cache_object.uid), null);

        // // Click event handlers
        // itsalltext.listen(gumdrop, "click", itsalltext.hitch(cache_object, 'onClick'), false);
        // itsalltext.listen(gumdrop, "contextmenu", itsalltext.hitch(cache_object, 'onContext'), false);

        // Insert it into the document
        parent = node.parentNode;
        nextSibling = node.nextSibling;

        if (nextSibling) {
            parent.insertBefore(gumdrop, nextSibling);
        } else {
            parent.appendChild(gumdrop);
        }

        // Add mouseovers/outs
        // itsalltext.listen(gumdrop, 'mouseover', itsalltext.hitch(cache_object, 'mouseover'), false);
        // itsalltext.listen(gumdrop, 'mouseout', itsalltext.hitch(cache_object, 'mouseout'), false);

        cache_object.mouseout(null);
        cache_object.adjust();
    } catch (e) {
        itsalltext.monitor.decrementLock();
    }
    itsalltext.monitor.decrementLock();
};

/**
 * Updates the position of the gumdrop, incase the textarea shifts around.
 */
// PASSES
CacheObj.prototype.adjust = function () {
    var gumdrop  = this.button,
        el       = this.node,
        doc      = el.ownerDocument,
        style    = /*: nsIDOMCSSStyleDeclaration */null,
        display  = /*: Str */""+null,
        cstyle   = /*: nsIDOMCSSStyleDeclaration + Bool*/null,
        left     = /*: Num */0,
        top      = /*: Num */0,
        coord    = /*: Array<Num> */[],
        pos      = /*: Str */""+null;

    if (itsalltext.getDisableGumdrops()) {
        if (gumdrop && gumdrop.style.display != 'none') {
            gumdrop.style.setProperty('display', 'none', 'important');
        }
        return;
    }

    style    = gumdrop.style;
    if (!gumdrop || !el) {
        return;
    }
    display  = '';
    cstyle = doc.defaultView && doc.defaultView.getComputedStyle(el, '');
    if ((cstyle && (cstyle.display == 'none' ||
                    cstyle.visibility == 'hidden')) ||
        el.getAttribute('readonly') ||
        el.readOnly ||
        el.getAttribute('disabled')
        ) {
        display = 'none';
    }
    if (display === 'none' && style.display != display) {
        style.setProperty('display', display, 'important');
    }

    /**
     * Position the gumdrop.
     * Updates in case the DOM changes.
     */
    pos = itsalltext.preferences.gumdrop_position;
    if (pos === 'upper-right' || pos === 'lower-right') {
        left = Math.max(1, el.offsetWidth - this.gumdrop_width);
    } else {
        left = 0;
    }
    if (pos === 'lower-left' || pos === 'lower-right') {
        top  = el.offsetHeight;
    } else {
        top  = 0 - this.gumdrop_height;
    }
    if (el.offsetParent === gumdrop.offsetParent) {
        left += el.offsetLeft;
        top  += el.offsetTop;
    } else {
        coord = itsalltext.getContainingBlockOffset(el, gumdrop.offsetParent);
        left += coord[0];
        top  += coord[1];
    }
    if (left && top) {
        var left2 = [left, 'px'].join('');
        var top2  = [top, 'px'].join('');
        if (style.left != left2) {
            style.setProperty('left', left2, 'important');
        }
        if (style.top != top2) {
            style.setProperty('top',  top2, 'important');
        }
    }
};

/**
 * Returns a cache object
 * Note: These UIDs are only unique for Its All Text.
 * @param {Object} node A dom object node or ID to one.
 * @returns {String} the UID or null.
 */
CacheObj.get = /*: cheat Unsafe */function (node) {
    var str = itsalltext.MYSTRING + "_UID",
    id = /*: cheat realisticStr  */''+null;
    if (typeof(node) === 'string') {
        id = /*: cheat realisticStr */node;
    } else if (node && (/*: cheat nsIDOMNode */node).getUserData(str)) {
        id = /*: cheat realisticStr */(node.getUserData(str));
    }
    return itsalltext.getFromTracker(id);
};

/**
 * Creates a cache object, unless one exists already.
 * Note: These UIDs are only unique for Its All Text.
 * @param {DOMElement} node A dom object node or id to one.
 * @param {Boolean} create_gumdrop Should a gumdrop be created (html).
 * @returns {String} the UID or null.
 */

CacheObj.make = /*: cheat Unsafe */ function (node, create_gumdrop) {
    var cobj = CacheObj.get(node);
    // Too noisy itsalltext.debug('CacheObj.make(',node,', ', create_gumdrop,') = ',cobj, ' : ', cobj ? cobj.uid : 'null');
    if (!cobj) {
        cobj = new CacheObj(/*: cheat nsIDOMHTMLTextAreaElement */node);
        if (create_gumdrop) {
            cobj.addGumDrop();
        }
    }
    return cobj;
};
