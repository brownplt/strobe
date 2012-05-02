var YOONO_CL = Components.classes;
var YOONO_CI = Components.interfaces;

/*::

type YServices = {Ext with
  core : {Ext with
    require: Str * (-> Undef) -> Undef,
  },
  relativePopups : {Ext with
    open: Str * Str * Num * Num * Ext * Str * Str * Str -> Undef
  },
  skin : {Ext with
    replacePartnerName: Array<Str> -> Undef
  }
};

type YObserver = rec o . {AnObject with
  init : Unsafe,
  continueInit : Unsafe,
  mouseUpHandler : Ext -> Undef,
  mouseMouveHandler : [this('o)] nsIDOMUIEvent -> Undef,
  processMouseMove : Str * Num -> Undef,
  uninit : -> Undef,
  changePassword : Ext * Ext -> Undef
};

rec
type yoonoGeneric = {Ext with
  init : yoonoInst -> Undef
}
and
type YoonoCmpt = {yoonoGeneric with
  init : yoonoInst -> Undef,
  getYServices: -> YServices,
  isAddonInstalled : Str -> Bool,
  addStat : Array<Str> -> Undef,
  uninstallAddonWhenQuitting : (nsIUpdateItem + Undef) -> Undef,
  getExtensionVersion : -> Str,
  getInstalledAddons : (-> Undef) -> Undef,
  DEVICEVM_EXTENSION_ID : Str
}
and
type YoonoYexitf = {yoonoGeneric with
  init : yoonoInst -> Undef,
  getWindowId : Window -> Ext,
  notifyTabActivity : Ext * Ext * Ext * Ext -> Undef
}
and
type yoonoLog = {yoonoGeneric with
  exception : Ext -> Undef
}
and
type yoonoInst = {Ext with
  log : yoonoLog,
  utils : yoonoGeneric,
  prefs : yoonoGeneric,
  yextif : YoonoYexitf,
  storage : yoonoGeneric,
  main : YoonoCmpt,
  dialogs : yoonoGeneric,
};

type YoonoGlob = rec g . {AnObject with
  WMED : nsIWindowMediator,
  updateInterface : Unsafe,
  customizeDeviceVM : -> Undef,
  activateSmartView : Unsafe,
  activateDeviceVM : Unsafe,
  uninstallFromDeviceVM : -> Undef,
  installYoonoButton : -> Undef,
  persistToolbarSet : Ext -> Undef,
  onSidebarClose : -> Undef,
  toggleSb: Ext -> Undef,
  openShare : [this('g)] -> Undef,
  openPrivacy : -> Undef,
  openYoodget : [this('g)] Ext -> Undef,
  openUrlInNewTab : [this('g)] Str * Ext -> Undef,
  showMemoContextMenu : Ext * Ext -> Undef,
  hideMemoContextMenu : Ext -> Undef,
  currentRelease : Str,
  previousRelease : Str,
  externalInterfaces : YoonoYexitf
};

type YNTabBrowserListener = rec t . {AnObject with
  init : [this('t)] -> Undef,
  uninit : [this('t)] -> Undef,
  notifyTabActivity : Ext * Ext * Ext -> Undef,
  onTabClose : [this('t)] Ext -> Undef,
  onDomContentLoaded : [this('t)] nsIDOMNSEvent -> Undef,
  setYoonoProperties : [nsIDOMEventTarget] nsIDOMEvent -> Undef,
  onStateChange : [this('t)] Ext * Ext * Bool * Ext -> Undef, 
  onLinkIconAvailable : Ext -> Num,
  QueryInterface : QueryInterfaceType
};
*/

/*:: type GBrowser = { nsIDOMElement with
  tabContainer : Window
} ; */

var gBrowser = /*: GBrowser */null;

var YOONO_LOG = /*: yoonoLog */null;
var YOONO_UTILS = /*:yoonoGeneric*/null;
var YOONO_PREFS = /*:yoonoGeneric*/null;
var YOONO_YEXTIF = /*: YoonoYexitf */null;
var YOONO_STORAGE = /*:yoonoGeneric*/null;
var YOONO_CMPT = /*: YoonoCmpt */null;
var YOONO_DIALOGS = /*:yoonoGeneric*/null;
var yoono = /*: yoonoInst */null;

var AddonManager = /*:Ext*/null;
var navigator = /*:Ext*/null;

var yoonoGlob = /*: YoonoGlob */null;
var yoonoObserver = /*: YObserver */null;
function openUILink (url, event) { }
function checkForMiddleClick(t, event) { }
var YOONO_KEYVALUEDB = /*: Ext*/null;

var YNPREFBRANCH = /*: nsIPrefBranch */null;
var YServices = /*: YServices */null;
var YOONO_SIDEBAR = /*: {Ext with init : YoonoCmpt -> Undef} */null;
function alert(str) {}
function BrowserFullScreen() {}
var serverUrl = /*:Str*/'';

function(event) { yoonoGlob.toggleSb(event); };
function(event) { YOONO_CMPT.addStat(['clic', 'menu', 'yoonobutton']); };
function(event) { yoonoGlob.openYoodget('{friends@yoono.com}'); };
function(event) { yoonoGlob.openYoodget('{friends@yoono.com}'); };
function(event) { yoonoGlob.openShare(); };
function(event) { openUILink('http://www.yoono.com', event); };
/*: [Ext] Ext -> Undef */ function(event) { checkForMiddleClick(this, event); };
function(event) { openUILink('http://www.yoono.com/terms.html', event); };
/*: [Ext] Ext -> Undef */ function(event) { checkForMiddleClick(this, event); };
function(event) { yoonoGlob.openPrivacy(); };
/*: [Ext] Ext -> Undef */ function(event) { checkForMiddleClick(this, event); };
function(event) { openUILink('http://support.yoono.com', event); };
/*: [Ext] Ext -> Undef */ function(event) { checkForMiddleClick(this, event); };
function(event) { openUILink('http://static.yoono.com/redirect/donate.html', event); };
/*: [Ext] Ext -> Undef */ function(event) { checkForMiddleClick(this, event); };
function(event) { openUILink('http://www.yoono.com/help.html', event); };
/*: [Ext] Ext -> Undef */ function(event) { checkForMiddleClick(this, event); };
function(event) { yoonoObserver.changePassword(); };
function(event) { yoonoGlob.openShare(); };
/*: [Ext] Ext -> Undef */  function(event) { yoonoGlob.showMemoContextMenu(event, this); };
/*: [Ext] Ext -> Undef */  function(event) { yoonoGlob.hideMemoContextMenu(this); };
function(event) { yoonoGlob.sidebar.toggle(); };
function(event) { yoonoGlob.sidebar.toggle(); };
function(event) { yoonoGlob.sidebar.toggle(); };
function(event) { yoonoGlob.sidebar.toggleMultiColumn(); };
function(event) { yoonoGlob.sidebar.toggleThin(); };
function(event) { yoonoGlob.sidebar.fullScreenColumns(); };

/*************************************************
 *       This is loaded in the main xul document 
 *************************************************/

const YOONO_ID = "{d9284e50-81fc-11da-a72b-0800200c9a66}";
const YOONO_WMED = Components.classes['@mozilla.org/appshell/window-mediator;1'].getService(Components.interfaces.nsIWindowMediator);

/*:: rec
type YoonoSidebar = {AnObject with
  splitterMouseDown : [ysInstance] Ext -> Undef,
  toggleMultiColumn : [ysInstance] -> Undef,
  fullScreenColumns : [ysInstance] -> Undef,
  toggleThin : [ysInstance] Any -> Undef,
  setThin : [ysInstance] Num -> Undef,
  setLarge : [ysInstance] Num -> Undef,
  isVisible : [ysInstance] -> Bool,
  showIfVisible : [ysInstance] -> Undef,
  show : [ysInstance] -> Undef,
  loadTOSXulDoc : [ysInstance] -> Undef,
  loadSidebarXulDoc : [ysInstance] -> Undef,
  openOrShowColumns : [ysInstance] -> Undef,
  hide : [ysInstance] -> Undef,
  toggle : [ysInstance] -> Bool,
  uninit : [ysInstance] -> Undef
 }
and 
type ysInstance = {Ext with
  __proto__: YoonoSidebar,
  visible: Bool,
  container: Ext,
  browser : Ext,
  splitter : Ext,
  externalInterfaces : YoonoYexitf,
  methods : YoonoYexitf,
  resizing: Bool,
  thin : Bool,
  win: {Ext with
    ynSidebar : {Ext with
      getResizeWidth : Num * Str -> Num
    }
  },
  splitterMouseDown :^ [ysInstance] Ext -> Undef,
  toggleMultiColumn :^ [ysInstance] -> Undef,
  fullScreenColumns :^ [ysInstance] -> Undef,
  toggleThin :^ [ysInstance] Any -> Undef,
  setThin :^ [ysInstance] Num -> Undef,
  setLarge :^ [ysInstance] Num -> Undef,
  isVisible :^ [ysInstance] -> Bool,
  showIfVisible :^ [ysInstance] -> Undef,
  show :^ [ysInstance] -> Undef,
  loadTOSXulDoc :^ [ysInstance] -> Undef,
  loadSidebarXulDoc :^ [ysInstance] -> Undef,
  openOrShowColumns :^ [ysInstance] -> Undef,
  hide :^ [ysInstance] -> Undef,
  toggle :^ [ysInstance] -> Bool,
  uninit :^ [ysInstance] -> Undef
};

type YoonoSidebarConstructor = {[ysInstance] YoonoYexitf -> Undef with prototype : YoonoSidebar};
*/

/*: YoonoSidebarConstructor */
function YoonoSidebar(externalInterfaces) {
  var e = /*: Ext */null;
  try {
    var _self = this;
    this.visible = false; // sidebar visible or not, this flag is handled by xul document loading and unloading...
    this.container = document.getElementById('yoono-sidebar-box');
    this.browser = document.getElementById('yoono-sidebar');
    // Get the skin chosen by user
    var skinName = YNPREFBRANCH.getCharPref("skin");

    this.browser.setAttribute('skin', skinName);

    this.splitter = document.getElementById('yoono-splitter');
    // define services made available to external calls
    this.externalInterfaces = externalInterfaces;
    this.methods = externalInterfaces.methods;
    // at startup, sidebar might be opened because it was last time
    // In order for everything to be ready, the sidebar browser is loaded just now
    this.showIfVisible();
    // Init splitter movement detection
    if(this.splitter) {
      this.splitter.addEventListener('mousedown', function(evt) {_self.splitterMouseDown(evt);}, false);
      
    }
    this.resizing = false;
    this.thin = false;
  } catch(e) {
    Components.utils.import("resource://yoono/yoonoLog.js");    
    YOONO_LOG.exception(e);
  }
}

YoonoSidebar.prototype.splitterMouseDown = /*: [ysInstance] Ext -> Undef */ function(evt) {
  var _self = this;
  
  var initialWidth = parseInt(this.browser.parentNode.getAttribute('width'));
  var initialX = evt.screenX;
  var previousX = evt.clientX;

  var mouseUp = /*: Ext */ function (evt) {
    document.removeEventListener('mouseup',  mouseUp, true);
    document.removeEventListener('mousemove', mouseMove, true);
  };

  var mouseMove = /*: Ext */ function (evt) {
    var direction = (evt.clientX > previousX)?'right':'left';
    var sgn = 1;
    if(_self.onTheRight) {
      direction = (evt.clientX > previousX)?'left':'right';
      sgn = -1;
    }
    previousX = evt.clientX;
    
    var diffX = evt.screenX - initialX;
    
    var width = /*:Num*/initialWidth + (sgn * diffX);
    if(width >= 700) return;
    //dump("mouse move -- resize --> "+width+" - "+direction+"\n");

    // NEAL: 'in' doesn't seem to be working correctly
    if ('ynSidebar' in _self.win) {
      width = _self.win.ynSidebar.getResizeWidth(width, direction);
    }
    
    _self.browser.parentNode.setAttribute('width',width);
  };
  
  document.addEventListener('mouseup', mouseUp, true);
  document.addEventListener('mousemove', mouseMove, true);
};

YoonoSidebar.prototype.toggleMultiColumn = /*: [ysInstance] -> Undef */ function() {
  this.win.ynSidebar.toggleMultiColumn();
};

YoonoSidebar.prototype.fullScreenColumns = /*: [ysInstance] -> Undef */ function() {
  this.win.ynSidebar.openOrShowColumns();
  BrowserFullScreen();
};

YoonoSidebar.prototype.openOrShowColumns = /*: [ysInstance] -> Undef */ function() {
};

YoonoSidebar.prototype.toggleThin = /*: [ysInstance] Any -> Undef */ function(thinWidth) {
  if(this.thin) {
    this.win.ynSidebar.setLarge();
  } else {
    this.win.ynSidebar.setThin(false);
  }
};

YoonoSidebar.prototype.setThin = /*: [ysInstance] Num -> Undef */ function(thinWidth) {
  this.thin = true;
  this.browser.parentNode.setAttribute('class', 'sidebar thin');
  this.browser.parentNode.setAttribute('width', thinWidth);
};

YoonoSidebar.prototype.setLarge = /*: [ysInstance] Num -> Undef */ function(largeWidth) {
  this.thin = false;
  this.browser.parentNode.setAttribute('class', 'sidebar large');
  this.browser.parentNode.setAttribute('width', largeWidth);
};

YoonoSidebar.prototype.isVisible = /*: [ysInstance] -> Bool */ function() {
  return(this.visible); // flag handled by the loading/unloading of the xul sidebar document
};

/**
 * check if sidebar was visible last time 
 * and open the sidebar accordingly
 */

YoonoSidebar.prototype.showIfVisible = /*: [ysInstance] -> Undef */ function() {
  if(!this.container) return;
  var hidden = this.container.getAttribute('hidden');
  // Open the sidebar if updated or it was opened last time
  if(yoonoGlob.firstReleaseRun || ('true' != hidden)) {
    this.show();
  }
};

YoonoSidebar.prototype.show = /*: [ysInstance] -> Undef */ function() {
  if(!this.container) return;
  var e = /*: Ext */null;
  try {
    Components.utils.import("resource://yoono/yoonoService.js");
    Components.utils.import("resource://yoono/yoonoSidebarService.js");
  } catch(e) {
    Components.utils.reportError(e);
    return;
  }
  YOONO_SIDEBAR.init(YOONO_CMPT);

  this.container.removeAttribute('hidden');
  this.splitter.removeAttribute('hidden');
  this.loadSidebarXulDoc();
};

YoonoSidebar.prototype.loadTOSXulDoc = /*: [ysInstance] -> Undef */ function() {
  var sbUrl = 'chrome://yoono/content/TOSPanel.xul';
  this.browser.parentNode.setAttribute("class", "tos");
  this.browser.parentNode.setAttribute("style","max-width:300px;");
  this.browser.parentNode.setAttribute("style","min-width:100px;");
  this.browser.setAttribute('src', sbUrl);
  this.container.removeAttribute('hidden');
  this.splitter.removeAttribute('hidden');
};

YoonoSidebar.prototype.loadSidebarXulDoc = /*: [ysInstance] -> Undef */ function() {
  var sbUrl = 'chrome://yoono/content/sidebarPanel.xul';
  this.browser.parentNode.setAttribute("style","max-width:0px;");
  this.browser.parentNode.setAttribute("class", "sidebar");
  this.browser.setAttribute('src', sbUrl);
};

YoonoSidebar.prototype.hide = /*: [ysInstance] -> Undef */ function() {
  if (this.uninit)
	this.uninit(); // this method is defined in sidebarPanel.js
  if(this.container) {
    this.container.setAttribute('hidden', true);
    this.splitter.setAttribute('hidden', true);
  }
  this.browser.setAttribute('src', '');
};


YoonoSidebar.prototype.toggle = /*: [ysInstance] -> Bool */ function() {
  var hidden = this.container.getAttribute('hidden') || false ;
  if(hidden) {
    this.show();
  } else {
    this.hide();
  }
  return(!hidden);
};

/**
   Global stuff for yoono toolbar
   @author : Xavier Grosjean
   Copyright 2006, Yoono SAS.
**/
if ("undefined" == typeof(yoonoGlob)) {
  var yoonoGlob = /*: YoonoGlob */null;
}

yoonoGlob.WMED = Components.classes['@mozilla.org/appshell/window-mediator;1'].getService(Components.interfaces.nsIWindowMediator);

// Il faut gerer l'affichage de boutons nouveaux dans cette version: s'ils sont customizables
// ils n'apparaissent pas tout seuls. Mais on ne veut pas faire apparaitre d'anciens boutons que
// l'utilisateur aurait supprimé par customisation...
yoonoGlob.updateInterface = /*: cheat @Unsafe */function() {
    YOONO_LOG.debug('updateInterface');
    var partner = YOONO_CMPT.isAddonInstalled(yoono.main.DEVICEVM_EXTENSION_ID);

    if(yoonoGlob.firstReleaseRun) {
      YOONO_LOG.debug('Nouvelle version');
      // Supprimer les anciennes prefs de stats
      if(yoonoGlob.previousRelease < '5.0.5') {
        YOONO_LOG.debug('Removing old stuff');
        var e = /*: Ext */null;
        try {
          var OLDPREF = Components.classes["@mozilla.org/preferences-service;1"].getService(Components.interfaces.nsIPrefService).getBranch("extensions.yoono.stats.data.exposition.");
          OLDPREF.deleteBranch("");
          OLDPREF = Components.classes["@mozilla.org/preferences-service;1"].getService(Components.interfaces.nsIPrefService).getBranch("extensions.yoono.stats.data.expo");
          OLDPREF.deleteBranch("");
          YNPREFBRANCH.clearUserPref('stats.data.running');
          YNPREFBRANCH.clearUserPref('stats.data.run');
        } catch(e) { }
      }

      // NEAL: NOT SURE IF THIS WAS THE RIGHT THING TO DO HERE. This was originally one variable.
      var numVersionA = yoonoGlob.currentRelease.split('.');
      numVersionA.splice(3, 1);
      var numVersion = numVersionA.join('.');
      var url = /*: Str */'';
      // If it's an install, display the install page for the release.
      if(yoonoGlob.firstRun) {
        // unless it's a partner 
        if(!partner) {
          url = 'http://www.yoono.com/releases/ff/' + numVersion + '/postinstall_new.html';
          setTimeout(yoonoGlob.openUrlInNewTab, 1000, url, /*:Ext*/null);
        }
      } else {
        // If it's an update, display the update page for the release.
        url = 'http://www.yoono.com/releases/ff/' + numVersion
                + (partner?'/devicevm':'')
                + '/postinstall_update.html';
        setTimeout(yoonoGlob.openUrlInNewTab, 1000, url, /*:Ext*/null);
      }

      YNPREFBRANCH.setCharPref("release", yoonoGlob.currentRelease);
    }

    // Au premier lancement de la 5.0.0, il faut installer le bouton Yoono de toggle des sidebar et toolbar
    if(yoonoGlob.firstRun || (yoonoGlob.previousRelease < '5.0.4')) {
      YOONO_LOG.debug('Premier lancement de la version 5 avec nouveau bouton toggle');
      yoonoGlob.installYoonoButton();
    }

    // If devicevm, some things should be displayed differently
    if(partner) {
      yoonoGlob.customizeDeviceVM();
    }
};

yoonoGlob.customizeDeviceVM = function() {
  var win = document.getElementById("main-window");
  win.className += " devicevm";

  var yoonoToggleButton = document.getElementById('yoono-toggle-sb');
  yoonoToggleButton.setAttribute('tooltiptext', "Toggle Splashtop Connect sidebar");
  var agreed = /*: Bool */false;
  try {
    agreed = YNPREFBRANCH.getBoolPref("devicevm-agreed");
  } catch(e) {}
  if(!agreed) {
    yoonoGlob.sidebar.hide();
  } else {
    win.className += ' devicevm-agreed';
  }
};

// Old name left for compatibility
yoonoGlob.activateSmartView = /*: cheat @Unsafe */function() {
  yoonoGlob.activateDeviceVM();
};

// this method can be called to activate Yoono from deviceVM add-on
yoonoGlob.activateDeviceVM = /*: cheat @Unsafe */function() {
  var agreed = /*: Bool */ false;
  try {
    agreed = YNPREFBRANCH.getBoolPref("devicevm-agreed");
  } catch(e) {}
  if(agreed) return;
  
  var win = document.getElementById("main-window");
  win.className += ' devicevm-agreed';
  YNPREFBRANCH.setBoolPref("devicevm-agreed", true);
  yoonoGlob.sidebar.show();
};

// this method can be called from deviceVM to uninstall Yoono
yoonoGlob.uninstallFromDeviceVM = function() {
  YOONO_CMPT.uninstallFromDeviceVM();
};

yoonoGlob.installYoonoButton = function() {
  var palette = document.getElementById('navigator-toolbox').palette;
  // On ne trouve pas les boutons par getElementById tant qu'ils sont dans la palette
  // Mais méfiance, s'il y a eu un jour le bouton, il reste ds les caches d'extension... 
  var yoonoToggleButton = document.getElementById('yoono-toggle-sb');
  if(!yoonoToggleButton) {
    yoonoToggleButton = palette.getElementsByAttribute('id', 'yoono-toggle-sb')[0];
  }
  var insertBeforeElt =  document.getElementById('urlbar-container');
  if(!insertBeforeElt) {
    insertBeforeElt =  document.getElementById('search-container');
  }
  if(!insertBeforeElt) {
    insertBeforeElt =  document.getElementById('throbber-box');
  }
  if(insertBeforeElt) {
    insertBeforeElt.parentNode.insertBefore(yoonoToggleButton, insertBeforeElt);
    yoonoGlob.persistToolbarSet(insertBeforeElt.parentNode);
  }
};

// Mettre a jour et persister le currentset d'une toolbar 
yoonoGlob.persistToolbarSet = function(tb) {
  var e = /*: Ext */null;
  try {
    var id = tb.getAttribute('id');
    YOONO_LOG.debug('Persisting toolbar buttons for ' + id);
    // Il faut parcourir la toolbar
    var newset = /*: Str */'';
    var eltId = /*: Ext */'';
    for(var offTb=0 ; offTb < tb.childNodes.length; offTb++) {
        if(tb.childNodes[offTb].nodeName != 'toolbarspring') {
          eltId = tb.childNodes[offTb].getAttribute('id');
        } else {
          eltId = 'spring';
        }
        newset +=  eltId + ',';
    }
    newset = newset.replace(/,$/,'');
    YOONO_LOG.debug('newset '+ newset);
    tb.setAttribute('currentset', newset);
    document.persist(id, 'currentset');
  } catch(e) {
    YOONO_LOG.exception(e);
  }
};

yoonoGlob.onSidebarClose = function() {
  var brand = "";
  YServices = YOONO_CMPT.getYServices();

  YServices.core.require("relativePopups",function () {
    var title = "Closing Yoono disconnects you from your services so you won't see any pop up notifications or IMs.";
    var text = "You can relaunch Yoono by clicking on the Yoono icon in your browser toolbar.";
    var disableText = "Don't show this message again";
    var t = /*: Array<Str> */[title, text];
    YServices.skin.replacePartnerName(t);
    YServices.relativePopups.open("Close","relative-bottom",177,-7,document.getElementById("yoono-toggle-sb"),t[0],t[1],disableText);
  });
  
};
yoonoGlob.toggleSb = function(evt) {
  if(!evt || ('yoono-toggle-sb' == evt.target.id)) {
    var e = /*: Ext */null;
    try {
      // returns new hidden flag (true if now hidden => closed)
      var result = yoonoGlob.sidebar.toggle();
      if(result) {
        YOONO_CMPT.addStat(['clic', 'closesidebar', 'yoonobutton']);
        yoonoGlob.onSidebarClose();
      } else {
        YOONO_CMPT.addStat(['clic', 'opensidebar', 'yoonobutton']);
      }
    } catch(e) {
    alert(e);
      YOONO_LOG.exception(e);
      var container = document.getElementById('yoono-sidebar-box');
      var splitter = document.getElementById('yoono-splitter');
      var hidden = container.getAttribute('hidden') || false ;
      if(hidden) {
        container.removeAttribute('hidden');
        splitter.removeAttribute('hidden');
      } else {
        container.setAttribute('hidden', "true");
        splitter.setAttribute('hidden', "true");
      }
    }
  }
};

// Called from Yoono button menu, open the sidebar and the Share feature
yoonoGlob.openShare = function() {
  if(this.sidebar) {
    var delay = 10;
    if(!this.sidebar.isVisible()) {
      this.sidebar.show();
      delay = 200;
    }
    setTimeout(function() {
      yoonoGlob.sidebar.sidebarMethods.methods.share(null);
    }, delay);
  }
};

yoonoGlob.openPrivacy = function() {
  var partner = YOONO_CMPT.isAddonInstalled(yoono.main.DEVICEVM_EXTENSION_ID);
  if(partner) {
    yoonoGlob.openUrlInNewTab('http://www.splashtop.com/privacy.php');
  } else {
    yoonoGlob.openUrlInNewTab('http://www.yoono.com/privacy.html');
  }
};

// Called from Yoono button menu, to open a yoodget
yoonoGlob.openYoodget = function(aYuid) {
  if(this.sidebar) {
    var delay = 10;
    if(!this.sidebar.isVisible()) {
      this.sidebar.show();
      delay = 200;
    }
    var tryOpeningYoodget = /*: [Window] -> Undef */null;
    tryOpeningYoodget = function () {
      cptr --;
      done = yoonoGlob.sidebar.sidebarMethods.methods.callYoodget(aYuid, -1, 'highlightYoodget');
      if(!done && cptr) {
        setTimeout(tryOpeningYoodget, delay);
      }
    };
    var done = false;
    var cptr = 20;
    setTimeout(tryOpeningYoodget, delay);

  }
};

// Ouverture d'une URL dans nouvo tab
yoonoGlob.openUrlInNewTab = function(url, event) {
  var e = /*:Ext*/null;
  try {
    var win = yoonoGlob.WMED.getMostRecentWindow("navigator:browser");
    var target = 'tab';
    // on slow computers, the test below might result in overwritting
    // a tab that should otherwise have been initialized to a start page
    //var doc = win.getBrowser().contentDocument;
    //var currentUrl = doc.location.href ;
    //if('about:blank' == currentUrl) {
    //target = 'current';
    //}
    win.openUILinkIn(url, target);
  } catch(e) {
    YOONO_LOG.exception(e);
  }
};

if ("undefined" == typeof(ynTabbrowserListener)) {
  var ynTabbrowserListener = /*: YNTabBrowserListener */{
    init : /*: [YNTabBrowserListener] -> Undef */ function () {
      this.notifyTabActivity(null, null, 'window-open');

      var aContent = document.getElementById( "content" );
      if( aContent ) {
        aContent.addProgressListener(/*:cheat Ext*/this, Components.interfaces.nsIWebProgress.NOTIFY_STATE_DOCUMENT);
        aContent.addEventListener('DOMContentLoaded', /*:cheat Ext*/this.onDomContentLoaded, false);
      }
      gBrowser.tabContainer.addEventListener("TabClose", this.onTabClose, false);
    },

    uninit : /*: [YNTabBrowserListener] -> Undef */ function () {
      gBrowser.tabContainer.removeEventListener("TabClose", this.onTabClose, false);
      var aContent = document.getElementById( "content" );
      if( aContent ) {
        aContent.removeProgressListener(/*:cheat Ext*/this, Components.interfaces.nsIWebProgress.NOTIFY_STATE_DOCUMENT);
        aContent.removeEventListener('DOMContentLoaded', /*:cheat Ext*/this.onDomContentLoaded, false);
      }

      this.notifyTabActivity(null, null, 'window-close');
    },

    notifyTabActivity : function (doc, url, state) {
      YOONO_YEXTIF.notifyTabActivity(YOONO_YEXTIF.getWindowId(window), doc, url, state);
    },


    onTabClose : /*: [YNTabBrowserListener] Ext -> Undef */function (event) {
      var browser = event.target.linkedBrowser;
        this.notifyTabActivity(browser.contentDocument, browser.contentDocument.location.href, 'tabclosed');
    },


    onDomContentLoaded : /*: [YNTabBrowserListener] nsIDOMNSEvent -> Undef */function(event){
//NEAL: isReady doesn't exist anywhere.
      /*:cheat False*/(event.originalTarget.isReady = true);
      var doc = /*:cheat nsIDOMDocument*/event.originalTarget;
      var href = doc.location.href;
// NEAL: I REWROTE THIS IF STATEMENT TO REFERENCE DOC/HREF, SO THAT WE DIDN'T HAVE TO CHEAT EVERY TIME
      if(doc.defaultView &&
        doc.defaultView.parent == doc.defaultView.self &&
        href == window.content.document.location.href) {
        if(YOONO_YEXTIF.methods.partnerPresent('deviceVM')) {
          try {
            if('about:addons' == href) {
              var list = doc.getElementById('addon-list');
              if(list) {
                list.addEventListener('DOMSubtreeModified', this.setYoonoProperties, false);
              }
            }
          } catch(e) {}
        }

        this.notifyTabActivity(/*:cheat Ext*/doc, href, 'domcontentloaded');

      }
    },

    setYoonoProperties : /*: [nsIDOMEventTarget] nsIDOMEvent -> Undef */function(aEvent) {
      var doc = /*:cheat nsIDOMXULDocument */aEvent.target.ownerDocument ;
      var items = doc.getElementsByAttribute('value', '{d9284e50-81fc-11da-a72b-0800200c9a66}');
      if(items && items[0]) {
        var item = items[0];
        var icon = doc.getAnonymousElementByAttribute(item, 'src', 'chrome://yoono/skin/yoono.png');
        if(icon) {
          icon.setAttribute("src", "chrome://yoono/skin/devicevm/devicevm24.png");
          item.setAttribute("name", "Splashtop Connect sidebar");
        }
      }
    } ,
    // Only works on current Tabpanel !
    onStateChange: /*: [YNTabBrowserListener] Ext * Ext * Bool * Ext -> Undef */function(aProgress, aRequest, aFlag, aStatus) {
      if(aFlag & Components.interfaces.nsIWebProgressListener.STATE_STOP) {
          var e = /*:Ext*/null;
          try {
              var tabbrowser = getBrowser();
              var doc = tabbrowser.contentDocument;
              var url = doc.URL;
              // Return for 'subrequests' that we might sometimes get (ajax pages...)
              if(!aRequest || (url != aRequest.name)) return;
              // Detection de la fin de chargement du document
              YOONO_LOG.debug("Document Loaded " + aRequest.name);
              //YOONO_LOG.error("Document Loaded " + (aFlag & Components.interfaces.nsIWebProgressListener.STATE_STOP) + "/"+ (aFlag & Components.interfaces.nsIWebProgressListener.STATE_IS_DOCUMENT));
              this.notifyTabActivity(doc, url, 'loaded');
          } catch (e) {
              YOONO_LOG.exception(e);
          }
       }
    },

    // Detection debut de chargement ds onglet courant ou changement d'onglet
    onLocationChange: function(aProgress, aRequest, aURI) {
      var e = /*:Ext*/null;
      try {
        this.requestProcessed = false;
        var tabbrowser = getBrowser();
        var doc = tabbrowser.contentDocument;
        var url = doc.URL;
        if( aRequest == null ) {
            YOONO_LOG.debug('Tab switching to ' + doc.location.href );
            this.notifyTabActivity(doc, url, 'tabswitching');
        } else {
            if(url != aRequest.name) return;
            YOONO_LOG.debug("Document Loading " + aRequest.name);
            this.notifyTabActivity(doc, aRequest.name, 'loadstart');
        }
      } catch(e) {
        YOONO_LOG.exception(e);
      }
    },

    onProgressChange: function(webProgress , aRequest , curSelfProgress , maxSelfProgress , curTotalProgress , maxTotalProgress) {
      var e = /*:Ext*/null;
      try {
        var tabbrowser = getBrowser();
        var doc = tabbrowser.contentDocument;
        if(doc) {
          var url = doc.URL;
          if(url && (-1 != url.indexOf('mail.google'))) return 0;
          if(!aRequest || url != aRequest.name) return 0;

          var body = doc.getElementsByTagName('BODY');
          //YOONO_LOG.debug('XXXXXXXXXXXX  Body : ' + body.length + ', head : ' + head.length + ', meta : ' + meta.length);
          // body should not be present before head is totally there (hence, meta tags)
          if(url && body && !this.requestProcessed && body.length) {
            this.notifyTabActivity(doc, url, 'loadprogress');
            this.requestProcessed = true;
          }
        }
      } catch(e) {
        YOONO_LOG.exception(e);
      }
      return 0;

    },

    onStatusChange: function(webProgress , request , stateFlags , status) {
      return 0;
    },

    onSecurityChange: function(a,b,c) { return 0;},
    onLinkIconAvailable: function(a) { return 0;},

    QueryInterface: /*:cheat QueryInterfaceType*/ function(aIID) {
     if (aIID.equals(Components.interfaces.nsIWebProgressListener) ||
         aIID.equals(Components.interfaces.nsISupportsWeakReference) ||
         aIID.equals(Components.interfaces.nsISupports))
       return this;
     throw Components.results.NS_NOINTERFACE;
    }
  };
}


/**
   Toolbar yoono Firefox/Mozilla
   @author : L. Quérel
   Copyright 2005, Yoono SAS.
**/

var YNPREFBRANCH = Components.classes["@mozilla.org/preferences-service;1"].getService(Components.interfaces.nsIPrefService).getBranch("extensions.yoono.");

// Observer qui capte les desinstallations pour lancer les proc de clean up
/*::
type YUninstallObserver = rec o . {AnObject with
  QueryInterface : QueryInterfaceType,
  observe : [this('o)] nsISupports * Str * Str -> Undef,
  onUninstalling : Ext -> Undef,
  onOperationCancelled : Ext -> Undef,
  register : [this('o)] -> Undef,
  deregister : [this('o)] -> Undef,
}; */
if ("undefined" == typeof(yoonoUninstallObserver)) {
  var yoonoUninstallObserver = /*: YUninstallObserver */{
    QueryInterface: /*: QueryInterfaceType*/null,
    // That's for FF3
    observe:function(subj,topic,data) {
        var subject = subj.QueryInterface(Components.interfaces.nsIUpdateItem);
        if(subject.name == "Yoono") {
            YOONO_LOG.debug('Em Action: ' + data);
            switch(data) {
              case "item-uninstalled" :
                YOONO_CMPT.uninstallAddonWhenQuitting(subject);
                break;

              case "item-cancel-action" :
                YOONO_CMPT.uninstallWhenQuitting = false;
                break;

              case "item-upgraded" :
                break;
            }
        }
    },
    
    // That's for FF4
    onUninstalling: function(aAddon) {
      if(aAddon.id != YOONO_ID) return;
      YOONO_CMPT.uninstallAddonWhenQuitting();
    },
    // That's for FF4
    onOperationCancelled: function(aAddon) {
      if(aAddon.id != YOONO_ID) return;
      YOONO_CMPT.uninstallWhenQuitting = false;
    },

    register:/*: [YUninstallObserver] -> Undef*/function() {
      try {
        // That's for FF4
        Components.utils.import("resource://gre/modules/AddonManager.jsm");
        AddonManager.addAddonListener(yoonoUninstallObserver);
      } catch (e) {
         // That's for FF3
        var observerService = Components.classes["@mozilla.org/observer-service;1"].getService(Components.interfaces.nsIObserverService);
        observerService.addObserver(this, "em-action-requested", false);
      }
    },
  
    deregister:/*: [YUninstallObserver] -> Undef*/function() {
      try {
        // That's for FF4
        Components.utils.import("resource://gre/modules/AddonManager.jsm");
        AddonManager.removeAddonListener(yoonoUninstallObserver);
      } catch (e) {
         // That's for FF3
        var observerService = Components.classes["@mozilla.org/observer-service;1"].getService(Components.interfaces.nsIObserverService);
        observerService.removeObserver(this,"em-action-requested");
      }
    }
  };
}

if ("undefined" == typeof(yoono)) {
  var yoono = /*:yoonoInst*/null;
}

if ("undefined" == typeof(yoonoObserver)) {
  var yoonoObserver = /*: YObserver*/ {
    init: /*: cheat @Unsafe */function() {
      var e = /*:Ext*/null;
      try {
        // Yoono services
        Components.utils.import("resource://yoono/yoonoService.js");
      } catch(e) {
        Components.utils.reportError(e);
        return;
      }
      
      try {
        YOONO_CMPT.getInstalledAddons(function() {
          yoonoObserver.continueInit();
        });
      } catch (e) {
        this.continueInit();
      }

    },

    continueInit : /*: cheat @Unsafe */ function() {
      var e = /*:Ext*/null;
      try {
        var ua = navigator.userAgent.toLowerCase();
        if(ua.indexOf('mac os')!=-1) {
          var navBar = document.getElementById('nav-bar');
          if(navBar) {
            var classname = navBar.getAttribute('class');
            classname += ' mac' ;
            // if not FF3, then FF4...
            if(!("@mozilla.org/extensions/manager;1" in Components.classes)) {
              classname += ' ff4' ;
            }
            navBar.setAttribute('class', classname);

          }
          var popup = document.getElementById("account-selector-popup");
          if(popup) {
            var classname = popup.getAttribute('class');
            classname += ' mac' ;
            popup.setAttribute('class', classname);
          }

        }
 
        
        // Lire ds les pref la version qui a ete utilisée au lancement précédent
        yoonoGlob.previousRelease = '';
        try {
            yoonoGlob.previousRelease = YNPREFBRANCH.getCharPref("release");
        } catch(e) {
            // pref not found : first install
            YNPREFBRANCH.setIntPref("install_date", Math.round(new Date().getTime()/1000));
        }

        yoonoGlob.currentRelease = YOONO_CMPT.getExtensionVersion();
        yoonoGlob.tosAccepted = YNPREFBRANCH.getBoolPref("tos-accepted");
        try {
          Components.utils.import("resource://yoono/yoonoKeyValueDB.js");
        } catch(e) {
          Components.utils.reportError(e);
          return;
        }

        // If user just updated
        if(yoonoGlob.currentRelease != yoonoGlob.previousRelease) {
          yoonoGlob.firstReleaseRun =  true;
          YNPREFBRANCH.setBoolPref("first-release-run", true);
          YNPREFBRANCH.setCharPref("updated.from", yoonoGlob.previousRelease); // for stats, reset after sent
          YOONO_KEYVALUEDB.setKeyValue('sidebarUpdatedFrom', yoonoGlob.previousRelease);
          YOONO_KEYVALUEDB.setKeyValue("sidebarFirstReleaseRun", true); // this will be reset by sidebar when it closes
        } else {
          yoonoGlob.firstReleaseRun =  false;
          YNPREFBRANCH.setBoolPref("first-release-run", false);
        }
        YNPREFBRANCH.setCharPref("release", yoonoGlob.currentRelease);
        YOONO_KEYVALUEDB.setKeyValue('sidebarRelease', yoonoGlob.currentRelease);
        
        yoonoGlob.firstRun = YNPREFBRANCH.getBoolPref("firstrun");
        if(yoonoGlob.firstRun) {
          // check the campaign cookie
          var cookiesEnum = Components.classes["@mozilla.org/cookiemanager;1"].getService(Components.interfaces.nsICookieManager).enumerator;
          var campaignId = /*:Str*/'';
          while (cookiesEnum.hasMoreElements()) {
            var cookie = cookiesEnum.getNext().QueryInterface(Components.interfaces.nsICookie2);
            if(('www.yoono.com' == cookie.host) && ('rc' == cookie.name)) {
              campaignId = cookie.value;
              YOONO_KEYVALUEDB.setKeyValue("sidebarMarketingCampaign", campaignId);
              cookiesEnum = null;
              break;
            }
          }
        }

        try {
          Components.utils.import("resource://yoono/yoonoYEXTIF.js");
          Components.utils.import("resource://yoono/yoonoLog.js");
          Components.utils.import("resource://yoono/yoonoDialogs.js");
          Components.utils.import("resource://yoono/yoonoUtils.js");
          Components.utils.import("resource://yoono/yoonoPrefs.js");
          Components.utils.import("resource://yoono/yoonoSidebarService.js");
          Components.utils.import("resource://yoono/yoonoStorage.js");
        } catch(e) {
          Components.utils.reportError(e);
          return;
        }
        yoono.log=YOONO_LOG;
        yoono.utils=YOONO_UTILS;
        yoono.prefs=YOONO_PREFS;
        yoono.yextif=YOONO_YEXTIF;
        yoono.storage=YOONO_STORAGE;
        
        yoono.main=YOONO_CMPT;
        yoono.dialogs=YOONO_DIALOGS;
        
        YOONO_LOG.init(yoono);
        YOONO_PREFS.init(yoono);
        YOONO_DIALOGS.init(yoono);
        YOONO_CMPT.init(yoono);
//        YOONO_SIDEBAR.init(YOONO_CMPT);
        YOONO_YEXTIF.init(yoono);
        YOONO_STORAGE.init(yoono);

        var originalSplit = /*:Str*/'';
        try {
          var originalSplit = YNPREFBRANCH.getCharPref("originalSplit");
        } catch(e) {}

        if(!originalSplit) {
          originalSplit = YNPREFBRANCH.getCharPref("autoinstall"); // can be 'full', 'discovery', 'friends'
          YNPREFBRANCH.setCharPref("originalSplit", originalSplit);
        }

        // On first install, inc a counter on the server
        serverUrl = YNPREFBRANCH.getCharPref('serverurl');
        if(yoonoGlob.firstRun) {
          YOONO_CMPT.sendRequest(serverUrl + 'rest/counters/one/install/xpi', 'POST', 'async', '', null);
          if(campaignId) {
            var campaignString = campaignId + '-install-ff' ;
            YOONO_CMPT.sendRequest(serverUrl + 'rest/counters/one/campaign/' + campaignString, 'POST', 'async', '', null);
          }
        }
        // update flag for next time...
        YNPREFBRANCH.setBoolPref("firstrun", false);
        
        this.requestProcessed = false;
        
        yoonoGlob.externalInterfaces = yoono.yextif;
        
        // create the instance to manage the sidebar.
        // Its init method will be called each time the sidebar is opened, and its uninit method
        // each time it is closed.
        // The YoonoSidebar class is declared in sidebar.js (loaded in main document), and enriched in
        // sidebarPanel.js, loaded in sidebarPanel.xul document when sidebar is opened
        yoonoGlob.sidebar = new YoonoSidebar(yoonoGlob.externalInterfaces);
        
        yoonoGlob.gStrbundle=document.getElementById("yoono-strings");
        
        yoonoUninstallObserver.register();
        
        ynTabbrowserListener.init();
        
        // Afficher d'éventuels nouveaux boutons apparus depuis la derniere version installée
        // (qui sinon n'apparaissent pas en cas de customisation)
        yoonoGlob.updateInterface();

       
        // Add a mouseup listener to store the focused window when a selection is done,
        // So that selection can be accessed when clicking on a shortcut in the sidebar
        // (which changes the focused window !!)
        var content = document.getElementById('content');
        var panel = content.mPanelContainer;
        var _self = this;
        panel.addEventListener('mouseup', function(evt) {_self.mouseUpHandler(evt);}, true);
        // code below is just an investigation, not used for now
        // panel.addEventListener('mousemove', function(evt) {_self.mouseMouveHandler(evt)}, true);
        
   	  } catch(e) {
        alert(e.message + ' -- ' + e.stack);
        // we don't use yoono.log because it does'nt work at this time :/
        var CONSOLESERVICE = YOONO_CL["@mozilla.org/consoleservice;1"].getService(YOONO_CI.nsIConsoleService);
        CONSOLESERVICE.logStringMessage(" ### MAIN OVERLAY INIT EXCEPTION : \n "+e+"\n"+e.stack+"\n ###\n");
  	  }
    },

    // Memorize the focused window, so that it is available when querying the selection even if focuse has changed
    mouseUpHandler : function(evt) {
      var focusedWindow = document.commandDispatcher.focusedWindow;
      var content = document.getElementById('content');
      if (focusedWindow == window)
        focusedWindow = content;
      yoonoGlob.focusedWindow = focusedWindow;
    },

    // Memorize the focused window, so that it is available when querying the selection even if focuse has changed
    mouseMouveHandler : /*: [YObserver] nsIDOMUIEvent -> Undef */function(aEvent) {
      if(yoonoGlob.mouseOverTimer) {
        clearTimeout(yoonoGlob.mouseOverTimer);
        yoonoGlob.mouseOverTimer = null;
      }
      var textContent = aEvent.rangeParent.textContent;
      var rangeOffset = aEvent.rangeOffset ;
      var _self = this;

      yoonoGlob.mouseOverTimer = setTimeout(function() {
        _self.processMouseMove(textContent, rangeOffset);
      }, 500);
    },

    processMouseMove : /*: Str * Num -> Undef */function(textContent, aRangeOffset) {
      var words = textContent.replace(/[\.\?,;:\/!=+]/g, ' ');

      var begining = words.substr(0, aRangeOffset);
      var ending = words.substr(aRangeOffset);

      var firstCharOffset = begining.lastIndexOf(' ') + 1 ;
      var wordBegining = begining.substr(firstCharOffset);
      var lastCharOffset = ending.indexOf(' ');
      var wordEnding = ending.substr(0, lastCharOffset);

      var word = wordBegining + wordEnding;
    },
     // Désabonnement aux événements
    uninit: function() {
      ynTabbrowserListener.uninit();

      yoonoUninstallObserver.deregister();

      if(yoonoGlob.sidebar.visible && yoonoGlob.sidebar.win && yoonoGlob.sidebar.win.ynSidebar) {
        yoonoGlob.sidebar.win.ynSidebar.saveConfig();
      }
      YOONO_LOG.debug("Closing popouted yoodgets from extension");
      yoonoGlob.externalInterfaces.goodbye();

    },

    changePassword: function (uri, arg) {
      var userId = YOONO_CMPT.getUserCredential();
      if(userId.anonymous) {
        alert(yoonoGlob.gStrbundle.getString('changepwd.error.anonymous'));
        return;
      }
      window.openDialog('chrome://yoono/content/dialogs/changepasswd.xul', "changepasswd", 'chrome,modal');
    }
  };
};


window.addEventListener("load", /*: -> Undef */function() { /*: cheat Innocuous */yoonoObserver.init(); }, false);
window.addEventListener("unload", /*: -> Undef */function() { yoonoObserver.uninit(); }, false);

