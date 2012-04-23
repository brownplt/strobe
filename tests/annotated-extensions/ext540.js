/*::
type GBrowser = {Ext with
  currentURI : nsIURI
};

type GContextMenu = {
  getLinkURL : -> Str,
  linkURL : -> Str,
  showItem : Str * Bool -> Undef,
  onLink : Bool,
};

*/
var gBrowser = /*: GBrowser */null;
var gContextMenu = /*: GContextMenu */null;

/*: Str -> Str */function openWebfolderGetContents(aURL){
  var ioService=Components.classes["@mozilla.org/network/io-service;1"]
    .getService(Components.interfaces.nsIIOService);

  var scriptableStream=Components
    .classes["@mozilla.org/scriptableinputstream;1"]
    .getService(Components.interfaces.nsIScriptableInputStream);

  var channel=ioService.newChannel(aURL,'',null);
  var input=channel.open();
  scriptableStream.init(input);
  var str=scriptableStream.read(input.available());
  scriptableStream.close();
  input.close();
  return str;
}

/*: cheat @Unsafe*/
function openWebfolderViewForUrl(url) {

  var oldcursor = window.content.document.body.style.cursor;
  window.content.document.body.style.cursor = "wait";

  var wscript = Components.classes["@mozilla.org/file/directory_service;1"].
    getService(Components.interfaces.nsIProperties).
    get("WinD", Components.interfaces.nsIFile);
  wscript.append("system32");
  wscript.append("wscript.exe");
  
  if (! wscript.exists()) {
    alert("Windows Script engine not found at '" + wscript.path + "'!");
  }
  else {
    var content = openWebfolderGetContents("chrome://openwebfolder/content/open-webfolder.js");
  
    var process = Components.classes['@mozilla.org/process/util;1'].getService(Components.interfaces.nsIProcess);
    process.init(wscript);
    var arguments = /*:Array<Str>*/[] ;
  
    var tmpfile = Components.classes["@mozilla.org/file/directory_service;1"].
      getService(Components.interfaces.nsIProperties).
      get("TmpD", Components.interfaces.nsIFile);
    tmpfile.append("open-webfolder.js");

    tmpfile.createUnique(Components.interfaces.nsIFile.NORMAL_FILE_TYPE, 0x664);
  
    var foStream = Components.classes["@mozilla.org/network/file-output-stream;1"]
      .createInstance(Components.interfaces.nsIFileOutputStream);
  
    foStream.init(tmpfile, 0x02 | 0x08 | 0x20, 0x664, 0); // write, create, truncate
    foStream.write(content, content.length);
    foStream.close();
  
    // make sure WSH uses the right scripting engine even if the setup 
    // is broken
    arguments.push("//E:jscript");
    // make sure the script will time out after 30 seconds
    arguments.push("//T:30");
    
    arguments.push(tmpfile.path);
    arguments.push(url);
  
    var result = {};
    
    process.run(true, arguments, arguments.length, result);
      
    tmpfile.remove(false);
  }
  
  window.content.document.body.style.cursor = oldcursor;
  return false;
}

/*: cheat @Unsafe*/
function openWebfolderView() {
  openWebfolderViewForUrl(gBrowser.currentURI.spec);
  return true;
}

/*: cheat @Unsafe*/
function openWebfolderViewLink() {
  openWebfolderViewForUrl('getLinkURL' in gContextMenu ? gContextMenu.getLinkURL() : gContextMenu.linkURL());
  return true;
}

/*: Ext */function openWebfolderChangeState(e) {

  // jre: code below doesn't work anymore in FF 1.5; help appreciated
  // e.view.openWebfolderViewForUrl = openWebfolderViewForUrl;
  
  var anchors = (/*:cheat nsIDOMWindow*/window).content.document.getElementsByTagName("a");
  if (anchors) {
    for (var i = 0; i < anchors.length; i++) {
      var anch = anchors[i];
      var stattr = anch.getAttribute("folder");
      if (stattr) {
        anch.setAttribute("onclick", "javascript:openWebfolderViewForUrl('" + stattr + "');");
      }
    }
  }
}

/*: Ext */function openWebfolderContextListener(e) {
  if (gContextMenu.onLink) {
    var target = ('getLinkURL' in gContextMenu ? gContextMenu.getLinkURL() : gContextMenu.linkURL());
    var isHttpTarget = target != null &&
      (target.substr(0, 5).toLowerCase() == "http:" || target.substr(0, 6).toLowerCase() == "https:");
    gContextMenu.showItem("openwebfolder-do-link", isHttpTarget);
  }
  else {
    gContextMenu.showItem("openwebfolder-do-link", false);
  }
}

// add listeners to various necessary parts of the browser, to handle page load and context menu display
/*: Ext -> Undef */function openWebfolderInit(e) {
  // event listeners for browser
  var contentArea = document.getElementById("appcontent");
  if (contentArea != null) {
    contentArea.addEventListener("load", openWebfolderChangeState, true);
  }
  var menuArea = document.getElementById("contentAreaContextMenu");
  if (menuArea != null) {
    menuArea.addEventListener("popupshowing", openWebfolderContextListener, false);
  }

  // event listeners for mail client
  var messageArea = document.getElementById("messagePaneContext");
  if (messageArea != null) {
    messageArea.addEventListener("popupshowing", openWebfolderContextListener, false);
  }
}

window.addEventListener("load", openWebfolderInit, false);

function(event) { /*: cheat Ext*/(openWebfolderView()); }; // XXX
function(event) { /*: cheat Ext*/(openWebfolderViewLink()); }; // XXX

