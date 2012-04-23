// May be ugly code cuz I don't know JavaScript that well :-/

var Cc = Components.classes;
var Ci = Components.interfaces;
var gContextMenu = /*: {Ext with isTextSelected: Bool} */null;
function alert(str) {}
function toOpenWindowByType(a,b,c) {}

const rfDM = Cc["@mozilla.org/download-manager;1"].getService(Ci.nsIDownloadManager);
const rfIOS = Cc["@mozilla.org/network/io-service;1"].getService(Ci.nsIIOService);
const rfPrefs = Cc["@mozilla.org/preferences-service;1"].getService(Components.interfaces.nsIPrefService).getBranch("extensions.rapidfire.");

/*: nsIXMLHttpRequest -> Str */function rfProcessResponse(req) {
	if (req.status != 200)
        return "Could not verify status of the files:\n" + req.statusText;
    if (req.statusText.indexOf("ERROR") === 0)
        return "Could not verify status of the files:\n" + req.responseText;

    var lines = req.responseText.split("\n");
    for (var i = 0; i < lines.length - 1; i++) {
        var attrs = lines[i].split(",");
		var name = attrs[1];
		var status = attrs[4];
        if (status == 1 || status == 2 || status == 6) // OK
            continue;
        else {
            var msg = /*: Str */"";
            if (status == 0)
                msg = "does not exist on server.";
            else if (status == 3)
                msg = "cannot be downloaded because the server is down.";
            else if (status == 4)
                msg = "has been marked as illegal so cannot be downloaded.";
            else if (status == 5)
                msg = "is an anonymous file that has been locked because of more than 10 downloads.";
            else
                msg = "cannot be downloaded because of an unknown error.";
            return "The file `" + name + "` " + msg + "\n\nDownload aborted.";
        }
    }
	
	return "";
}

/*: Array<Str> * Array<Str> -> Bool */function rfCheck(ids, names) {
	var errMsg = /*: Str */"";
    var req = /*: cheat nsIXMLHttpRequest */new XMLHttpRequest();
    var url = "http://api.rapidshare.com/cgi-bin/rsapi.cgi?sub=checkfiles_v1&files=" + ids + "&filenames=" + names;
    req.open("GET", url, false);
    req.send();
    errMsg = rfProcessResponse(req);
	if (errMsg === "")
		return true;
	else {
		alert(errMsg);
		return false;
	}
}

/*: Str * nsIFile -> Undef */function rfDownload(link, dir) {
	var target = dir.clone();
	var filename = link.match(/http:\/\/rapidshare.com\/files\/\d+\/(\S+)/)[1];
	target.append(filename);
	var persist = Cc["@mozilla.org/embedding/browser/nsWebBrowserPersist;1"].createInstance(Ci.nsIWebBrowserPersist);
	persist.persistFlags = Ci.nsIWebBrowserPersist.PERSIST_FLAGS_REPLACE_EXISTING_FILES | Ci.nsIWebBrowserPersist.PERSIST_FLAGS_CLEANUP_ON_FAILURE | Ci.nsIWebBrowserPersist.PERSIST_FLAGS_AUTODETECT_APPLY_CONVERSION;
	var dl = rfDM.addDownload(rfDM.DOWNLOAD_TYPE_DOWNLOAD, rfIOS.newURI(link, "", null), rfIOS.newFileURI(target), "rapidfire: " + filename, null, Math.round(Date.now()*1000), null, persist);
	persist.progressListener = dl.QueryInterface(Ci.nsIWebProgressListener);
	persist.saveURI(dl.source, null, null, null, "", dl.targetFile);
}

/*: nsIDOMElement -> Undef */function rapidfire(el) {
    var links = el.getAttribute("selection").match(/(http:\/\/rapidshare.com\/files\/\d+\/\S+)\b/gm);
	if (links == null) {
		alert("rapidfire: no links found.");
		return;
	}
    var ids = new /*: Array![Str] */Array(links.length);
    var names = new /*: Array![Str] */Array(links.length);
    for (var i = 0; i < links.length; i++) {
        var info = links[i].match(/\/files\/(\d+)\/(\S+)/);
        ids[i] = info[1];
        names[i] = info[2];
    }
    
	if (rfCheck(ids, names)) {		
		var dir = rfDM.userDownloadsDirectory;
		if (rfPrefs.getBoolPref("destdir.ask")) {
			var picker = Cc["@mozilla.org/filepicker;1"].createInstance(Components.interfaces.nsIFilePicker);
			picker.displayDirectory = dir;
			picker.appendFilters(Ci.nsIFilePicker.filterAll);
			picker.init(/*: cheat nsIDOMWindow */window, "Choose destination directory", Components.interfaces.nsIFilePicker.modeGetFolder);
			if (picker.show() != 0) // User cancelled
				return;
			dir = picker.file;
		}
		
		toOpenWindowByType('Download:Manager', 'chrome://mozapps/content/downloads/downloads.xul', 'chrome,dialog=no,resizable'); // Raise DM window
	    
		for(var i = 0; i < links.length; i++)
			rfDownload(links[i], dir);
	}
}

	/*: Ext */function rapidfireShow() {
		var menuitem = document.getElementById("rapidfire-download");
		if(menuitem) {
			menuitem.hidden = !gContextMenu.isTextSelected;
			menuitem.setAttribute("selection", document.commandDispatcher.focusedWindow.getSelection().toString());
		}
	}

	/*: [Window] -> Undef */function rapidfireOverlayInit() {
		var menu = document.getElementById("contentAreaContextMenu");
		menu.addEventListener("popupshowing", rapidfireShow, false);
	}
	
window.addEventListener("load", rapidfireOverlayInit, false);
	
/*: [nsIDOMElement] nsIDOMEvent -> Undef */function(event) { rapidfire(this); };
