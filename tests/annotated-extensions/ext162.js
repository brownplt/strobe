var gContextMenu = /*: Ext */null;
var gBrowser = /*: Ext */null;
if(!com) var com=/*: Ext */{};
if(!com.jeeradejthaworntaweewong) com.jeeradejthaworntaweewong={};
if(!com.jeeradejthaworntaweewong.print) com.jeeradejthaworntaweewong.print={};

com.jeeradejthaworntaweewong.print = {
    DEBUG: false,
    APP_NAME: "Print",
    VERSION: "0.3.3",

	dumpObject: function(obj) {
		for(i in obj){         
			this.debug(i + " = " + obj[i] + "\n");
		}
	},

	debug: function(str) {
		if (!this.DEBUG) {
			return;
		}
		var consoleService = Components.classes["@mozilla.org/consoleservice;1"]
			.getService(Components.interfaces.nsIConsoleService);
		consoleService.logStringMessage(this.APP_NAME + ": " + str);
	},
	
	printInit: function () {
		this.debug("printInit()");
		var cacm = document.getElementById("contentAreaContextMenu");
		if(cacm) {
			cacm.addEventListener("popupshowing", function() {com.jeeradejthaworntaweewong.print.onPrintPopup();}, false);
		}
	},

	onPrintPopup: function () {
		this.debug("onPrintPopup()");
		// hide the Print item when apropriate
		var item = document.getElementById("print_menu");
		if(item) {
			if (!(gContextMenu.onLink || gContextMenu.onImage || gContextMenu.onTextInput)) {
				item.hidden = false;
			}
			else {
				item.hidden = true;
			}
		}
	},
	
	onPrintPage: function () {
		this.debug("onPrintPage()");
		gBrowser.contentWindow.print();
	}
};

// Every time a new browser window is made printInit will be called
window.addEventListener("load", /*: [Window] -> Undef */function() {com.jeeradejthaworntaweewong.print.printInit();}, false);

function(event) { com.jeeradejthaworntaweewong.print.onPrintPage(); };

