/*


	The Middle Mouse Button
	Copyright (C) 2009 Jonathan Zarate


*/
var TheMiddleMouseButtonExt = /*:Ext*/null;
TheMiddleMouseButtonExt = {
	init: function() {
		window.addEventListener('load', function() { TheMiddleMouseButtonExt.load(); }, false);
	},

	load: function() {
		var p = Components.classes['@mozilla.org/preferences-service;1']
			.getService(Components.interfaces.nsIPrefService)
			.getBranch('extensions.TheMiddleMouseButton.');
		this.closeTab = p.getBoolPref('closetab');
	
		var c = document.getElementById('content');
		c.addEventListener('mousedown', this.mDown, true);
		c.addEventListener('click', this.mClick, true);
	},

	isMine: function(event) {
		return ((event.button == 1) && (!event.shiftKey) && (!event.altKey) && (!event.metaKey) &&
				(event.originalTarget.baseURI != 'chrome://browser/content/browser.xul'));
	},
	
	mDown: function(event) {
		if (TheMiddleMouseButtonExt.isMine(event)) {
			var o = /*:Ext*/null;
			if ((o = getBrowser()).canGoBack) {
				o.goBack();
			}
			else {
				if (TheMiddleMouseButtonExt.closeTab)
					BrowserCloseTabOrWindow();
			}
			event.stopPropagation();
		}
	},
	
	mClick: function(event) {
		if (TheMiddleMouseButtonExt.isMine(event))
			event.stopPropagation();
	}
};

TheMiddleMouseButtonExt.init();
