/* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1/GPL 2.0/LGPL 2.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is Fireclam.
 *
 * The Initial Developer of the Original Code is
 * Christof Efkemann.
 * Portions created by the Initial Developer are Copyright (C) 2009
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * Alternatively, the contents of this file may be used under the terms of
 * either the GNU General Public License Version 2 or later (the "GPL"), or
 * the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
 * in which case the provisions of the GPL or the LGPL are applicable instead
 * of those above. If you wish to allow use of your version of this file only
 * under the terms of either the GPL or the LGPL, and not to allow others to
 * use your version of this file under the terms of the MPL, indicate your
 * decision by deleting the provisions above and replace them with the notice
 * and other provisions required by the GPL or the LGPL. If you do not delete
 * the provisions above, a recipient may use your version of this file under
 * the terms of any one of the MPL, the GPL or the LGPL.
 *
 * ***** END LICENSE BLOCK ***** */


var cc = Components.classes;
var ci = Components.interfaces;
var cr = /*: cheat nsIXPCComponents_Results */Components.results;
var cu = Components.utils;

cu.import("resource://gre/modules/XPCOMUtils.jsm");
/*::
type xpcomUtilsType = {Ext with
  generateNSGetFactory : Array<Any> -> Ext,
  generateNSGetModule : Array<Any> -> Ext
}; */
var XPCOMUtils = /*: xpcomUtilsType */null;

var stringService = cc["@mozilla.org/intl/stringbundle;1"].getService(ci.nsIStringBundleService);
var strings = stringService.createBundle("chrome://fireclam/locale/fireclam.properties");
var osString  = cc["@mozilla.org/xre/app-info;1"].getService(ci.nsIXULRuntime).OS;


/*: Str -> Str*/function getString(name)
{
    return strings.GetStringFromName(name);
}


/*: Str * Array<Str> -> Str */function formatString(name, args)
{
    return strings.formatStringFromName(name, args); // BUG: had a length parameter
}


/*:: type constructor resultThreadConstructor = [resultThreadInst] nsILocalFile * Num * nsIThread * nsIFile -> resultThreadInst
and prototype resultThreadProto = {AnObject with
  run : [resultThreadInst] -> Undef,
  QueryInterface : QueryInterfaceType
}
and instance resultThreadInst = {Ext with
  file : nsILocalFile,
  exitcode : Num,
  bgThread : nsIThread, 
  logFile : nsIFile
}; */
/*: resultThreadConstructor */function resultThread(file, exitcode, bgThread, logFile)
{
    this.file = file;
    this.exitcode = exitcode;
    this.bgThread = bgThread;
    this.logFile = logFile;
}


resultThread.prototype = {
    run: /*: [resultThreadInst] -> Undef */function()
    {
	if (this.bgThread != null)
	    this.bgThread.shutdown();
	var promptService = cc["@mozilla.org/embedcomp/prompt-service;1"].getService(ci.nsIPromptService);
	var fstream = cc["@mozilla.org/network/file-input-stream;1"].createInstance(ci.nsIFileInputStream);
	var cstream = cc["@mozilla.org/intl/converter-input-stream;1"].createInstance(ci.nsIConverterInputStream);
	var logMessage = /*:Str*/'';

	fstream.init(this.logFile, -1, 0, 0);
	cstream.init(fstream, "UTF-8", 0, 0);
	    var str = /*:Outparam<Str>*/null;
	    cstream.readString(-1, str);
	    logMessage = str.value;

	cstream.close();  // also closes fstream

	switch (this.exitcode) {
	case 0:
	    /* file is clean, nothing to do */
	    break;
		
	case 1:
	    /* file is infected, tell user */
	    promptService.alert(null,
				getString("virus_report_title"),
				formatString("virus_report_message", [this.file.path, logMessage]));
	    break;

	case 50:
	    /* database error */
	    promptService.alert(null,
				getString("problem_title"),
				formatString("database_error_message", [this.file.path]));
	    break;

	case -127:
	    /* special return: executable not found */
	    promptService.alert(null,
				getString("problem_title"),
				formatString("missing_executable_message", [this.file.path]));
	    break;

	default:
	    /* some other error occurred */
	    promptService.alert(null,
				getString("problem_title"),
				formatString("problem_message", [this.file.path, "" + this.exitcode]));
	}

	/* remove log file */
	this.logFile.remove(false);
    },

    QueryInterface: /*:cheat QueryInterfaceType*/ function(iid)
    {
	if (iid.equals(ci.nsIRunnable) ||
	    iid.equals(ci.nsISupports)) {
	    return this;
	}
	throw cr.NS_ERROR_NO_INTERFACE;
    }
};

/*:: type constructor scanConstructor = [scanInst] Str * Array<Str> * nsILocalFile * nsIThread * nsIFile -> scanInst
and prototype scanProto = {AnObject with
  run : Unsafe,
  QueryInterface : QueryInterfaceType
}
and instance scanInst = {Ext with
  prog : Str,
  args : Array<Str>,
  file : nsILocalFile,
  thread : nsIThread, 
  logFile : nsIFile
}; */
/*: scanConstructor */function scanThread(prog, args, file, thread, logFile)
{
    this.prog = prog;
    this.args = args;
    this.file = file;
    this.thread = thread;
    this.logFile = logFile;
}


scanThread.prototype = {
    run: /*: cheat Unsafe */function()
    {
	var exitcode;

	try {
	    var clamScanFile = cc["@mozilla.org/file/local;1"].createInstance(ci.nsILocalFile);
	    clamScanFile.initWithPath(this.prog);

	    var clamScanProc = cc["@mozilla.org/process/util;1"].createInstance(ci.nsIProcess);
	    clamScanProc.init(clamScanFile);
	    clamScanProc.run(true, this.args, this.args.length);
	    exitcode = clamScanProc.exitValue;
	}
	catch (e) {
	    exitcode = -127;
	    this.file = this.prog;
	}

	var main = cc["@mozilla.org/thread-manager;1"].getService().mainThread;
	main.dispatch(new resultThread(this.file, exitcode, this.thread, this.logFile), main.DISPATCH_NORMAL);
    },

    QueryInterface: /*:cheat QueryInterfaceType*/ function(iid)
    {
	if (iid.equals(ci.nsIRunnable) ||
	    iid.equals(ci.nsISupports)) {
	    return this;
	}
	throw cr.NS_ERROR_NO_INTERFACE;
    }
};

/*:: type constructor fireclamConstructor = [fireclamInst] -> fireclamInst
and prototype fireclamProto = {AnObject with
  classDescription: Str,
  classID: Ext,
  contractID: Str,
  _xpcom_categories: Array<Ext>,
  QueryInterface: QueryInterfaceType,
  observe : Unsafe,
  scanFile: Unsafe
}
and instance fireclamInst = {Ext with
  observe :^ Unsafe
}; */
/*: fireclamConstructor */function fireclamService()
{
    var defaultPath = /*: Str*/"/usr/bin/clamscan";
    var defaultDBPath = /*: Str*/"/var/lib/clamav";

    if (osString == "WINNT") {
        try {
	    var wrk = cc["@mozilla.org/windows-registry-key;1"].
		createInstance(ci.nsIWindowsRegKey);
	    wrk.open(wrk.ROOT_KEY_LOCAL_MACHINE,
		     "SOFTWARE\\ClamWin",
		     wrk.ACCESS_READ);
	    var iniPath = wrk.readStringValue("Path") + "\\ClamWin.conf";

	    var iniFile = cc["@mozilla.org/file/local;1"].createInstance(ci.nsILocalFile);
	    iniFile.initWithPath(iniPath);

	    var fis2 = cc["@mozilla.org/network/file-input-stream;1"].createInstance(ci.nsIFileInputStream);
	    fis2.init(iniFile, 0x01, 0x444, 0);
	    var fis = fis2.QueryInterface(ci.nsILineInputStream);

	    var reClamScan = /^clamscan = (.*)$/;
	    var reDBPath = /^database = (.*)$/;
	    var line = /*:Outparam<Str>*/null;
	    var cont = /*:Bool*/false;
	    var result = /*:Array<Str>*/null;

	    do {
		cont = fis.readLine(line);
		if ((result = line.value.match(reClamScan))) {
		    defaultPath = result[1];
		}
		if ((result = line.value.match(reDBPath))) {
		    defaultDBPath = result[1];
		}
	    } while (cont);
	    fis2.close();
	}
	catch (e)
	{
	}
    }
    var prefService = cc["@mozilla.org/preferences-service;1"].getService(ci.nsIPrefService);
    var branch = prefService.getDefaultBranch("extensions.fireclam.");
    (/*: cheat Innocuous */(branch.setCharPref("clamavexecutable", defaultPath)));
    (/*: cheat Innocuous */(branch.setCharPref("dbpath", defaultDBPath)));

    var obsService = cc["@mozilla.org/observer-service;1"].getService(ci.nsIObserverService);
    obsService.addObserver(/*:cheat nsIObserver*/this, "dl-done", false);
}


fireclamService.prototype = {
    classDescription: "fireclam Service",
    classID: Components.ID("{13E3DFE2-FC31-4358-8273-8C0A3FC69003}"),
    contractID: "@efkemann.net/fireclam;1",
    _xpcom_categories: [{ category: "app-startup", service: true }],

    QueryInterface: /*: cheat QueryInterfaceType */XPCOMUtils.generateQI([ci.nsIObserver]),

    observe: /*: cheat Unsafe */function(subject, topic, data)
    {
	if (topic == "dl-done") {
	    var download = subject.QueryInterface(ci.nsIDownload);
	    var file = download.targetFile;
	    this.scanFile(file);
	}
    },

    scanFile: /*: cheat Unsafe */function(file)
    {
	var prefService = cc["@mozilla.org/preferences-service;1"].getService(ci.nsIPrefService);
	var dirService = cc["@mozilla.org/file/directory_service;1"].getService(ci.nsIProperties);
	var branch = prefService.getBranch("extensions.fireclam.");
	var path = branch.getCharPref("clamavexecutable");
	var dbpath = branch.getCharPref("dbpath");
	var logFile = dirService.get("TmpD", ci.nsIFile);
	var uFile;
	var uLogFile;

	logFile.append("fireclam.tmp");
	logFile.createUnique(ci.nsIFile.NORMAL_FILE_TYPE, 0x600);

	if (osString != "WINNT") {
	    var converter = cc["@mozilla.org/intl/scriptableunicodeconverter"].createInstance(ci.nsIScriptableUnicodeConverter);
	    converter.charset = "UTF-8";
	    uFile = converter.ConvertFromUnicode(file.path) + converter.Finish();
	    uLogFile = converter.ConvertFromUnicode(logFile.path) + converter.Finish();
	}
	else {
	    uFile = file.path;
	    uLogFile = logFile.path;
	}

	var args = ["--no-summary", "--quiet", "-l", uLogFile, "-d", dbpath, uFile];
	if (XPCOMUtils.generateNSGetFactory) {
	    // Firefox 4: cannot use background thread
	    var main = cc["@mozilla.org/thread-manager;1"].getService().mainThread;
	    main.dispatch(new scanThread(path, args, file, null, logFile), main.DISPATCH_NORMAL);
	}
	else {
	    var bgThread = cc["@mozilla.org/thread-manager;1"].getService().newThread(0);
	    bgThread.dispatch(new scanThread(path, args, file, bgThread, logFile), bgThread.DISPATCH_NORMAL);
	}
    },
};


var components = /*: Array<Any> */[fireclamService];

if (XPCOMUtils.generateNSGetFactory)
    var NSGetFactory = XPCOMUtils.generateNSGetFactory(components);
else
    var NSGetModule = XPCOMUtils.generateNSGetModule(components);

// Local Variables:
// coding: iso-8859-1
// c-basic-offset: 4
// End:

/* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1/GPL 2.0/LGPL 2.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is Fireclam.
 *
 * The Initial Developer of the Original Code is
 * Christof Efkemann.
 * Portions created by the Initial Developer are Copyright (C) 2009
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * Alternatively, the contents of this file may be used under the terms of
 * either the GNU General Public License Version 2 or later (the "GPL"), or
 * the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
 * in which case the provisions of the GPL or the LGPL are applicable instead
 * of those above. If you wish to allow use of your version of this file only
 * under the terms of either the GPL or the LGPL, and not to allow others to
 * use your version of this file under the terms of the MPL, indicate your
 * decision by deleting the provisions above and replace them with the notice
 * and other provisions required by the GPL or the LGPL. If you do not delete
 * the provisions above, a recipient may use your version of this file under
 * the terms of any one of the MPL, the GPL or the LGPL.
 *
 * ***** END LICENSE BLOCK ***** */

// create namespace
if (!net) var net = /*: Ext*/{};
if (!net.efkemann) net.efkemann = {};


net.efkemann.Fireclam = function() {
    // private stuff
    var ci = Components.interfaces;
    var cc = Components.classes;
    var stringService = cc["@mozilla.org/intl/stringbundle;1"].getService(ci.nsIStringBundleService);
    var strings = stringService.createBundle("chrome://fireclam/locale/fireclamprefs.properties");

    /*: Str -> Str*/function getString(name)
    {
	return strings.GetStringFromName(name);
    }

    // public stuff
    return {

    onBrowseFile: function()
    {
	var pref = /*: cheat Mutable<nsIDOMXULTextBoxElement> */document.getElementById("clamavexecutable");
	var fp = cc["@mozilla.org/filepicker;1"].createInstance(ci.nsIFilePicker);

	fp.init(/*: cheat nsIDOMWindow*/window, getString("browse_clamav_executable"), ci.nsIFilePicker.modeOpen);
	fp.defaultString = pref.value;

	if (fp.show() == ci.nsIFilePicker.returnOK) {
	    pref.value = fp.file.path;
	}
    },

    onBrowseDir: function()
    {
	var pref = /*: cheat Mutable<nsIDOMXULTextBoxElement> */document.getElementById("dbpath");
	var fp = cc["@mozilla.org/filepicker;1"].createInstance(ci.nsIFilePicker);

	fp.init(/*: cheat nsIDOMWindow*/window, getString("browse_db_path"), ci.nsIFilePicker.modeGetFolder);
	try {
	    var dir = cc["@mozilla.org/file/local;1"].createInstance(ci.nsILocalFile);
	    dir.initWithPath(pref.value);
	    fp.displayDirectory = dir;
	}
	catch (e) {
	}

	if (fp.show() == ci.nsIFilePicker.returnOK) {
	    pref.value = fp.file.path;
	}
    }

    };
}();

// Local Variables:
// coding: utf-8
// c-basic-offset: 4
// End:
function(event) { net.efkemann.Fireclam.onBrowseFile(); };
function(event) { net.efkemann.Fireclam.onBrowseDir(); };
