/*:: type CommandRunInit = rec c . {AnObject with
  onLoad : [this('c)] Ext -> Undef,
  onPageLoad : nsIDOMEvent -> Undef
}; */

/*:: type constructor CommandRunHandlerConstructor = [CommandRunHandlerInst] -> CommandRunHandlerInst
and prototype CommandRunHandler = {AnObject with
  notsurewhyineedthis : Ext
}
and instance CommandRunHandlerInst = {Ext with
  run : Unsafe,
  isCommandAllowed : [CommandRunHandlerInst] Str * Str -> Bool,
  contains : forall a . 'a * Array<'a> -> Bool,
  isPrefix : Str * Str -> Bool,
  page : Str
};

type aStrDict = {AnObject with /^("-*- code -*-"|"__proto__")/ :! Array<Str>};
*/

function alert(str) {}

/*: CommandRunHandlerConstructor */function CommandRunHandler() {
  this.run = /*: cheat @Unsafe */function(command,args) {
    /* check whether command is allowed */
    if (!this.isCommandAllowed(command,this.page)) {
      var alertText = "Command '"+command+"'\n"
      	+"is not allowed for page "+this.page+".";
      alert(alertText);
      return 1;
    }
    var file = Components.classes["@mozilla.org/file/local;1"].
                     createInstance(Components.interfaces.nsILocalFile);
    try {
    	file.initWithPath(command);
    } catch (e) {
    	alert("command not found: "+command);
    	return 1;
    }
    var blocking = true;
    var process = Components.classes["@mozilla.org/process/util;1"].
                      createInstance(Components.interfaces.nsIProcess);
    try {
    	process.init(file);
    } catch (e) {
    	alert("command not found: "+command);
    	return 1;
    }

    process.run(blocking, args, args.length);
    
    result = process.exitValue;

    return result;
  },
  this.isCommandAllowed = /*: [CommandRunHandlerInst] Str * Str -> Bool */function(command,page) {
    /* get the root preferences branch */
    var prefs = Components.classes["@mozilla.org/preferences-service;1"]
                    .getService(Components.interfaces.nsIPrefBranch);
                    
    /* get the allowed commands preference */
    try {
	    var allowedCommandsPref = 
	      prefs.getComplexValue(
	        "extensions.commandrun.allowedcommands",
	        Components.interfaces.nsISupportsString).data;
	} catch (NS_ERROR_UNEXPECTED) {
		/* ignore */
	}
	    
    /* evaluate the json text */
    if (allowedCommandsPref) {
    	var allowedCommands = (/*: cheat Array<Str> */(JSON.parse(allowedCommandsPref)));
    	if (this.contains(command,allowedCommands)) return true;
    }
    
    /* continue with allowed commands per host preference */
    try {
	    var allowedCommandsPerHostPref = 
	      prefs.getComplexValue(
	        "extensions.commandrun.allowedcommandsperhost",
	        Components.interfaces.nsISupportsString).data;
	} catch (NS_ERROR_UNEXPECTED) {
		/* ignore */
	}
	
	if (allowedCommandsPerHostPref) {
		var obj = (/*: cheat aStrDict */(JSON.parse(allowedCommandsPerHostPref)));
		/* keys of the object are prefixes for pages,
		   values are lists of commands */
                var e = /*: Ext*/null;
		try {
			for (prefixB in obj) {
				var prefix = /*: cheat /^("-*- code -*-"|"__proto__")/ */prefixB;
				if (this.isPrefix(page,prefix)) {
					/* get allowed commands for this prefix */
					var allowedCommands = obj[prefix];
    				if (this.contains(command,allowedCommands)) {
    					return true;
    				}
				}		
			}
		} catch (e) {
			alert(
				"failed to parse extensions.commandrun.allowedcommandsperhost: "
				+e);
		}
	}
	
    return false;
  },
  this.contains = function(element,array) {
    var i = /*: Num*/0;
    for (i=0; i<array.length; i++) {
      if (array[i] === element) {
        return true;
      }
    }
    return false;
  },
  /**
   * Checks whether the given prefix is a prefix of the
   * given string.
   */
  this.isPrefix = function(string,prefix) {
  	return (prefix === string.substring(0,prefix.length)); 	
  };
}

var CommandRun = /*: CommandRunInit */null;
CommandRun = {
  onLoad: /*: [CommandRunInit] Ext -> Undef */function(e) {
    // initialization code
    this.initialized = true;
    var appcontent = (/*: cheat nsIDOMDocument */document).getElementById("appcontent");
    if (appcontent) {
      appcontent.addEventListener("DOMContentLoaded", CommandRun.onPageLoad, true);
    }
  },
  onPageLoad: function(event) {
    var win = (/*: cheat {Ext with CommandRun : CommandRunHandlerInst, document : nsIDOMDocument} */(event.originalTarget.defaultView.wrappedJSObject));
    win.CommandRun = new CommandRunHandler();
	win.CommandRun.page = win.document.location.href;
  }
};
window.addEventListener("load", /*: [Window] Ext -> Undef */function(e) { CommandRun.onLoad(e); }, false);

