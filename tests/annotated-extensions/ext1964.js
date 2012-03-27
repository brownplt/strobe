const nsX509CertDB = "The mozilla nsX509CertDB constant string";
var nsIX509CertDB = Components.interfaces.nsIX509CertDB; // Missing include
var nsIX509Cert = Components.interfaces.nsIX509Cert; // Missing include

function toOpenWindowByType(inType, uri) {
  var winopts = /*:Ext*/"chrome,extrachrome,menubar,resizable,scrollbars,status,toolbar";
  window.open(uri, "_blank", winopts);
}
function(event) { toOpenWindowByType('mozilla:certmanager', 'chrome://pippki/content/certManager.xul'); };
function(event) { toOpenWindowByType('mozilla:certmanager', 'chrome://pippki/content/certManager.xul'); };
function(event) { toOpenWindowByType('mozilla:certmanager', 'chrome://pippki/content/certManager.xul'); };
function(event) { toOpenWindowByType('mozilla:certmanager', 'chrome://pippki/content/certManager.xul'); };
function(event) { toOpenWindowByType('mozilla:certmanager', 'chrome://pippki/content/certManager.xul'); };

/*:: type MutableDOMXULTreeElement = {Ext with view : nsIASN1Tree, columns : nsITreeColumns};*/
/*: -> nsIX509Cert*/
function getCurrentCert()
{
  var realIndex = /*:Ext*/0;
  var tree = document.getElementById('treesetDump');
  if (tree.view.selection.isSelected(tree.currentIndex)
      && !document.getElementById('general_tab').selected) {
    /* if the user manually selected a cert on the Details tab,
       then take that one */
    realIndex = tree.currentIndex;    
  } else {
    /* otherwise, take the one at the bottom of the chain
       (i.e. the one of the end-entity, unless we're displaying
       CA certs) */
    realIndex = tree.view.rowCount - 1;
  }
  if (realIndex >= 0) {
    var item = tree.contentView.getItemAtIndex(realIndex);
    var dbKey = item.firstChild.firstChild.getAttribute('display');
    var certdb = Components.classes[nsX509CertDB].getService(nsIX509CertDB);
    var cert = certdb.findCertByDBKey(""+dbKey,/*:cheat nsISupports*/null);
    return cert;
  }
  /* shouldn't really happen */
  return /*:cheat nsIX509Cert*/null;
}

/*: nsIX509Cert -> Str */
function getPEMString(cert)
{
  return '-----BEGIN CERTIFICATE-----\r\n'
        + /*:cheat String*/(btoa(getDERString(cert))).replace(/(\S{64}(?!$))/g, "$1\r\n")
         + '\r\n-----END CERTIFICATE-----\r\n';
}

/*: nsIX509Cert -> Str */
function getDERString(cert)
{
  var length = /*:cheat {value:Num}*/{value:0};
  var derArray = /*:cheat Array<Ext>*/cert.getRawDER(length);
  var derString = /*:Str*/'';
  for (var i = 0; i < derArray.length; i++) {
    derString += String.fromCharCode(derArray[i]);
  }
  return derString;
}

// Reviews from AMO editors now (2009) seem to insist on not polluting
// the global namespace - so here we go, wrap our stuff into ch.velox.cvp...
/*:: 
  type Cvp = rec cvp . {AnObject with
      displayPEM : [this('cvp)] Any * nsIX509Cert -> Undef,
      reverseRDNs : [this('cvp)] Ext -> Ext,
      indent : [this('cvp)] Num -> Str,
      setCertTrust : [this('nsIDOMElement)] nsIDOMEvent -> Undef,
      getCertTrust : [this('cvp)] Ext -> Ext,
      addTrustSettingCheckBoxes: [this('cvp)] -> Undef,
      };
*/
/*::
  type MainObject = {AnObject with
  velox : {AnObject with cvp : Cvp}
      };
*/
if (!ch) var ch = /*:cheat MainObject*/{};
if (!ch.velox) ch.velox = /*: cheat {AnObject with cvp : Cvp}*/{};
if (!ch.velox.cvp) ch.velox.cvp = /*:Cvp*/{

trustconstants: { SSL:  nsIX509CertDB.TRUSTED_SSL,
                  Mail: nsIX509CertDB.TRUSTED_EMAIL,
                  Code: nsIX509CertDB.TRUSTED_OBJSIGN },

reverseRDNs: /*: [Cvp] Ext -> Ext*/function(dn)
{
  /*
   * CERT_NameToAscii always returns RDNs in reverse order - undo that here.
   * Doesn't do the right thing for pathological cases like:
   * CN="Example User,O=Foobar",C=CH
   * CN="Example User,O=\"Foobar",C=CH
   * CN="Example User,O=",C=CH
   * etc. - but it's good enough for now
   */
  return(dn.split(/,(?=[^,]+=)/).reverse().join(","));
},


displayPEM: /*: [Cvp] Any * nsIX509Cert -> Undef*/function(parent, cert)
{
  if (!cert)
    return;

  // required string bundles
  var bundle = document.getElementById("certviewerplusbundle");
  var pipnss = document.getElementById("pipnssbundle");

  var prefs = Components.classes["@mozilla.org/preferences-service;1"].
              getService(Components.interfaces.nsIPrefService).
              getBranch("extensions.certviewerplus.");
  var format = 0;
  /*
   * preference indicating the level of text details in PEM view
   * 1 = subject, issuer, validity
   * 2 = subject, issuer, validity, extensions
   * 3 = full text dump
   */
  try {
   format = prefs.getIntPref("pemview");
  } catch(e) {}

  /*
   * get our temporary tree - kind of silly, but needed because:
   * - the nsITreeView methods (which are inherited by nsIASN1Tree)
   *   require column objects to be specified when retrieving the
   *   text of a particular cell
   *   (cf. also https://developer.mozilla.org/en/Tree_Widget_Changes)
   * - column objects are associated with a tree (only)
   * - we need the cell content of the first column, since these
   *   are the labels for the ASN1Structure data (SEQUENCE names)
   * Note that the tree element may not have its hidden attribute set
   * to true, this apparently has the effect that no tree is built.
   * Setting collapsed to true, however, will work "as intended".
   */
  var tempTree = /* want this to be a nsIDOMXULTreeElement, but they're immutable*/
    /*: cheat MutableDOMXULTreeElement*/(document.getElementById('tempTree'));
  var asn1Tree = Components.classes["@mozilla.org/security/nsASN1Tree;1"].
                 createInstance(Components.interfaces.nsIASN1Tree);
  tempTree.view = asn1Tree;

  // the "title" line - use the nickname (if available)
  var certname = (cert.nickname == pipnss.getString("CertNoNickname"))?
                 cert.windowTitle : cert.nickname;
  var chain = cert.getChain();
  var sourceString = bundle.getFormattedString(chain.length > 1 ?
                                               "displayPEMChainLabel" :
                                               "displayPEMLabel",
                                               /*:cheat Ext*/[ certname ], 1)
                     + "%0d%0a";

  for (var i = 0; i < chain.length; i++) {
    var currCert = chain.queryElementAt(i, Components.interfaces.nsIX509Cert);

    if (format < 3) {
      /*
       * always add one-line subject, issuer and validity information,
       * unless we're displaying full text dumps
       */
      var subjectName = /*:Ext*/'';
      var issuerName = /*:Ext*/'';
      /*
       * empty subject/issuer names will throw NS_ERROR_FAILURE
       * (see also https://bugzilla.mozilla.org/show_bug.cgi?id=344817)
       */
      try { subjectName = ch.velox.cvp.reverseRDNs(currCert.subjectName); } catch(e) {}
      try { issuerName = ch.velox.cvp.reverseRDNs(currCert.issuerName); } catch(e) {}
      // same for unparseable time values...
      var notBefore = /*:Ext*/'';
      var notAfter = /*:Ext*/'';
      try {
        notBefore = currCert.validity.notBeforeGMT;
      } catch(e) {
        notBefore = pipnss.getString("VerifyUnknown");
      }
      try {
        notAfter = currCert.validity.notAfterGMT;
      } catch(e) {
        notAfter = pipnss.getString("VerifyUnknown");
      }
      
      sourceString +=   pipnss.getString("CertDumpSubject") + ": "
                      + subjectName + "%0d%0a"
                      + pipnss.getString("CertDumpIssuer") + ": "
                      + issuerName  + "%0d%0a"
                      + pipnss.getString("CertDumpValidity") + ": "
                      + bundle.getFormattedString("displayPEMValidity",
                               /*:cheat Ext*/[ notBefore, notAfter ], 2) + "%0d%0a";
    }

    if (format > 1) {
      // display either extensions or full text dump
      asn1Tree.loadASN1Structure(currCert.ASN1Structure);

      // keep track of whether we should output the current row
      var doDump = 0;

      for (var j = 1; j < tempTree.view.rowCount; j++) {
        var currLabel = tempTree.view.
                        getCellText(j, tempTree.columns.getFirstColumn());
        var currLevel = tempTree.view.getLevel(j);

        if (format > 2)
          // always show everything
          doDump = 1;
        else if (format == 2) {
          if (currLabel == pipnss.getString("CertDumpExtensions"))
            // turn extensions output on
            doDump = 1;
          else if (currLevel < 2)
            // turn it off again, if we're done with the extensions
            doDump = 0;
          // and finally, remove one indenting level (for displaying only)
          currLevel--;
        }

        if (doDump) {
          sourceString += ch.velox.cvp.indent(currLevel - 1) + currLabel + ":\n";

          if (!tempTree.view.isContainer(j)) {
            sourceString += ch.velox.cvp.indent(currLevel);
            // get the text representation of the ASN.1 data and re-indent
              sourceString += (/*:cheat String*/(asn1Tree.getDisplayData(j)).
                                       replace(/\n/g, "\n" + ch.velox.cvp.indent(currLevel)));
            sourceString += "\n";
          }
        }
      }
      // replace newlines (and double-newlines) with URI escapes
        sourceString = (/*:cheat String*/sourceString).replace(/\n( *\n)*/g, "%0d%0a");
    }
    // add the Base64 encoded cert
      sourceString += (/*: cheat String*/(getPEMString(currCert))).replace(/\r\n/g, "%0d%0a");
  }

  // finally, open a view-source window (non-modal, no parent)
  var dataString = Components.classes["@mozilla.org/supports-string;1"].
                   createInstance(Components.interfaces.nsISupportsString);
    /*:cheat Undef*/(dataString.data = "data:;charset=utf-8," + sourceString);
  Components.classes["@mozilla.org/embedcomp/window-watcher;1"].
             getService(Components.interfaces.nsIWindowWatcher).
             openWindow(/*:cheat nsIDOMWindow*/null, "chrome://global/content/viewSource.xul",
                        "_blank","scrollbars,resizable,chrome,dialog=no",
                        dataString);
},

indent: /*: [Cvp] Num -> Str*/function(level)
{
  // number of spaces for a one-level indent
  const spaces = 4;

  var indent = /*:Ext*/'';
  for (var i = 0; i < level * spaces; i++)
    indent += ' ';

  return ""+indent;
},

tuneGeneralTab: function()
{
  /*
   * Slightly improve/extend the "General" tab of Cert Viewer
   *
   * Resorts to some ugly contortions, but unless we want to redo
   * the whole tab ourselves, it seems we have to live with that...
   * see also https://bugzilla.mozilla.org/show_bug.cgi?id=380775
   * ("Make general page of certificate viewer easier to understand"),
   * which was originally scheduled for Firefox 3, but seems to have
   * fallen off the radar.
   */

  /* 
   * Make the fingerprint values selectable/copyable.
   * (partly) addresses https://bugzilla.mozilla.org/show_bug.cgi?id=200621
   */
  var fps = [ "sha1fingerprint", "md5fingerprint" ];
  for (var i = 0; i < fps.length; i++) {
    var elt = document.getElementById(fps[i]);

    if (elt.id == "sha1fingerprint") {
      /*
       * Ugly workaround, take 1: the overall width of the Cert Viewer
       * dialog is determined by the length of the SHA1 hash, for many
       * certs. If we're changing the label to a textbox, then its length
       * will no longer work for properly "stretching" the width. The only
       * reliable (and cross-platform compatible) way is to add a hidden
       * label with the same content (the SHA1 hash). Experiments with
       * using the "size" property of the textbox XUL element
       * (https://developer.mozilla.org/en/XUL:textbox#p-size)
       * turned out to have quite different effects depending on the
       * platform (e.g. Windows vs. OS X).
       */
      var spacer = elt.parentNode.cloneNode(true);
      spacer.style.visibility = "hidden";
      // avoid duplicate id's
      spacer.lastChild.setAttribute("id", "sha1spacer");
      // another hack: avoid pixels being cut off at the end
      spacer.lastChild.setAttribute("value",
                                    spacer.lastChild.getAttribute("value")+" ");
      elt.parentNode.parentNode.insertBefore(spacer, null);

      /*
       * Ugly workaround, take 2: textboxes are focusable, and the
       * "postLoadInit" method from toolkit/content/widgets/dialog.xml
       * (which is called after the onload event) will set the focus
       * to the first focusable element. The net effect of this
       * would be that the first textbox element (either the SHA1 hash,
       * or the e-mail address field, if present), gets highlighted
       * when the Cert Viewer dialog is opened. To work around this,
       * we insert an empty text box on the "Issued To" line (so it
       * appears before any of our own textboxes), make sure that it's
       * invisible, and also "redirect" the focus to the "View PEM"
       * button whenever the toolkit tries focus on it.
       */
      var dummy_tbox = document.createElement("textbox");
      dummy_tbox.setAttribute("onfocus",
                 "document.getElementById('display_pem_gt').focus();");
      dummy_tbox.setAttribute("class", "plain");
      dummy_tbox.style.background = "transparent";
      dummy_tbox.style.maxWidth = "0px";
      elt.parentNode.parentNode.firstChild.
          replaceChild(dummy_tbox,
          elt.parentNode.parentNode.firstChild.firstChild.nextSibling);
    }

    /*
     * Finally, morph the label into a textbox (we need to do this
     * after having cloned the "sha1fingerprint" label)
     */
    elt.style.setProperty("-moz-binding",
        "url(chrome://global/content/bindings/textbox.xml#textbox)", null);
    elt.setAttribute("readonly", true);
    elt.setAttribute("clickSelectsAll", true);
  }


  /*
   * Add the e-mail addresses after the "Serial Number" line, if there are
   * any in the certificate (displays addresses from both from the subject
   * and the subjectAltName extension)
   */
    var temp = /*:cheat {value:Num}*/{value:0};
    var mailaddrs = /*:cheat Array<Ext>*/getCurrentCert().getEmailAddresses(temp);
  if (mailaddrs.length == 0)
    return;

  // build a multi-line string of unique addresses
  var email = /*:Ext*/'';
  var numlines = 0;
  for (var i = 0; i < mailaddrs.length; i++) {
    if (email.search(/*:cheat Ext*/(new RegExp("^" + mailaddrs[i], "m"))) == -1) {
      email += (email ? '\n' : '') + mailaddrs[i];
      numlines++;
    }
  }

  /*
   * retrieve the "Serial Number" row, which we use for both getting
   * the CSS properties and for inserting the new "Email address(es)" row
   */
  var snr = document.getElementById("serialnumber");
  var snrst = document.defaultView.getComputedStyle(snr, null);

  /*
   * create a new textbox (labels can't be multiline, and don't provide
   * scrollbars in case we have a large number of lines)
   */
  var ea_tbox = document.createElement("textbox");
  ea_tbox.setAttribute("id", "email");
  ea_tbox.setAttribute("readonly", true);
  ea_tbox.setAttribute("clickSelectsAll", true);
  ea_tbox.setAttribute("value", email);
  ea_tbox.setAttribute("class", "plain");
  ea_tbox.style.background = "transparent";
  /*
   * note that depending on the theme, there can be (one-)pixel positioning
   * differences, e.g. if the original "serialnumber" element uses the
   * "-moz-margin-start" CSS property - which we can't retrieve from
   * the "snrst" object, unfortunately
   */
  ea_tbox.style.setProperty("margin-left", snrst.marginLeft, "important");
  ea_tbox.style.setProperty("margin-top", snrst.marginTop, "important");
  if (numlines > 1) {
    ea_tbox.setAttribute("multiline", true);
    // setting rows to "2" has space for displaying three lines, actually
    ea_tbox.setAttribute("rows", numlines < 3 ? numlines - 1 : 2);
  }

  /*
   * create a new row by cloning the "Serial Number" row, adapt as needed,
   * and insert at the proper place, finally
   */
  var ea_row = snr.parentNode.cloneNode(true);
  var bundle = document.getElementById("certviewerplusbundle");
  ea_row.firstChild.setAttribute("value", numlines > 1 ?
                                 bundle.getString("CertEmailAddresses") :
                                 bundle.getString("CertEmailAddress"));
  ea_row.replaceChild(ea_tbox, ea_row.lastChild);
  snr.parentNode.parentNode.insertBefore(ea_row, snr.parentNode.nextSibling);
},

addTrustSettingCheckBoxes: /*: [Cvp] -> Undef*/ function()
{
  /*
   * Add checkboxes for displaying/editing the trust settings.
   * To save space, we insert the text labels outside of the tree element
   * (and keep "hideheader=true" for all additional tree columns we
   * insert). We can't do this with a simple overlay, as some of
   * the elements we'd like to reference lack an "id" attribute, and
   * in addition to that, we also need to surround the existing label
   * ("Certificate Hierarchy") with a new hbox element
   */

  var bundle = document.getElementById("certviewerplusbundle");
  var trustsettings = [ "SSL", "Mail", "Code" ];

  var tsd = /*:cheat nsIDOMXULTreeElement*/(document.getElementById("treesetDump"));

  /*
   * Make the tree editable (needed for the treecol checkbox elements)
   * With older releases (Fx/Tb < 3.0, Sm < 2.0), nsIX509Cert2/3 is
   * not available, so we can't retrieve the "certType" attribute.
   * Modifying trust settings without access to this information
   * looks like a rather big mess, so we just stick to read-only
   * mode for these releases.
   */
  if (Components.interfaces.nsIX509Cert2)
    tsd.setAttribute("editable", "true");

  /*
   * Slightly adjust the onselect handler: since we're disabling the
   * double click in this tree body (in certViewerPlusBindings.xul,
   * see below) with a somewhat dubious method, updateCertDump()
   * might be called with currentIndex < 0 (and then producing an
   * unwanted alert saying "No items are selected").
   */
  tsd.setAttribute("onselect", "if (this.view.selection.currentIndex < 0) this.view.selection.currentIndex = 0; updateCertDump();");

  /*
   * Insert an hbox element where we can insert the trust setting labels
   * afterwards. The current label element needs to be enclosed by
   * this hbox, otherwise they would appear vertically.
   */
  var hb = /*:cheat nsIDOMElement*/document.createElement("hbox");
  hb.setAttribute("id", "tsdlabels");
  var currLabel = /*:cheat nsIDOMElement*/tsd.parentNode.replaceChild(hb, tsd.previousSibling);
  // insert the current label into the hbox again
  hb.appendChild(currLabel);
  // allow stretching
  currLabel.setAttribute("flex", "1");
  // add the tooltip to this label, too
  currLabel.setAttribute("tooltip", "tctooltip");

  // certificate names must not be editable
  var dc = /*:cheat nsIDOMElement*/(document.getElementById("dumpCol"));
  dc.setAttribute("editable", "false");

  /*
   * Calculate the width of the trust configuration columns, based
   * on the computed length of the labels. We have to temporarily
   * add the label to the hbox, otherwise getComputedStyle won't
   * give us its size (in px)
   */
  var tcwidth = 0;
  for (var i = 0; i < trustsettings.length; i++) {
    var tlbl = /*:cheat nsIDOMElement*/document.createElement("label");
    tlbl.setAttribute("value", ""+(bundle.getString("trust" + trustsettings[i])));
    hb.appendChild(tlbl);
    var currwidth = parseInt(window.getComputedStyle(tlbl, "").width);
    if (currwidth > tcwidth)
      tcwidth = currwidth;
    hb.removeChild(hb.lastChild);
  }

  // add three pixels on each side of the label
  tcwidth += 6;

  /*
   * For each of the three trust flags, add a label and treecol
   * elements (the corresponding treecells will be inserted by
   * addTreeItemToTreeChild() afterwards)
   */
  for (var i = 0; i < trustsettings.length; i++) {
    var lbl = /*:cheat nsIDOMElement*/(document.createElement("label"));
    lbl.setAttribute("class", "plain");
    lbl.setAttribute("width", ""+tcwidth);
    lbl.setAttribute("value", ""+(bundle.getString("trust" + trustsettings[i])));
    lbl.setAttribute("tooltip", "tctooltip");
    hb.appendChild(lbl);

    var tc = /*:cheat nsIDOMElement*/(document.createElement("treecol"));
    tc.setAttribute("type", "checkbox");
    tc.setAttribute("editable", "true");
    tc.setAttribute("width", ""+tcwidth);
    tc.setAttribute("hideheader", "true");
    dc.parentNode.appendChild(tc);
  }

  /*
   * Spacer elements (description and treecol) for the case where the
   * "Certificate Hierarchy" tree has a scrollbar on the right-hand side.
   * In this case, we move our trust config columns to the left, so they
   * a) are not hidden by the scrollbar and b) don't flap when the
   * scrollbar is shown/not shown.
   * By default, this spacer elements are hidden. We unhide them in
   * addTreeItemToTreeChild() when we encounter a hierarchy with more
   * than 4 certificates/rows (which, for most platforms, means that
   * a scrollbar will appear at the right-hand side).
   * It would be better to not hardcode the width (to 18px), but it does
   * the trick for the default themes (winstripe, pinstripe, gnomestripe),
   * scrollbars don't seem to exceed this width.
   */
  var descspc = /*:cheat nsIDOMElement*/(document.createElement("description"));
  descspc.setAttribute("id", "descspc");
  descspc.setAttribute("class", "plain box-padded");
  descspc.setAttribute("width", "18");
  descspc.setAttribute("hidden", "true");
  hb.appendChild(descspc);

  var tcolspc = /*:cheat nsIDOMElement*/(document.createElement("treecol"));
  tcolspc.setAttribute("id", "tcolspc");
  tcolspc.setAttribute("width", "18");
  tcolspc.setAttribute("hideheader", "true");
  tcolspc.setAttribute("hidden", "true");
  dc.parentNode.appendChild(tcolspc);
},

setCertTrust: /*:[nsIDOMElement] nsIDOMEvent -> Undef*/function(evt)
{
  /* we're only interested in DOMAttrModified events where the
   * "value" attribute is changed (changing the "properties" attribute
   * of the treecell elements will also fire this event, but we don't
   * want to do anything in that case)
   */
  if ((/*:cheat nsIDOMMutationEvent*/evt).attrName != "value")
    return;

  var certdb = Components.classes[nsX509CertDB].getService(nsIX509CertDB);
  var cert = /*:cheat nsIX509Cert2*/(certdb.
             findCertByDBKey(this.parentNode.firstChild.getAttribute("display"),
                             null));
  if (!cert) // shouldn't happen, but fail silently if it does...
    return;

  // needed in order to retrieve .certType
  cert.QueryInterface(Components.interfaces.nsIX509Cert2);

  if ((cert.certType == nsIX509Cert.USER_CERT) ||
      (cert.certType == nsIX509Cert.UNKNOWN_CERT) ||
      (cert.certType == nsIX509Cert.SERVER_CERT &&
       this.getAttribute("ts") != "SSL") ||
      (cert.certType == nsIX509Cert.EMAIL_CERT &&
       this.getAttribute("ts") != "Mail")) {
    /*
     * for all these cases, calling nsIX509CertDB's setCertTrust method
     * has no effect, actually (it will either return without doing anything
     * or will not set the requested trust, as can be seen by inspecting
     * the implementation of nsNSSCertificateDB::SetCertTrust
     */
     if (this.getAttribute("value") == "true")
       this.setAttribute("properties", "disabled");
     else
       this.removeAttribute("properties");
  } else {
    /*
     * nsIX509CertDB's setCertTrust method does not allow to add/subtract
     * single trust bits, so we must do the "bitkeeping" ourselves.
     * The "trustconstants" object uses the "SSL"/"Mail"/"Code" identifiers
     * as properties, so we can retrieve the corresponding constant by
     * looking at the "ts" attribute from the treecell element we're
     * currently dealing with
     */
    var currTrust = ch.velox.cvp.getCertTrust(cert.dbKey);
    if (this.getAttribute("value") == "true") {
      certdb.setCertTrust(cert, cert.certType,
                          currTrust | ch.velox.cvp.trustconstants[this.getAttribute("ts")]);
      this.setAttribute("properties", "checked");
    } else {
      certdb.setCertTrust(cert, cert.certType,
                          currTrust & ~ch.velox.cvp.trustconstants[this.getAttribute("ts")]);
      this.removeAttribute("properties");
    }
  }

  /*
   * Visually represent trust flag inheritance: for every cert which
   * has a cert in its chain higher above where the corresponding bit
   * is set already, we show a checkbox in its disabled state (by
   * setting the "properties" attribute to "disabled").
   * To do so, we iterate over all rows, pick the appropriate column
   * (through the "ts" attribute) and will turn on the istrusted boolean
   * as soon as we encounter a CA cert where that bit is enabled.
   */
  var rows = /*:cheat Array<nsIDOMXULElement>*/(document.getElementById("treesetDump").getElementsByTagName("treerow"));

  if (rows.length > 1) {
    // we only have to do this if we have a hierarchy with more than one cert
    var istrusted = /*:Bool*/false;
    for (var i = 0; i < rows.length; i++) {
        var c = (/*:cheat nsIDOMXULElement*/(rows[i].getElementsByAttribute("ts", this.getAttribute("ts"))[0]));
      if (c.getAttribute("value") == "true")
        istrusted = true;
      else {
        if (istrusted)
          c.setAttribute("properties", "disabled");
        else
          c.removeAttribute("properties");
      }
      if (i == rows.length - 1)
        // remember the last cert - needed for requestUsagesArrayAsync
        cert = /*:cheat nsIX509Cert2*/(certdb.findCertByDBKey(c.parentNode.firstChild.
                                                              getAttribute("display"), null));
    }
  }

  /*
   * update the usages information on the General tab
   * (remove existing usage boxes first)
   */
    var ub = (/*:cheat nsIDOMDocument*/document).getElementById("verify_info_box").getElementsByTagName("textbox");
  for (var i = ub.length-1; i >= 0; i--)
    ub[i].parentNode.removeChild(ub[i]);

  if (false && cert instanceof nsIX509Cert3)
      /*:nsIX509Cert3*/cert.requestUsagesArrayAsync(getProxyOnUIThread(new listener(),
                                 Components.interfaces.nsICertVerificationListener));
},

getCertTrust: /*: [Cvp] Ext -> Ext*/function(dbKey)
{
  /*
   * return the current trust bitmask for a certificate identified by dbKey,
   * as defined by these constants (from nsIX509CertDB.idl):
   *
   *  const unsigned long UNTRUSTED       =      0;
   *  const unsigned long TRUSTED_SSL     = 1 << 0;
   *  const unsigned long TRUSTED_EMAIL   = 1 << 1;
   *  const unsigned long TRUSTED_OBJSIGN = 1 << 2;
   *
   */

  var trust = nsIX509CertDB.UNTRUSTED;

  var certdb = Components.classes[nsX509CertDB].getService(nsIX509CertDB);
  var cert = certdb.findCertByDBKey(""+dbKey,/*:cheat nsISupports*/null);
  if (!cert) // shouldn't happen, but fail silently if it does...
    return trust;

  /*
   * Sigh. nsIX509Cert2/3 is not included with Fx/Tb < 3.0 / Sm < 2.0,
   * and we can't retrieve the certType attribute therefore. As an
   * approximation for these (older) releases, use CA_CERT, but
   * also try with SERVER_CERT/EMAIL_CERT when checking against
   * the respective trust bit
   *
   */
  var certType = nsIX509Cert.CA_CERT;
  if (Components.interfaces.nsIX509Cert2) {
    // Great. No ugly workaround needed
    cert.QueryInterface(Components.interfaces.nsIX509Cert2);
    certType = (/*:cheat nsIX509Cert2*/cert).certType;
  }

  for (var c in ch.velox.cvp.trustconstants) {
    /*
     * isCertTrusted will fail with NS_ERROR_FAILURE if the
     * cert is in the temporary DB only... hmmpff.
     */
    try {
      if (certdb.isCertTrusted(cert, certType, parseNum(ch.velox.cvp.trustconstants[c])))
        trust |= parseNum(ch.velox.cvp.trustconstants[c]);
    } catch(e) {}

    /*
     * hack: for older releases (those lacking nsIX509Cert2/3)
     * also check with SERVER_CERT and EMAIL_CERT, when looking
     * for the matching trust bit
     */
    if (!Components.interfaces.nsIX509Cert2 && c == "SSL")
      try {
        if (certdb.isCertTrusted(cert, nsIX509Cert.SERVER_CERT,
                                 parseNum(ch.velox.cvp.trustconstants[c])))
          trust |= parseNum(ch.velox.cvp.trustconstants[c]);
      } catch(e) {}

    if (!Components.interfaces.nsIX509Cert2 && c == "Mail")
      try {
        if (certdb.isCertTrusted(cert, nsIX509Cert.EMAIL_CERT,
                                 parseNum(ch.velox.cvp.trustconstants[c])))
          trust |= parseNum(ch.velox.cvp.trustconstants[c]);
      } catch(e) {}

  }

  return trust;
}
}; // end of ch.velox.cvp wrapper

window.addEventListener("load", ch.velox.cvp.tuneGeneralTab, false);

/*
 * We're replacing the current implementation of addTreeItemToTreeChild(),
 * and fill in the checkboxes when we add a new cert
 */
/*: [Any] nsIDOMElement * Str * Str * Bool -> nsIDOMElement */
function addTreeItemToTreeChild(treeChild,label,value,addTwistie)
{
  // add labels and treecols, if required
  if(!document.getElementById("tsdlabels"))
    ch.velox.cvp.addTrustSettingCheckBoxes();

  var treeElem1 = (/*:cheat nsIDOMDocument*/document).createElement("treeitem");
  if (addTwistie) {
    treeElem1.setAttribute("container","true");
    treeElem1.setAttribute("open","true");
  }
  var treeRow = (/*:cheat nsIDOMDocument*/document).createElement("treerow");
  var treeCell = (/*:cheat nsIDOMDocument*/document).createElement("treecell");
  treeCell.setAttribute("label",label);
  treeCell.setAttribute("editable","false");
  if (value)
    treeCell.setAttribute("display",value);
  treeRow.appendChild(treeCell);

  /*
   * Trust flags are inherited from the issuer cert. To visually represent
   * this behavior, we add a checkbox in its disabled state (grayed out)
   * to any row appearing below one with a cert where the trust is
   * explicitly set.
   * We're not implementing a custom view for this tree, so we have to
   * resort to some generic DOM methods for retrieving cell contents
   * (for creating custom views, see 
   * https://developer.mozilla.org/en/XUL_Tutorial:Custom_Tree_Views,
   * but this would probably require a pretty extensive overhaul, including
   * changes in the backend).
   */
  var tsd = /*:cheat nsIDOMXULTreeElement*/(document.getElementById("treesetDump"));
  var trustsettings = /*:cheat Array<Str>*/[ "SSL", "Mail", "Code" ];
  for (var i = 0; i < trustsettings.length; i++) {
    var istrusted = (ch.velox.cvp.getCertTrust(value)
                     & ch.velox.cvp.trustconstants[trustsettings[i]]) ? 
                    "true" : "false";
    var tc = (/*:cheat nsIDOMDocument*/document).createElement("treecell");
    /*
     * the "ts" (trustsetting) attribute is used by setCertTrust()
     * to retrieve the proper column
     */
    tc.setAttribute("ts", trustsettings[i]);
    /*
     * the "value" attribute specifies the checkbox state (true/false):
     * https://developer.mozilla.org/en/Tree_Widget_Changes#Checkbox_columns
     */
    tc.setAttribute("value", istrusted);

    // if it's not explicitly trusted, check if it inherits something
    if (istrusted == "false") {
      var rowsabove = /*:cheat Array<nsIDOMElement>*/(tsd.getElementsByTagName("treerow"));
      /*
       * we haven't inserted the current row yet, so we can iterate
       * over all existing treerow childs
       */
      for (var j = 0; j < rowsabove.length; j++) {
        if (/*:cheat nsIDOMElement*/(rowsabove[j].childNodes[i+1]).getAttribute("value") == "true")
          tc.setAttribute("properties", "disabled");
      }
    }
    /*
     * events other than "DOMAttrModified" (such as "select",
     * "CheckboxStateChange" or similar) won't fire if the checkbox 
     * changes its state, unfortunately... so we have to listen to this
     * mutation event, despite its negative impact performance-wise - see
     * https://developer.mozilla.org/en/XUL:Events#Mutation_DOM_events
     */
    tc.addEventListener("DOMAttrModified", ch.velox.cvp.setCertTrust, false);
    treeRow.appendChild(tc);
  }

  treeElem1.appendChild(treeRow);
  treeChild.appendChild(treeElem1);
  /*
   * set the tooltip for <treechildren> - element-specific tooltips
   * within a tree do not seem to be supported currently, see also
   * http://mxr.mozilla.org/seamonkey/source/layout/xul/base/src/nsXULTooltipListener.cpp#456
   */
  treeChild.setAttribute("tooltip", "tctooltip");

  /*
   * Disable the default double click action in the upper tree dialog
   * (would collapse the respective entry and hide all certificates
   * below, which is not what we want when a user mistakenly doubleclicks
   * one of the trust check boxes).
   */
  treeChild.style.setProperty("-moz-binding",
      "url(chrome://certviewerplus/content/certViewerPlusBindings.xul#cvptreebody)",
      /*:cheat Str*/null);

  // enable the spacer columns if more than 4 rows are in the tree
  if (tsd.getElementsByTagName("treerow").length > 3) {
    document.getElementById("descspc").setAttribute("hidden", "false");
    document.getElementById("tcolspc").setAttribute("hidden", "false");
  }
  return treeElem1;
}

var srGetStrBundle = /*:cheat [Any] Str -> nsIStringBundle*/null; // Unknown function...

/*: nsIDOMWindow * nsIX509Cert -> Undef */
function exportToFile(parent, cert)
{
  var bundle = srGetStrBundle("chrome://certviewerplus/locale/certViewerPlus.properties");
  if (!cert)
    return;

  var nsIFilePicker = Components.interfaces.nsIFilePicker;
  var fp = Components.classes["@mozilla.org/filepicker;1"].
           createInstance(nsIFilePicker);
  fp.init(parent, bundle.GetStringFromName("SaveCertAs"),
          nsIFilePicker.modeSave);
  var filename = cert.commonName;
  if (!(/*:cheat String*/filename).length)
    filename = cert.windowTitle;
  // remove all whitespace from the default filename
  /*:cheat Undef*/(fp.defaultString = filename.replace(/\s*/g,''));
  /*:cheat Undef*/(fp.defaultExtension = "crt");
  fp.appendFilter(bundle.GetStringFromName("CertFormatBase64"), "*.crt; *.pem");
  fp.appendFilter(bundle.GetStringFromName("CertFormatBase64Chain"), "*.crt; *.pem");
  fp.appendFilter(bundle.GetStringFromName("CertFormatDER"), "*.der");
  fp.appendFilters(nsIFilePicker.filterAll);
  var res = fp.show();
  if (res != nsIFilePicker.returnOK && res != nsIFilePicker.returnReplace)
    return;

  var content = /*:Ext*/'';
  switch (fp.filterIndex) {
    case 1:
      content = getPEMString(cert);
      var chain = cert.getChain();
      for (var i = 1; i < chain.length; i++)
        content += getPEMString(chain.queryElementAt(i, Components.interfaces.nsIX509Cert));
      break;
    case 2:
      content = getDERString(cert);
      break;
    //case 0: // we don't correctly parse empty clauses
    default:
      content = getPEMString(cert);
      break;
  }

  var msg = /*:Str*/'';
  var written = 0;
  try {
    var file = Components.classes["@mozilla.org/file/local;1"].
               createInstance(Components.interfaces.nsILocalFile);
    file.initWithPath(fp.file.path);
    var fos = Components.classes["@mozilla.org/network/file-output-stream;1"].
              createInstance(Components.interfaces.nsIFileOutputStream);
    // flags: PR_WRONLY | PR_CREATE_FILE | PR_TRUNCATE
    fos.init(file, 0x02 | 0x08 | 0x20, 0x644, 0);
    written = fos.write(content, content.length);
    fos.close();
  }
  catch(e) {
    switch (e.result) {
      case Components.results.NS_ERROR_FILE_ACCESS_DENIED:
        msg = bundle.GetStringFromName("writeFileAccessDenied");
        break;
      case Components.results.NS_ERROR_FILE_IS_LOCKED:
        msg = bundle.GetStringFromName("writeFileIsLocked");
        break;
      // case Components.results.NS_ERROR_FILE_NO_DEVICE_SPACE: // We don't parse empty clauses properly
      case Components.results.NS_ERROR_FILE_DISK_FULL:
        msg = bundle.GetStringFromName("writeFileNoDeviceSpace");
        break;
      default:
        msg = e.message;
        break;
    }
  }
  if (written != content.length) {
    if (!msg.length)
      msg = bundle.GetStringFromName("writeFileUnknownError");
    alertPromptService(bundle.GetStringFromName("writeFileFailure"),
                       bundle.formatStringFromName("writeFileFailed",
                       [ fp.file.path, msg ], 2));
  }
}

function(event) { ch.velox.cvp.displayPEM(window, getCurrentCert()); };
function(event) { exportToFile(/*:cheat nsIDOMWindow*/window, getCurrentCert()); }; // XXXXXXX!!





    /* a hack for adding the "Display in PEM Format" button to the
       "Details" tab, for those versions where the export button
       is already present (the enclosing hbox element doesn't have an id,
       so we can't use a simple overlay with "insertbefore=" */

    window.addEventListener("load", function()
    {
      /* return early, if the element is already there */
      if (document.getElementById("display_pem_dt"))
        return;

      /* see if we have an export button */
      var exportButton = document.getElementById("export_cert");
      if (!exportButton)
        return;

      /* handcraft our new button */
      var button = document.createElement("button");
      button.setAttribute("id", "display_pem_dt");
      button.setAttribute("class", "normal");
      button.setAttribute("label", "View PEM");
      button.setAttribute("accesskey", "P");
      button.setAttribute("oncommand", "ch.velox.cvp.displayPEM(window, getCurrentCert());");

      /* and add it, finally */
      exportButton.parentNode.insertBefore(button, exportButton);
    }, false);

  
function(event) { toOpenWindowByType('mozilla:certmanager', 'chrome://pippki/content/certManager.xul'); };
function(event) { toOpenWindowByType('mozilla:certmanager', 'chrome://pippki/content/certManager.xul'); };
function(event) { toOpenWindowByType('mozilla:certmanager', 'chrome://pippki/content/certManager.xul'); };
function(event) { toOpenWindowByType('mozilla:certmanager', 'chrome://pippki/content/certManager.xul'); };
function(event) { toOpenWindowByType('mozilla:certmanager', 'chrome://pippki/content/certManager.xul'); };



/* UNKNOWN
function getPKCS7String(cert, chainMode)
{
  // var length = {};
  // var pkcs7Array = cert.getPKCS7(chainMode, length);
  // var pkcs7String = '';
  // for (var i = 0; i < pkcs7Array.length; i++) {
  //   pkcs7String += String.fromCharCode(pkcs7Array[i]);
  // }
  // return pkcs7String;
} */

 
function alertPromptService(title, message)
{
  var ps = Components.classes["@mozilla.org/embedcomp/prompt-service;1"].
           getService(Components.interfaces.nsIPromptService);
  ps.alert(/*:cheat nsIDOMWindow*/window, ""+title, ""+message);
}



function(event) { ch.velox.cvp.displayPEM(window, getCurrentCert()); };
function(event) { exportToFile(/*:cheat nsIDOMWindow*/window, getCurrentCert()); };
function(event) { toOpenWindowByType('mozilla:certmanager', 'chrome://pippki/content/certManager.xul'); };
function(event) { toOpenWindowByType('mozilla:certmanager', 'chrome://pippki/content/certManager.xul'); };
function(event) { toOpenWindowByType('mozilla:certmanager', 'chrome://pippki/content/certManager.xul'); };
function(event) { toOpenWindowByType('mozilla:certmanager', 'chrome://pippki/content/certManager.xul'); };
function(event) { toOpenWindowByType('mozilla:certmanager', 'chrome://pippki/content/certManager.xul'); };
function(event) { toOpenWindowByType('mozilla:certmanager', 'chrome://pippki/content/certManager.xul'); };
function(event) { toOpenWindowByType('mozilla:certmanager', 'chrome://pippki/content/certManager.xul'); };
function(event) { toOpenWindowByType('mozilla:certmanager', 'chrome://pippki/content/certManager.xul'); };
function(event) { toOpenWindowByType('mozilla:certmanager', 'chrome://pippki/content/certManager.xul'); };
function(event) { toOpenWindowByType('mozilla:certmanager', 'chrome://pippki/content/certManager.xul'); };
