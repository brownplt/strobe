var ANIMATION_SLIDE_TIME = 200;
var DETAILS_IMAGE_WIDTH_MAX = 82;
var DETAILS_IMAGE_HEIGHT_MAX = 150;
var DETAILS_IMAGE_X = 260;
var DETAILS_IMAGE_Y = 11;
var DEFAULT_HTML_STYLE = "<html>" +
  // Set the style (fonts, sizes, etc)
  "<style type=\"text/css\">" +
  "<!--" +
  "body,td,th {" +
  "	 font-family: Geneva, Arial, Helvetica, sans-serif;" +
  "	 font-size: 12px;" +
  "	 color: #333333;" +
  "}" +
  "body {" +
  "	 margin-left: 0px;" +
  "	 margin-top: 0px;" +
  "	 margin-right: 0px;" +
  "	 margin-bottom: 0px;" +

  // Remove scrollbar from side
  "  overflow: auto;" +
  "}" +
  "-->" +
  "</style>" +

  // Disable right click
  "<script language=\"Javascript\">" +
  "  document.oncontextmenu=new Function(\"return false\")" +
  "</script>";
var DETAILS_HTML = DEFAULT_HTML_STYLE +
  "<style type=\"text/css\">" +
  "<!--" +
  ".articleimg {" +
  "  max-width: 82px;" +
  "  width:expression(this.width > 82 ? \"82px\" : this.width);" +
  "}" +
  "-->" +
  "</style>" +
  "<body>" +
  "<table border=0>" +
  "<tr><td>[ARTICLETEXT]</td><td valign=top>" +
  "[IMAGESRC]" +
  "</td></tr></table>" +
  "</body>" +
  "</html>";
var IMAGE_SRC = "<img src=\"[IMAGEURL]\" class=\"articleimg\">";

// This class controls the article details panel. This class controls showing
// and hiding of the panel with animations and also displays the text inside.
function DetailsController(mainDivElement_, textElement_, imageElement_,
    popoutElement_, detailsLoadedCallback_, showArticleCallback_) {
  var curArticleText_ = null;
  var curImageURL_ = null;
  var curKeywords_ = null;
  var imageRequest_ = null;
  var animation_ = null;
  var inSidebar_ = (mainDivElement_ == null);
  var ieDetailsView_ = null;

  if (!inSidebar_)
    popoutElement_.onclick = onPopoutClick;

  // Show the panel (details of the article)
  function show(articleText, imageURL, keywords) {
    curArticleText_ = articleText;
    curImageURL_ = imageURL;
    curKeywords_ = keywords;

    if (!inSidebar_) {
      textElement_.innerText = curArticleText_;

      // If there is an image, start downloading it
      if (imageURL != null) {
        imageRequest_ = new SimpleHTTPRequest();
        imageRequest_.request(curImageURL_, onImageReceived, true);
      } else {
        // Since no images need to be downloaded, finish.
        if (detailsLoadedCallback_ != null)
          detailsLoadedCallback_();
      }

      // Show details opening animation
      mainDivElement_.y = -mainDivElement_.height;
      mainDivElement_.visible = true;

      if (animation_ != null)
        animation_.stop();
      animation_ = new SimpleAnimation(onAnimateSlide, -mainDivElement_.height, 0,
        ANIMATION_SLIDE_TIME, onOpenAnimationCompleted);
      animation_.start();
    } else {
      // Open a details view panel instead of our own panel.
      var html = DETAILS_HTML;
      html = html.replace("[ARTICLETEXT]", curArticleText_);

      // Only show an image if one exists
      if ((curImageURL_ != null) && (curImageURL_ != "")) {
        html = html.replace("[IMAGESRC]", IMAGE_SRC);
      } else {
        html = html.replace("[IMAGESRC]", "");
      }
      html = html.replace("[IMAGEURL]", curImageURL_);

      ieDetailsView_ = new DetailsView();
      ieDetailsView_.html_content = true;
      ieDetailsView_.setContent("", undefined, html, false, 0);
      pluginHelper.showDetailsView(ieDetailsView_, VIEW_ON_WIKIPEDIA,
        gddDetailsViewFlagToolbarOpen, onDetailsViewFeedback);

      // End the loading now since IE will take care of the rest
      if (detailsLoadedCallback_ != null)
        detailsLoadedCallback_();
    }
  }

  // Called when the user clicks on the wikipedia details view
  function onDetailsViewFeedback(detailsViewFlags) {
    if (detailsViewFlags == gddDetailsViewFlagToolbarOpen)
      showArticleCallback_();
  }

  // Remove the details panel
  function hide() {
    curArticleText_ = null;
    curImageURL_ = null;

    if (!inSidebar_) {
      // Stop the animation
      if (animation_ != null)
        animation_.stop();
      animation_ = new SimpleAnimation(onAnimateSlide, 0, -mainDivElement_.height,
        ANIMATION_SLIDE_TIME, onCloseAnimationCompleted);
      animation_.start();

      // Check if an image download is happening, if so, cancel it.
      if (imageRequest_ != null) {
        imageRequest_.stop();
        imageRequest_ = null;
      }
    } else {
    }
  }

  // Called when the next animation frame is to be drawn
  function onAnimateSlide(value) {
    mainDivElement_.y = value;
  }

  // Resizes the image element to use the correct aspect ratio when displaying
  // the image
  function resizeImageElement() {
    // Calculate sizes such that the image can occupy as much space as possible
    // and also keep the same aspect ratio
    var heightRatio = imageElement_.srcHeight / DETAILS_IMAGE_HEIGHT_MAX;
    var widthRatio = imageElement_.srcWidth / DETAILS_IMAGE_WIDTH_MAX;

    var resizeRatio = 1;
    if (heightRatio > widthRatio) {
      // Since the height ratio is larger than the width ratio, this means that
      // the height of the image will be its maximum and the width will be resized
      // according to this ratio.
      resizeRatio = 1 / heightRatio;
    } else {
      // The width ratio is larger than the height ratio (or they're the same)
      resizeRatio = 1 / widthRatio;
    }

    // Calculate the new size such that at least one border (width or height) is maxed
    var newWidth = imageElement_.srcWidth * resizeRatio;
    var newHeight = imageElement_.srcHeight * resizeRatio;

    // Calculate the new positions such that the image is aligned to top right
    var newX = DETAILS_IMAGE_X + (DETAILS_IMAGE_WIDTH_MAX - newWidth);
    var newY = DETAILS_IMAGE_Y;

    imageElement_.x = newX;
    imageElement_.y = newY;

    // If the image is smaller than the box size, keep the image at the same size
    if ((imageElement_.srcWidth < DETAILS_IMAGE_WIDTH_MAX) &&
        (imageElement_.srcHeight < DETAILS_IMAGE_HEIGHT_MAX)) {
      imageElement_.width = imageElement_.srcWidth;
      imageElement_.height = imageElement_.srcHeight;
    } else {
      imageElement_.width = newWidth;
      imageElement_.height = newHeight;
    }
  }

  // Called when the animation of the details opening is completed
  function onOpenAnimationCompleted() {
    animation_ = null;
  }

  // Called when the animation of the details closing is completed
  function onCloseAnimationCompleted() {
    animation_ = null;
    imageElement_.src = "";
    textElement_.innerText = "";
    mainDivElement_.visible = false;
  }

  // Called when the current article's image finishes downloading (or errors)
  function onImageReceived(data) {
    imageRequest_ = null;

    if (detailsLoadedCallback_ != null)
      detailsLoadedCallback_();

    if (data == null)
      return;

    // Update the UI
    imageElement_.visible = false;
    imageElement_.src = data;
    resizeImageElement();
    imageElement_.visible = true;
  }

  // Called when the popout button is clicked on
  function onPopoutClick() {
    showArticleCallback_();
  }

  // Called when the user click the title of the details view popup
  function onDetailsViewTitleClick() {
    showArticleCallback_();
  }

  this.show = show;
  this.hide = hide;
}
