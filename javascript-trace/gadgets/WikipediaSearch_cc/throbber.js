// Creates a throbber with elements. A throbber is a 'spinning thing' that
// normally symbolizes waiting. Firefox has one in the top right animated when
// loading a webpage.
function Throbber(ownerDiv, imagePrefix, imageSuffix, totalFrames, frameDelay) {
  var imagePrefix_ = imagePrefix;
  var imageSuffix_ = imageSuffix;
  var totalFrames_ = totalFrames;
  var frameDelay_ = frameDelay;

  var element_ = ownerDiv.appendElement("<img visible=\"false\" />");
  var curFrame_ = 0;
  var timer_ = null;

  // Returns a frame's filename
  function getImageFilename(frame) {
    return imagePrefix_ + frame + imageSuffix_;
  }

  // Returns the next frame number in the animation
  function getNextFrame() {
    curFrame_++;
    if (curFrame_ == totalFrames_ + 1)
      curFrame_ = 1;

    return curFrame_;
  }
  
  // Show the throbber and animate it
  function show() {
    curFrame_ = 0;
    element_.visible = true;
    
    // Start the animation if it is not already running
    if (timer_ == null) {
      timer_ = setInterval(onAnimate, frameDelay_);
      onAnimate();
    }
  }

  // Hide the throbber
  function hide() {
    element_.visible = false;

    // Stop the animation only if it is running
    if (timer_ != null) {
      clearInterval(timer_);
      timer_ = null;
    }
  }

  // Called whenever a frame needs to be drawn
  function onAnimate() {
    element_.src = getImageFilename(getNextFrame());
  }

  this.show = show;
  this.hide = hide;
}