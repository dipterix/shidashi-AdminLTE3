import $ from 'jquery';

function registerProgressOutput(Shiny) {
  const progressOutputBinding = new Shiny.OutputBinding();

  progressOutputBinding.name = "shidashi.progressOutputBinding";

  $.extend(progressOutputBinding, {
    find: function(scope) {
      return $(scope).find(".shidashi-progress-output");
    },
    renderValue: function(el, value) {
      let v = parseInt(value.value);
      if(isNaN(v)){ return; }
      if(v < 0){ v = 0; }
      if(v > 100){ v = 100; }
      $(el).find(".progress-bar").css("width", `${v}%`);
      if(typeof(value.description) === "string"){
        $(el)
          .find(".progress-description.progress-message")
          .text(value.description);
      }
    },
    renderError: function(el, err) {
      if(err.message === "argument is of length zero"){
        $(el).removeClass("shidashi-progress-error");
        $(el).find(".progress-bar").css("width", "0%");
      } else {
        $(el)
          .addClass("shidashi-progress-error")
          .find(".progress-description.progress-error")
          .text(err.message);
      }
    },
    clearError: function(el) {
      $(el).removeClass("shidashi-progress-error");
    }
  });
  Shiny.outputBindings.register(
    progressOutputBinding,
    "shidashi.progressOutputBinding");
}

export { registerProgressOutput }
