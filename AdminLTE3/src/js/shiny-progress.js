import $ from 'jquery';

function registerProgressOutput(Shiny) {
  const progressOutputBinding = new Shiny.OutputBinding();
  const clsName = "shidashi-progress-output";
  progressOutputBinding.name = "shidashi.progressOutputBinding";

  $.extend(progressOutputBinding, {
    find: function(scope) {
      const $scope = $(scope);
      const re = [];

      $scope.each((i, el) => {
        const $el = $(el);
        if( $el.hasClass( clsName ) ) {
          re.push(el);
        } else {
          $el.find( `.${ clsName }` ).each( re.push );
        }
      })

      return $(re);
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

  // BindAll outputs since the outputs are registered after shiny connection
  Shiny.bindAll(".shidashi-progress-output");
}

export { registerProgressOutput }
