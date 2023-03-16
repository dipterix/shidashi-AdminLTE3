import $ from 'jquery';
import ClipboardJS from 'clipboard';

function registerClipboardOutput(Shiny, shidashi) {
  const clipboardOutputBinding = new Shiny.OutputBinding();
  clipboardOutputBinding.name = "shidashi.clipboardOutputBinding";

  $.extend(clipboardOutputBinding, {
    find: function(scope) {
      return $(scope).find(".shidashi-clipboard-output");
    },
    renderValue: function(el, value) {
      let el_ = $(el);
      if(!el_.hasClass("clipboard-btn")){
        el_ = $(el).find(".clipboard-btn");
      }
      $(el_).attr("data-clipboard-text", value);
    },
    renderError: function(el, err) {
      let el_ = $(el);
      if(!el_.hasClass("clipboard-btn")){
        el_ = $(el).find(".clipboard-btn");
      }
      $(el_).attr("data-clipboard-text", "Error: " + err.message);
    }
  });

  Shiny.outputBindings.register(clipboardOutputBinding, "shidashi.clipboardOutputBinding");

  // No need to re-register because they use delegation
  new ClipboardJS(".clipboard-btn").on('success', function(e) {
    shidashi.createNotification({
      title : "Copied to clipboard",
      delay: 1000,
      autohide: true,
      icon: "fa fas fa-copy",
      "class" : "bg-success"
    });
    e.clearSelection();
  });
}

export { registerClipboardOutput }
