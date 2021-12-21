library(shiny)
library(shidashi)

server <- function(input, output, session, ...){

  reg <- ravedash::register_rave_session(session = session)

  module_server(input, output, session, reg, ...)


  local_reactives <- shiny::reactiveValues()

  shiny::observeEvent(
    event.quoted = FALSE,
    eventExpr = reg$rave_event$data_changed,
    handlerExpr = {

      print(1)
      # Check whether is there any missing data for this module
      tryCatch({

        shiny::withLogErrors({
          check_results <- isTRUE(on_data_loaded())
        })
      }, error = function(e){
        print(2)
        logger::log_warn("[Module { .module_id }] Found an error while checking data...")
        logger::log_error(paste(e$message, sep = "\n", collapse = "\n"))
        msg <- paste(utils::capture.output({
          if(length(e$call)){
            cat("Error in ", deparse1(e$call), ": ", sep = "")
          } else {
            cat("Error: ")
          }
          cat(e$message, "\nTraceback:\n")
          traceback(e)
        }), collapse = "\n")
        shidashi::show_notification(
          title = "Error found!", autohide = FALSE, close = TRUE, type = 'danger',
          message = shiny::div(
            "Found an error while trying to check this module. The debug message is displayed below.",
            shiny::hr(),
            shiny::pre(msg)
          )
        )
      })

      print(check_results)

      if(!isTRUE(check_results)){
        shidashi::clear_notifications()
        local_reactives$open_loader <- FALSE
        # return(module_ui_main())
      } else {
        local_reactives$open_loader <- Sys.time()
        # return(module_ui_loader())
      }

    }, ignoreInit = TRUE, ignoreNULL = TRUE
  )

  shiny::observeEvent(reg$rave_event$open_loader, {
    # Listen to a global event on whether data has changed
    if(length(reg$rave_event$open_loader) &&
       !isFALSE(reg$rave_event$open_loader)){
      local_reactives$open_loader <- Sys.time()
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  output[['_module_ui_']] <- shiny::renderUI({

    shiny::validate(
      shiny::need(
        length(local_reactives$open_loader),
        message = "Initializing..."
      )
    )
    if(isFALSE(local_reactives$open_loader)){
      return(module_ui_main())
    } else {
      return(module_ui_loader())
    }

  })

}
