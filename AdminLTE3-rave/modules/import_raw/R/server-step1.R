ui_step1 <- function(){
  all_projects <- c(get_projects(), "[New Project]")
  shiny::column(
    width = 3L,
    shidashi::card(
      title = "Select project & subject",
      # class = "bg-light",
      tools = list(
        shidashi::as_badge("STEP 1")
      ),
      shiny::selectInput(
        inputId = ns("loader_project_name"),
        label = "Project",
        choices = all_projects,
        selected = local_data$get("loader_project_name", pipeline_get('project_name', all_projects[[1]]))
      ),
      shiny::textInput(
        inputId = ns("loader_subject_code"),
        label = "Subject code",
        placeholder = "Letters, digits, dash (-), and/or underscore (_)",
        value = local_data$get("loader_subject_code", pipeline_get('subject_code', ""))
      ),
      shiny::selectInput(
        inputId = ns("loader_data_format"),
        label = "Data source/format",
        choices = names(raveio::IMPORT_FORMATS),
        selected = local_data$get("loader_data_format", names(raveio::IMPORT_FORMATS)[pipeline_get('file_format', 1)])
      ),
      shiny::uiOutput(ns("loader_data_format_explain")),
      footer = shiny::div(
        class = "float-right",
        shiny::uiOutput(ns("loader_step1_message"))
      )
    )
  )
}

server_step1 <- function(input, output, session, server_env, ...){

  with(server_env, {
    # ------------ Validator ------------
    ## Step 1

    # Modal to add new project
    validator_step1_modal <- shinyvalidate::InputValidator$new(session = session)
    validator_step1_modal$condition(function(){
      isTRUE(input$loader_project_name == "[New Project]")
    })
    validator_step1_modal$add_rule(inputId = "loader_new_project_name", function(value){
      if(!length(value)){ return("Project name cannot be blank") }
      value <- trimws(value)
      if(!nchar(value)){ return("Project name cannot be blank") }

      all_projects <- tolower(get_projects())
      if( tolower(value) %in% all_projects ){
        return("Project has already existed (case-insensitive)")
      }

      result <- grepl("^[a-zA-Z0-9][a-zA-Z0-9_-]{0,}$", value)
      if(!result){
        return("The subject code is invalid: can only contain letters, digits, dash (-), or underscore (_). The first letter should only contain letters and digits.")
      }
    })

    # input validator
    validator_step1_inputs <- shinyvalidate::InputValidator$new(session = session)

    validator_step1_inputs$add_rule(inputId = "loader_project_name", shinyvalidate::sv_required())
    validator_step1_inputs$add_rule(inputId = "loader_project_name", shinyvalidate::sv_regex("^[a-zA-Z0-9][a-zA-Z0-9_-]{0,}$", "The project name is invalid: can only contain letters, digits, dash (-), or underscore (_). The first letter should only contain letters and digits."))

    validator_step1_inputs$add_rule(inputId = "loader_subject_code", shinyvalidate::sv_optional())
    validator_step1_inputs$add_rule(inputId = "loader_subject_code", shinyvalidate::sv_regex("^[a-zA-Z0-9][a-zA-Z0-9_-]{0,}$", "The subject code is invalid: can only contain letters, digits, dash (-), or underscore (_). The first letter should only contain letters and digits."))

    validator_step1_inputs$add_validator(validator_step1_modal)
    validator_step1_inputs$enable()

    validator_step1 <- shiny::reactive({
      if(isFALSE(input$loader_subject_code != "")){ return(list(
        pass = FALSE,
        message = "Subject code is blank"
      )) }
      if(!isTRUE(validator_step1_inputs$is_valid())){ return(list(
        pass = FALSE,
        message = "Invalid input(s), see message(s) above"
      )) }

      project_name <- input$loader_project_name
      subject_code <- input$loader_subject_code

      pipeline_set(
        project_name = project_name,
        subject_code = subject_code,
        file_format = c(which(names(raveio::IMPORT_FORMATS) %in% input$loader_data_format), 1)[[1]]
      )

      tryCatch({
        raveio::pipeline_run(pipe_dir = pipeline_path, type = "basic",
                             names = "preprocess_settings", reporter = "silent")
        preproc <- raveio::pipeline_read(var_names = "preprocess_settings", pipe_dir = pipeline_path)
        list(
          pass = TRUE,
          message = raveio::glue("Subject directory (raw) is found at {preproc$raw_path}")
        )
      }, error = function(e){
        list(
          pass = FALSE,
          message = e$message
        )
      })

    }) |>
      shiny::bindEvent(
        input$loader_project_name,
        input$loader_subject_code,
        input$loader_data_format
      )

    # ------------ Handlers ------------
    handler_loader_project_name <- shiny::observe({
      project_name <- input$loader_project_name
      if(isTRUE(project_name == "[New Project]")){

        nprojects <- length(get_projects())

        shiny::showModal(
          session = session,
          shiny::modalDialog(
            title = "Create new project",
            easyClose = FALSE,
            shiny::div(
              class = 'fill-width',
              shiny::textInput(
                inputId = ns("loader_new_project_name"),
                label = "Enter a valid project name:",
                placeholder = "Letters, digits, `-`, and `_`"
              )
            ),
            footer = tagList(
              shiny::uiOutput(ns("loader_new_project_confirm_ui")),
              if(nprojects > 0){
                shiny::actionButton(ns("loader_new_project_dismiss_btn"), label = "Dismiss")
              } else { NULL }
            )
          )
        )
        validator_step1_modal$enable()
      } else if(length(project_name)){
        local_data$set("loader_project_name", project_name)
      }
    }) |>
      shiny::bindEvent(input$loader_project_name, ignoreNULL = TRUE)
    handler_loader_new_project_dismiss <- shiny::observe({
      validator_step1_modal$disable()
      shiny::removeModal(session = session)
      shiny::updateSelectInput(session = session, inputId = "loader_project_name", selected = local_data$get("loader_project_name", pipeline_get('project_name', all_projects[[1]])))
    }) |>
      shiny::bindEvent(input$loader_new_project_dismiss_btn, ignoreNULL = TRUE, ignoreInit = TRUE)
    handler_loader_new_project_confirm <- shiny::observe({
      tryCatch({
        is_valid <- validator_step1_modal$is_valid()
        if(isTRUE(is_valid)){
          project_name <- trimws(input$loader_new_project_name)
          project <- raveio::RAVEProject$new(project_name = project_name, strict = FALSE)
          raveio::dir_create2(project$path)
          shiny::updateSelectInput(
            session = session,
            inputId = 'loader_project_name',
            choices = c(get_projects(refresh = TRUE), "[New Project]"),
            selected = project_name
          )
          shidashi::show_notification(message = "A new RAVE project folder has been created!", type = "success", title = "Success!", subtitle = "New Project")
          shiny::removeModal(session = session)
          validator_step1_modal$disable()
        } else {
          shidashi::show_notification(message = is_valid, type = "danger")
        }
      }, error = function(e){
        shidashi::show_notification(message = e$message, type = "danger")
        return()
      })
    }) |>
      shiny::bindEvent(input$loader_new_project_confirm_btn, ignoreNULL = TRUE, ignoreInit = TRUE)
    handler_new_subject <- shiny::observe({
      check <- validator_step1()
      if(!check$pass){
        shidashi::show_notification(check$message, subtitle = "Validation error", type = "danger")
        return()
      }
      project_name <- input$loader_project_name
      subject_code <- input$loader_subject_code

      pipeline_set(
        project_name = project_name,
        subject_code = subject_code,
        file_format = c(which(names(raveio::IMPORT_FORMATS) %in% input$loader_data_format), 1)[[1]]
      )

      tryCatch({
        dirs <- raveio::rave_directories(subject_code = subject_code, project_name = project_name)
        exists_before <- dir.exists(dirs$rave_path)
        raveio::pipeline_run(pipe_dir = pipeline_path, type = "basic",
                             names = "ensure_subject")
        subject_exists <- raveio::pipeline_read(var_names = "ensure_subject", pipe_dir = pipeline_path)
        if(!subject_exists){
          stop("Unable to create subject folders.")
        }
        if(!exists_before){
          shidashi::show_notification(
            raveio::glue("A new subject has been created at: {dirs$rave_path}"),
            title = "Congratulations!",
            subtitle = "Success!",
            close = TRUE,
            autohide = TRUE,
            type = "success"
          )
        } else {
          shidashi::show_notification(
            raveio::glue("The subject folder at {dirs$rave_path} has been updated."),
            title = "Format update",
            subtitle = "Message",
            close = TRUE,
            autohide = TRUE,
            type = "info"
          )
        }
      }, error = function(e){
        shidashi::show_notification(e$message, title = "Failed to initialize subject", subtitle = "Error!", close = TRUE, autohide = FALSE, type = "danger")
      })
      local_reactives$refresh_subject <- Sys.time()
    }) |>
      shiny::bindEvent(input$loader_step1_checkpoint, ignoreNULL = TRUE, ignoreInit = TRUE)

    # ------------ Renderers ------------
    output$loader_new_project_confirm_ui <- shiny::renderUI({

      shiny::validate(
        shiny::need(
          isTRUE(validator_step1_modal$is_valid()),
          message = "Please check the message above"
        )
      )

      dipsaus::actionButtonStyled(inputId = ns("loader_new_project_confirm_btn"), label = "Confirm", icon = shidashi::as_icon("check"), type = "primary")

    })
    output$loader_step1_message <- shiny::renderUI({

      fast_validate <- validator_step1()

      shiny::validate(
        shiny::need(
          isTRUE(fast_validate$pass),
          message = fast_validate$message
        )
      )

      preproc <- raveio::pipeline_read(var_names = "preprocess_settings", pipe_dir = pipeline_path)

      shiny::validate(
        shiny::need(
          !file.exists(preproc$path) || !any(preproc$data_imported),
          message = "Subject has already been created"
        )
      )

      shiny::tagList(
        shiny::p(style = "color:green;", fast_validate$message),
        dipsaus::actionButtonStyled(ns("loader_step1_checkpoint"), label = "Create/Update subject", class = "float-right")
      )


    }) |>
      shiny::bindEvent(validator_step1())
    output$loader_data_format_explain <- shiny::renderUI({
      fmt <- input$loader_data_format
      file_formats <- names(raveio::IMPORT_FORMATS)

      if(fmt == file_formats[[1]]){
        shiny::div(
          shiny::p("In each block folder, one Matlab/HDF5 file stands for one electrode. ",
                   "File name should match with format XXX1.h5 or xxx2.mat. ",
                   "Each file only contains a one-dimensional vector. ",
                   "The vector lengths stand for total time points and they must be the same across all electrode files. ",
                   "For example:"),
          shiny::tags$pre(
            dipsaus::print_directory_tree(
              c('block1', 'block2'),
              root = '<subject folder>',
              child = c(
                'datafile_e1.mat <vector of time>',
                'datafile_e2.mat <same length>',
                'datafile_e3.mat',
                '...'
              ),
              collapse = '\n'
            )
          )
        )
      } else if(fmt == file_formats[[2]]){
        shiny::div(
          shiny::p("A single Matlab/HDF5 file containing all electrode information. ",
                   "Data must be a matrix. One of the dimension must be electrodes, ",
                   "the other dimension must be time points. ",
                   "ALL blocks must share the same file & data name; for example:"),
          shiny::tags$pre(
            dipsaus::print_directory_tree(
              c('block1', 'block2'),
              root = '<subject folder>',
              child = c(
                'datafile.mat <one big matrix>'
              ),
              collapse = '\n'
            )
          ))
      }else{
        shiny::div(
          shiny::p("In each block folder, one EDF(+) file containing all electrode data; for example:"),
          shiny::tags$pre(
            dipsaus::print_directory_tree(
              c('block1', 'block2'),
              root = '<subject folder>',
              child = c(
                'datafile.edf (or .vhdr) <ONLY one EDF/BrainVis file per block>'
              ),
              collapse = '\n'
            )
          ))
      }
    })
  })

}
