library(dipsaus)

# options("shinyvalidate.verbose" = TRUE)

module_ui_loader <- function(){

  shiny::tagList(
    ui_step1(),
    shiny::column(
      width = 3L,
      shidashi::card(
        title = "Configure subject",
        # class = "bg-light",
        tools = list(
          shidashi::as_badge("STEP 2")
        ),
        shiny::uiOutput(ns("loader_step2"), class = "fill-width"),
        footer = shiny::div(
          class = "float-right",
          shiny::uiOutput(ns("loader_step2_message"))
        )
      )
    )
  )


}

module_ui_main <- function(){
  column(
    width = 12L,
    h2("Render your first project", class = "shidashi-anchor"),
    p(
      span(
        class = "inline-all",
        "Once you start a new shidashi project, a ",
        tags$code("start.R"), " file will be created with three lines:"
      )
    ),
    fluidRow(
      column(
        6L,
        tags$pre(
          class='no-padding bg-gray-90 pre-compact',
          tags$code(
            class="r",
            "library(shidashi)

# Render this project
shidashi::template_settings$set(root_path = '<your project folder>')

# Render project
shidashi::render(host = '127.0.0.1', port = 8310L)"
          )
        )
      )
    ),

    p(
      span(
        class = "inline-all",
        "The first line loads the shidashi package into your R session. ",
        "The second line sets the ",
        tags$code("root_path"), ", which should be your project path, ",
        "so that shidashi knows where to look at when rendering websites. ",
        "The last line starts a local R-shiny application from port ",
        tags$code("8310"), "."
      )
    ),


  )
}

module_server <- function(input, output, session, tools, ...){

  server_step1(input, output, session, environment(), ...)

  # ------------ Validator ------------
  validator_step2_inputs <- shinyvalidate::InputValidator$new(session = session)
  validator_step2_inputs$add_rule(inputId = "loader_blocks", rule = function(value){
    if(!length(value)){ return("At least one session/block is required.") }
    return(NULL)
  })
  validator_step2_inputs$add_rule(inputId = "loader_electrodes", rule = function(value){
    value <- dipsaus::parse_svec(value)
    if(!length(value)){ return("Please enter at least one valid electrode") }
    return(NULL)
  })
  validator_step2_inputs$add_rule(inputId = "loader_sample_rate", rule = function(value){
    if(!length(value) || is.na(value) || value <= 0){
      return("Positive numerical sample rate is required.")
    }
    return(NULL)
  })


  shiny::observe({
    shiny::removeModal()
    tryCatch({
      raveio::pipeline_run(pipe_dir = pipeline_path, type = "basic",
                           names = "generate_electrode_file")
      dipsaus::shiny_alert2(text = "Imported!", title = "Success", icon = "success", auto_close = TRUE)
      local_reactives$refresh_subject <- Sys.time()
    }, error = function(e){
      dipsaus::shiny_alert2(text = paste("Error while importing the data. Details: ", c(e$message), collapse = "\n"), title = "Import error", icon = "error", danger_mode = TRUE, auto_close = TRUE)
    })
  }) |>
    shiny::bindEvent(input$loader_import_subject, ignoreInit = TRUE, ignoreNULL = TRUE)


  output$loader_step2 <- shiny::renderUI({

    fast_validate <- validator_step1()
    validator_step2_inputs$disable()

    shiny::validate(
      shiny::need(
        isTRUE(fast_validate$pass),
        message = "Please finish and correct the previous step."
      )
    )

    # load preprocess settings instance
    project_name <- input$loader_project_name
    subject_code <- input$loader_subject_code
    subject_id <- sprintf("%s/%s", project_name, subject_code)
    preproc <- raveio::RAVEPreprocessSettings$new(subject = subject_id)

    shiny::validate(
      shiny::need(
        isTRUE(dir.exists(preproc$subject$rave_path)),
        message = "Please create subject first by simply press the `Create/Update subject` button."
      )
    )

    # get blocks, channel, sample rate

    blocks <- preproc$blocks
    if(!length(blocks)){ blocks <- preproc$all_blocks }

    electrodes <- dipsaus::deparse_svec(preproc$electrodes)

    physical_unit <- pipeline_get('physical_unit')
    if(!isTRUE(physical_unit %in% c("uV", "mV", "V"))){
      physical_unit <- "as-is (no change)"
    }

    validator_step2_inputs$enable()

    elec_imported <- preproc$electrodes[preproc$data_imported]
    if(length(elec_imported)){
      elec_label <- sprintf("Electrodes (already imported: %s)", dipsaus::deparse_svec(elec_imported))
    } else {
      elec_label <- "Electrodes"
    }

    shiny::tagList(

      shiny::selectInput(ns("loader_blocks"), label = "Sessions/Blocks", choices = preproc$all_blocks, selected = blocks, multiple = TRUE),

      shiny::textInput(ns("loader_electrodes"), label = elec_label, value = electrodes, placeholder = "E.g. 1-84,86,90-100"),

      shiny::numericInput(ns("loader_sample_rate"), label = "Sample rate (Hz)", min = 0, value = preproc$`@lfp_ecog_sample_rate`),

      shiny::selectInput(ns("loader_physical_unit"), label = "Physical unit", choices = c("as-is (no change)", "uV", "mV", "V"), selected = physical_unit),

    )

  }) |>
    shiny::bindEvent(
      validator_step1(),
      local_reactives$refresh_subject
    )

  output$loader_step2_message <- shiny::renderUI({
    shiny::validate(
      shiny::need(
        validator_step2_inputs$is_valid(),
        message = "Please fill/fix the settings above."
      )
    )

    dipsaus::actionButtonStyled(ns("loader_step2_validate_btn"), "Set, validate, and import")

  })


  set_subject <- function(){
    project_name <- input$loader_project_name
    subject_code <- input$loader_subject_code

    electrodes <- dipsaus::parse_svec(input$loader_electrodes)
    physical_unit <- input$loader_physical_unit
    if(!isTRUE(physical_unit %in% c("uV", "mV", "V"))){
      physical_unit <- "as-is"
    }

    pipeline_set(
      project_name = project_name,
      subject_code = subject_code,
      file_format = c(which(names(raveio::IMPORT_FORMATS) %in% input$loader_data_format), 1)[[1]],
      blocks = input$loader_blocks,
      channels = lapply(electrodes, function(e){
        list(
          number = as.integer(e),
          sample_rate = input$loader_sample_rate,
          type = "LFP"
        )
      }),
      physical_unit = physical_unit
    )
  }
  shiny::observe({

    project_name <- input$loader_project_name
    subject_code <- input$loader_subject_code

    electrodes <- dipsaus::parse_svec(input$loader_electrodes)
    physical_unit <- input$loader_physical_unit
    if(!isTRUE(physical_unit %in% c("uV", "mV", "V"))){
      physical_unit <- "as-is"
    }

    pipeline_set(
      project_name = project_name,
      subject_code = subject_code,
      file_format = c(which(names(raveio::IMPORT_FORMATS) %in% input$loader_data_format), 1)[[1]],
      blocks = input$loader_blocks,
      channels = lapply(electrodes, function(e){
        list(
          number = as.integer(e),
          sample_rate = input$loader_sample_rate,
          type = "LFP"
        )
      }),
      physical_unit = physical_unit
    )

    dipsaus::shiny_alert2(text = "Please wait...", title = "Validating...", icon = "info", auto_close = FALSE, buttons = FALSE)
    res <- tryCatch({
      # unlink(file.path(pipeline_path, "shared"), recursive = TRUE)
      raveio::pipeline_run(pipe_dir = pipeline_path, type = "basic",
                           names = "validate_preimport_results")
      TRUE
    }, error = function(e){
      dipsaus::shiny_alert2(text = e$message, title = "Validation error", icon = "error", danger_mode = TRUE, auto_close = TRUE)
      FALSE
    })

    if(!res){ return() }

    dipsaus::close_alert2()

    subject_id <- sprintf("%s/%s", project_name, subject_code)
    preproc <- raveio::RAVEPreprocessSettings$new(subject = subject_id)

    etypes <- preproc$electrode_types
    etypes_unique <- unique(etypes)
    electrodes <- preproc$electrodes
    srates <- preproc$sample_rates
    electrodes_info <- unlist(lapply(etypes_unique, function(type){
      sel <- etypes == type
      if(any(sel)){
        raveio::glue("{type} electrodes: {dipsaus::deparse_svec(electrodes[sel])} (sample rate: {srates[sel][[1]]} Hz)")
      }
    }))
    physical_unit <- input$loader_physical_unit
    already_loaded <- dipsaus::deparse_svec(electrodes[preproc$data_imported])
    blocks <- paste(input$loader_blocks, collapse = ", ")

    shiny::showModal(
      shiny::modalDialog(
        title = "Ready?",
        easyClose = FALSE,
        shiny::tagList(
          shiny::p("The raw data has passed validation."),
          shiny::tags$ul(
            shiny::tags$li(raveio::glue("Project name: {project_name}")),
            shiny::tags$li(raveio::glue("Subject code: {subject_code}")),
            shiny::tags$li(raveio::glue("Session/Blocks: {blocks}")),
            shiny::tags$ul(
              shiny::tagList(lapply(electrodes_info, shiny::tags$li))
            ),
            shiny::tags$li(raveio::glue("Electrodes already imported (*): {already_loaded}"))
          ),
          shiny::p("Ready to import? (**)",
                   shiny::br(),
                   shiny::tags$small("(*) These electrodes has been imported before and their data will not be altered nor removed."),
                   shiny::br(),
                   shiny::tags$small("(**) Once imported, you will not be able to change the block number, sample rate, nor to delete/alter the electrodes that have been imported."))
        ),
        footer = shiny::tagList(
          shiny::modalButton("Wait a second"),
          dipsaus::actionButtonStyled(inputId = ns("loader_import_subject"), label = "Yes, I'm ready", icon = shidashi::as_icon("check"), type = "primary")
        )
      )
    )
  }) |>
    shiny::bindEvent(input$loader_step2_validate_btn)


}
