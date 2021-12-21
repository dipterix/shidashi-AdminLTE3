# global variables for the module

# Stores global variables
local_data <- fastmap::fastmap()
local_reactives <- shiny::reactiveValues()

on_data_loaded <- function(){
  return(FALSE)
}

get_projects <- local({
  re <- NULL
  function(refresh = FALSE){
    if(refresh || !length(re)){
      re <<- rave::get_projects()
    }
    re
  }
})

pipeline_path <- normalizePath("./modules/import_raw/import-LFP")
pipeline_settings_path <- file.path(pipeline_path, "settings.yaml")
pipeline_settings <- local({
  settings <- raveio::load_yaml(pipeline_settings_path)
  list(
    set = function(...){
      args <- list(...)
      argnames <- names(args)
      if(!length(argnames)){
        return(as.list(settings))
      }
      args <- args[argnames != ""]
      argnames <- names(args)
      if(!length(argnames)){
        return(as.list(settings))
      }
      for(nm in argnames){
        settings[[nm]] <<- args[[nm]]
      }
      raveio::save_yaml(x = settings, file = pipeline_settings_path)
      return(as.list(settings))
    },
    get = function(key, default = NA){
      if(missing(key)){
        return(as.list(settings))
      }
      if(!settings$`@has`(key)){
        return(default)
      }
      settings[[key]]
    }
  )
})
pipeline_set <- pipeline_settings$set
pipeline_get <- pipeline_settings$get

