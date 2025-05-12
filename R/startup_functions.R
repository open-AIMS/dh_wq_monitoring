
##' A wrapper for a set of functions that should be run at the start
##'
##' This wrapper controls the following:
##' - set the initial stage (Stage 1)
##' - initialises the status list
##' - loads the necessary packages
##' - retrieve any settings from CLA or other
##' - defines the paths used across the analyses
##' - clear any transient data from various paths
##' - ensure all necessary paths are present
##' @title Start matter 
##' @param args optional arguments supplied to establish settings
##' @return NULL 
##' @author Murray Logan
##' @export
##' @import status
start_matter <- function(args = commandArgs()) {
  status::status_set_stage(stage = 1, title = "Prepare environment")   ## set the analysis stage
  initialize()                                                         ## create the status list
  load_packages()                                                      ## load required packages
  get_settings(args)                                                   ## get settings (either from CLA or shiny)
  define_paths()                                                       ## define the location of paths/files
  invisible(prepare_paths())                                           ## prepare file structure
}

##' Initialise status
##'
##' This will call the status::status_initialize function
##' which will look through all the R scripts in the current
##' working directory and use regex to find status tokens from which
##' to build a status list
##' @title Intialise 
##' @return NULL
##' @author Murray Logan
##' @export
initialize <- function() {
  ## status::status_try_catch(
  ## {
     ## status::status_initialize(pkgs = "sedMod")
     status::status_initialize(box_width = 100)
  ## },
  ## stage_ = 1,
  ## name_ = "Initialise status",
  ## item_ = "initialise_status"
  ## )
}

##' Load the necessary packages for this project
##'
##' Load all the packages
##' @title Load packages 
##' @return NULL 
##' @author Murray Logan
load_packages <- function() {
  status::status_try_catch(
  {
    missing <- ""
    options(tidyverse.quiet = TRUE)
    pkgs <- c(
      "tidyverse", "testthat", "cli", "rlang", "crayon",
      "assertthat", "lubridate", "rmarkdown", "bookdown",
      "sf", "validate", "status", "plotly",
      "patchwork", "future", "purrr", "promises", "bslib", "ggdist"
    )

    for (p in pkgs) {
      ## unforunately we must do this the base r way until rlang is
      ## loaded
      eval(parse(text = paste0("suppressPackageStartupMessages(if(!require(",
                             p, ",quietly = TRUE, warn.conflicts = FALSE)) missing <- c(missing, ",
                             p, "))")))
    }

    if (missing != "") {
      stop(paste(
        "The following required package(s) are missing: ",
        paste(missing, collapse = ", ")
      ))
    }
    plan(multisession)
  },
  stage_ = 1,
  name_ = "Load package dependencies",
  item_ = "load_packages"
  )
}

##' Retrieve any settings that are parsed on from either command line
##' arguments or from Shiny
##'
##' Retrieve any settings that are parsed on from either the command line(
##' or from Shiny).  The settings are:
##' - ...
##' @title Get settings 
##' @param args name:value pairs for the major settings 
##' @return NULL
##' @author Murray Logan
get_settings <- function(args) {
  status::status_try_catch(
  {
    assign("input_path", "../input/", env = .GlobalEnv)
    add_setting(element = "input_path", item = input_path, name = "Input path")

    ## put in some logic that decides whether this code has been called from the
    ## command line or shiny
    params_path <- "../parameters/" #status::get_setting(element = "params_path")
    if (file.exists(paste0(params_path, "config.ini"))) {
        config <- readLines(paste0(params_path, "config.ini"))
        config <- gsub('(.*Date)=(.*)','\\1=as.Date(\'\\2\')',config)
        key_value_pairs <- strsplit(config, "=")
        invisible(lapply(key_value_pairs, \(v)
          status::add_setting(element = v[[1]], item = v[[2]], name = v[[1]])))
    }
    ## Set the limit of reporting option to 1 (censor data at limit of reporting)
    status::add_setting(element = "lor", item = 1, name = "Limit of Reporting")

    ## Set the current reporting year (if it is not already set)
    if (is.null(status::get_setting("focal_year"))) {
      status::add_setting(element = "focal_year", item = year(today()),
        name = "focal year")
    }
  },
  stage_ = 1,
  name_ = "Get settings",
  item_ = "get_settings"
  )
}

##' Define the various paths used throughout the project
##'
##' Assign paths to the global env and add them to status_$settings
##' @title Define paths
##' @param args name:value pairs for the major settings
##' @return NULL
##' @author Murray Logan
define_paths <- function() {
  status::status_try_catch(
  {
    ## location of folder containing R data objects
    assign("data_path", paste0("../data/",
      status::get_setting(element = "focal_year"), "/"),
      env = .GlobalEnv)
    add_setting(element = "data_path", item = data_path, name = "Data path")

    ## location of folder containing perpetual data used in this project
    assign("params_path", "../parameters/", env = .GlobalEnv)
    add_setting(element = "params_path", item = params_path, name = "Parameters path")

    ## location of folder containing outputs (individual figures and tables)
    assign("output_path", paste0("../outputs/",
      status::get_setting(element = "focal_year"), "/"),
      env = .GlobalEnv)
    add_setting(element = "output_path", item = output_path, name = "Output path")

    ## location of folder containing generated documents
    assign("docs_path", "../docs/", env = .GlobalEnv)
    add_setting(element = "docs_path", item = docs_path, name = "Documents path")

    ## location of folder containing generated documents
    assign("progress_path", data_path, env = .GlobalEnv)
    add_setting(element = "progress_path", item = progress_path, name = "Progress path")

    ## location of the model_logs_file
    ## assign("model_log_file", paste0(data_path, "modelled/log_models.log"), env = .GlobalEnv)
    ## add_setting(element = "model_log_file", item = model_log_file, name = "Model Logs file")
    
  },
  stage_ = 1,
  name_ = "Define paths",
  item_ = "define_paths"
  )
}

##' Delete various files and paths that are considered internal only
##' to ensure that subsequent runs of the analyses are not contaminated
##' with previous outputs.
##'
##' Delete paths and files nominated in input
##' @title Cleanse paths
##' @param paths character vector representing the path(s) to be deleted
##' @param files character vector representing the file(s) to be deleted
##' @return NULL
##' @author Murray Logan
cleanse_paths <- function(paths, files) {
  # status::status_try_catch(
  # {
  ## Entire paths
  eval_parse <- function(x) {
    eval(parse(text = paste0(x)))
  }
  for (d in paths) {
    if (dir.exists(eval_parse(d))) {
      unlink(eval_parse(d), recursive = TRUE)
    }
  }
  ## Specific files
  remove_only <- function(x) if (file.exists(x)) file.remove(x)
  ## if (length(files) > 0) do.call(remove_only, list(files))
  # },
  # stage_ = 1,
  # name_ = "Cleanse paths",
  # item_ = "cleanse_paths"
  # )
}

##' Ensure that the important paths are created if they do not already exist
##'
##' Ensure that the important paths are created if they do not already exist
##' @title Prepare paths
##' @return NULL 
##' @author Murray Logan
###################################################################
## Following this script, the final directory structure will be: ##
## |- data                                                       ##
## |    |- logs                                                  ##
## |    |- GIS                                                   ##
## |    |   |- DH_spatial.R                                      ##
## |    |   |- zonemasks                                         ##
## |    |- 2016                                                  ##
## |    |   |- primary                                           ##
## |    |   |- processed                                         ##
## |    |   |     |- stage1                                      ##
## |    |   |     |- stage2                                      ##
## |    |   |     |- indices                                     ##
## |- output                                                     ##
##       |- QAQC                                                 ##
##       |- zones                                                ##
##       |    |- indicators                                      ##
##       |    |- subindicators                                   ##
##       |    |- measures                                        ##
##       |- wh                                                   ##
##       |    |- indicators                                      ##
##       |    |- subindicators                                   ##
##       |- regions                                              ##
##       |    |- indicators                                      ##
##       |    |- subindicators                                   ##
##       |- summaries                                            ##
##                                                               ##
###################################################################
prepare_paths <- function() {
  status::status_try_catch(
  {

    if (!dir.exists("../data/GIS"))
      dir.create("../data/GIS", recursive = TRUE)

    if (!dir.exists(data_path)) dir.create(data_path, recursive = TRUE)
    if (!dir.exists(paste0(data_path, "primary/"))) {
      dir.create(paste0(data_path, "primary/"), recursive = TRUE)
      ## dir.create(paste0(data_path, "primary/GIS/"))
    }
    if (!dir.exists(paste0(data_path, "processed/"))) {
      dir.create(paste0(data_path, "processed/"), recursive = TRUE)
    }
    if (!dir.exists(paste0(data_path, "processed/stage1"))) {
      dir.create(paste0(data_path, "processed/stage1"), recursive = TRUE)
    }
    if (!dir.exists(paste0(data_path, "processed/stage2"))) {
      dir.create(paste0(data_path, "processed/stage2"), recursive = TRUE)
    }
    if (!dir.exists(paste0(data_path, "processed/indices"))) {
      dir.create(paste0(data_path, "processed/indices"), recursive = TRUE)
    }
    if (!dir.exists(paste0(data_path, "boot"))) {
      dir.create(paste0(data_path, "boot"), recursive = TRUE)
    }
    if (!dir.exists(paste0(data_path, "summaries"))) {
      dir.create(paste0(data_path, "summaries"), recursive = TRUE)
    }

    if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE)
    if (!dir.exists(paste0(output_path, "tables/"))) {
      dir.create(paste0(output_path, "tables/"), recursive = TRUE)
    }
    if (!dir.exists(paste0(output_path, "figures/"))) {
      dir.create(paste0(output_path, "figures/"), recursive = TRUE)
    }
    if (!dir.exists(paste0(output_path, "figures/QAQC"))) {
      dir.create(paste0(output_path, "figures/QAQC"), recursive = TRUE)
    }
    if (!dir.exists(paste0(output_path, "figures/zones"))) {
      dir.create(paste0(output_path, "figures/zones"), recursive = TRUE)
    }
    if (!dir.exists(paste0(output_path, "figures/zones/indicators"))) {
      dir.create(paste0(output_path, "figures/zones/indicators"), recursive = TRUE)
    }
    if (!dir.exists(paste0(output_path, "figures/zones/subindicators"))) {
      dir.create(paste0(output_path, "figures/zones/subindicators"), recursive = TRUE)
    }
    if (!dir.exists(paste0(output_path, "figures/zones/measures"))) {
      dir.create(paste0(output_path, "figures/zones/measures"), recursive = TRUE)
    }
    if (!dir.exists(paste0(output_path, "figures/wh"))) {
      dir.create(paste0(output_path, "figures/wh"), recursive = TRUE)
    }
    if (!dir.exists(paste0(output_path, "figures/wh/indicators"))) {
      dir.create(paste0(output_path, "figures/wh/indicators"), recursive = TRUE)
    }
    if (!dir.exists(paste0(output_path, "figures/wh/subindicators"))) {
      dir.create(paste0(output_path, "figures/wh/subindicators"), recursive = TRUE)
    }
    if (!dir.exists(paste0(output_path, "figures/wh/measures"))) {
      dir.create(paste0(output_path, "figures/wh/measures"), recursive = TRUE)
    }
    if (!dir.exists(paste0(output_path, "figures/regions"))) {
      dir.create(paste0(output_path, "figures/regions"), recursive = TRUE)
    }
    if (!dir.exists(paste0(output_path, "figures/regions/indicators"))) {
      dir.create(paste0(output_path, "figures/regions/indicators"), recursive = TRUE)
    }
    if (!dir.exists(paste0(output_path, "figures/regions/subindicators"))) {
      dir.create(paste0(output_path, "figures/regions/subindicators"), recursive = TRUE)
    }
    if (!dir.exists(paste0(output_path, "figures/regions/measures"))) {
      dir.create(paste0(output_path, "figures/regions/measures"), recursive = TRUE)
    }
    if (!dir.exists(paste0(output_path, "figures/summaries"))) {
      dir.create(paste0(output_path, "figures/summaries"), recursive = TRUE)
    }

    if (!dir.exists(docs_path)) dir.create(docs_path)

    ## if (file.exists(model_log_file)) {
    ##   unlink(model_log_file)
    ## }
    ## file.create(model_log_file)
    },
  stage_ = 1,
  name_ = "Prepare paths",
  item_ = "prepare_paths"
  )
 return(NULL)
}
