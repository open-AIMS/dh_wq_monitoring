
##' Load and validate data
##'
##' Read in the data from the nominated input_path, validate the data
##' against a set of rules (manifest like) and also generate some spatial
##' artifacts
##' @title Load data module
##' @return NULL
##' @author Murray Logan
##' @export
module_load_data <- function() {
  status::status_set_stage(stage = 2, title = "Obtain data")

  ## read in the guideline values
  guidelines <- read_guidelines_data(params_path)
  saveRDS(guidelines, file = paste0(data_path, "primary/guidelines.rds"))

  ## get the names of wq files
  ## wq_files <- get_input_data_file_names(input_path)

  ## read in the raw data
  wq <- read_input_data(input_path)
  saveRDS(wq, file = paste0(data_path, "primary/wq.rds"))

  ## The config.ini file should like the focal_year
  ## Ideally, this should be equal to the most recent year in the wq data
  ## If it is not, overrite the focal_year setting and redo the paths
  if (status::get_setting("focal_year") != as.character(max(sapply(wq, function(x) max(year(x$Date)))))) {
     status::update_setting("focal_year", as.character(max(sapply(wq, function(x) max(year(x$Date)))))) 
     define_paths()                                                       ## define the location of paths/files
     invisible(prepare_paths())                                           ## prepare file structure
  }

  
  ## read in the overwrites data
  overwrites <- read_other_data(input_path, type = "overwrites.csv")
  saveRDS(overwrites, file = paste0(data_path, "primary/overwrites.rds"))

  ## read in the weights data
  weights_m <- read_other_data(input_path, type = "weights_m.csv")
  saveRDS(weights_m, file = paste0(data_path, "primary/weights_m.rds"))
  weights_s <- read_other_data(input_path, type = "weights_s.csv")
  saveRDS(weights_s, file = paste0(data_path, "primary/weights_s.rds"))

  ## read in the hierarchies data
  hierarchy <- read_other_data(input_path, type = "hierarchy.csv")
  saveRDS(hierarchy, file = paste0(data_path, "primary/hierarchy.rds"))

  ## read in the spatial data
  spatial <- read_other_data(params_path, type = "spatial.csv")
  saveRDS(spatial, file = paste0(data_path, "primary/spatial.rds"))

  data <- list(
    ## wq_files = wq_files,
    wq = wq,
    guidelines = guidelines,
    overwrites = overwrites,
    weights_m = weights_m,
    weights_s = weights_s,
    hierarchy = hierarchy,
    spatial = spatial)
  
  ## Validate the data and guidelines data
  data_validation <- validate_data(data)
  saveRDS(data_validation, file = paste0(data_path, "primary/data_validation.RData"))

  ## Handle validation issues
  ## If the validation issue is a number format issue, then delete the cases
  ## If the validation issue is a missing field, we must stop
  ## if there are data that should not be there, make them NA
  data <- handle_validation_issues(data, data_validation)
  saveRDS(data, file = paste0(data_path, "primary/data.rds"))

  ## data_validation$wq$status
  ## data_validation$overwrites$status
  ## data_validation$weights_m$status
  ## data_validation$weights_s$status

  ## ## make the spatial data
  ## spatial <- make_spatial_data()
  ## saveRDS(spatial, file = paste0(data_path, "primary/spatial.RData"))

  ## ## make the spatial lookup
  ## spatial_lookup <- make_spatial_lookup(spatial)
  ## saveRDS(spatial_lookup, file = paste0(data_path, "primary/spatial_lookup.RData"))
  ## saveRDS(spatial_lookup, file = paste0(data_path, "processed/spatial_lookup.RData"))

  ## make filename lookup
  filename_lookup <- tribble(
    ~type, ~name, ~path,
    "wq", names(wq), paste0(input_path, names(wq)),
    "overwrites", "overwrites", paste0(input_path, "overwrites.csv"),
    "guidelines", "guidelines", paste0(params_path, "water_quality_guidelines.csv"),
    "weights_m", "weights_m", paste0(input_path, "weights_m.csv"),
    "weights_s", "weights_s", paste0(input_path, "weights_s.csv"),
    "hierarchy", "hierarchy", paste0(input_path, "hierarchy.csv"),
    "spatial", "spatial", paste0(params_path, "spatial.csv"),
    ) |>
    unnest(c(name, path))
  saveRDS(filename_lookup, file = paste0(data_path, "primary/filename_lookup.rds"))
}

##' Read in guidelines (csv files) from the nominated input_path folder
##'
##' Read in all the data files and standardise (and limit to) required
##' sheets
##' @title Read guidelines data
##' @param params_path character representing the path from which to read the input guidelines files
##' @return nested list of data files and their sheets
##' @author Murray Logan
##' @export
read_guidelines_data <- function(params_path) {
  ## Get the filenames and types of any files in the `input_path` folder
  status::status_try_catch(
  {
    ## guidelines_file <- paste0(params_path, "WaterQuality.guidelines.csv")
    guidelines_file <- paste0(params_path, "water_quality_guidelines.csv")
    if (file.exists(guidelines_file)) {
      guidelines <- readr::read_csv(guidelines_file)
    } else {
      stop("No guidelines data found")
    }
  },
  stage_ = 2,
  name_ = "Read guidelines data",
  item_ = "read_guidelines_data"
  )
  return(guidelines)
}

##' Get the filenames of the wq data (csv files) from the nominated input_path folder
##'
##' Get the filenames of the wq data (csv files) from the nominated input_path folder
##' sheets
##' @title Get input data filenames
##' @param input_path character representing the path from which to read the input data files
##' @return a named vector of filenames
##' @author Murray Logan
##' @export
get_input_data_file_names <- function(input_path) {
  status::status_try_catch(
  {
    wq_files <- list.files(input_path,
      pattern = "[0-9]*_wq.csv",
      full.names = TRUE
    )
    wq_files <- setNames(wq_files, basename(wq_files))
  },
  stage_ = 2,
  name_ = "Get input data",
  item_ = "get_input_data"
  )
  return(wq_files)
}

##' Read in data (csv files) from the nominated input_path folder
##'
##' Read in all the data files and standardise (and limit to) required
##' sheets
##' @title Read input data
##' @param input_path character representing the path from which to read the input data files
##' @return nested list of data files and their sheets
##' @author Murray Logan
##' @export
read_input_data <- function(input_path) {
  ## Get the filenames and types of any files in the `input_path` folder
  status::status_try_catch(
  {
    input_files <- list.files(input_path, pattern = "[0-9]*_wq.csv", full.names = TRUE)
    data <- setNames(lapply(input_files, function(x) {
      dat <- readr::read_csv(x) |> 
        suppressMessages() |> suppressWarnings()
      if (!"Date" %in% colnames(dat))
        stop(paste0("The ", basename(x), " file is missing a 'Date' field"))
      dat |> mutate(Date = as.POSIXct(lubridate::dmy(Date))) 
    }), basename(input_files))
      ## purrr::reduce(dplyr::bind_rows) |>
      ## mutate(Date = as.POSIXct(lubridate::dmy(Date)))
  },
  stage_ = 2,
  name_ = "Read input data",
  item_ = "read_input_data"
  )
  return(data)
}

##' Read in other data (csv files) from the nominated input_path folder
##'
##' Read in all the data files and standardise (and limit to) required
##' sheets
##' @title Read input data
##' @param input_path character representing the path from which to read the input data files
##' @return nested list of data files and their sheets
##' @author Murray Logan
##' @export
read_other_data <- function(input_path, type) {
  ## Get the filenames and types of any files in the `input_path` folder
  status::status_try_catch(
  {
    col_types <- case_when(
      type == "overwrites.csv"~ "ccccccnncc",
      type == "weights_m.csv"~ "cccccnncn",
      type == "weights_s.csv"~ "cccccnncn",
      type == "hierarchy.csv"~ "ccccc",
      type == "spatial.csv"~ "ncncnnc",
    )
    input_file <- paste0(input_path, type)
    if (file.exists(input_file)) {
      data <- readr::read_csv(input_file, col_types = col_types)
    } else {
      stop(paste(input_file, "not found"))
    }
  },
  stage_ = 2,
  name_ = "Read other data",
  item_ = "read_other_data"
  )
  return(data)
}

##' Validate the input data against a series of sheet specific rules
##'
##' Validate the input data against a series of sheet specific rules
##' and return a nested list that includes validation summaries for
##' each of the sheets of each dataset
##' @title Validate input data 
##' @param raw_data 
##' @return a nested list of validation summaries 
##' @author Murray Logan
##' @export
validate_data <- function(data) {
  status::status_try_catch(
  {
    validation_data <- sapply(c("wq", "guidelines", "overwrites", "weights_m", "weights_s",
      "hierarchy", "spatial"),
      function(x) {
        if (x == "wq") {  ## wq data comprises multiple files
          lapply(data[["wq"]], function(y) {
            run_validations(y, x)
          })
        } else run_validations(data[[x]], x)
      }, simplify = FALSE) |>
      suppressWarnings() |> suppressMessages()
    validation_info <- get_validation_info(c(validation_data[["wq"]], validation_data[-1]))

    if (any(validation_info$Status == FALSE)) {
      warning("WARNING: Raw data validation issues")
    }
    validation_data
  },
  stage_ = 2,
  name_ = "Validate input data",
  item_ = "validate_input_data"
  )
}

##' Run all the validations
##'
##' Run all the validations
##' @title Run validations 
##' @param df 
##' @param data_type 
##' @param sheet_type 
##' @return a tibble of validations 
##' @author Murray Logan
##' @import validate
run_validations <- function(df, type) {
  ## print(df)
  ## print(type)
  if (nrow(df) == 0) {
    df <- df |> mutate(id = n())
  } else {
    df <- df |> mutate(id = 1:n())
  }
  targets <-
    get_rule_templates(type) |>
    dplyr::nest_by(item, .key = "rule_template", .keep = TRUE) |>
    dplyr::mutate(rules = list(rule_template |>
                                 dplyr::left_join(get_rules(),
                                   by = "applied_to", relationship = "many-to-many") |>
                                 mutate(
                                   rule = stringr::str_replace_all(rule, "<NAME>", item),
                                   description = stringr::str_replace_all(description, "<NAME>", item)
                                 )
    ),
    validate = list(validate::validator(.data = rules)),
    confront = list(validate::confront(df, validate, key = "id")),
    summary = list(summary(confront))
    )
  if (nrow(df) > 0) {
    targets <- targets |>
      mutate(df = list(validate::as.data.frame(confront) |>
                 dplyr::filter(!value) |>
                 dplyr::left_join(df, by = "id") |>
                 dplyr::left_join(rules |>
                           dplyr::select(name, severity, description) |>
                           dplyr::distinct(),
                           by = "name")
      ),
      status = list({
        msg <- "success"
        if (nrow(df) > 0) {
            if (unique(df$severity) == "fail") {
                msg <- paste("Failure:", paste(unique(df$description), collapse = ", "))
            } else {
                msg <- paste("Warning:", paste(unique(df$description), collapse = ", "))
            }
        }
        msg
      })
    )
  } else {
    targets <-
      targets |>
      dplyr::mutate(
        ## df = list(
        ##   df |>
        ##   dplyr::mutate(value = FALSE, id = NA) |>
        ##     dplyr::bind_rows(rules |>
        ##                        dplyr::select(name, severity, expression = rule, description) |>
        ##                        dplyr::distinct()) |>
        ##   dplyr::select(id, name, value, expression, everything(), severity, description)
        ## ),
       status = list( {
         ## msg <- paste("Warning:", paste(unique(df$description), collapse = ", "))
         msg <- paste("Warning: ", type, " has no data", collapse = ", ")
         msg
       })
      )
  }
  return(targets)
}


##' Get the rule templates
##'
##' The rule templates outline for each sheet what field types should be present
##' @title Get rule templates 
##' @param type either "wq" or "guidelines" 
##' @return a nested list of rule templates 
##' @author Murray Logan
get_rule_templates <- function(type = "wq") {
  rule_templates <-
    list(
      wq = tribble(
        ~item,   ~applied_to,
        "Zone",  "area",
        "Region","area",
        "Source", "source",
        "Date",   "datetime",
        "Latitude", "latitude",
        "Longitude", "longitude",
        "Chla_mug_PER_L", "analyte",
        "Turbidity_NTU", "analyte",
        "DO_PERCENT_saturation", "analyte",
        "NH3_mug_PER_L", "analyte",
        "PO4_mug_PER_L", "analyte",
        "Nox_mug_PER_L", "analyte",
        ## "SAMPTYPE", "source"
        "CFM", "cfm_data",
        "Discrete", "discrete_data"
      ),
      guidelines = tribble(
        ~item,   ~applied_to,
        ## "Region","area",
        "ZoneName","area_name",
        ## "database_name", "db_name",
        "HydstraName", "db_name",
        "Conversion", "conversion",
        "Measure", "measure",
        "UnitsLabel", "units",
        "Label", "label",
        "DirectionOfFailure", "dof",
        "GL", "gl",
        "RangeFrom", "gl",
        "RangeTo", "gl",
        "DetectionLimit", "lor"
      ),
      overwrites = tribble(
        ~item,   ~applied_to,
        "Component","component",
        "IndicatorGroup","indicator_group",
        "Indicator","indicator",
        "Subindicator","subindicator",
        "Measure", "measure",
        ## "SAMPTYPE", "source",
        "Sample", "source",
        "Zone",  "area",
        "Region","area",
        "Site","site",
        "overwrittenGrade","grade",
      ),
      weights_m = tribble(
        ~item,   ~applied_to,
        "Component","component",
        "IndicatorGroup","indicator_group",
        "Indicator","indicator",
        "Subindicator","subindicator",
        "Measure", "measure",
        "Zone",  "area",
        "Region","area",
        "Site","site",
        "Weight","weight",
      ),
      weights_s = tribble(
        ~item,   ~applied_to,
        "Component","component",
        "IndicatorGroup","indicator_group",
        "Indicator","indicator",
        "Subindicator","subindicator",
        "Measure", "measure",
        "Zone",  "area",
        "Region","area",
        "Site","site",
        "Weight","weight",
        ),
      hierarchy = tribble(
        ~item,   ~applied_to,
        "Component","component",
        "IndicatorGroup","indicator_group",
        "Indicator","indicator",
        "Subindicator","subindicator",
        "Measure", "measure",
      ),
      spatial = tribble(
        ~item,   ~applied_to,
        "Region","area",
        "RegionName","area_name",
        "Zone","area",
        "ZoneName","area_name",
        "Lab_lat", "latitude",
        "Lab_long", "longitude",
        "HexColor", "hex",
      )

    )
  return(rule_templates[[type]])
}

##' Get rules for each field type
##'
##' Get the rules for each field type
##' @title Get rules 
##' @return tibble of rules 
##' @author Murray Logan
get_rules <- function() {
  rules <- tribble(
    ~applied_to, ~name, ~rule, ~severity, ~description,
    "area", "has_field", "'<NAME>' %in% names(.)", "fail", "'<NAME>' field is missing",
    "source", "has_field", "'<NAME>' %in% names(.)", "fail", "'<NAME>' field is missing",
    "datetime", "has_field", "'<NAME>' %in% names(.)", "fail", "'<NAME>' field is missing",
    "latitude", "has_field", "'<NAME>' %in% names(.)", "fail", "'<NAME>' field is missing",
    "longitude", "has_field", "'<NAME>' %in% names(.)", "fail", "'<NAME>' field is missing",
    "analyte", "has_field", "'<NAME>' %in% names(.)", "fail", "'<NAME>' field is missing",
    "db_name", "has_field", "'<NAME>' %in% names(.)", "fail", "'<NAME>' field is missing",
    "conversion", "has_field", "'<NAME>' %in% names(.)", "fail", "'<NAME>' field is missing",
    "measure", "has_field", "'<NAME>' %in% names(.)", "fail", "'<NAME>' field is missing",
    "units", "has_field", "'<NAME>' %in% names(.)", "fail", "'<NAME>' field is missing",
    "label", "has_field", "'<NAME>' %in% names(.)", "fail", "'<NAME>' field is missing",
    "dof", "has_field", "'<NAME>' %in% names(.)", "fail", "'<NAME>' field is missing",
    "gl", "has_field", "'<NAME>' %in% names(.)", "fail", "'<NAME>' field is missing",
    "lor", "has_field", "'<NAME>' %in% names(.)", "fail", "'<NAME>' field is missing",

    "component", "has_field", "'<NAME>' %in% names(.)", "fail", "'<NAME>' field is missing",
    "indicator_group", "has_field", "'<NAME>' %in% names(.)", "fail", "'<NAME>' field is missing",
    "indicator", "has_field", "'<NAME>' %in% names(.)", "fail", "'<NAME>' field is missing",
    "subindicator", "has_field", "'<NAME>' %in% names(.)", "fail", "'<NAME>' field is missing",
    "site", "has_field", "'<NAME>' %in% names(.)", "fail", "'<NAME>' field is missing",
    "grade", "has_field", "'<NAME>' %in% names(.)", "fail", "'<NAME>' field is missing",
    "weight", "has_field", "'<NAME>' %in% names(.)", "fail", "'<NAME>' field is missing",
    "hex", "has_field", "'<NAME>' %in% names(.)", "fail", "'<NAME>' field is missing",
    "area_name", "has_field", "'<NAME>' %in% names(.)", "fail", "'<NAME>' field is missing",

    "area", "class", "class(`<NAME>`) %in% c('numeric', 'factor')", "fail", "'<NAME>' should be a number vector",
    "area_name", "class", "class(`<NAME>`) %in% c('character', 'factor')", "fail", "'<NAME>' should be a character vector",
    "source", "class", "class(`<NAME>`) %in% c('character', 'factor')",  "fail", "'<NAME>' should be a character vector",
    "datetime", "class", "any(class(`<NAME>`) %in% c('POSIXct'))",  "fail", "'<NAME>' should be a date time",
    "latitude", "class", "class(`<NAME>`) %in% c('numeric')",  "fail", "'<NAME>' should be a numeric",
    "longitude", "class", "class(`<NAME>`) %in% c('numeric')",  "fail", "'<NAME>' should be a numeric",
    "analyte", "class", "(class(`<NAME>`) %in% c('numeric')) | (any(grepl('^<.*', `<NAME>`))) | (all(is.na(`<NAME>`)))",  "fail", "'<NAME>' must only contain numbers (unless a '<' is present)",
    "db_name", "class", "class(`<NAME>`) %in% c('character', 'factor')",  "fail", "'<NAME>' should be a character vector",
    "conversion", "class", "class(`<NAME>`) %in% c('numeric')",  "fail", "'<NAME>' should be a numeric",
    "measure", "class", "class(`<NAME>`) %in% c('character', 'factor')",  "fail", "'<NAME>' should be a character vector",
    "units", "class", "class(`<NAME>`) %in% c('character', 'factor')",  "fail", "'<NAME>' should be a character vector",
    "label", "class", "class(`<NAME>`) %in% c('character', 'factor')",  "fail", "'<NAME>' should be a character vector",
    "dof", "class", "class(`<NAME>`) %in% c('character', 'factor')",  "fail", "'<NAME>' should be a character vector",
    "gl", "class", "(class(`<NAME>`) %in% c('numeric')) | (any(grepl('^<.*', `<NAME>`))) | (all(is.na(`<NAME>`)))",  "fail", "'<NAME>' must only contain numbers (unless a '<' is present)",
    "lor", "class", "(class(`<NAME>`) %in% c('numeric')) | (any(grepl('^<.*', `<NAME>`))) | (all(is.na(`<NAME>`)))",  "fail", "'<NAME>' must only contain numbers (unless a '<' is present)",
    "component", "class", "class(`<NAME>`) %in% c('character', 'factor')",  "fail", "'<NAME>' should be a character vector",
    "indicator_group", "class", "class(`<NAME>`) %in% c('character', 'factor')",  "fail", "'<NAME>' should be a character vector",
    "indicator", "class", "class(`<NAME>`) %in% c('character', 'factor')",  "fail", "'<NAME>' should be a character vector",
    "subindicator", "class", "class(`<NAME>`) %in% c('character', 'factor')",  "fail", "'<NAME>' should be a character vector",
    "site", "class", "class(`<NAME>`) %in% c('character', 'factor')",  "fail", "'<NAME>' should be a character vector",
    "grade", "class", "class(`<NAME>`) %in% c('character', 'factor')",  "fail", "'<NAME>' should be a character vector",
    "weight", "class", "class(`<NAME>`) %in% c('numeric')",  "fail", "'<NAME>' should be a numeric",

    "source", "value", "unique(`<NAME>`) %in% c('Discrete', 'CFM')", "fail", "'<NAME>' should only contain 'Discrete' or 'CFM'",
    "latitude", "number_format", "number_format(format(`<NAME>`, nsmall=1L), format = '-d*.d*') | is.na(`<NAME>`)",  "fail", "'<NAME>' should be in the format -d.d",
    "longitude", "number_format", "number_format(format(`<NAME>`, nsmall=1L), format = 'd*.d*') | is.na(`<NAME>`)",  "fail", "'<NAME>' should be in the format d.d",
    ## "longitude", "number_format", "as.numeric(`<NAME>`) | is.na(`<NAME>`)",  "fail", "'<NAME>' should be in the format d.d",
    "analyte", "field_format", "field_format(replace_na(as.character(`<NAME>`), \"\"), pattern = '(^[0-9\\\\.]|^<\\\\s?[0-9]*\\\\.?[0-9]*$|^$)', type = 'regex')",  "fail", "'<NAME>' values can only be either numbers or a '<'",
    "dof", "value", "unique(`<NAME>`) %in% c('B', 'H')", "fail", "'<NAME>' should only contain 'B' or 'H'",

    
    "grade", "value", "unique(`<NAME>`) %in% c(LETTERS[1:5], '-')", "fail", "'<NAME>' should only contain either 'A', 'B', 'C', 'D' or 'E' or a '-'",
    "hex", "field_format", "field_format(replace_na(as.character(`<NAME>`), \"\"), pattern = '(^#[A-Fa-f0-9]{6}$)', type = 'regex')",  "fail", "'<NAME>' values must be six ddgit hex codes starting with a #",

    "cfm_data", "cfm_data", "if (Source=='CFM') is.na(NH3_mug_PER_L) & is.na(PO4_mug_PER_L) & is.na(Nox_mug_PER_L)", "fail", "CFM data should only have Chla_mug_PER_L, Turbidity_NTU and DO_PERCENT_saturation",
    "discrete_data", "discrete_data", "if (Source=='Discrete') is.na(Turbidity_NTU) & is.na(DO_PERCENT_saturation)", "fail", "Discrete data should only have Chla_mug_PER_L, NH3_mug_PER_L, PO4_mug_PER_L and Nox_mug_PER_L",
    
    )
  return(rules)
}

##' Extract the validation information from the validation tibble
##'
##' Extract the validation information from the validation tibble
##' @title Get validation info from tibble 
##' @param tbl 
##' @return a tibble of validation status 
##' @author Murray Logan
get_validation_info_from_tibble <- function(tbl) {
  tbl |>
    dplyr::mutate(Status = map(
      .x = value,
      .f = ~ {
        .x |>
          dplyr::pull(status) |>
          unlist() |>
          unique() |>
          stringr::str_detect("[fF]ailure:") |>
          (\(.) all(. == FALSE))()
      }
    )) |>
    tidyr::unnest(Status)
}

##' Get validation info
##'
##' Get the validation info
##' @title Get validation info 
##' @param lst 
##' @return list 
##' @author Murray Logan
get_validation_info <- function(lst) {
  lst |>
    tibble::enframe(name = "File") |>
    ## dplyr::mutate(data = map(.x = value, .f = ~ tibble::enframe(.x, name = "Sheet"))) |>
    ## dplyr::mutate(data = map(.x = value, .f = ~ .x)) |>
    ## dplyr::select(-value) |>
    ## tidyr::unnest(value) |>
    get_validation_info_from_tibble()
}


##' Handle validation issues
##'
##' Handle validation issues
##' @title Handle validation issues
##' @param dat a list of data sets
##' @param df a list of validation summaries
##' @return a list of data sets with validation issues resolved
##' @author Murray Logan
handle_validation_issues <- function(dat, df) {
  status::status_try_catch(
  {
    ## df <- c(df[["wq"]], df[-1])
    ## WQ issues
    dat_wq <- purrr::pmap(
      .l = list(names(dat[["wq"]]), dat[["wq"]], df[["wq"]]), ## for each WQ data set
      .f =  ~ {
        nms <- ..1
        dat <- ..2
        df <- ..3
        handle_validation_issues_source(nms, dat, df)
      })
    names(dat_wq) <- names(dat[["wq"]])
    ## Other sources issues
    dat_others <- purrr::pmap(
      .l = list(names(dat[-1]), dat[-1], df[-1]), ## for each WQ data set
      .f =  ~ {
        nms <- ..1
        dat <- ..2
        df <- ..3
        handle_validation_issues_source(nms, dat, df)
      })
    names(dat_others) <- names(dat[-1])
  },
  stage_ = 2,
  name_ = "Validate exclude data",
  item_ = "validate_exclude_data"
  )
  return(c(list(wq = dat_wq), dat_others))
}

handle_validation_issues_source <- function(nms, dat, df) {
  items <- df$item
  dat <- dat |> mutate(exclude = NA)
  ## if the error is "discrete_data" then replace the Turbidity and DO
  ## with NA values take the expression and parse out the items in
  ## is.na() to get the names of the fields to replace
  for (i in 1:nrow(df)) {
    if (df$status[[i]] != "success") {
      if (any(str_detect(df$status, "Failure"))) {
        if (any("discrete_data" %in% unique(df$df[[i]]$name)) |
              any("cfm_data" %in% unique(df$df[[i]]$name))) {
          rows_to_change <- df$df[[i]]$id
          vars <- str_match_all(df$df[[i]]$expression[[1]], "is.na\\(([^)]*)\\)")[[1]][,2]
          dat[rows_to_change, vars] <- NA
        }
      }
    }
  }
  rows_to_exclude <- NULL
  for (i in 1:nrow(df)) {
    if (df$status[[i]] != "success") {
      if (any(str_detect(df$status, "Failure"))) {
        ## if (any("has_field" %in% unique(df$df[[i]]$name)))
        ## stop(paste("Validation issue, field ", items[i], "of", nms, " is missing"))
        ## print(any("number_format" %in% unique(df$df[[i]]$name)))
        if (any("number_format" %in% unique(df$df[[i]]$name))) {
          rows_to_exclude <- c(rows_to_exclude, df$df[[i]]$id)
        }
      }
    }
  }
  dat[unique(rows_to_exclude), "exclude"] <- "exclude"
  dat <- dat |> dplyr::filter(is.na(exclude)) 
  dat <- dat |> dplyr::select(-exclude)

  return(dat)
}
