
##' Calculate WQ indices
##'
##' Calculate water quality indices from the data
##' @title Calculate WQ indices
##' @return NULL
##' @author Murray Logan
##' @export
module_indices <- function() {
  status::status_set_stage(stage = 5, title = "Calculate indices")

  ## data <- readRDS(file = paste0(data_path, "processed/data.rds"))
  data <- indices_load_data(file = paste0(data_path, "processed/data.rds"))

  ## Calculate indices
  #######################################################################
  ## The following module applies the scaled modified amplitude method ##
  ## to all water quality Measures (at the Site level)                 ##
  ## except DO (which uses a modification of the Fitzroy Partnership   ##
  ## method). It also starts the process of LOD by identifying if and  ##
  ## which LOD Rule should be applied.  Note the actual Rules are      ##
  ## applied in the DH_water_prepareBoot.R module                      ##
  #######################################################################
  data_idx <- indices(data = data)

  saveRDS(data_idx, file = paste0(data_path, "/processed/indices/data_idx.rds"))
  write_csv(data_idx, file = paste0(data_path, "/processed/indices/data_idx.csv"))

  ## Prepare for bootstrapping
  ##############################################################
  ## The following module prepares the water and quality data ##
  ## for bootstrapping.  This essentially comprises:          ##
  ## - generating Site level Measure boostrap distributions   ##
  ##############################################################
  data_boot_zone <- prepare_boot(data = data_idx)

  rm(list = ls(pattern = c("data", "data_idx", "data_boot_zone")))
  invisible(gc())
}

##' Load data for indices calculations
##'
##' Load data for indices calculations
##' @title Load data for indices calculations
##' @param file a filename (and path) for the R data file to load
##' @return data a list containing an item called "wq_long" that contains
##' a tibble representing the water quality data
##' @author Murray Logan
##' @export
indices_load_data <- function(file = paste0(data_path, "processed/data.rds")) {
  status::status_try_catch(
  {
    data <- readRDS(file)
  },
  stage_ = 5,
  name_ = "Load data",
  item_ = "indices_load_data",
  )
  return(data)
}

##' Indices
##'
##' Indices
##' @title Indices
##' @param data a list containing an item called "wq_long" that contains
##' a tibble representing the water quality data
##' @return a data frame containing the indices
##' @author Murray Logan
##' @export
indices <- function(data) {
  status::status_try_catch(
  {
    tuning <- as.numeric(status::get_setting("tuning"))
    foldcap <- as.numeric(status::get_setting("foldcap"))
    method <- gsub("\\\"", "", status::get_setting("method"))
    data_idx <- data$wq_long |>
      ungroup() |>
      group_by(DirectionOfFailure) |>
      mutate(
        Binary = reportcards::RC_index(Value, GL = GL,
          DOF = DirectionOfFailure, Lower = RangeFrom, Upper = RangeTo,
          type = "Binary"),
        MAMP = reportcards::RC_index(Value, GL = GL,
          DOF = DirectionOfFailure, type = "MAMP"),
        fMAMP = reportcards::RC_index(Value, GL = GL,
          DOF = DirectionOfFailure, type = "MAMP", fold = foldcap),
        fsMAMP = reportcards::RC_index(Value, GL = GL,
          DOF = DirectionOfFailure, type = "MAMP",
          Lower = RangeFrom, Upper = RangeTo,
          scaled = TRUE, capped = TRUE, fold = foldcap),
        sMAMP = reportcards::RC_index(Value, GL = GL,
          DOF = DirectionOfFailure, type = "MAMP", scaled = TRUE),
        lMAMP = reportcards::RC_index(Value, GL = GL,
          DOF = DirectionOfFailure, type = "LAMP", T = tuning,
          Lower = RangeFrom, Upper = RangeTo)
      ) |>
      mutate(Index = !!sym(method))

    ## Recalculate for pH - use the Fitzroy Partnership Method
    data_idx <- data_idx |>
      mutate(Index = ifelse(method == "Binary" &
                              DirectionOfFailure == "B",
        ifelse(Value > RangeFrom & Value < RangeTo, 1, 0), Index),
        Index = ifelse(method != "Binary" &
                         DirectionOfFailure == "B",
          DH_WQI_FPM(Value, RangeFrom = RangeFrom,
            RangeTo = RangeTo, rescale = FALSE), Index)
      )
  },
  stage_ = 5,
  name_ = "Calculate indices",
  item_ = "calculate_indices",
  )
  return(data_idx)
}

DH_WQI_FPM <- function(Value, RangeFrom, RangeTo, rescale=TRUE) {
  if (rescale==FALSE) {
    ifelse(Value < RangeFrom, exp(Value * 2 - RangeFrom * 2),
      ifelse(Value > RangeTo, exp(RangeTo - Value), 1))
  } else {
    scales:::rescale(
      ifelse(Value < RangeFrom, exp(Value * 2 - RangeFrom * 2),
        ifelse(Value > RangeTo, exp(RangeTo - Value), 1)),
      from = c(0,1), to = c(-1,1))
  }
}


##' Prepare for bootstrapping
##'
##' Prepare the data for bootstrapping
##' @title Prepare for bootstrapping
##' @param data a tibble containing a "wq_long" element
##' @return a list containing an item called "wq" that contains
##' a tibble representing the water quality data
##' @author Murray Logan
##' @export
prepare_boot <- function(data) {
  status::status_try_catch(
  {
    seed <- as.numeric(status::get_setting("seed"))
    size <- as.numeric(status::get_setting("size"))
    focal_year <- status::get_setting("focal_year")
    ## Site level
    if (!focal_year %in% unique(data$Year)) stop("Focal year not in data")
    data_boot_zone <- data |>
      ungroup() |>
      dplyr::filter(Year == focal_year) |> 
      group_by(Region, Zone, Measure, Source) |> 
      sample_ns(seed = seed, size = size, replace = TRUE)
    data_boot_zone <- data_boot_zone |>
      dplyr::select(Region, Zone, Component, IndicatorGroup, Indicator,
        Subindicator, Measure, Source, Index) 
    saveRDS(data_boot_zone, file = paste0(data_path, "/processed/indices/data_boot_zone.rds"))
  },
  stage_ = 5,
  name_ = "Prepare for bootstrapping",
  item_ = "prepare_boot",
  )
  return(data_boot_zone)
}

sample_ns <- function(df, seed, size, Weight = NULL, replace = TRUE){
    set.seed(seed)
    dplyr:::sample_n(df, size = size, weight = Weight, replace = replace)
}


