##' Process data
##'
##' Process data
##' @title Process data
##' @return NULL
##' @author Murray Logan
##' @export
module_process_data <- function() {
  status::status_set_stage(stage = 4, title = "Process data")

  data <- readRDS(file = paste0(data_path, "primary/data.rds"))

  ## Compile all wq data into a single tibble
  data <- process_combine(data)
  
  ## Ensure that the wq dates are in the correct format
  data <- process_dates(data)

  ## Filter out all records that do not fall within the Date
  ## range defined in config/config.ini
  data <- process_filter_date_range(data)

  ## Select only those measures for which there are guideline
  ## values
  data <- process_select_measures(data)

  ## add focal year
  data <- process_focal_year(data)

  ## pivot data longer
  data <- process_pivot_longer(data)

  ## apply spatial - this is a long process
  data <- process_assign_spatial(data)
  
  ## join to guidelines
  data <- process_join_guidelines(data)

  ## convert units
  data <- process_convert_units(data)
  
  ## Replace Values that are less than the DetectionLimit, with
  ## the DetectionLimit value and flagg these values
  data <- process_detection_limit(data)

  ## join to hierarchy
  data <- process_join_hierarchy(data)

  ## Save the processed data
  saveRDS(object = data, file = paste0(data_path, "processed/data.rds"))
}

##' Combine WQ data
##'
##' Combine WQ data
##' @title Combine WQ data
##' @param data a list containing an item called "wq" that contains a list of tibbles
##' representing the water quality data
##' @return a list containing an item called "wq" that contains
##' a tibble representing the water quality data
##' @author Murray Logan
##' @export
process_combine <- function(data) {
  status::status_try_catch(
  {
    data$wq <- data$wq |>
      dplyr::bind_rows()
  },
  stage_ = 4,
  name_ = "Combine WQ date",
  item_ = "process_combine",
  )
  return(data)
}


##' Process dates
##'
##' Process dates
##' @title Process dates
##' @param data a list containing an item called "wq" that contains
##' a tibble representing the water quality data
##' @return a list containing an item called "wq" that contains
##' a tibble representing the water quality data
##' @author Murray Logan
##' @export
process_dates <- function(data) {
  status::status_try_catch(
  {
    data$wq <-
      data$wq |> 
      dplyr::mutate(Date = as.Date(Date, format = "%d/%m/%Y"))
  },
  stage_ = 4,
  name_ = "Process dates",
  item_ = "process_dates",
  )
  return(data)
}

##' Filter date range
##'
##' Filter date range
##' There may be a need to exclude observation at either the start or the end of the time series
##' @title Filter date range
##' @param data a list containing an item called "wq" that contains
##' a tibble representing the water quality data
##' @return a list containing an item called "wq" that contains
##' a tibble representing the water quality data
##' @author Murray Logan
##' @export
process_filter_date_range <- function(data) {
  status::status_try_catch(
  {
    if (is.null(status::get_setting(element = "start_date"))) {
      start_date <- data$wq |>
        filter(Date == min(Date)) |>
        pull(Date) |>
        unique()
    } else {
      start_date <- status::get_setting(element = "start_date")
    }
    if (is.null(status::get_setting(element = "end_date"))) {
      end_date <- data$wq |>
        filter(Date == max(Date)) |>
        pull(Date) |>
        unique()
    } else {
      end_date <- status::get_setting(element = "end_date")
    }
    data$wq <-
      data$wq |>
      dplyr:::filter(between(Date, start_date, end_date))
  },
  stage_ = 4,
  name_ = "Filter date range",
  item_ = "process_filter_date_range",
  )
  return(data)
}

##' Select measures
##'
##' Select only the measures for which there are guideline values
##' @title Select measures
##' @param data a list containing an item called "wq" that contains
##' a tibble representing the water quality data
##' @return a list containing an item called "wq" that contains
##' a tibble representing the water quality data
##' @author Murray Logan
##' @export
process_select_measures <- function(data) {
  status::status_try_catch(
  {
    measures <- paste0('^',
      as.character(unique(data$guidelines$Measure)),"$")
    ref <- paste0(c("^Region$", "^Zone", "^Source$", "^Date",
      "^Latitude", "^Longitude", measures), collapse = "|")
    data$wq <-
      data$wq |>
      dplyr:::select(matches(ref))
  },
  stage_ = 4,
  name_ = "Select measures",
  item_ = "process_select_measures",
  )
  return(data)
}

##' Add focal year
##'
##' Add a focal Year field that reflects the financial year
##' @title Add focal year
##' @param data a list containing an item called "wq" that contains
##' a tibble representing the water quality data
##' @return a list containing an item called "wq" that contains
##' a tibble representing the water quality data
##' @author Murray Logan
##' @export
process_focal_year <- function(data) {
  status::status_try_catch(
  {
    if (is.null(status::get_setting(element = "focal_year"))) {
      focal_year <- data$wq |>
        filter(Date == max(Date)) |>
        pull(Date) |>
        unique() |>
        year()
    } else {
      focal_year <-  status::get_setting(element = "focal_year")
    }
    data$wq <-
      data$wq |> 
      dplyr::mutate(Focal_Year = focal_year,
        Year = year(Date))
  },
  stage_ = 4,
  name_ = "Add focal year",
  item_ = "process_focal_year",
  )
  return(data)
}


##' Pivot data longer
##'
##' Pivot data longer
##' @title Pivot data longer
##' @param data a list containing an item called "wq" that contains
##' a tibble representing the water quality data
##' @return a list containing an item called "wq" that contains
##' a tibble representing the water quality data
##' @author Murray Logan
##' @export
process_pivot_longer <- function(data) {
  status::status_try_catch(
  {
    wch = which(colnames(data$wq) %in%
                  as.character(unique(data$guidelines$Measure)))
    data$wq_long <-
      data$wq |>
      pivot_longer(cols = all_of(wch),
        names_to = "Measure",
        values_to = "Value") |>
      ## omit records with missing values
      filter(!is.na(Value))

  },
  stage_ = 4,
  name_ = "Pivot longer",
  item_ = "process_pivot_longer",
  )
  return(data)
}

##' Join guidelines
##'
##' Join guidelines
##' @title Join guidelines
##' @param data a list containing an item called "wq" that contains
##' a tibble representing the water quality data
##' @return a list containing an item called "wq" that contains
##' a tibble representing the water quality data
##' @author Murray Logan
##' @export
process_join_guidelines <- function(data) {
  status::status_try_catch(
    {
      data$wq_long <-
        data$wq_long |>
        ## left_join(data$spatial |> dplyr::select(Zone, ZoneName), by = c("Zone")) |>
        ## left_join(data$guidelines, by = c("Region", "Measure"))
        left_join(data$guidelines, by = c("ZoneName", "Measure"))
    },
    stage_ = 4,
    name_ = "Join guidelines",
    item_ = "process_join_guidelines",
  )
  return(data)
}


##' Assign spatial
##'
##' Assign spatial
##' Although the raw wq data has Region and Zone fields, these field are not always
##' reliable.  It is better to use the spatial (shapefiles) to assign the
##' correct Zones and Regions.
##' @title Assign spatial
##' @param data a list containing an item called "wq_long" that contains
##' a tibble representing the water quality data
##' @return a list containing an item called "wq_long" that contains
##' a tibble representing the water quality data
##' @author Murray Logan
##' @export
process_assign_spatial <- function(data) {
  status::status_try_catch(
  {
    dh_sf <- readRDS(file = paste0(data_path, "processed/dh_sf.rds"))
    sf_use_s2(FALSE)
    wq_long_sf <-
      data$wq_long |>
      filter(!(is.na(Latitude) & is.na(Longitude))) |>
      dplyr::mutate(OldZone = Zone) |>
      st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) |>
      st_transform(crs = st_crs(dh_sf)) |>
      dplyr::select(-Zone, -Region) |> 
      mutate(ZoneName = NA)

    ## points_mat <- st_coordinates(wq_long_sf)
    ## polygons_mat <- st_coordinates(dh_sf)
    ## # Find nearest polygon index for each point
    ## nearest_idx <- nn2(polygons_mat, points_mat, k = 1)$nn.idx
    ## # Assign nearest polygon ID
    ## nearest_ids <- nngeo::st_nn(wq_long_sf, dh_sf, k = 1, progress = FALSE)

    # Assign polygon ID
    ## wq_long_sf$ZoneName <- sapply(nearest_ids, function(x) dh_sf$ZoneName[x])
    # wq_long_sf$ZoneName <- dh_sf$ZoneName[nearest_idx]

    ## start by calculating the intersects. This is faster than nearest
    ## neighbor, but any points that are outside the polygons will not
    ## be assigned.
    intersections <-
      wq_long_sf |>
      st_intersects(dh_sf, sparse = TRUE) |>
      suppressMessages() |>
      suppressWarnings()
    ## wq_long_sf <-
    ##   wq_long_sf |>
    ##   mutate(ZoneName = dh_sf$ZoneName[unlist(intersections)])

    ## intersections <-
    ##   wq_long_sf |> 
    ##   st_nearest_feature(dh_sf)

    ## dh <- as.data.frame(dh_sf)

    ## intersection_vector <- sapply(intersections, function(x) {
    ##   if (length(x) == 0) {
    ##     return(NA)
    ##   } else {
    ##     return(x[1])
    ##   }
    ## })

    ## convert the intersections list into a vector retaining any NA value
    intersection_vector <- vapply(intersections, function(x) {
      if (length(x) == 0) {
        return(NA_integer_)
      } else {
        return(x[1])
      }
    }, integer(1))

    wq_long_sf <-
      wq_long_sf |>
      mutate(ZoneName = dh_sf$ZoneName[intersection_vector]) 

    ## Test ===================
    ## ggplot() +
    ##   geom_sf(data = dh_sf, aes(fill = ZoneName)) +
    ##   geom_sf(data = wq_long_sf, aes(color = ZoneName)) +
    ##   theme_minimal()
    ## ========================

    ## now we take any that have a ZoneName of NA and find the nearest neighbour
    intersections_1 <-
      wq_long_sf |> filter(is.na(ZoneName)) |>
      st_nearest_feature(dh_sf) |> 
      suppressMessages() |>
      suppressWarnings()
    ## Test ===================
    ## ggplot() +
    ##   geom_sf(data = dh_sf, aes(fill = ZoneName)) +
    ##   geom_sf(data = wq_long_sf |> filter(is.na(ZoneName)) |>
    ##             mutate(ZoneName = dh_sf$ZoneName[intersections_1]), aes(color = ZoneName)) +
    ##   theme_minimal()
    ## ========================
    wq_long_sf_nn <- 
      wq_long_sf |> filter(is.na(ZoneName)) |>
      mutate(ZoneName = dh_sf$ZoneName[intersections_1])

    wq_long_sf1 <-
      wq_long_sf |>
      filter(!is.na(ZoneName)) |>
      bind_rows(wq_long_sf_nn) |>
      ## dplyr::select(-Region) |>
      left_join(data$spatial |>
                  dplyr::select(Region, Zone, ZoneName),
        by = c("ZoneName")) |>
      arrange(Region, Zone, ZoneName, Date, Latitude, Longitude, Measure)

    ## ## Test ===================
    ## ggplot() +
    ##   geom_sf(data = dh_sf, aes(fill = ZoneName)) +
    ##   geom_sf(data = wq_long_sf1, aes(color = ZoneName)) +
    ##   theme_minimal()
    ## ## ========================
    
    data$wq_long <- wq_long_sf1 |>
      st_drop_geometry()
    
  },
  stage_ = 4,
  name_ = "Assign spatial",
  item_ = "process_assign_spatial",
  )
  return(data)
}


##' Convert units
##'
##' Convert units
##' @title Convert units
##' @param data a list containing an item called "wq" that contains
##' a tibble representing the water quality data
##' @return a list containing an item called "wq" that contains
##' a tibble representing the water quality data
##' @author Murray Logan
##' @export
process_convert_units <- function(data) {
  status::status_try_catch(
  {
    ## Convert units (Nox,PO4,NH3)
    data$wq_long <-
      data$wq_long |>
      mutate(Value = Value * as.numeric(as.character(Conversion)))
  },
  stage_ = 4,
  name_ = "Convert units",
  item_ = "process_convert_units",
  )
  return(data)
}

##' Detection limit
##'
##' Detection limit
##' @title Detection limit
##' @param data a list containing an item called "wq" that contains
##' a tibble representing the water quality data
##' @return a list containing an item called "wq" that contains
##' a tibble representing the water quality data
##' @author Murray Logan
##' @export
process_detection_limit <- function(data) {
  status::status_try_catch(
  {
    data$wq_long <-
      data$wq_long |>
      mutate(Flag = NA, Flag = ifelse(Value < DetectionLimit, "LOD", Flag),
        Value = ifelse(!is.na(DetectionLimit) &
                         Value < DetectionLimit, DetectionLimit, Value)) |> 
      mutate(Flag = ifelse(Measure == "DO_PERCENT_saturation" &
                             Value < 50, "DO", Flag),
        Value = ifelse(Measure == "DO_PERCENT_saturation" & Value < 50, NA, Value))
  },
  stage_ = 4,
  name_ = "Detection limit",
  item_ = "process_detection_limit",
  )
  return(data)
}

##' Join hierarchy
##'
##' Join hierarchy
##' @title Join hierarchy
##' @param data a list containing an item called "wq" that contains
##' a tibble representing the water quality data
##' @return a list containing an item called "wq" that contains
##' a tibble representing the water quality data
##' @author Murray Logan
##' @export
process_join_hierarchy <- function(data) {
  status::status_try_catch(
  {
    data$wq_long <-
      data$wq_long |>
      full_join(data$hierarchy, by = c("Measure"),
        relationship = "many-to-many"
        )
  },
  stage_ = 4,
  name_ = "Join hierarchy",
  item_ = "process_join_hierarchy",
  )
  return(data)
}
