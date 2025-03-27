##' Prepare spatial data
##'
##' This module prepares the spatial data for the water quality data.
##' @title Prepare spatial data
##' @return NULL
##' @author Murray Logan
##' @export
module_prepare_spatial <- function() {
  status::status_set_stage(stage = 3, title = "Prepare spatial data")

  shapefiles <- get_individual_shapefiles()

  ## combine the shapefiles (after splitting Shoal Bay into two parts)
  dh_sf <- combine_shapefiles(shapefiles)

  ## save the Darwin Harbour polygons
  saveRDS(dh_sf, file = paste0(data_path, "processed/dh_sf.rds"))
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


##' Get individual shapefiles
##'
##' This function gets individual shapefiles
##' @title Get individual shapefiles
##' @author Murray Logan
##' @return NULL
##' @export
##' @examples
##' get_individual_shapefiles()
get_individual_shapefiles <- function() {
  status::status_try_catch(
  {
    dh_sf <- st_read(paste0(params_path, "GIS/", "RCZ_rev24.shp"), quiet = TRUE) |>
      ## mutate(subzone = Zone_Name) |>
      dplyr::select(ZoneName = Zone_Name, geometry) |>
      mutate(ZoneName = case_when(
        ZoneName == "Middle Arm" ~ "Blackmore River",
        ZoneName == "Central Harbour" ~ "Middle Harbour",
        .default = ZoneName
      ))
    
    sb_upper_sf <- st_read(paste0(params_path, "GIS/", "SBZone_upper.shp"), quiet = TRUE) |>
      mutate(Zone_Name = "Shoal Bay Upper") |> 
      dplyr::select(ZoneName = Zone_Name, geometry) 

    mh_upper_sf <- st_read(paste0(params_path, "GIS/", "Middle_Harbour_Upper.shp"), quiet = TRUE) |>
      mutate(Zone_Name = "Middle Harbour Upper") |> 
      dplyr::select(ZoneName = Zone_Name, geometry) 
  },
  stage_ = 3,
  name_ = "Get individual shapefiles",
  item_ = "get_individual_shapefiles"
  )
  return(list(dh_sf = dh_sf, sb_upper_sf = sb_upper_sf, mh_upper_sf = mh_upper_sf))
}

##' Combine shapefiles
##'
##' This function combines shapefiles
##' @title Combine shapefiles
##' @author Murray Logan
##' @return NULL
##' @export
combine_shapefiles <- function(shapefiles) {
  status::status_try_catch(
  {
    sf_use_s2(FALSE)
    dh_sf <- shapefiles$dh_sf |>
      st_difference(shapefiles$sb_upper_sf |>
        st_buffer(0.0001)) |>
      dplyr::select(ZoneName, geometry) |>
      rbind(shapefiles$sb_upper_sf) |>
      st_difference(shapefiles$mh_upper_sf |>
        st_buffer(0.0001)) |>
      dplyr::select(-any_of("ZoneName.1")) |> 
      rbind(shapefiles$mh_upper_sf) |>
      suppressWarnings() |>
      suppressMessages()
    sf_use_s2(TRUE)
  },
  stage_ = 3,
  name_ = "Combine shapefiles",
  item_ = "combine_shapefiles"
  )
  return(dh_sf)
}

   ##  ggplot() + geom_sf(data = dh_sf, aes(fill = Zone_Name)) + theme_bw()

   ## dh_sf 
   ##  shapefls <- tribble(
   ##    ~filename,             ~name,
   ##    "blackmore_mask.shp",  "Blackmore River",
   ##    "east_arm_mask.shp",   "East Arm",
   ##    "elizabeth_mask.shp",  "Elizabeth River",
   ##    "middleharbour_mask.shp", "Middle Harbour",
   ##    "outerharbour_mask.shp", "Outer Harbour",
   ##    "shoalbay_mask.shp", "Shoal Bay",
   ##    "westarm_mask.shp", "Western Arm",
   ##    "SBZone_upper.shp", "Shoal Bay Upper",
   ##    "Buffmask_ext.shp", "Buffalo Creek",
   ##    "Mask_Myrmidon.shp", "Myrmidon Creek")

   ##  shps <- lapply(1:nrow(shapefls),
   ##    function(x) {
   ##      sf::st_read(paste0(params_path, "GIS/", shapefls$filename[x]),
   ##        quiet = TRUE) |>
   ##        mutate(name = shapefls$name[x]) |>
   ##        dplyr::select(name, geometry) |> 
   ##        st_transform(crs = 4283)
   ##    })
   ##  shps <- do.call("rbind",shps) 

   ##  ggplot(shps) + geom_sf(aes(fill = name)) + theme_bw()
