
##############################################################################
## +------------------+   +-----------------------+   +-------------------+ ##
## | Zone (Measure)   |==>| Zone (Subindicator    |-->| Site (Indicator)  | ##
## +------------------+   +-----------------------+   +-------------------+ ##
##        |                         ||                                      ##
##        V                         VV                                      ##
## +------------------+   +-----------------------+   +-------------------+ ##
## | Region (Measure) |-->| Region (Subindicator) |-->| Zone (Indicator)  | ##
## +------------------+   +-----------------------+   +-------------------+ ##
##        |                         ||                                      ##
##        V                         VV                                      ##
## +------------------+   +-----------------------+   +-------------------+ ##
## |   WH (Measure)   |   |   WH (Subindicator    |-->|   WH (Indicator)  | ##
## +------------------+   +-----------------------+   +-------+-----------+ ##
##############################################################################

##' Bootstrapp data
##'
##' Bootstrapp the data
##' @title Bootstrapp data
##' @return NULL
##' @author Murray Logan
##' @export
module_boot <- function() {
  status::status_set_stage(stage = 7, title = "Bootstrapping")

  data_idx <-  readRDS(file = paste0(data_path, "/processed/indices/data_idx.rds"))
  data <-  readRDS(file = paste0(data_path, "/processed/data.rds"))

  data$overwrites <- process_overwrites(data$overwrites)
  data$weights_m <- process_weights(data$weights_m)
  data$weights_s <- process_weights(data$weights_s)

  ## Zone/Measure/Source
  data_idx_zone_measure_source_boot <- boot_zone_measure_source(data_idx, data) 
  saveRDS(data_idx_zone_measure_source_boot,
    file = paste0(data_path, "/boot/data_idx_zone_measure_source_boot.rds"))
  saveRDS(data_idx_zone_measure_source_boot$sum,
    file = paste0(data_path, "/boot/data_idx_zone_measure_source_boot_sum.rds"))

  ## Zone/Measure
  data_idx_zone_measure_boot <- boot_zone_measure(data_idx_zone_measure_source_boot, data) 
  saveRDS(data_idx_zone_measure_boot,
    file = paste0(data_path, "/boot/data_idx_zone_measure_boot.rds"))
  saveRDS(data_idx_zone_measure_boot$sum,
    file = paste0(data_path, "/boot/data_idx_zone_measure_boot_sum.rds"))
  figs <- bootstrap_figures(data_idx_zone_measure_boot, data,
    m_level = "Measure", s_level = "Zone")

  ## Zone/Subindicator
  data_idx_zone_subindicator_boot <- boot_zone_subindicator(data_idx_zone_measure_boot, data)
  saveRDS(data_idx_zone_subindicator_boot,
    file = paste0(data_path, "/boot/data_idx_zone_subindicator_boot.rds"))
  saveRDS(data_idx_zone_subindicator_boot$sum,
    file = paste0(data_path, "/boot/data_idx_zone_subindicator_boot_sum.rds"))
  figs <- bootstrap_figures(data_idx_zone_subindicator_boot, data,
    m_level = "Subindicator", s_level = "Zone")
  
  ## Zone/Indicator
  data_idx_zone_indicator_boot <- boot_zone_indicator(data_idx_zone_subindicator_boot, data)
  saveRDS(data_idx_zone_indicator_boot,
    file = paste0(data_path, "/boot/data_idx_zone_indicator_boot.rds"))
  saveRDS(data_idx_zone_indicator_boot$sum,
    file = paste0(data_path, "/boot/data_idx_zone_indicator_boot_sum.rds"))
  figs <- bootstrap_figures(data_idx_zone_indicator_boot, data,
    m_level = "Indicator", s_level = "Zone")

  ## Region/Subindicator
  data_idx_region_subindicator_boot <- boot_region_subindicator(data_idx_zone_subindicator_boot, data)
  saveRDS(data_idx_region_subindicator_boot,
    file = paste0(data_path, "/boot/data_idx_region_subindicator_boot.rds"))
  saveRDS(data_idx_region_subindicator_boot$sum,
    file = paste0(data_path, "/boot/data_idx_region_subindicator_boot_sum.rds"))
  figs <- bootstrap_figures(data_idx_region_subindicator_boot, data,
    m_level = "Subindicator", s_level = "Region")
  
  ## Region/Indicator
  data_idx_region_indicator_boot <- boot_region_indicator(data_idx_region_subindicator_boot, data)
  saveRDS(data_idx_region_indicator_boot,
    file = paste0(data_path, "/boot/data_idx_region_indicator_boot.rds"))
  saveRDS(data_idx_region_indicator_boot$sum,
    file = paste0(data_path, "/boot/data_idx_region_indicator_boot_sum.rds"))
  figs <- bootstrap_figures(data_idx_region_indicator_boot, data,
    m_level = "Indicator", s_level = "Region")

  ## Region/Measure
  data_idx_region_measure_boot <- boot_region_measure(data_idx_zone_measure_boot, data)
  saveRDS(data_idx_region_measure_boot,
    file = paste0(data_path, "/boot/data_idx_region_measure_boot.rds"))
  saveRDS(data_idx_region_measure_boot$sum,
    file = paste0(data_path, "/boot/data_idx_region_measure_boot_sum.rds"))
  figs <- bootstrap_figures(data_idx_region_measure_boot, data,
    m_level = "Measure", s_level = "Region")
  
  ## Wh/Subindicator
  data_idx_wh_subindicator_boot <- boot_wh_subindicator(data_idx_region_subindicator_boot, data)
  saveRDS(data_idx_wh_subindicator_boot,
    file = paste0(data_path, "/boot/data_idx_wh_subindicator_boot.rds"))
  saveRDS(data_idx_wh_subindicator_boot$sum,
    file = paste0(data_path, "/boot/data_idx_wh_subindicator_boot_sum.rds"))
  figs <- bootstrap_figures(data_idx_wh_subindicator_boot, data,
    m_level = "Subindicator", s_level = "WH")

  ## Wh/Indicator
  data_idx_wh_indicator_boot <- boot_wh_indicator(data_idx_wh_subindicator_boot, data)
  saveRDS(data_idx_wh_indicator_boot,
    file = paste0(data_path, "/boot/data_idx_wh_indicator_boot.rds"))
  saveRDS(data_idx_wh_indicator_boot$sum,
    file = paste0(data_path, "/boot/data_idx_wh_indicator_boot_sum.rds"))
  figs <- bootstrap_figures(data_idx_wh_indicator_boot, data,
    m_level = "Indicator", s_level = "WH")

  ## Wh/Measure
  data_idx_wh_measure_boot <- boot_wh_measure(data_idx_region_measure_boot, data)
  saveRDS(data_idx_wh_measure_boot,
    file = paste0(data_path, "/boot/data_idx_wh_measure_boot.rds"))
  saveRDS(data_idx_wh_measure_boot$sum,
    file = paste0(data_path, "/boot/data_idx_wh_measure_boot_sum.rds"))
  figs <- bootstrap_figures(data_idx_wh_measure_boot, data,
    m_level = "Measure", s_level = "WH")
  
}

##' Bootstrap Zone/Measure/Source level
##'
##' Bootstrap Zone/Measure/Source level - this is
##' not an aggregation as such, it is purely producing graphical and
##' tabular summaries of the Zone/Measure/Source level data.
##'
##' There are no weightings applied at this level, however overwrites
##' can apply
##' @title Bootstrap Zone/Measure/Source level
##' @param data_idx a data frame containing the simple indices
##' @return a list containing (dist) full bootstrapp and (sum) summaries 
##' @author Murray Logan
boot_zone_measure_source <- function(data_idx, data) {
  status::status_try_catch(
  {
    boot_zone <- data.frame()
    
    data_boot_zone <- readRDS(file = paste0(data_path, "/processed/indices/data_boot_zone.rds"))
    boot_zone <- rbind(boot_zone, data_boot_zone)

    boot_zone <- boot_zone |>
      mutate(Boot = Index) |>
      dplyr::select(-Index)
    ## As we dont have IndicatorGroup I might remove it
    boot_zone <- boot_zone |>
      dplyr:::select(-IndicatorGroup)

    boot <- data_idx |>
      mutate(Boot = Index) |> 
      DH_Overwrites(data$overwrites, recursive = TRUE) |> 
      mutate(Score = Index, Weight = 1) |>
      RC_boot_accumulate(size = 1000, seed = 123,
        grouping_cols = c("Year", "Component", "Indicator", "Subindicator",
          "Measure", "Region", "Zone", "Source")) 
  },
  stage_ = 7,
  name_ = "Zone/Meaure/Source",
  item_ = "boot_zone_measure_source",
  )
  return(boot)
}

##' Bootstrap Zone/Measure level
##'
##' Bootstrap Zone/Measure level - aggregating over Source to get a
##' Zone/Measure level
##' ##'
##' @title Bootstrap Zone/Measure level
##' @param data_idx a list containing the bootstrap indices (with dist element)
##' @return a list containing (dist) full bootstrapp and (sum)
##'   summaries
##' @author Murray Logan
boot_zone_measure <- function(data_boot, data) {
  status::status_try_catch(
  {
    boot <- data_boot$dist |>
      mutate(Boot = Score) |> 
      DH_Overwrites(data$overwrites, recursive = TRUE) |> 
      mutate(Score = Boot, Weight = 1) |> 
      RC_boot_aggregate(size = 1000,
        grouping_cols = c("Year", "Component", "Indicator", "Subindicator",
          "Region", "Zone", "Measure"), over = "Source")
  },
  stage_ = 7,
  name_ = "Zone/Measure",
  item_ = "boot_zone_measure",
  )
  return(boot)
}

##' Bootstrap Zone/Subindicator level
##'
##' Bootstrap Zone/Subindicator level - aggregating over Source to get a
##' Zone/Subindicator level
##' ##'
##' @title Bootstrap Zone/Subindicator level
##' @param data_idx a list containing the bootstrap indices (with dist element)
##' @return a list containing (dist) full bootstrapp and (sum)
##'   summaries
##' @author Murray Logan
boot_zone_subindicator <- function(data_boot, data) {
  status::status_try_catch(
  {
    boot <- data_boot$dist |>
      mutate(Boot = Score) |> 
      DH_Overwrites(data$overwrites, recursive = TRUE) |> 
      mutate(Score = Boot, Weight = 1) |> 
      RC_boot_aggregate(size = 1000,
        grouping_cols = c("Year", "Component", "Indicator", "Subindicator",
          "Region", "Zone"), over = "Measure")
  },
  stage_ = 7,
  name_ = "Zone/Subindicator",
  item_ = "boot_zone_subindicator",
  )
  return(boot)
}

##' Bootstrap Zone/Indicator level
##'
##' Bootstrap Zone/Indicator level - aggregating over Source to get a
##' Zone/Indicator level
##' ##'
##' @title Bootstrap Zone/Indicator level
##' @param data_idx a list containing the bootstrap indices (with dist element)
##' @return a list containing (dist) full bootstrapp and (sum)
##'   summaries
##' @author Murray Logan
boot_zone_indicator <- function(data_boot, data) {
  status::status_try_catch(
  {
    boot <- data_boot$dist |>
      mutate(Boot = Score) |> 
      DH_Overwrites(data$overwrites, recursive = TRUE) |> 
      mutate(Score = Boot, Weight = 1) |> 
      RC_boot_aggregate(size = 1000,
        grouping_cols = c("Year", "Component", "Indicator",
          "Region", "Zone"), over = "Subindicator")
  },
  stage_ = 7,
  name_ = "Zone/Indicator",
  item_ = "boot_zone_indicator",
  )
  return(boot)
}

##' Bootstrap Region/Subindicator level
##'
##' Bootstrap Region/Subindicator level - aggregating over Source to get a
##' Region/Subindicator level
##' ##'
##' @title Bootstrap Region/Subindicator level
##' @param data_idx a list containing the bootstrap indices (with dist element)
##' @return a list containing (dist) full bootstrapp and (sum)
##'   summaries
##' @author Murray Logan
boot_region_subindicator <- function(data_boot, data) {
  status::status_try_catch(
  {
    boot <- data_boot$dist |>
      mutate(Boot = Score) |> 
      DH_Overwrites(data$overwrites, recursive = TRUE) |> 
      mutate(Score = Boot, Weight = 1) |> 
      RC_boot_aggregate(size = 1000,
        grouping_cols = c("Year", "Component", "Indicator", "Subindicator",
          "Region"), over = "Zone")
  },
  stage_ = 7,
  name_ = "Region/Subindicator",
  item_ = "boot_region_subindicator",
  )
  return(boot)
}

##' Bootstrap Region/Indicator level
##'
##' Bootstrap Region/Indicator level - aggregating over Source to get a
##' Region/Indicator level
##' ##'
##' @title Bootstrap Region/Indicator level
##' @param data_idx a list containing the bootstrap indices (with dist element)
##' @return a list containing (dist) full bootstrapp and (sum)
##'   summaries
##' @author Murray Logan
boot_region_indicator <- function(data_boot, data) {
  status::status_try_catch(
  {
    boot <- data_boot$dist |>
      mutate(Boot = Score) |> 
      DH_Overwrites(data$overwrites, recursive = TRUE) |> 
      mutate(Score = Boot, Weight = 1) |> 
      RC_boot_aggregate(size = 1000,
        grouping_cols = c("Year", "Component", "Indicator", 
          "Region"), over = "Subindicator")
  },
  stage_ = 7,
  name_ = "Region/Indicator",
  item_ = "boot_region_indicator",
  )
  return(boot)
}

##' Bootstrap Region/Measure level
##'
##' Bootstrap Region/Measure level - aggregating over Source to get a
##' Region/Measure level
##' ##'
##' @title Bootstrap Region/Measure level
##' @param data_idx a list containing the bootstrap indices (with dist element)
##' @return a list containing (dist) full bootstrapp and (sum)
##'   summaries
##' @author Murray Logan
boot_region_measure <- function(data_boot, data) {
  status::status_try_catch(
  {
    boot <- data_boot$dist |>
      mutate(Boot = Score) |> 
      DH_Overwrites(data$overwrites, recursive = TRUE) |> 
      mutate(Score = Boot, Weight = 1) |> 
      RC_boot_aggregate(size = 1000,
        grouping_cols = c("Year", "Component", "Indicator", "Subindicator", "Measure",
          "Region"), over = "Zone")
  },
  stage_ = 7,
  name_ = "Region/Measure",
  item_ = "boot_region_measure",
  )
  return(boot)
}


##' Bootstrap WH/Subindicator level
##'
##' Bootstrap WH/Subindicator level - aggregating over Source to get a
##' Wh/Subindicator level
##' ##'
##' @title Bootstrap WH/Subindicator level
##' @param data_idx a list containing the bootstrap indices (with dist element)
##' @return a list containing (dist) full bootstrapp and (sum)
##'   summaries
##' @author Murray Logan
boot_wh_subindicator <- function(data_boot, data) {
  status::status_try_catch(
  {
    boot <- data_boot$dist |>
      mutate(Boot = Score) |> 
      DH_Overwrites(data$overwrites, recursive = TRUE) |> 
      mutate(Score = Boot, Weight = 1) |> 
      RC_boot_aggregate(
        size = 1000,
        grouping_cols = c("Year", "Component", "Indicator", "Subindicator"),
        over = "Region"
      )
  },
  stage_ = 7,
  name_ = "Wh/Subindicator",
  item_ = "boot_wh_subindicator",
  )
  return(boot)
}

##' Bootstrap WH/Indicator level
##'
##' Bootstrap WH/Indicator level - aggregating over Source to get a
##' Wh/Indicator level
##' ##'
##' @title Bootstrap WH/Indicator level
##' @param data_idx a list containing the bootstrap indices (with dist element)
##' @return a list containing (dist) full bootstrapp and (sum)
##'   summaries
##' @author Murray Logan
boot_wh_indicator <- function(data_boot, data) {
  status::status_try_catch(
  {
    boot <- data_boot$dist |>
      mutate(Boot = Score) |> 
      DH_Overwrites(data$overwrites, recursive = TRUE) |> 
      mutate(Score = Boot, Weight = 1) |> 
      RC_boot_aggregate(
        size = 1000,
        grouping_cols = c("Year", "Component", "Indicator"),
        over = "Subindicator"
      )
  },
  stage_ = 7,
  name_ = "Wh/Indicator",
  item_ = "boot_wh_indicator",
  )
  return(boot)
}

##' Bootstrap WH/Measure level
##'
##' Bootstrap WH/Measure level - aggregating over Source to get a
##' Wh/Measure level
##' ##'
##' @title Bootstrap WH/Measure level
##' @param data_idx a list containing the bootstrap indices (with dist element)
##' @return a list containing (dist) full bootstrapp and (sum)
##'   summaries
##' @author Murray Logan
boot_wh_measure <- function(data_boot, data) {
  status::status_try_catch(
  {
    boot <- data_boot$dist |>
      mutate(Boot = Score) |> 
      DH_Overwrites(data$overwrites, recursive = TRUE) |> 
      mutate(Score = Boot, Weight = 1) |> 
      RC_boot_aggregate(
        size = 1000,
        grouping_cols = c("Year", "Component", "Indicator", "Subindicator", "Measure"),
        over = "Region"
      )
  },
  stage_ = 7,
  name_ = "Wh/Measure",
  item_ = "boot_wh_measure",
  )
  return(boot)
}

##' Process overwrites
##'
##' Process overwrites
##' If you want something excluded at the Whole of Harbour level,
##' then dont indicate a ZONE or Site - that means all ZONEs and Sites
##' @title Process overwrites
##' @param overwrites data frame of overwrites
##' @return a data.frame/tibble containing the processed overwrites 
##' @author Murray Logan
##' @export
process_overwrites <- function(overwrites) {
  status::status_try_catch(
  {
    overwrites <-
      overwrites |>
      dplyr::select(everything(), Grade = overwrittenGrade) |>
      dplyr::mutate(Source = as.character(Source)) |>
      dplyr::select(Component, IndicatorGroup, Indicator, Subindicator,
        Measure, Source, Region, Zone, Grade) |>
      dplyr::mutate(Grade = ifelse(is.na(Grade),'-',as.character(Grade)),
        Score = grades2scores(Grade))

    if (nrow(overwrites)>0) overwrites[overwrites == ''] <- NA

    overwrites <- overwrites |> dplyr::arrange(Component,
      IndicatorGroup, Indicator, Subindicator, Measure, Source,
      Grade, Score)
    ## Remove IndicatorGroup since this does not apply to Darwin Harbour
    overwrites <- overwrites |>
      dplyr::select(-IndicatorGroup)
    ## Make the Source consistent
    overwrites <- overwrites |>
      mutate(Source == ifelse(Source == "Discrete", "Discrete", "CFM"))
    overwrites
  },
  stage_ = 7,
  name_ = "Process overwrites",
  item_ = "process_overwrites",
  )
  return(overwrites)
}

##' Process weights
##'
##' Process weights
##' @title Process weights
##' @param weights data frame of weights
##' @return a data.frame/tibble containing the processed weights 
##' @author Murray Logan
##' @export
process_weights <- function(weights) {
  status::status_try_catch(
  {
    weights |>
      dplyr::select(Component, IndicatorGroup,
        Indicator, Subindicator, Measure, Region, Zone, Weight) |>
      dplyr::arrange(Component, IndicatorGroup, Indicator,
        Subindicator, Measure, Weight)
  },
  stage_ = 7,
  name_ = "Process weights",
  item_ = "process_weights",
  )
  return(weights)
}



##' Convert grades to scoress
##'
##' Convert grades to scores
##' @title Convert grades to scores
##' @param x a vector of grades
##' @return a vector of scores
##' @author Murray Logan
##' @export
grades2scores <- function(x) {
  case_when(
    x == 'A' ~ 0.925,
    x == 'B' ~ 0.75,
    x == 'C' ~ 0.575,
    x == 'D' ~ 0.375,
    x == 'E' ~ 0.125,
    .default = NA
  )
}

DH_Overwrites<- function(data, overwrite, recursive = FALSE) {
  if (nrow(overwrite)>0) {
    overwrite <- overwrite %>% dplyr:::rename(Grade1=Grade)
    measures <- c("Indicator", "Subindicator", "Measure", "Source")
    cols.data <- colnames(data)[colnames(data) %in% measures]
    if (recursive==FALSE) {
      other.cols <-measures[is.na(match(measures,cols.data))]
      #print(other.cols)
      ovwts = overwrite %>% dplyr:::filter_(.dots=paste0("!is.na(",cols.data,")"))
      data = data %>% left_join(ovwts, by=cols.data)
      # if (recursive
      data = data %>% dplyr:::mutate(
        Match=
          ifelse(!is.na(Region.y) & as.character(Region.x) == as.character(Region.y),TRUE,
            ifelse(!is.na(Zone.y) & as.character(Zone.x) == as.character(Zone.y),TRUE,
              ifelse(is.na(Region.y) & is.na(Zone.y) & !is.na(Grade1), TRUE,
                ifelse(as.character(Region.x) != as.character(Region.y) | as.character(Zone.x) != as.character(Zone.y), FALSE, 
                  ifelse(!is.na(Grade1) & !is.na(Score),TRUE,FALSE)
                )
              )
            )
          ),
        Match=ifelse(is.na(Match),FALSE,Match),
        Boot=ifelse(!Match,Boot,
          ifelse(Grade1=='-',NA,Score))
        
      )
      data= data %>% dplyr:::select(-Region.y,-Zone.y,-Grade1,-Score) %>% dplyr:::rename(Region=Region.x, Zone=Zone.x, EO=Match)
    } else {
      measures <- c('Indicator','Subindicator','Measure','Source','Region','Zone')
      data$Match=FALSE
      replaces <-NULL
      for (i in 1:nrow(overwrite)) {
        ovwts=overwrite[i,] %>% "["(colSums(!is.na(.)) > 0) #remove columns with missing data from ovwts
        cols.data = cols.ovwts=colnames(ovwts)[colnames(ovwts) %in% measures]
        #cols.data = colnames(ovwts)[colnames(ovwts) %in% measures]
        cols.data = colnames(data)[colnames(data) %in% cols.data]
        if(length(cols.data)==length(cols.ovwts) && all(cols.data %in% cols.ovwts)) {
          data1 = data %>% left_join(ovwts, by=cols.data) %>% ungroup
          data1 = data1 %>% dplyr:::mutate(
            Match=
              #ifelse(exists('Zone') & !is.na(Grade1),TRUE,FALSE),
              ifelse(any(grepl('^Zone$',colnames(data1))) & !is.na(Grade1),TRUE,FALSE),
            Match=ifelse(is.na(Match),FALSE,Match),
            Boot=ifelse(!Match,Boot,
              ifelse(Grade1=='-',NA,Score))
            
          )
          mtchs <- data1$Match==TRUE
          if (any(mtchs)) {
            data[mtchs,'Boot'] <- data1[mtchs,'Boot']
            data$Match[mtchs] <- data1$Match[mtchs]
          }
        }
      }
      data= data %>% dplyr:::rename(EO=Match)
    }
  } else {
    data = data %>% mutate(EO=FALSE)
  }
  data
}

##' Bootstrap figures
##'
##' Bootstrap figures
##' @title Bootstrap figures
##' @param boot data frame of bootstrap scores
##' @return side effect of producing graphs
##' @author Murray Logan
##' @export
bootstrap_figures <- function(boot, dat, m_level = "Measure", s_level = "Zone") {
  status::status_try_catch(
  {
    configurations <- list(
      "Measure" = list(
        "Zone" = list(nests = c("Component", "Indicator", "Subindicator", "Measure", "Region", "Zone"),
          lab_field = "Label",
          fig_path = paste0(output_path, "figures/zones/measures/"), 
          join_by = c("Zone", "Region"),
          fields_to_extract = c("HexColor", "Zone", "Region", "ZoneName", "RegionName")),
        "Region" = list(nests = c("Component", "Indicator", "Subindicator", "Measure", "Region"),
          lab_field = "Label",
          fig_path = paste0(output_path, "figures/regions/measures/"), 
          join_by = c("Region"),
          fields_to_extract = c("HexColor", "Region", "RegionName")),
        "WH" = list(nests = c("Component", "Indicator", "Subindicator", "Measure"),
          lab_field = "Label",
          fig_path = paste0(output_path, "figures/wh/measures/"), 
          join_by = c(),
          fields_to_extract = character(0))
      ),
      "Subindicator" = list(
        "Zone" = list(nests = c("Component", "Indicator", "Subindicator", "Region", "Zone"),
          lab_field = "Subindicator",
          fig_path = paste0(output_path, "figures/zones/subindicators/"), 
          join_by = c("Zone", "Region"),
          fields_to_extract = c("HexColor", "Zone", "Region", "ZoneName", "RegionName")),
        "Region" = list(nests = c("Component", "Indicator", "Subindicator", "Region"),
          lab_field = "Subindicator",
          fig_path = paste0(output_path, "figures/regions/subindicators/"), 
          join_by = c("Region"),
          fields_to_extract = c("HexColor", "Region", "RegionName")),
        "WH" = list(nests = c("Component", "Indicator", "Subindicator"),
          lab_field = "Subindicator",
          fig_path = paste0(output_path, "figures/wh/subindicators/"), 
          join_by = c(),
          fields_to_extract = character(0))
      ),
      "Indicator" = list(
        "Zone" = list(nests = c("Component", "Region", "Zone", "Indicator"),
          lab_field = "Indicator",
          fig_path = paste0(output_path, "figures/zones/indicators/"), 
          join_by = c("Zone", "Region"),
          fields_to_extract = c("HexColor", "Zone", "Region", "ZoneName", "RegionName")),
        "Region" = list(nests = c("Component", "Indicator", "Region"),
          lab_field = "Indicator",
          fig_path = paste0(output_path, "figures/regions/indicators/"), 
          join_by = c("Region"),
          fields_to_extract = c("HexColor", "Region", "RegionName")),
        "WH" = list(nests = c("Component", "Indicator"),
          lab_field = "Indicator",
          fig_path = paste0(output_path, "figures/wh/indicators/"), 
          join_by = c(),
          fields_to_extract = character(0))
      ),
      "Component" = list(
        "Zone" = list(nests = c("Component", "Region", "Zone"),
          lab_field = "Component",
          fig_path = paste0(output_path, "figures/zones/components/"), 
          join_by = c("Zone", "Region"),
          fields_to_extract = c("HexColor", "Zone", "Region", "ZoneName", "RegionName")),
        "Region" = list(nests = c("Component", "Region"),
          lab_field = "Component",
          fig_path = paste0(output_path, "figures/regions/components/"), 
          join_by = c("Region"),
          fields_to_extract = c("HexColor", "Region", "RegionName")),
        "WH" = list(nests = c("Component"),
          lab_field = "Component",
          fig_path = paste0(output_path, "figures/wh/components/"), 
          join_by = c(),
          fields_to_extract = character(0))
      )
    )

    # Retrieve the configuration based on m_level and s_level
    if (m_level %in% names(configurations) && s_level %in% names(configurations[[m_level]])) {
      config <- configurations[[m_level]][[s_level]]
      lab_field <- config$lab_field
      nests <- config$nests
      join_by <- config$join_by
      fields_to_extract <- config$fields_to_extract[-1]
      fig_path <- config$fig_path
      all_fields <-  c("Component", "Indicator", "Subindicator", "Measure", "Region", "Zone")
      fields_to_add <- all_fields[!all_fields %in% nests]
    } else {
      stop("Invalid m_level or s_level")
    }

    if (m_level == "Measure") {
      figs <-
        boot$dist |>
        left_join(
          dat$guidelines |>
            dplyr::select(Measure, Label) |>
            distinct(),
          by = "Measure", relationship = "many-to-one"
        )
    } else {
      figs <-
        boot$dist 
    }


    figs <-
      figs |>
      {\(.)
        if (length(fields_to_extract) > 0) {
          . |> 
            left_join(
              dat$spatial |>
                dplyr::select(!!fields_to_extract) |>
                distinct(),
              by = join_by
            ) 
        } else .
      }() |> 
      ## left_join(
      ##   dat$spatial |>
      ##     dplyr::select(!!fields_to_extract) |>
      ##     distinct(),
      ##   by = join_by
      ## ) |>
      mutate(ZoneName = if ("ZoneName" %in% fields_to_extract) {
        paste0("(", Zone, ")~", gsub(" ", "~", ZoneName))
      } else {
        NA
      }) |>
      mutate(Group = if ("ZoneName" %in% fields_to_extract) {
        paste0(ZoneName, "~", !!sym(lab_field))
      } else {
        paste0(!!sym(lab_field))
      }) |>
      mutate(Group = gsub(" ", "~", Group))

      ## figs <-
      ##   figs |>
      ##   left_join(dat$spatial, by = join_by) |>
      ##   mutate(ZoneName = paste0("(", Zone, ")~", gsub(" ", "~", ZoneName))) |>
      ##   mutate(Group = paste0(ZoneName, "~", Label))

      figs <-
        figs |>
        nest(.by = !!nests) |>
        ## nest(.by = c(Component, Indicator, Subindicator, Region, Zone, Measure)) |>
        mutate(p = map(data,
          .f = ~ {
            summs <- .x |>
              summarise(
                Mean = mean(Score),
                Median = median(Score),
                sd = sd(Score),
                lower = quantile(Score, p = 0.025, na.rm = TRUE),
                upper = quantile(Score, p = 0.975, na.rm = TRUE)
              ) |>
              mutate(lab = sprintf(
                "Mean: %2.3f, Median: %2.3f [%2.3f,%2.3f]",
                Mean * 1, Median, lower, upper
              ))
            .x |>
              ggplot(aes(x = Score)) +
              scale_y_continuous("Density", limits = c(0, 1.1)) +
              stat_bin(
                geom = "bar", breaks = seq(-0.05, 1.05, by = 0.1),
                aes(y = ..ncount..)
              ) +
              ## geom_density(aes(fill = Label), show.legend = FALSE) +
              geom_text(
                data = summs,
                aes(
                  y = 1.05, x = 0.00,
                  label = lab
                ),
                size = 3, vjust = 0.5, hjust = 0
              ) +
              facet_wrap(~Group, scales = "free", labeller = label_parsed) +
              theme_bw() +
              coord_cartesian(expand = FALSE) +
              theme(
                axis.text.y = element_blank(),
                axis.title.y = element_blank(),
                panel.border = element_rect(fill = NA),
                panel.background = element_rect(fill = "lightblue"),
                plot.margin = unit(c(0.1, 0.1, 0.1, 1), "lines")
              )
          }
        )) |>
        (\(.) `[<-`(., fields_to_add, value = " "))() |> 
        ## mutate(!!fields_to_add := " ") |>
        rowwise() |>
        mutate(nm = paste0(
          fig_path,
          "boot_fig_",
          paste(across(all_of(all_fields)), collapse = "_"),
          ## paste(!!!syms(all_fields), collapse = "_"),
          ".png"
        )) |>
        ungroup() |> 
        mutate(plot = nm) |>
        suppressWarnings() |>
        suppressMessages()
    
    walk2(.x = figs$nm, .y = figs$p,
      .f = ~ {
        ggsave(
          file = .x,
          plot = .y,
          height = 3, width = 3
        )
      }
      )
      figs <- figs |> dplyr::select(-p)
  },
  stage_ = 7,
  name_ = "Bootstrap figures",
  item_ = "bootstrap_figures",
  )
  return(figs)
}

