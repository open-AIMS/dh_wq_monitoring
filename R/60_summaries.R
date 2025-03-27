##' Summaries
##'
##' Summaries
##' @title Summaries
##' @return NULL
##' @author Murray Logan
##' @export
module_summaries <- function() {
  status::status_set_stage(stage = 8, title = "Summaries")

  data <-  readRDS(file = paste0(data_path, "/processed/data.rds"))

  ## Compile report card scores
  ## saved to data_path/summaries/report_card_scores.rds
  report_card_scores <- compile_scores()

  scores <- tribble(
    ~s_scale, ~m_scale, ~extra, ~file
  )

  ## Zone/Measure/Source level
  scores <- bind_rows(
    scores,
    data.frame(
      s_scale = "Zone", m_scale = "Measure", extra = "Source",
      file = tbl_zone_measure_source(report_card_scores, data)
    )
  )

  ## Zone/Measure/ level
  scores <- bind_rows(
    scores,
    data.frame(
      s_scale = "Zone", m_scale = "Measure", extra = "",
      file = tbl_zone_measure(report_card_scores, data)
    )
  )

  ## Zone/Subindicator/ level
  scores <- bind_rows(
    scores,
    data.frame(
      s_scale = "Zone", m_scale = "Subindicator", extra = "",
      file = tbl_zone_subindicator(report_card_scores, data)
    )
  )
  
  ## Zone/Indicator/ level
  scores <- bind_rows(
    scores,
    data.frame(
      s_scale = "Zone", m_scale = "Indicator", extra = "",
      file = tbl_zone_indicator(report_card_scores, data)
    )
  )

  ## Region/Subindicator/ level
  scores <- bind_rows(
    scores,
    data.frame(
      s_scale = "Region", m_scale = "Subindicator", extra = "",
      file = tbl_region_subindicator(report_card_scores, data)
    )
  )

  ## Region/Indicator/ level
  scores <- bind_rows(
    scores,
    data.frame(
      s_scale = "Region", m_scale = "Indicator", extra = "",
      file = tbl_region_indicator(report_card_scores, data)
    )
  )

  ## WH/Subindicator/ level
  scores <- bind_rows(
    scores,
    data.frame(
      s_scale = "Wh", m_scale = "Subindicator", extra = "",
      file = tbl_wh_subindicator(report_card_scores, data)
    )
  )

  ## WH/Indicator/ level
  scores <- bind_rows(
    scores,
    data.frame(
      s_scale = "Wh", m_scale = "Indicator", extra = "",
      file = tbl_wh_indicator(report_card_scores, data)
    )
  )
  
}

generate_grades <- function(x) {
  case_when(
    is.na(x) ~ NA_character_,
    x >= 0.85 ~ "A",
    x >= 0.65 ~ "B",
    x >= 0.5 ~ "C",
    x >= 0.25 ~ "D",
    TRUE ~ "E"
  )
}

##' Compile scores
##'
##' Compile scores
##' @title Compile scores
##' @return a tibble of the compiled scores
##' @author Murray Logan
compile_scores <- function() {
  status::status_try_catch(
  {
    data_idx_zone_measure_source_boot_sum <-
      readRDS(file = paste0(data_path, "/boot/data_idx_zone_measure_source_boot_sum.rds"))
    data_idx_zone_measure_boot_sum <-
      readRDS(file = paste0(data_path, "/boot/data_idx_zone_measure_boot_sum.rds"))
    data_idx_zone_subindicator_boot_sum <-
      readRDS(file = paste0(data_path, "/boot/data_idx_zone_subindicator_boot_sum.rds"))
    data_idx_zone_indicator_boot_sum <-
      readRDS(file = paste0(data_path, "/boot/data_idx_zone_indicator_boot_sum.rds"))
    ## data_idx_zone_component_boot_sum <-
    ##   readRDS(file = paste0(data_path, "/boot/data_idx_zone_component_boot_sum.rds"))
    data_idx_region_measure_boot_sum <-
      readRDS(file = paste0(data_path, "/boot/data_idx_region_measure_boot_sum.rds"))
    data_idx_region_subindicator_boot_sum <-
      readRDS(file = paste0(data_path, "/boot/data_idx_region_subindicator_boot_sum.rds"))
    data_idx_region_indicator_boot_sum <-
      readRDS(file = paste0(data_path, "/boot/data_idx_region_indicator_boot_sum.rds"))
    data_idx_wh_measure_boot_sum <-
      readRDS(file = paste0(data_path, "/boot/data_idx_wh_measure_boot_sum.rds"))
    data_idx_wh_subindicator_boot_sum <-
      readRDS(file = paste0(data_path, "/boot/data_idx_wh_subindicator_boot_sum.rds"))
    data_idx_wh_indicator_boot_sum <-
      readRDS(file = paste0(data_path, "/boot/data_idx_wh_indicator_boot_sum.rds"))

    report_card_scores <-
      data_idx_zone_measure_source_boot_sum |>
      mutate(m_level = "Measure_Source", s_level = "Zone") |>
      dplyr::rename(Boot.Mean = Boot.mean) |> 
      bind_rows(data_idx_zone_measure_boot_sum |>
          mutate(m_level = "Measure", s_level = "Zone"))|>
      bind_rows(data_idx_zone_subindicator_boot_sum |>
                  mutate(m_level = "Subindicator", s_level = "Zone")) |>
      bind_rows(data_idx_zone_indicator_boot_sum |>
                  mutate(m_level = "Indicator", s_level = "Zone")) |>
      bind_rows(data_idx_region_measure_boot_sum |>
                  mutate(m_level = "Measure", s_level = "Region")) |>
      bind_rows(data_idx_region_subindicator_boot_sum |>
                  mutate(m_level = "Subindicator", s_level = "Region")) |>
      bind_rows(data_idx_region_indicator_boot_sum |>
                  mutate(m_level = "Indicator", s_level = "Region")) |>
      bind_rows(data_idx_wh_measure_boot_sum |>
                  mutate(m_level = "Measure", s_level = "WH")) |>
      bind_rows(data_idx_wh_subindicator_boot_sum |>
                  mutate(m_level = "Subindicator", s_level = "WH")) |>
      bind_rows(data_idx_wh_indicator_boot_sum |>
                  mutate(m_level = "Indicator", s_level = "WH")) 

    saveRDS(report_card_scores, file = paste0(data_path, "/summaries/report_card_scores.rds"))
  },
  stage_ = 8,
  name_ = "Compile scores",
  item_ = "summaries_compile_scores",
  )
  return(report_card_scores)
}

tbl_zone_measure_source <- function(report_card_scores, data) {
  status::status_try_catch(
  {
    scores <- 
      report_card_scores |>
      filter(m_level == "Measure_Source", s_level == "Zone") |>
      mutate(
        grade = generate_grades(Boot.Mean),
        grade = ifelse(is.na(grade), " ", grade),
        CI = sprintf("[%1.2f, %1.2f]", Lower, Upper)
      ) |>
      left_join(
        data$spatial |>
          dplyr::select(Region, RegionName, Zone, ZoneName),
        by = c("Region", "Zone")
      ) |>
      mutate(
        region_label = paste0(Region, ": ", RegionName),
        zone_label = paste0(Zone, ": ", ZoneName)
      ) |>
      dplyr::select(-RegionName, -ZoneName) |>
      tidyr::pivot_longer(
        cols = c(Lower, Upper, Boot.Mean, grade, CI),
        names_to = "stat",
        values_to = "values",
        values_transform = as.character
      ) |>
      mutate(stat = factor(stat, levels = unique(stat))) |>
      arrange(Component, Indicator, Subindicator, Measure, Source, stat) |>
      tidyr::unite("group", Subindicator, Measure, Source, stat, sep = "__") |>
      mutate(group = factor(group, levels = unique(group))) |>
      tidyr::pivot_wider(names_from = group, values_from = values) 
    file_path <- paste0(data_path, "/summaries/zone_measure_source_scores.rds")
    saveRDS(scores, file = file_path)
  },
  stage_ = 8,
  name_ = "Zone/Measure/Source scores",
  item_ = "summaries_zone_measure_source",
  )
  return(file_path)
}

tbl_zone_measure <- function(report_card_scores, data) {
  status::status_try_catch(
  {
    scores <- 
      report_card_scores |>
      filter(m_level == "Measure", s_level == "Zone") |>
      mutate(
        grade = generate_grades(Boot.Mean),
        grade = ifelse(is.na(grade), " ", grade),
        CI = sprintf("[%1.2f, %1.2f]", Lower, Upper)
      ) |>
      left_join(
        data$spatial |>
          dplyr::select(Region, RegionName, Zone, ZoneName),
        by = c("Region", "Zone")
      ) |>
      mutate(
        region_label = paste0(Region, ": ", RegionName),
        zone_label = paste0(Zone, ": ", ZoneName)
      ) |>
      dplyr::select(-RegionName, -ZoneName) |>
      tidyr::pivot_longer(
        cols = c(Lower, Upper, Boot.Mean, grade, CI),
        names_to = "stat",
        values_to = "values",
        values_transform = as.character
      ) |>
      mutate(stat = factor(stat, levels = unique(stat))) |>
      arrange(Component, Indicator, Subindicator, Measure, stat) |>
      tidyr::unite("group", Subindicator, Measure, stat, sep = "__") |>
      mutate(group = factor(group, levels = unique(group))) |>
      tidyr::pivot_wider(names_from = group, values_from = values) 
    file_path <- paste0(data_path, "/summaries/zone_measure_scores.rds")
    saveRDS(scores, file = file_path)
  },
  stage_ = 8,
  name_ = "Zone/Measure scores",
  item_ = "summaries_zone_measure",
  )
  return(file_path)
}

tbl_zone_subindicator <- function(report_card_scores, data) {
  status::status_try_catch(
  {
    scores <- 
      report_card_scores |>
      filter(m_level == "Subindicator", s_level == "Zone") |>
      mutate(
        grade = generate_grades(Boot.Mean),
        grade = ifelse(is.na(grade), " ", grade),
        CI = sprintf("[%1.2f, %1.2f]", Lower, Upper)
      ) |>
      left_join(
        data$spatial |>
          dplyr::select(Region, RegionName, Zone, ZoneName),
        by = c("Region", "Zone")
      ) |>
      mutate(
        region_label = paste0(Region, ": ", RegionName),
        zone_label = paste0(Zone, ": ", ZoneName)
      ) |>
      dplyr::select(-RegionName, -ZoneName) |>
      tidyr::pivot_longer(
        cols = c(Lower, Upper, Boot.Mean, grade, CI),
        names_to = "stat",
        values_to = "values",
        values_transform = as.character
      ) |>
      mutate(stat = factor(stat, levels = unique(stat))) |>
      arrange(Component, Indicator, Subindicator, stat) |>
      tidyr::unite("group", Subindicator, stat, sep = "__") |>
      mutate(group = factor(group, levels = unique(group))) |>
      tidyr::pivot_wider(names_from = group, values_from = values) 
    file_path <- paste0(data_path, "/summaries/zone_subindicator_scores.rds")
    saveRDS(scores, file = file_path)
  },
  stage_ = 8,
  name_ = "Zone/Subindicator scores",
  item_ = "summaries_zone_subindicator",
  )
  return(file_path)
}

tbl_zone_indicator <- function(report_card_scores, data) {
  status::status_try_catch(
  {
    scores <- 
      report_card_scores |>
      filter(m_level == "Indicator", s_level == "Zone") |>
      mutate(
        grade = generate_grades(Boot.Mean),
        grade = ifelse(is.na(grade), " ", grade),
        CI = sprintf("[%1.2f, %1.2f]", Lower, Upper)
      ) |>
      left_join(
        data$spatial |>
          dplyr::select(Region, RegionName, Zone, ZoneName),
        by = c("Region", "Zone")
      ) |>
      mutate(
        region_label = paste0(Region, ": ", RegionName),
        zone_label = paste0(Zone, ": ", ZoneName)
      ) |>
      dplyr::select(-RegionName, -ZoneName) |>
      tidyr::pivot_longer(
        cols = c(Lower, Upper, Boot.Mean, grade, CI),
        names_to = "stat",
        values_to = "values",
        values_transform = as.character
      ) |>
      mutate(stat = factor(stat, levels = unique(stat))) |>
      arrange(Component, Indicator , stat) |>
      tidyr::unite("group", Indicator, stat, sep = "__") |>
      mutate(group = factor(group, levels = unique(group))) |>
      tidyr::pivot_wider(names_from = group, values_from = values) 
    file_path <- paste0(data_path, "/summaries/zone_indicator_scores.rds")
    saveRDS(scores, file = file_path)
  },
  stage_ = 8,
  name_ = "Zone/Indicator scores",
  item_ = "summaries_zone_indicator",
  )
  return(file_path)
}

tbl_region_subindicator <- function(report_card_scores, data) {
  status::status_try_catch(
  {
    scores <- 
      report_card_scores |>
      filter(m_level == "Subindicator", s_level == "Region") |>
      mutate(
        grade = generate_grades(Boot.Mean),
        grade = ifelse(is.na(grade), " ", grade),
        CI = sprintf("[%1.2f, %1.2f]", Lower, Upper)
      ) |>
      left_join(
        data$spatial |>
          dplyr::select(Region, RegionName) |> distinct(),
        by = c("Region")
      ) |>
      mutate(
        region_label = paste0(Region, ": ", RegionName)
      ) |>
      dplyr::select(-RegionName) |>
      tidyr::pivot_longer(
        cols = c(Lower, Upper, Boot.Mean, grade, CI),
        names_to = "stat",
        values_to = "values",
        values_transform = as.character
      ) |>
      mutate(stat = factor(stat, levels = unique(stat))) |>
      arrange(Component, Indicator, Subindicator, stat) |>
      tidyr::unite("group", Subindicator, stat, sep = "__") |>
      mutate(group = factor(group, levels = unique(group))) |>
      tidyr::pivot_wider(names_from = group, values_from = values) 
    file_path <- paste0(data_path, "/summaries/region_subindicator_scores.rds")
    saveRDS(scores, file = file_path)
  },
  stage_ = 8,
  name_ = "Region/Subindicator scores",
  item_ = "summaries_region_subindicator",
  )
  return(file_path)
}

tbl_region_indicator <- function(report_card_scores, data) {
  status::status_try_catch(
  {
    scores <- 
      report_card_scores |>
      filter(m_level == "Indicator", s_level == "Region") |>
      mutate(
        grade = generate_grades(Boot.Mean),
        grade = ifelse(is.na(grade), " ", grade),
        CI = sprintf("[%1.2f, %1.2f]", Lower, Upper)
      ) |>
      left_join(
        data$spatial |>
          dplyr::select(Region, RegionName) |> distinct(),
        by = c("Region")
      ) |>
      mutate(
        region_label = paste0(Region, ": ", RegionName)
      ) |>
      dplyr::select(-RegionName) |>
      tidyr::pivot_longer(
        cols = c(Lower, Upper, Boot.Mean, grade, CI),
        names_to = "stat",
        values_to = "values",
        values_transform = as.character
      ) |>
      mutate(stat = factor(stat, levels = unique(stat))) |>
      arrange(Component, Indicator, stat) |>
      tidyr::unite("group", Indicator, stat, sep = "__") |>
      mutate(group = factor(group, levels = unique(group))) |>
      tidyr::pivot_wider(names_from = group, values_from = values) 
    file_path <- paste0(data_path, "/summaries/region_indicator_scores.rds")
    saveRDS(scores, file = file_path)
  },
  stage_ = 8,
  name_ = "Region/Indicator scores",
  item_ = "summaries_region_indicator",
  )
  return(file_path)
}

tbl_wh_subindicator <- function(report_card_scores, data) {
  status::status_try_catch(
  {
    scores <- 
      report_card_scores |>
      filter(m_level == "Subindicator", s_level == "WH") |>
      mutate(
        grade = generate_grades(Boot.Mean),
        grade = ifelse(is.na(grade), " ", grade),
        CI = sprintf("[%1.2f, %1.2f]", Lower, Upper)
      ) |>
      tidyr::pivot_longer(
        cols = c(Lower, Upper, Boot.Mean, grade, CI),
        names_to = "stat",
        values_to = "values",
        values_transform = as.character
      ) |>
      mutate(stat = factor(stat, levels = unique(stat))) |>
      arrange(Component, Indicator, Subindicator, stat) |>
      tidyr::unite("group", Subindicator, stat, sep = "__") |>
      mutate(group = factor(group, levels = unique(group))) |>
      tidyr::pivot_wider(names_from = group, values_from = values) 
    file_path <- paste0(data_path, "/summaries/wh_subindicator_scores.rds")
    saveRDS(scores, file = file_path)
  },
  stage_ = 8,
  name_ = "WH/Subindicator scores",
  item_ = "summaries_wh_subindicator",
  )
  return(file_path)
}

tbl_wh_indicator <- function(report_card_scores, data) {
  status::status_try_catch(
  {
    scores <- 
      report_card_scores |>
      filter(m_level == "Indicator", s_level == "WH") |>
      mutate(
        grade = generate_grades(Boot.Mean),
        grade = ifelse(is.na(grade), " ", grade),
        CI = sprintf("[%1.2f, %1.2f]", Lower, Upper)
      ) |>
      tidyr::pivot_longer(
        cols = c(Lower, Upper, Boot.Mean, grade, CI),
        names_to = "stat",
        values_to = "values",
        values_transform = as.character
      ) |>
      mutate(stat = factor(stat, levels = unique(stat))) |>
      arrange(Component, Indicator, stat) |>
      tidyr::unite("group", Indicator, stat, sep = "__") |>
      mutate(group = factor(group, levels = unique(group))) |>
      tidyr::pivot_wider(names_from = group, values_from = values) 
    file_path <- paste0(data_path, "/summaries/wh_indicator_scores.rds")
    saveRDS(scores, file = file_path)
  },
  stage_ = 8,
  name_ = "WH/Indicator scores",
  item_ = "summaries_wh_indicator",
  )
  return(file_path)
}
