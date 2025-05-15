##' Summaries
##'
##' Summaries
##' @title Summaries
##' @return NULL
##' @author Murray Logan
##' @export
module_summaries <- function() {
  status::status_set_stage(stage = 8, title = "Summaries")

  ## data <-  readRDS(file = paste0(data_path, "/processed/data.rds"))
  data <-  summaries_load_data(file = paste0(data_path, "/processed/data.rds"))

  ## Compile report card scores
  ## saved to data_path/summaries/report_card_scores.rds
  report_card_scores <- compile_scores()

  ## Collate score files
  scores_files <- collate_score_files(report_card_scores, data)
  
  ## Generate trend plots
  report_card_trend_plots <- generate_trend_plots(report_card_scores, data)
  ## Save trend plot metadata
  saveRDS(report_card_trend_plots |> dplyr::select(-trend_plot, -data),
    file = paste0(data_path, "/summaries/report_card_trend_plots.rds")
  )
  
  ## Calculate effects 
  report_card_effects <- calculate_effects(scores_files)

  ## Generate effects plots
  report_card_effects_plots <- generate_effects_plots(report_card_effects, data)
  saveRDS(report_card_effects_plots |>
            dplyr::select(-annual_effects, -annual_effects_sum,
              -period_effects, -period_effects_sum,
              -annual_effects_plots, -period_effects_plots),
    file = paste0(data_path, "/summaries/report_card_effects_plots.rds")
  )

  ## Stack plots together
  ## report_card_trend_plots$trend_plot[[2]] /
  ## (report_card_effects_plots$annual_effects_plots[[2]] +
  ## report_card_effects_plots$period_effects_plots[[2]])
  
}


summaries_load_data <- function(file = paste0(data_path, "processed/data.rds")) {
  status::status_try_catch(
  {
    data <- readRDS(file)
  },
  stage_ = 8,
  name_ = "Load data",
  item_ = "summaries_load_data",
  )
  return(data)
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


##' Collate score files
##'
##' Collate score files
##' @title Collate score files
##' @param report_card_scores a tibble of the compiled scores
##' @param data a list of data frames
##' @return a tibble of the collated score files
##' @author Murray Logan
collate_score_files <- function(report_card_scores, data) {
  scores <- tribble(
    ~s_scale, ~m_scale, ~extra, ~file, ~boot_file,
  )

  ## Zone/Measure/Source level
  scores <- bind_rows(
    scores,
    data.frame(
      s_scale = "Zone", m_scale = "Measure", extra = "Source",
      file = tbl_zone_measure_source(report_card_scores, data),
      boot_file = paste0(data_path, "/boot/data_idx_zone_measure_source_boot.rds")
    )
  )

  ## Zone/Measure/ level
  scores <- bind_rows(
    scores,
    data.frame(
      s_scale = "Zone", m_scale = "Measure", extra = "",
      file = tbl_zone_measure(report_card_scores, data),
      boot_file = paste0(data_path, "/boot/data_idx_zone_measure_boot.rds")
    )
  )

  ## Zone/Subindicator/ level
  scores <- bind_rows(
    scores,
    data.frame(
      s_scale = "Zone", m_scale = "Subindicator", extra = "",
      file = tbl_zone_subindicator(report_card_scores, data),
      boot_file = paste0(data_path, "/boot/data_idx_zone_subindicator_boot.rds")
    )
  )
  
  ## Zone/Indicator/ level
  scores <- bind_rows(
    scores,
    data.frame(
      s_scale = "Zone", m_scale = "Indicator", extra = "",
      file = tbl_zone_indicator(report_card_scores, data),
      boot_file = paste0(data_path, "/boot/data_idx_zone_indicator_boot.rds")
    )
  )

  ## Region/Measure/ level
  scores <- bind_rows(
    scores,
    data.frame(
      s_scale = "Region", m_scale = "Measure", extra = "",
      file = tbl_region_measure(report_card_scores, data),
      boot_file = paste0(data_path, "/boot/data_idx_region_measure_boot.rds")
    )
  )

  ## Region/Subindicator/ level
  scores <- bind_rows(
    scores,
    data.frame(
      s_scale = "Region", m_scale = "Subindicator", extra = "",
      file = tbl_region_subindicator(report_card_scores, data),
      boot_file = paste0(data_path, "/boot/data_idx_region_subindicator_boot.rds")
    )
  )

  ## Region/Indicator/ level
  scores <- bind_rows(
    scores,
    data.frame(
      s_scale = "Region", m_scale = "Indicator", extra = "",
      file = tbl_region_indicator(report_card_scores, data),
      boot_file = paste0(data_path, "/boot/data_idx_region_indicator_boot.rds")
    )
  )

  ## WH/Measure/ level
  scores <- bind_rows(
    scores,
    data.frame(
      s_scale = "Wh", m_scale = "Measure", extra = "",
      file = tbl_wh_measure(report_card_scores, data),
      boot_file = paste0(data_path, "/boot/data_idx_wh_measure_boot.rds")
    )
  )

  ## WH/Subindicator/ level
  scores <- bind_rows(
    scores,
    data.frame(
      s_scale = "Wh", m_scale = "Subindicator", extra = "",
      file = tbl_wh_subindicator(report_card_scores, data),
      boot_file = paste0(data_path, "/boot/data_idx_wh_subindicator_boot.rds")
    )
  )

  ## WH/Indicator/ level
  scores <- bind_rows(
    scores,
    data.frame(
      s_scale = "Wh", m_scale = "Indicator", extra = "",
      file = tbl_wh_indicator(report_card_scores, data),
      boot_file = paste0(data_path, "/boot/data_idx_wh_indicator_boot.rds")
    )
  )
  return(scores)
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

tbl_region_measure <- function(report_card_scores, data) {
  status::status_try_catch(
  {
    scores <- 
      report_card_scores |>
      filter(m_level == "Measure", s_level == "Region") |>
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
      arrange(Component, Indicator, Subindicator, Measure, stat) |>
      tidyr::unite("group", Subindicator, Measure, stat, sep = "__") |>
      mutate(group = factor(group, levels = unique(group))) |>
      tidyr::pivot_wider(names_from = group, values_from = values) 
    file_path <- paste0(data_path, "/summaries/region_measure_scores.rds")
    saveRDS(scores, file = file_path)
  },
  stage_ = 8,
  name_ = "Region/Measure scores",
  item_ = "summaries_region_measure",
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

tbl_wh_measure <- function(report_card_scores, data) {
  status::status_try_catch(
  {
    scores <- 
      report_card_scores |>
      filter(m_level == "Measure", s_level == "WH") |>
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
      arrange(Component, Indicator, Subindicator, Measure, stat) |>
      tidyr::unite("group", Subindicator, Measure, stat, sep = "__") |>
      mutate(group = factor(group, levels = unique(group))) |>
      tidyr::pivot_wider(names_from = group, values_from = values) 
    file_path <- paste0(data_path, "/summaries/wh_measure_scores.rds")
    saveRDS(scores, file = file_path)
  },
  stage_ = 8,
  name_ = "WH/Measure scores",
  item_ = "summaries_wh_measure",
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

trend_plot <- function(dat, label, ylabel, title_label) {
    g <-
      dat |>
      mutate(grade = generate_grades(Boot.Mean)) |>
      mutate(grade = factor(grade, levels = LETTERS[1:5])) |>
      ggplot(aes(x = Year, y = Boot.Mean)) +
      geom_hline(yintercept = 0.85, linetype = "dashed") +
      geom_hline(yintercept = 0.65, linetype = "dashed") +
      geom_hline(yintercept = 0.5, linetype = "dashed") +
      geom_hline(yintercept = 0.25, linetype = "dashed") +
      geom_line() +
      geom_pointrange(aes(ymin = Lower, ymax = Upper, fill = grade),
        shape = 21,
        show.legend = c(fill = TRUE)
      ) +
      scale_y_continuous(ylabel, limits = c(0, 1), expand = c(0, 0.02)) +
      scale_fill_manual("Grade",
        breaks = LETTERS[1:5],
        values = reportcards::RC_reportCardColors[1:5],
        limits = LETTERS[1:5],
        drop = FALSE) +
      ggtitle(title_label) +
      theme_bw()
    ## file_str <- paste0(output_path, "/figures/summaries/", label, ".png") 
    ## ggsave(
    ##   filename = file_str,
    ##   plot = g,
    ##   width = 8,
    ##   height = 8/1.6,
    ##   dpi = 300
    ## )
  return(g)
}

make_labels <- function(effects, data) {
  effects |> 
    left_join(
      data$guidelines |>
        dplyr::select(Measure, UnitsLabel, Label) |>
        distinct(),
      by = "Measure",
      relationship = "many-to-many"
    ) |>
    mutate(title_m_label = case_when(
      !is.na(Measure) ~ paste(UnitsLabel),
      !is.na(Subindicator) ~ Subindicator,
      !is.na(Indicator) ~ Indicator,
      !is.na(Component) ~ Component
    )) |>
    mutate(title_m_label = ifelse(!is.na(Source),
      paste0(title_m_label, " (", Source, ")"),
      title_m_label
    )) |>
    ## need to join the spatial scales separately
    left_join(
      data$spatial |>
        dplyr::select(Zone, ZoneName) |>
        distinct(),
      by = c("Zone"),
      relationship = "many-to-many"
    ) |>
    left_join(
      data$spatial |>
        dplyr::select(Region, RegionName) |>
        distinct(),
      by = c("Region"),
      relationship = "many-to-many"
    ) |>
    mutate(title_s_label = case_when(
      !is.na(Zone) ~ paste0("(", Zone, ") ", ZoneName),
      !is.na(Region) ~ paste0("(", Region, ") ", RegionName),
      is.na(Region) & is.na(Zone) ~ "Whole Harbour"
    )) |>
    mutate(title_label = paste0(title_s_label, " ", title_m_label)) |>
    mutate(file_str = paste(
      Component,
      Indicator, Subindicator, Measure,
      RegionName, ZoneName, Source,
      sep = "__"
    )) |>
    mutate(file_str = str_replace_all(file_str, "NA", " ")) 
}

generate_trend_plots <- function(report_card_scores, data) {
  status::status_try_catch(
  {
  progress_file <- paste0(data_path, "/progress.log")
  if (file.exists(progress_file)) {
    file.remove(progress_file)
  }
  report_card_plots <-
    report_card_scores |>
    make_labels(data) |>
    mutate(file_str = paste0("trend___", file_str)) |>
    dplyr::select(-m_level, -s_level) |>
    group_by(across(c(-Year, -Lower, -Upper, -Boot.Mean))) |>
    nest() |>
    mutate(trend_plot = pmap(
      .l = list(data, file_str, title_label),
      .f = ~ {
        dat <- ..1
        file_str <- ..2
        title_label <- ..3
        trend_plot(dat, file_str, ylabel = "Index", title_label)
      }
    )) |>
    ungroup() |> 
    mutate(count = 1:n(), total = max(count)) |> 
    mutate(trend_plot_files = pmap(
      .l = list(trend_plot, file_str, count, total),
      .f = ~ {
        g <- ..1
        label <- ..2
        i <- ..3
        total <- ..4
        file_str <- paste0(output_path, "/figures/summaries/", label, ".png")
        ggsave(
          filename = file_str,
          plot = g,
          width = 8,
          height = 8 / 1.6,
          dpi = 300
        )
        cat(paste("Trend plots,", ",", i, ",", total, "\n"), file = progress_file, append = TRUE)
        return(file_str)
      }
    ))
  },
  stage_ = 8,
  name_ = "Trend plots",
  item_ = "summaries_trend_plots",
  )
  return(report_card_plots)
}

calculate_effects <- function(scores_files) {
  status::status_try_catch(
  {
    progress_file <- paste0(data_path, "/progress.log")
    if (file.exists(progress_file)) {
      file.remove(progress_file)
    }
    effects <-
      scores_files |>
      dplyr::select(s_scale, m_scale, boot_file) |>
      ## create a new column (data) with the full posteriors of all
      ## within this s_scale and m_scale
      mutate(count = 1:n(), total = max(count)) |> 
      mutate(data = map(boot_file, ~ readRDS(.x)$dist)) |>
      ## add a new nested column (data) with the posterior samples
      mutate(data = map(
        data,
        ~ {
          .x |>
            group_by(across(c(-Score, -Year))) |>
            nest() |>
            mutate(data = map(
              data,
              ~ .x |>
                group_by(Year) |>
                mutate(.draw = 1:n())
            ))
        }
      )) |>
      ## add annual_effects nested within data/
      mutate(data = pmap(
        list(data, count, total),
        ~ {
          i <- ..2
          total <- ..3
          eff <-
            ..1 |>
            mutate(annual_effects = map(
              .x = data,
              .f = ~ {
                yrs <- .x |>
                  pull(Year) |>
                  unique()
                xmat <- make_contrast_matrix_annual(yrs)
                effects <- .x |>
                  ungroup() |>
                  group_by(.draw) |>
                  mutate(
                    effect = c(NA, t(Score %*% xmat)),
                    contrast = c(yrs[1], colnames(xmat)),
                    ## val2 = lead(Score),
                    Year = Year,
                    val2 = Score,
                    val1 = c(NA, Score[-length(Score)]),
                    grade1 = as.numeric(factor(generate_grades(val1))),
                    grade2 = as.numeric(factor(generate_grades(val2))),
                    ## grade2 = as.numeric(generate_grades(val2))#,
                    ## grade1 = as.numeric(generate_grades(val1))#,
                    grade_diff = grade2 - grade1  ## keep in mind that high score = low numeric factor grade
                  ) |>
                  ungroup()
                effects
              }
            ))
          cat(paste("Calculate Effects,", "annual,", i, ",", total, "\n"), file = progress_file, append = TRUE)
          eff
        }
      )) |>
      ## add annual_effects_sum nested within data/
      mutate(data = pmap(
        list(data, count, total),
        ~ {
          i <- ..2
          total <- ..3
          eff <- ..1 |>
            mutate(annual_effects_sum = map(annual_effects,
              .f = ~ {
                .x |>
                  group_by(Year, contrast) |>
                  summarise(
                    lower = quantile(effect, 0.025, na.rm = TRUE),
                    upper = quantile(effect, 0.975, na.rm = TRUE),
                    Pl = mean(effect < 0),
                    Pg = mean(effect > 0),
                    effect = mean(effect),
                    val2_lower = quantile(val2, 0.025),
                    val2_upper = quantile(val2, 0.975),
                    val2 = mean(val2),
                    val1 = mean(val1),
                    Pgl = mean(grade_diff > 0),
                    Pgg = mean(grade_diff < 0),
                    grade_diff = mean(grade_diff),
                    .groups = "keep"
                  ) |>
                  dplyr::select(effect, everything()) |>
                  ungroup()
              }
            ))
          cat(paste("Calculate Effects,", "annual summaries,", i, ",", total, "\n"), file = progress_file, append = TRUE)
          eff
        }
      )) |>
      ## add period_effects nested within data/
      mutate(data = pmap(
        list(data, count, total),
        ~ {
          i <- ..2
          total <- ..3
          eff <- ..1 |>
            mutate(period_effects = map(
              .x = data,
              .f = ~ {
                yrs <- .x |>
                  pull(Year) |>
                  unique()
                xmat <- make_contrast_matrix_year_span(yrs, span = 5)
                xmat_1 <- xmat_2 <- xmat
                xmat_2[xmat_2 > 0] <- 0
                xmat_2[xmat_2 < 0] <- abs(xmat_2[xmat_2 < 0])
                xmat_1[xmat_1 < 0] <- 0
                effects <- .x |>
                  ungroup() |>
                  group_by(.draw) |>
                  reframe(
                    effect = t(Score %*% xmat),
                    contrast = colnames(xmat),
                    val2 = t(Score %*% xmat_2),
                    val1 = t(Score %*% xmat_1),
                    grade2 = as.numeric(factor(generate_grades(val2))),
                    grade1 = as.numeric(factor(generate_grades(val1))),
                    grade_diff = grade2 - grade1
                  ) |>
                  ungroup()
                effects
              }
            ))
          cat(paste("Calculate Effects,", "contrasts,", i, ",", total, "\n"), file = progress_file, append = TRUE)
          eff
        }
      )) |>
      ## add period_effects_sum nested within data/
      mutate(data = pmap(
        list(data, count, total),
        ~ {
          i <- ..2
          total <- ..3
          eff <- ..1 |>
            mutate(period_effects_sum = map(period_effects,
              .f = ~ {
                .x |>
                  group_by(contrast) |>
                  summarise(
                    lower = quantile(effect, 0.025, na.rm = TRUE),
                    upper = quantile(effect, 0.975, na.rm = TRUE),
                    Pl = mean(effect < 0),
                    Pg = mean(effect > 0),
                    effect = mean(effect),
                    Pgl = mean(grade_diff > 0),
                    Pgg = mean(grade_diff < 0),
                    grade_diff = mean(grade_diff),
                    .groups = "keep"
                  ) |>
                  dplyr::select(effect, everything()) |>
                  ungroup()
              }
            ))
          cat(paste("Calculate Effects,", "contrast summaries,", i, ",", total, "\n"), file = progress_file, append = TRUE)
          eff
        }
      ))
    
    ## create a label to use as filenames
    ## I tried to do this within a mutate map, but it alsowas
    ## seems to lose scope
    cols <- c(
      "Component" = NA,
      "Indicator" = NA, "Subindicator" = NA, "Measure" = NA,
      "Region" = NA, "Zone" = NA, "Source" = NA
    )
    effects$data <-
      effects$data |>
      purrr::map(~tibble::add_column(., !!!cols[!names(cols) %in% names(.)]))
    effects |>
      mutate(data = map(
        data,
        ~ .x |>
          mutate(label = paste(Component,
            Indicator, Subindicator, Measure, Region, Zone, Source,
            sep = "__"
          ))
      ))
  },
  stage_ = 8,
  name_ = "Calculate effects",
  item_ = "summaries_calculate_effects",
  )
  return(effects)
}

make_contrast_matrix_annual <- function(yrs) {
  contrast_matrix <- matrix(0, nrow = length(yrs), ncol = length(yrs) - 1)

  # Fill the contrast matrix to compare each year to the previous one
  for (i in 2:length(yrs)) {
    contrast_matrix[i, i - 1] <- 1  # Current year
    contrast_matrix[i - 1, i - 1] <- -1  # Previous year
  }

  # Assign row and column names for clarity
  rownames(contrast_matrix) <- yrs
  colnames(contrast_matrix) <- paste(yrs[-1], "vs", yrs[-length(yrs)])
  return(contrast_matrix)  
}
make_contrast_matrix_year_span <- function(yrs, span = 5) {
  if (span > length(yrs)) {
    span <- length(yrs)
  }
  periods <- ceiling((yrs - min(yrs) + 1) / span)
  unique_periods <- unique(periods)
  
  # Initialize a contrast matrix
  contrast_matrix <- matrix(0, nrow = length(yrs), ncol = length(unique_periods) - 1)
  
  # Fill the contrast matrix to compare each five-year period to the previous one
  for (i in 2:length(unique_periods)) {
    tot_1 <- length(periods[periods == unique_periods[i - 1]])
    tot_2 <- length(periods[periods == unique_periods[i]])
    current_period <- unique_periods[i]
    previous_period <- unique_periods[i - 1]
    
    # Assign 1 to rows in the current period and -1 to rows in the previous period
    contrast_matrix[periods == current_period, i - 1] <- 1/tot_2
    contrast_matrix[periods == previous_period, i - 1] <- -1/tot_1
  }
  
  # Assign row and column names for clarity
  rownames(contrast_matrix) <- yrs

  period_ranges <- sapply(unique_periods, function(p) {
    period_years <- yrs[periods == p]
    if (length(period_years) == 1) {
      as.character(period_years)  # Single year
    } else {
      paste0(min(period_years), "-", max(period_years))  # Year range
    }
  })
  colnames(contrast_matrix) <- paste(
    paste0(period_ranges[-1], " vs\n", period_ranges[-length(period_ranges)])
    ## paste0(yrs[unique(range(which(unique_periods[-1] %in% periods)))], collapse = "-"),
    ## "vs",
    ## paste0(yrs[unique(range(which(unique_periods[-length(unique_periods)] == periods)))], collapse = "-")
    ## paste0("Period_", unique_periods[-1]),
    ## "vs",
    ## paste0("Period_", unique_periods[-length(unique_periods)])
  )
  return(contrast_matrix)
}

generate_effects_plots <- function(effects, data) {
  status::status_try_catch(
  {
    progress_file <- paste0(data_path, "/progress.log")
    if (file.exists(progress_file)) {
      file.remove(progress_file)
    }
    effects_plots <- 
      effects |>
      ungroup() |>
      dplyr::select(data) |>
      unnest(data) |>
      ## make the labels
      make_labels(data) |> 
      ## annual effects plots
      mutate(count = 1:n(), total = max(count)) |>
      mutate(file_strg = paste0("annual_effects___", file_str)) |>
      mutate(annual_effects_plots = pmap(
        .l = list(annual_effects_sum, annual_effects, file_strg, title_label),
        ~ {
          effects_sum <- ..1
          effects_posteriors <- ..2
          file_str <- ..3
          title_label <- ..4
          effects_plot(effects_sum, effects_posteriors, file_str, title_label)
        }
      )) |> 
      mutate(annual_effects_plots_files = pmap(
        .l = list(annual_effects_plots, file_strg, count, total),
        ~ {
          g <- ..1
          file_str <- ..2
          i <- ..3
          total <- ..4
          file_str <- paste0(output_path, "/figures/summaries/", file_str, ".png") 
          ggsave(
            filename = file_str,
            plot = g,
            width = 8,
            height = 6,
            dpi = 300
          )
          cat(paste("Effects plot,", "annual,", i, ",", total, "\n"), file = progress_file, append = TRUE)
          return(file_str)
        }
      )) |> 
      ## period effects plots
      mutate(file_strg = paste0("period_effects___", file_str)) |>
      mutate(period_effects_plots = pmap(
        .l = list(period_effects_sum, period_effects, file_strg, title_label),
        ~ {
          effects_sum <- ..1
          effects_posteriors <- ..2
          file_str <- ..3
          title_label <- ..4
          effects_plot(effects_sum, effects_posteriors, file_str, title_label)
        }
      )) |> 
      mutate(period_effects_plots_files = pmap(
        .l = list(period_effects_plots, file_strg, count, total),
        ~ {
          g <- ..1
          file_str <- ..2
          i <- ..3
          total <- ..4
          file_str <- paste0(output_path, "/figures/summaries/", file_str, ".png") 
          ggsave(
            filename = file_str,
            plot = g,
            width = 8,
            height = 6,
            dpi = 300
          )
          cat(paste("Effects plot,", "contrasts,", i, ",", total, "\n"), file = progress_file, append = TRUE)
          return(file_str)
        }
      ))  
  },
  stage_ = 8,
  name_ = "Effects plots",
  item_ = "summaries_effects_plots",
  )
  return(effects_plots)
  
}

effects_plot <- function(effects_sum, effects_posteriors, file_str, title_label) {
  bc_sum <- effects_sum |>
    filter(!is.na(effect)) |> 
    mutate(flag = ifelse(Pl > 0.90 & effect < 0, "decline",
      ifelse(Pg > 0.90 & effect > 0, "increase", "neutral")
    )) |>
    mutate(p_label = ifelse(effect < 0,
      sprintf("P(Δi<0)=%.3f", Pl),
      sprintf("P(Δi>0)=%.3f", Pg)
    )) |> 
    mutate(pg_label = ifelse(effect < 0,
      sprintf("P(Δg<0)=%.3f", Pgl),
      sprintf("P(Δg>0)=%.3f", Pgg)
    )) 

  xlims <- c(min(bc_sum$lower)*1.25, max(bc_sum$upper)*1.25)
  xlims <- max(abs(xlims))
  xlims <- c(-xlims, xlims)

  g <-
    effects_posteriors |>
    filter(!is.na(effect)) |>
    left_join(bc_sum |>
                ## dplyr::select(contrast, Year, Pl, Pg, flag, p_label),
      ## by = c("Year", "contrast")) |>
                dplyr::select(contrast, Pl, Pg, flag, p_label),
      by = c("contrast")) |>
    mutate(flag = factor(flag, levels = c("decline", "neutral", "increase"))) |>
    ggplot(aes(y = contrast, x = effect)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    ## stat_slab() +
    stat_slab(aes(fill = flag),
      normalize = "groups",
      height = 1.1,
      expand = TRUE,
      trim = TRUE,
      density = "bounded",
      adjust = 1,
      ## density = "histogram",
      width = 0.95,
      alpha = 0.6,
      fill_type = "segments",
      show.legend = c(fill = TRUE, width = FALSE)
    ) +
    stat_slabinterval(
      position = position_dodge(width = 0.5, preserve = "total"),
      height = 0,
      fill = "white",
      ## point_size = 0,
      show.legend = c(size = FALSE)
    ) +
    geom_text(data = bc_sum, aes(label = p_label), nudge_y = 0.3, hjust = 0.5, size = 3) +
    geom_text(data = bc_sum, aes(label = pg_label), nudge_y = 0.6, hjust = 0.5, size = 3) +
    ggtitle(title_label) +
    theme_bw() +
    scale_color_manual("Trend",
      values = c(
        "decline" = reportcards::RC_reportCardColors[5],
        "increase" = reportcards::RC_reportCardColors[1],
        "neutral" = "grey"),
      limits = c("decline", "neutral", "increase")) +
    scale_fill_manual("Trend",
      values = c(
        "decline" = reportcards::RC_reportCardColors[5],
        "increase" = reportcards::RC_reportCardColors[1],
        "neutral" = "grey"),
      limits = c("decline", "neutral", "increase")) +
    scale_x_continuous("Change in index", limits =  xlims) +
    scale_y_discrete("") 
  ## parts <- parse_label_to_parts(label)
  return(g)
}
