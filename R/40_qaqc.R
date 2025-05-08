

##' Perform QAQC
##'
##' Perform QAQC
##' @title Perform QAQC
##' @return NULL
##' @author Murray Logan
##' @export
module_qaqc <- function() {
  status::status_set_stage(stage = 6, title = "QAQC")

  data_idx <-  readRDS(file = paste0(data_path, "/processed/indices/data_idx.rds"))
  data <-  readRDS(file = paste0(data_path, "/processed/data.rds"))

  data$wq_long <- data$wq_long |>
    mutate(Subindicator = gsub(" ", "~", Subindicator)) 
  data_idx <- data_idx |>
    mutate(Subindicator = gsub(" ", "~", Subindicator)) |>
    dplyr::select(Subindicator)

  ## Generate QAQC outlier plots
  qaqc_outliers_plots(data = data)

  ## Generate LOR (LOD) flags table
  qaqc_lor_tbl(data = data)

  ## Generate LOR (LOD) flags table
  qaqc_boxplots(data = data)

  ## Generate boxplots in timeseries 
  qaqc_ts(data)

}




##' QAQC Outliers Plots
##'
##' QAQC Outliers Plots
##' @title QAQC Outliers Plots
##' @param data a list containing an item called "wq_long" that contains
##' a tibble representing the water quality data
##' @return a side effect of saving a plot to the output directory
##' @author Murray Logan
##' @export
qaqc_outliers_plots <- function(data) {
  status::status_try_catch(
  {
    qaqc_outliers_plot(data, sample_type = "all")
    qaqc_outliers_plot(data, sample_type = "Discrete")
    qaqc_outliers_plot(data, sample_type = "CFM")
  },
  stage_ = 6,
  name_ = "QAQC outliers plots",
  item_ = "qaqc_outliers_plots",
  )
  invisible(return(NULL))
}


## library(progress)
## file_progress_handler <- function(total, progress_file) {
##   # Create a progress bar object
##   ## pb <- progress_bar$new(
##   ##   format = "[:bar] :current/:total (:percent)",
##   ##   total = total,
##   ##   clear = FALSE,
##   ##   width = 60
##   ## )
  
##   # Return a function that updates the progress bar and writes to the file
##   function(i) {
##    ## pb$tick()  # Update the progress bar in the console
##     writeLines(as.character(i), progress_file)  # Write progress to the file
##   }
## }

##' QAQC Outliers Plots
##'
##' QAQC Outliers Plots
##' @title QAQC Outliers Plots
##' @param data a list containing an item called "wq_long" that contains
##' a tibble representing the water quality data
##' @return a side effect of looping through each year and calling a function that builds and saves a plot to the output directory
##' @author Murray Logan
##' @export
qaqc_outliers_plot <- function(data, sample_type) {
  spatial <- data$spatial
  flname <- paste0("waterQAQC_", sample_type)

  ## ## progress_file <- paste0(tempdir(), "/progress.log")
  progress_file <- paste0(data_path, "/progress.log")
  ## print(progress_file)
  if (file.exists(progress_file)) {
    file.remove(progress_file)
  }
  ## loop through each year and generate a plot
  n_yr <- data$wq_long$Year |>
    unique() |>
    length()
  walk(
    .x = data$wq_long$Year |> unique(),
    .f = ~ {
      qaqc_outliers_plot_(data = data, year = .x, sample_type = sample_type)
      i <- which(data$wq_long$Year |> unique() == .x)
      cat(paste("QAQC outliers plots,", sample_type, " samples,", i, ",", n_yr, "\n"), file = progress_file, append = TRUE)
    },
    )
}

##' QAQC Outliers Plot
##'
##' QAQC Outliers Plot
##' @title QAQC Outliers Plot
##' @param data a list containing an item called "wq_long" that contains
##' a tibble representing the water quality data
##' @return a side effect of saving a plot to the output directory
##' @author Murray Logan
##' @export
qaqc_outliers_plot_ <- function(data, year, sample_type) {
    spatial <- data$spatial
    flname <- paste0("waterQAQC_", sample_type, "_", year)

    dat <- data$wq_long |>
      ## filter(Year == Focal_Year) |>
      filter(Year == year) |>
      droplevels() |>
      mutate(Label = gsub("(.*)\\~(\\(.*\\))", "atop(\\1,\\2)", Label)) |>
      dplyr::select(-Zone) |> 
      left_join(spatial, by = c("ZoneName", "Region")) |>
      mutate(ZoneName = paste0("(", Zone, ")~", gsub(" ", "~", ZoneName))) |>
      mutate(Type = ifelse(is.na(GL), "Range", "GL")) |>
      mutate(Source = ifelse(Source == "Discrete", "Discrete", "CFM")) 

    if (sample_type == "Discrete") {
      dat <- dat |>
        filter(Source == "Discrete") |>
        droplevels()
    }
    if (sample_type == "CFM") {
      dat <- dat |>
        filter(Source == "CFM") |>
        droplevels()
    }
    
    guides <- dat |>
      group_by(Region, Zone, ZoneName, Subindicator, Measure, Label) |> 
      summarize(
        RangeFrom = mean(RangeFrom),
        RangeTo = mean(RangeTo),
        Lower = mean(GL)/2,
        Upper = mean(GL)*2
      ) |>
      suppressWarnings() |> suppressMessages()

    g <-
      dat |>
      ggplot() +
      geom_point(data = dat, aes(y = 1, x = Value,
        size = Source, alpha = Source),
        position = position_jitter(width = 0),
        show.legend = FALSE) +
      geom_vline(data =  dat, aes(xintercept = GL), color = "red") +
      geom_rect(data = guides, aes(ymin = -Inf, ymax = Inf,
        xmin = RangeFrom, xmax = RangeTo),
        color = NA, fill = "blue", alpha = 0.3) 
    if (sample_type == "Discrete") {
      g <- g +
        scale_alpha_manual(values = 1) +
        scale_size_manual(values = 1)
    } else {
      g <- g + scale_alpha_manual(values = c(0.1,1)) +
        scale_size_manual(values = c(0.1,1))
    }
    g <- g +
      scale_x_log10("Observed value") +
      scale_y_continuous("Jittered unordered space") +
      facet_grid(ZoneName ~ Subindicator + Label,
        scale = "free", space = "free_y",
        as.table = TRUE,
        labeller = label_parsed) +
      theme(strip.background=element_rect(fill=NA,color="black"),
        strip.text.y=element_text(angle=0),
        axis.title.x=element_text(margin=margin(t=1,unit="lines")),
        axis.title.y=element_text(margin=margin(r=1,unit="lines")),
        axis.text.y=element_blank(),
        panel.border=element_rect(fill=NA, color="black"))
    ggsave(file = paste0(output_path, "figures/QAQC/", flname, ".png"),
      g,
      width = length(unique(dat$Measure))*2.5,
      height = length(unique(dat$Zone))*1
    ) |> suppressMessages() |> suppressWarnings()
}

##' QAQC Limit of detection table
##'
##' QAQC Limit of detection table
##' @title QAQC Limit of detection table
##' @param data a list containing an item called "wq_long" that contains
##' a tibble representing the water quality data
##' @return a side effect of saving a table to the output directory
##' @author Murray Logan
##' @export
qaqc_lor_tbl <- function(data) {
  status::status_try_catch(
  {
    
    spatial <- data$spatial
    dat <- data$wq_long |>
      filter(Year == Focal_Year) |>
      droplevels() |> 
      mutate(Label = gsub("(.*)\\~(\\(.*\\))","atop(\\1,\\2)", Label)) |>
      dplyr::select(-Zone) |> 
      left_join(spatial, by = c("ZoneName", "Region")) |>
      ## left_join(spatial, by = c("Zone", "Region")) |>
      mutate(ZoneName = paste0("(", Zone, ")~", gsub(" ", "~", ZoneName))) |>
      mutate(Type = ifelse(is.na(GL), "Range", "GL")) |>
      mutate(Source = ifelse(Source == "Discrete", "Discrete", "CFM")) 
    flags <-
      dat |>
      filter(Flag %in% c("LOD","DO")) |> 
      droplevels() |>
      group_by(Region, Zone, Component, Indicator, Subindicator, Measure,
        UnitsLabel, Flag) |> 
      summarize(Count = n()) |> 
      full_join(
        dat |>
          group_by(Region, Zone, Component, Indicator, Subindicator,
            UnitsLabel, Measure) |> 
          summarize(CountT = n())) |>
      mutate(Percent = 100 * Count / CountT) |>
      select(-CountT) |>
      ungroup() |> 
      mutate(Count = ifelse(Count == 0, NA,
        paste0(Count, " (", round(Percent, 2), "%)"))) |>
      distinct(Region, Zone, Component, Indicator, Subindicator, Measure,
        UnitsLabel, .keep_all = TRUE) |> 
      left_join(select(spatial, Zone, ZoneName, Region)) |> 
      arrange(Zone, Subindicator, UnitsLabel) |> 
      select(Region, Zone, ZoneName, UnitsLabel, Count) |> 
      tidyr:::spread(UnitsLabel, Count) |> 
      as.data.frame()
    ## n <- gsub("(.*) \\((.*)", "\\\\specialcell{\\1\\\\\\\\(\\2}", colnames(flags))
    ## colnames(flags) <- n
    ## colnames(flags) <- gsub("%", "\\\\%", n)
    flags <- flags |> rename("Zone name" = ZoneName)
    saveRDS(flags, file = paste0(output_path, "tables/qaqc_lor_tbl.rds"))
  },
  stage_ = 6,
  name_ = "QAQC LOR table",
  item_ = "qaqc_lor_tbl",
  )
  invisible(return(NULL))
}
#########################################################################
## The following function ensures that all panels in a figure are a    ##
## set height and that the figure dimensions are adjusted accordingly. ##
#########################################################################
set_panel_size <- function(p=NULL, g=ggplotGrob(p), file=NULL, margin=unit(1,'mm'), width=unit(4,'cm'), height=unit(4,'cm')) {
    panels=grep('panel', g$layout$name)
    panel_index_w<-unique(g$layout$l[panels])
    panel_index_l<-unique(g$layout$t[panels])
    nw<-length(panel_index_w)
    nh <- length(panel_index_l)
    if(getRversion() < '3.3.0') {
        g$widths <- grid:::unit.list(g$widths)
        g$heights <- grid:::unit.list(g$heights)
        g$widths[panel_index_w] <- rep(list(width), nw)
        g$heights[panel_index_l] <- rep(list(height), nh)
    } else {
        g$widths[panel_index_w] <- rep(width, nw)
        g$heights[panel_index_l] <- rep(height, nh)
    }
    ggsave(file, g,
           width=grid::convertWidth(sum(g$widths) + margin, unitTo='in', valueOnly=TRUE),
           height=grid::convertHeight(sum(g$heights) + margin, unitTo='in', valueOnly = TRUE))
    invisible(g)
    
}


##' QAQC Boxplots
##'
##' QAQC Boxplots
##' @title QAQC Boxplots
##' @param data a list containing an item called "wq_long" that contains
##' a tibble representing the water quality data
##' @return a side effect of boxplots to the output directory
##' @author Murray Logan
##' @export
qaqc_boxplots <- function(data) {
  status::status_try_catch(
  {
    spatial <- data$spatial

    progress_file <- paste0(data_path, "/progress.log")
    ## print(progress_file)
    if (file.exists(progress_file)) {
      file.remove(progress_file)
    }

    dat <- data$wq_long |>
      ## filter(Year == Focal_Year) |>
      droplevels() |> 
      ## left_join(spatial |>
      ##             dplyr::select(Zone, HexColor),
      ##   by = "Zone") |> 
      dplyr::select(-Zone) |> 
      left_join(spatial |> dplyr::select(Zone, ZoneName, Region, HexColor),
        by = c("ZoneName", "Region")) |>
      mutate(Source = ifelse(Source == "Discrete", "Discrete", "CFM")) 

    dat_plots <- dat |>
      nest(.by = c(Zone, Year)) |>
      mutate(g = map(.x = data,
        .f =  ~ {
          guides <- .x |>
            group_by(Subindicator, Measure, Label) |> 
            summarize(
              RangeFrom = mean(RangeFrom), RangeTo = mean(RangeTo),
              Lower = mean(GL)/2, Upper = mean(GL)*2
            ) |> suppressWarnings() |> suppressMessages()
          HexColor<- unique(.x$HexColor)
          p <-
            .x |>
            ggplot(aes(y = Value, x = Source)) +
            geom_hline(aes(yintercept = GL), color = "black",
              linetype = "dashed") +
            geom_hline(data = guides, aes(yintercept = RangeFrom),
              color = "black", linetype = "dashed") +
            geom_hline(data = guides, aes(yintercept = RangeTo),
              color = "black", linetype = "dashed") +
            geom_boxplot(fill = HexColor, width = 0.5) +
            facet_wrap(~Subindicator+Label, scales = "free",
              labeller = label_parsed) +
            theme_classic() +
            theme(strip.background = element_rect(fill = NA, color = "black"),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              panel.background = element_rect(fill = NA, color = "black"))
        }
      )) |>
      mutate(i = 1:n(), total = max(i))
    ## walk2(dat_plots$Zone, dat_plots$g,
    pwalk(list(dat_plots$Zone, dat_plots$Year, dat_plots$g, dat_plots$i, dat_plots$total),
      .f = ~ {
        zn <- ..1
        yr <- ..2
        g <- ..3
        i <- ..4
        total <- ..5
        set_panel_size(p = g, file = paste0(output_path, "figures/QAQC/wq_boxplot_Zone_", zn, "_", yr, ".png"),
          width = unit(6, "cm"), height = unit(6, "cm")) |> 
          suppressMessages() |> suppressWarnings()

        cat(paste("QAQC boxplots,", paste0("Zone ", zn, " ", yr), ", ", i, ",", total, "\n"),
          file = progress_file,
          append = TRUE
        )
      })

    
    dat_plots <-
      dat |>
      ## data$wq_long |>
      ## filter(Year == Focal_Year) |>
      ## droplevels() |>
      mutate(Sources = ifelse(Source == "Discrete", "Discrete", "CFM")) |>
      mutate(Zone = factor(Zone)) |>
      nest(.by = c(Year)) |>
      mutate(g = map(
        .x = data,
        .f = ~ {
          .x |>
            ggplot(aes(y = Value, x = Zone)) +
            geom_boxplot(aes(fill = Sources)) +
            facet_wrap(~ Subindicator + Label, scales = "free", labeller = label_parsed) +
            theme_classic() +
            theme(
              strip.background = element_rect(fill = NA, color = "#ffffff"),
              ## axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              panel.background = element_rect(fill = NA, color = "black")
            ) +
            scale_y_log10("")
        }
      )) |> 
      mutate(i = 1:n(), total = max(i))

    pwalk(list(dat_plots$Year, dat_plots$g, dat_plots$i, dat_plots$total),
      .f = ~ {
        yr <- ..1
        g <- ..2
        i <- ..3
        total <- ..4
        set_panel_size(p = g, file = paste0(output_path, "figures/QAQC/wq_boxplot_", yr, ".png"),
          width = unit(6, "cm"), height = unit(6, "cm")) |> 
          suppressMessages() |> suppressWarnings()
        cat(paste("QAQC boxplots,", paste0(yr), ", ", i, ",", total, "\n"),
          file = progress_file,
          append = TRUE
        )
      })
    ## set_panel_size(p = p, file = paste0(output_path, "figures/QAQC/wq_boxplot.png"),
    ##   width = unit(6, "cm"), height = unit(6, "cm")) |> 
    ##   suppressMessages() |> suppressWarnings()
  
  },
  stage_ = 6,
  name_ = "QAQC boxplots",
  item_ = "qaqc_boxplots",
  )
  invisible(return(NULL))
}


##' QAQC Boxplots in timeseries
##'
##' QAQC Boxplots in timeseries
##' @title QAQC Boxplots in timeseries
##' @param data a list containing an item called "wq_long" that contains
##' a tibble representing the water quality data
##' @return a side effect of boxplots to the output directory
##' @author Murray Logan
##' @export
qaqc_ts <- function(data) {
  status::status_try_catch(
  {
    spatial <- data$spatial
    dat <- data$wq_long |>
      filter(!(is.na(Latitude) | is.na(Longitude))) |>
      droplevels()

    dat <- dat |>
      ## filter(Year == Focal_Year) |>
      ## droplevels() |> 
      mutate(Label = gsub("(.*)\\~(\\(.*\\))","atop(\\1,\\2)", Label)) |>
      ## left_join(spatial, by = c("Zone", "Region")) |>
      dplyr::select(-Zone) |> 
      left_join(spatial, by = c("ZoneName", "Region")) |>
      mutate(ZoneName = paste0("(", Zone, ")~", gsub(" ", "~", ZoneName))) |>
      mutate(Type = ifelse(is.na(GL), "Range", "GL")) |>
      mutate(Source = ifelse(Source == "Discrete", "Discrete", "CFM")) 

    ## stripes <- with(dat, seq(as.Date(paste0(format(min(Date),'%Y'),'-10-01')),
    ##                        as.Date(paste0(format(max(Date),'%Y'),'-10-01')), by = '2 years'))
    stripes <- with(dat, seq(min(Year), max(Year), by = 2)) - min(dat$Year)

    g <-
      dat |>
      ggplot(aes(y = Value, x = Year)) +
      ## geom_blank() +
      annotate(geom='rect', ymin = Inf, ymax = -Inf,
        xmin = stripes-0.5, xmax = stripes+0.5, alpha = 0.1) +
      geom_boxplot(aes(x = factor(Year))) +
      geom_hline(aes(yintercept = GL), color = "red") +
      ## facet_grid(Subindicator+Measure~Zone, scales = 'free') +
      ## facet_grid(Label~Zone, scales = 'free') +
      facet_grid(ZoneName ~ Subindicator + Label,
        scale = "free", #space = "free_y",
        as.table = TRUE,
        labeller = label_parsed) +
      scale_y_continuous("Observed value",
        ## trans = scales::log1p_trans(),
        ## trans = scales::pseudo_log_trans(sigma = 5),
        trans = "log10",
        ## trans = scales::sqrt_trans()
       ## breaks = scales::log_breaks(base = 5)
       breaks = function(lims) {
         scales::log_breaks()(lims) %>% .[. > 0]  # Remove 0
       },
       labels = scales::label_number()
      ) +
      ## scale_y_log10("Observed value") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        strip.background = element_rect(fill = NA, color = "black"),
        axis.title.x = element_blank(),
        strip.text.y=element_text(angle=0),
        panel.background = element_rect(fill = NA, color = "black")) 
    
    ggsave(file = paste0(output_path, "figures/QAQC/wq_boxplot_timeseries.png"),
      g,
      width = length(unique(dat$Measure))*2,
      height = length(unique(dat$Zone))*1
    ) |> suppressMessages() |> suppressWarnings()
    
  },
  stage_ = 6,
  name_ = "QAQC timeseries",
  item_ = "qaqc_ts",
  )
  invisible(return(NULL))
}



##   data$wq_long <- data$wq_long |>
##     filter(!(is.na(Latitude) | is.na(Longitude))) |>
##     droplevels()
##   qaqc_missing <- reportcards:::qaqc(
##     df = data$wq_long |>
##       mutate(Site = as.character(Region)) |>
##       as.data.frame(),
##     GL = data$guidelines |>
##       mutate(Site = as.character(Region)) |>
##       as.data.frame(),
##     type = "Missing"
##   )
##   qaqc_ts <- reportcards:::qaqc(
##     df = data$wq_long |>
##       mutate(Site = as.character(Region)) |>
##       as.data.frame(),
##     GL = data$guidelines |>
##       mutate(Site = as.character(Region)) |>
##       as.data.frame(),
##     type = "TS"
##   )

