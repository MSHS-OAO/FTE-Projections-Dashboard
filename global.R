# Shiny App for FTE Trend

rm(list = ls())

# Import Libraries --------------------------------------------------------
suppressMessages({
  library(readxl)
  library(tidyverse)
  library(dplyr)
  library(reshape2)
  library(data.table)
  library(plotly)
  library(shiny)
  library(shinythemes)
  library(shinydashboard)
  library(shinycssloaders)
  library(shinyWidgets)
  library(patchwork)
  library(gridExtra)
  library(grid)
  library(scales)
  library(DT)
  library(knitr)
  library(lubridate)
  library(kableExtra)
})

# Maximize R Memory Size
#memory.limit(size = 8000000)

# Import Data -------------------------------------------------------------
system_summary <- readRDS(paste0(
  "/SharedDrive/deans/Presidents/SixSigma/",
  "MSHS Productivity/Productivity/",
  "Universal Data/Labor/RDS/",
  "system_summary_dashboard.rds"
))

# system_summary <- readRDS(paste0("J:/deans/Presidents/SixSigma/",
#                                  "MSHS Productivity/Productivity/",
#                                  "Universal Data/Labor/RDS/",
#                                  "system_summary_dashboard.rds"))

## constants ------------------------------------------------------------------
report_period_length <- 3
biweekly_fte <- 75
digits_round <- 2

# Get Reporting Period data range
report_start_date <- format(max(system_summary$PP.END.DATE) - 41, "%m/%d/%y")
report_end_date <- format(max(system_summary$PP.END.DATE), "%m/%d/%y")

## Pre filter data ------------------------------------------------------------
data <- system_summary %>%
  filter(
    PP.END.DATE < as.Date("3/1/2020", format = "%m/%d/%Y") |
      PP.END.DATE > as.Date("5/9/2020", format = "%m/%d/%Y"),
    PROVIDER == 0,
    INCLUDE.HOURS == 1,
    WORKED.PAY.CODE == 1
  ) %>%
  group_by(
    PAYROLL, DEFINITION.CODE, DEFINITION.NAME,
    CORPORATE.SERVICE.LINE, PP.END.DATE
  ) %>%
  summarise(FTE = sum(HOURS, na.rm = TRUE) / biweekly_fte) %>%
  pivot_wider(
    id_cols = c(
      PAYROLL, DEFINITION.CODE, DEFINITION.NAME,
      CORPORATE.SERVICE.LINE
    ),
    values_from = FTE, names_from = PP.END.DATE
  )

data[, 5:ncol(data)][is.na(data[, 5:ncol(data)])] <- 0

data <- data %>%
  pivot_longer(cols = 5:ncol(data), names_to = "PP.END.DATE", values_to = "FTE")

data <- data %>%
  mutate(
    DEPARTMENT = case_when(
      CORPORATE.SERVICE.LINE %in% c("IT", "HR", "CMO") ~ CORPORATE.SERVICE.LINE,
      is.na(DEFINITION.CODE) ~ "Non-Premier Department",
      TRUE ~ paste0(DEFINITION.CODE, " - ", toupper(DEFINITION.NAME))
    ),
    CORPORATE.SERVICE.LINE = case_when(
      is.na(DEFINITION.CODE) ~ "Non-Premier Department",
      TRUE ~ CORPORATE.SERVICE.LINE
    ),
    service_group = case_when(
      str_detect(
        CORPORATE.SERVICE.LINE,
        "Nursing"
      ) ~ "Nursing",
      str_detect(
        CORPORATE.SERVICE.LINE,
        "Radiology"
      ) ~ "Radiology",
      str_detect(
        CORPORATE.SERVICE.LINE,
        "Support Services"
      ) ~ "Support Services",
      PAYROLL == "Corporate" ~ "Corporate",
      TRUE ~ "Other"
    ),
    DATES = as.character(PP.END.DATE),
    PP.END.DATE = as.Date(PP.END.DATE, format = "%Y-%m-%d"),
    dates = format(as.Date(PP.END.DATE, "%B %d %Y"), "%m/%d/%y")
  )

## Constants After Data Import-------------------------------------------------
start_date <- max(data$PP.END.DATE) - 130

payroll_choices <- sort(unique(as.character(data$PAYROLL)))

group_choices <-
  sort(unique(as.character(data$service_group[data$PAYROLL %in% "MSH"])))

service_choices <-
  sort(unique(as.character(data$CORPORATE.SERVICE.LINE[data$PAYROLL
                                                       %in% "MSH"
                                                       & data$service_group
                                                       %in% "Nursing"])))

date_options <- sort(unique(data$PP.END.DATE), decreasing = TRUE)
date_options <- format(as.Date(date_options, "%B %d %Y"), "%m/%d/%y")

# Color Theme -----------------------------------------------------------

# Mount Sinai corporate colors
mount_sinai_colors <- c(
  `light pink`   = "#fcc9e9",
  `med pink`     = "#fa93d4",
  `dark pink`    = "#DC298D",
  `light purple` = "#c7c6ef",
  `med purple`   = "#8f8ce0",
  `light blue`   = "#5cd3ff",
  `med blue`     = "#06ABEB",
  `dark blue`    = "#212070",
  `light grey`   = "#b2b3b2",
  `dark grey`    = "#686868",
  `yellow`       = "#E69F00",
  `navy`         = "#00002D"
)

# Function to extract Mount Sinai colors as hex codes
# Use Character names of mount_sinai_colors

mount_sinai_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols)) {
    return(mount_sinai_colors)
  }

  mount_sinai_colors[cols]
}


#Create palettes
mount_sinai_palettes <- list(
  `all` = mount_sinai_cols(
    "med blue", "dark pink", "dark blue", "light grey",
    "light blue", "light pink", "light purple",
    "med pink", "med purple", "yellow"
  ),
  `main` = mount_sinai_cols(
    "navy", "dark blue", "med blue", "dark pink", "dark grey"
  ),
  `pink` = mount_sinai_cols("light pink", "dark pink"),
  `blue` = mount_sinai_cols("light blue", "dark blue"),
  `grey` = mount_sinai_cols("light grey", "med blue")
)

mount_sinai_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- mount_sinai_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  colorRampPalette(pal, interpolate = "spline", ...)
}

#Scale Function for ggplot can be used instead of scale_color_manual
scale_color_mount_sinai <- function(palette = "all", discrete = TRUE, reverse = FALSE, ...) {
  pal <- mount_sinai_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("colour", mount_sinai_palettes, palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

# Other Functions ---------------------------------------------------------
string_separate_to_lines <- function(string, max_length) {
  max_lines <- ceiling(nchar(string) / max_length)
  if (max_lines == 1) {
    return(string)
  } else if (max_lines > 1) {
    word_list <- unlist(str_split(string, pattern = " "))
    text_lines <- vector(mode = "character", length = max_lines)
    for (x in 1:max_lines) {
      y <- 0
      while (sum(nchar(paste(word_list[1:y], collapse = " "))) < max_length &
        y <= length(word_list)) {
        y <- y + 1
      }
      text_lines[x] <- paste(word_list[1:y - 1], collapse = " ")
      word_list <- word_list[y:length(word_list)]
    }
    return(paste(text_lines, collapse = "<br>"))
  }
}
