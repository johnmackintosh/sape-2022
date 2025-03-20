library(fs)
library(data.table)
library(dplyr)
library(arrow)
library(purrr)
library(readxl)
library(dthelpers)
library(ggplot2)
library(scales)
library(phicharts)

pq_path <- "./outputs/cp-age-sex"

sape_pq <- open_dataset(pq_path)

#  maximum value by CP

maximals <- sape_pq %>%
  filter(sex != "Persons") %>%
  group_by(CP_Name) %>%
  mutate(maxval = max(pop)) %>%
  ungroup() %>%
  select(CP_Name, maxval) %>%
  distinct() %>%
  collect()


pyramid_wrapper <- function(cp, maxima, data){

  t1 <- data %>%
    filter(CP_Name == cp & sex != "Persons")|>
    collect()

  p <- phi_pop_pyramid(t1,
                       xcol = age_band,
                       ycol = pop,
                       fill_by = sex,
                       male_val = "Males",
                       female_val = "Females",
                       ylimit = maxima)

  p <- p + labs(p,
                title = paste0("Aggregated small area population estimates - ",cp),
               subtitle = "Mid-2022",
               caption = "Source: NRS small area population estimates\nPublished: Nov 2024")

  print(p)
}


  walk2(maximals$CP_Name,
        maximals$maxval,
        ~ pyramid_wrapper(.x, .y, data = sape_pq))


t1 <- sape_pq |>
  filter(CP_Name == "Sutherland" & sex != "Persons") |>
  collect()

