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
