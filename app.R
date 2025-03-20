#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(data.table)
library(dplyr)
library(arrow)
library(purrr)
library(dthelpers)
library(ggplot2)
library(scales)
library(phicharts)
library(plotly)

source("./phi_pop_pyramid.R")
source("./pyramid_wrapper.R")

maximals <- fread("./maximal_values.txt")

pq_path <- "./outputs/cp-age-sex"

sape_pq <- open_dataset(pq_path)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Small area population estimates"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput("cps",
                        "Select Community Partnership:",
                        choices = c("Badenoch and Strathspey",
                                    "Bute and Cowal",
                                    "Caithness",
                                    "East Ross",
                                    "Helensburgh and Lomond",
                                    "Inverness",
                                    "Lochaber",
                                    "Mid-Argyll, Kintyre and Islay",
                                    "Mid Ross",
                                    "Nairn and Nairnshire",
                                    "Oban, Lorn and the Isles",
                                    "Skye, Lochalsh and West Ross",
                                    "Sutherland"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("pyramid")
        )
    )
)




# Define server logic required to draw a histogram
server <- function(input, output) {

    output$pyramid <- renderPlotly({

      # get the correct max value for the chosen CP area

      max_value <- maximals[CP_Name == input$cps,maxval]

      t1 <- sape_pq |>
        filter(CP_Name == input$cps & sex != "Persons") |>
        collect()

      p <- phi_pop_pyramid(t1,
                           xcol = age_band,
                           ycol = pop,
                           fill_by = sex,
                           male_val = "Males",
                           female_val = "Females",
                           ylimit = max_value)

      p <- p + ggplot2::labs(p,
                    title = paste0("Aggregated small area population estimates - ",input$cps),
                    subtitle = "Mid-2022",
                    caption = "Source: NRS small area population estimates\nPublished: Nov 2024",
                    xlab =  "Age band",
                    ylab = "Population")

      #print(p)
      ggplotly(p)
    })
}

# Run the application
shinyApp(ui = ui, server = server)
