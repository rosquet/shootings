## This Shiny app (with Leaflet and Plotly) uses the Washington Post data
## on fatal police shootings between 2015 and 2020. The app allows the 
## user to filter by race, age, and sex. It then generates a Leaflet map
## showing locations of all fatal shootings in that filter; popups indicate
## the name of the victim and location of the shooting. It also generates a
## Plotly pie chart to show what fraction of all fatal shootings are in
## the filter.
##
## Data is from the Washington Post GitHub:
## https://github.com/washingtonpost/data-police-shootings

## Load libraries

library(tidyverse)
library(data.table)
library(scales)
library(shiny)
library(shinythemes)
library(leaflet)
library(plotly)

## Begin Shiny UI ##

ui = fluidPage(
        
        theme = shinytheme("superhero"),
        
        titlePanel("US Fatal Police Shootings, 2015 - 2020"),
        
        sidebarLayout(
                
                sidebarPanel(
                        width = 2,
                        h4("Filters"),
                        radioButtons("race", "Race:",
                                     c("Any" = "any",
                                       "Asian" = "A",
                                       "Black" = "B",
                                       "Hispanic" = "H",
                                       "Native American" = "N",
                                       "White" = "W")),
                        radioButtons("age", "Age:",
                                     c("Any" = "any",
                                       "under 13" = "kid",
                                       "13 - 19" = "teen",
                                       "20 - 29" = "twenties",
                                       "30 - 39" = "thirties",
                                       "40 - 49" = "forties",
                                       "50 - 59" = "fifties",
                                       "60 and over" = "senior")),
                        radioButtons("sex", "Sex:",
                                     c("Either" = "either",
                                       "Female" = "F",
                                       "Male" = "M"))
                ),
                
                mainPanel(
                        width = 10,
                        leafletOutput("shootingsMap"),
                        htmlOutput("textHTML"),
                        plotlyOutput("shootingsPlot")
                        
                )
        )
)

## End Shiny UI ##