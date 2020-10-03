## Read data
        
shootings = fread("fatal-police-shootings-data.csv")
        
## Change age values to reflect ranges rather than specific ages
        
shootings = shootings %>%
rename(sex = gender) %>%
mutate(age = case_when(
        age < 13 ~ "kid",
        age > 12 & age < 20 ~ "teen",
        age > 19 & age < 30 ~ "twenties",
        age > 29 & age < 40 ~ "thirties",
        age > 39 & age < 50 ~ "forties",
        age > 49 & age < 60 ~ "fifties",
        age > 60 ~ "senior"))
        
## Different subsetted data tables for generating the map and the plot
        
shootingsMap = shootings %>%
unite("cityState", city:state, sep = ", ") %>%
select(name, date, age, sex, race, cityState, longitude,
       latitude)
        
shootingsPlot = shootings %>%
select(age, sex, race)
        
conditional = function(condition, success) {
        if (condition) success else TRUE
        }

## Begin Shiny Server ##

server = function(input, output) {
        
        ## Filter map data table based on user inputs
        
        mapDat = reactive({
                shootingsMap %>%
                        filter(
                                conditional(input$race != "any",
                                            race == input$race),
                                conditional(input$age != "any",
                                            age == input$age),
                                conditional(input$sex != "either",
                                            sex == input$sex)
                        )
        })
        
        ## Filter plot data table base on user inputs, aggregate total for filter
        
        plotDat = reactive({
                shootingsPlot %>%
                        filter(
                                conditional(input$race != "any",
                                            race == input$race),
                                conditional(input$age != "any",
                                            age == input$age),
                                conditional(input$sex != "either",
                                            sex == input$sex)
                        ) %>%
                        mutate(numb = nrow(.)) %>%
                        select(numb) %>%
                        slice_head(.)
        })       
        
        ## raceFull(), ageFull(), and sexFull() simply create display-friendly
        ## versions of those variables. 
        ## count() returns the number of shootings in the selected filter
        ## plotPer() returns the percentage of all shootings for the selected filter
        
        raceFull = reactive({
                case_when(
                        input$race == "any" ~ "",
                        input$race == "A" ~ "Asian",
                        input$race == "B" ~ "Black",
                        input$race == "H" ~ "Hispanic",
                        input$race == "N" ~ "Native American",
                        input$race == "W" ~ "White")
        })
        
        count = reactive({
                if (nrow(plotDat()) == 0) {
                        count = 0
                } else {
                        count = plotDat()$numb
                }
        })
        
        plotPer = reactive({
                if (nrow(plotDat()) == 0) {
                        plotPer = percent(0)
                } else {
                        plotPer = percent(plotDat()$numb / nrow(shootings),
                                          accuracy = .01)
                }
        })
        
        sexFull = reactive({
                case_when(  
                        input$sex == "F" ~ "females",
                        input$sex == "M" ~ "males")
        })
        
        ageFull = reactive({
                case_when(
                        input$age == "kid" ~ "under 13 years old",
                        input$age == "teen" ~ "between 13 and 19 years old",
                        input$age == "twenties" ~ "between 20 and 29 years old",
                        input$age == "thirties" ~ "between 30 and 39 years old",
                        input$age == "forties" ~ "between 40 and 49 years old",
                        input$age == "fifties" ~ "between 50 and 59 years old",
                        input$age == "senior" ~ "over 60 years old")
        })
        
        count = reactive({
                if (nrow(plotDat()) <= 0) {
                        count = 0
                } else {
                        count = plotDat()$numb
                }
        })
        
        plotPer = reactive({
                if (nrow(plotDat()) == 0) {
                        plotPer = percent(0)
                } else {
                        plotPer = percent(plotDat()$numb / nrow(shootings),
                                          accuracy = .01)
                }
        })
        
        ## Generate text to explain plot. General information about the app is
        ## displayed initially and whenever no filters are chosen
        
        output$textHTML = renderText({
                count = 
                        case_when(
                                input$race == "any" & input$age == "any" &
                                        input$sex == "either" ~
                                        paste("<h4><br>The",
                                              tags$a(href = "https://www.washingtonpost.com/",
                                                     "Washington Post"),
                                              "has tracked all fatal police
                                              shootings in the US since 2015.",
                                              "<br>",
                                              "On this page you can display this
                                              data on a map, filtering for race,
                                              age, and sex.",
                                              "<br><br>",
                                              "If you click down on the map to
                                              the level of individual shootings,
                                              you will see the",
                                              "<br>", "date and location of the
                                              shooting, as well as the name of
                                              the victim.",
                                              "<br><br>",
                                              "The pie chart beneath the map
                                              displays what fraction of all
                                              fatal shootings were of",
                                              "<br>",
                                              "those victims in the selected
                                              filter.",
                                              "</h4><br>",
                                              "Reference:",
                                              tags$a(href = "https://www.washingtonpost.com/graphics/investigations/police-shootings-database/", "Washington Post: Fatal Force"),
                                              "<br>",
                                              "Data:",
                                              tags$a(href = "https://github.com/washingtonpost/data-police-shootings",
                                              "Washington Post GitHub repository
                                              on fatal police shootings"),
                                              "<br><br>",
                                              "This page last updated 27
                                              September 2020",
                                              "<br>",
                                              "WaPo repository last updated 22
                                              September 2020", 
                                              sep = " "),
                                input$race != "any" & input$age == "any" &
                                        input$sex == "either" ~
                                        paste("<h4><br>", count(), raceFull(), "people
                                were fatally shot between 2015 and 2020,",
                                              "<br><br>",
                                              plotPer(),
                                              "of all fatal police shootings.",
                                              sep = " "),
                                input$race != "any" & input$age != "any" &
                                        input$sex == "either" ~
                                        paste("<h4><br>",
                                              count(),
                                              raceFull(),
                                              "people",
                                              ageFull(),
                                              "were fatally shot between 2015
                                              and 2020,",
                                              "<br><br>",
                                              plotPer(),
                                              "of all fatal police shootings.",
                                              "</h4>",
                                              sep = " "),
                                input$race != "any" & input$age == "any" &
                                        input$sex != "either" ~
                                        paste("<h4>",
                                              plotPer(),
                                              "of fatal police shootings were of",
                                              raceFull(),
                                              sexFull(),
                                              "</h4>",
                                              sep = " "),
                                input$race != "any" & input$age != "any" &
                                        input$sex != "either" ~
                                        paste("<h4>",
                                              plotPer(),
                                              "of fatal police shootings were of",
                                              raceFull(),
                                              sexFull(),
                                              ageFull(),
                                              "</h4>",
                                              sep = " "),
                                input$race == "any" & input$age != "any" &
                                        input$sex == "either" ~
                                        paste("<h4>",
                                              plotPer(),
                                              "of fatal police shootings were of
                                              people",
                                              ageFull(),
                                              "</h4>",
                                              sep = " "),
                                input$race == "any" & input$age != "any" &
                                        input$sex != "either" ~
                                        paste("<h4>", plotPer(),
                                              "of fatal police shootings were of",
                                              sexFull(),
                                              ageFull(),
                                              "</h4>",
                                              sep = " "),
                                input$race == "any" & input$age == "any" &
                                        input$sex != "either" ~
                                        paste("<h4>",
                                              plotPer(),
                                              "of fatal police shootings were of",
                                              sexFull(),
                                              "</h4>",
                                              sep = " "))
                
        })
        
        ## Generate map
        
        output$shootingsMap = renderLeaflet({
                mapDat() %>%
                        leaflet() %>%
                        addTiles() %>%
                        addMarkers(clusterOptions = markerClusterOptions(),
                                   popup = paste(mapDat()$name,
                                                 mapDat()$date,
                                                 mapDat()$cityState,
                                                 sep = " * "))
        })
        
        ## If filtered data != 0, generate plot
        ## Display plot only if some filters are selected
        
        output$shootingsPlot = renderPlotly({
                if (nrow(plotDat()) > 0) {
                        splot = plot_ly(
                                data = data.frame(
                                        cat = c("spec", "total"),
                                        number = c(plotDat()$numb,
                                                   (nrow(shootings) - plotDat()$numb))),
                                values = ~number, type = "pie",
                                hoverinfo = "skip") %>%
                                layout(autosize = F, height = 300,
                                       showlegend = FALSE) %>%
                                layout(plot_bgcolor ="#323E50") %>%
                                layout(paper_bgcolor ="#323E50")
                } else {
                        splot = NULL
                }
                if (input$race != "any" | input$age != "any" |
                    input$sex != "either") {
                        splot
                }
        })
}
