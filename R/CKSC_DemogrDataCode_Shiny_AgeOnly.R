# Author: Thomas Quintero
# Date Created: 9/15/2020
# Date Updated: 1/19/2021
# Description: To create a Shapefile of ACS data by Census Tract, Zip Code, Minor Civil Division, or Parish Boundary

library(shiny)
library(tigris)
library(acs)
library(stringr)
library(sf)
library(dplyr)
library(leaflet)

#************************#
# API Key Input
#************************#
# set your API key (retrieve your personal API key from http://api.census.gov/data/key_signup.html)
api.key.install('92a362005671c1729d433cdc54b9f49ba399300c', file = 'key.rda')
options(tigris_use_cache = TRUE)


# Define UI ----
ui <- fluidPage(
  selectInput(inputId = "DATA_YEAR", 
              "Choose a Year",
              c("2009" = 2009,
                "2014" = 2014,
                "2019" = 2019)),
  radioButtons(inputId = "GEOGRAPHY",
               "Geographic Boundaries of Data",
               c("Census Tracts" = "tracts",
                 "Zip Codes" = "zipcode")),
  checkboxGroupInput(inputId = "COUNTIES",
                     "Select which counties you want data for",
                     c("Lapeer" = 87,
                       "Macomb" = 99,
                       "Monroe" = 115,
                       "Oakland" = 125,
                       "StClaire" = 147,
                       "Wayne" = 163),
                     selected = 163),
  actionButton("recalc", "Go!"),
  # actionButton("downloadSHP", "Download Shapefile"),
  # actionButton("downloadCSV", "Download .CSV File"),
  br(),
  p("The plot won't update until the button is clicked."),
  leafletOutput(outputId = "demo")
)


# Define server logic ----
server <- function(input, output) {
  
  BOUNDARY <- reactive({
    if(input$GEOGRAPHY == "tracts"){
      BOUNDARY = 
        tracts(state = "MI", 
               county = input$COUNTIES, 
               cb = TRUE, 
               year = input$DATA_YEAR)
    } else {
      BOUNDARY = 
        zctas(state = "MI", 
              starts_with = c('48','49'), 
              cb = TRUE, 
              year = input$DATA_YEAR)
    }
    
  })
  
  ACS_BOUNDARY <- reactive({
    if(input$GEOGRAPHY == "tracts"){
      ACS_BOUNDARY = 
        geo.make(state = "MI", 
                 county = input$COUNTIES, 
                 tract="*")
    } else {
      ACS_BOUNDARY = 
        geo.make(zip.code = '*')
    }
  })
  
  #************************#
  # ----Age and Sex % B01001----
  #************************#
  age <- reactive({acs.fetch(endyear = input$DATA_YEAR, 
                     span = 5, 
                     geography = ACS_BOUNDARY(), 
                     table.number = "B01001", 
                     col.names = "pretty")
  })
  
  
  
  # convert to a data.frame for merging
  age_df <- reactive({data.frame(paste0(str_pad(age@geography$state,
                                      2, "left", pad ="0"),
                              str_pad(age@geography$county,
                                      3, "left", pad ="0"),
                              str_pad(age@geography$tract,
                                      6, "left", pad = "0"),
                              str_pad(age@geography$zipcodetabulationarea,
                                      5, "left", pad = "0")),
                       age@estimate[,c(1:49)],
                       stringsAsFactors = FALSE)
    
    # adding together age intervals to get specified definitions of youth, young adults, adults, and seniors
    age_df$AgeUnder17 <- as.numeric(apply(age_df[,c(4:7,28:31)],1,sum))
    age_df$Age18to34 <- as.numeric(apply(age_df[,c(8:13,32:37)],1,sum))
    age_df$Age18to24 <- as.numeric(apply(age_df[,c(8:11,32:35)],1,sum))
    age_df$Age25to64 <- as.numeric(apply(age_df[,c(12:20,36:44)],1,sum))
    age_df$Age65Up <- as.numeric(apply(age_df[,c(21:26,45:50)],1,sum))
  })
  
  # cutting out the original intervals of ages
  age_dfcut <- reactive({subset(age_df, select = -c(4:26,28:50))
  
  # renaming the columns to the correct titles
    age_dfcut <- select(age_dfcut, c(1:9))
    rownames(age_dfcut)<-1:nrow(age_dfcut)
    names(age_dfcut)<-c("GEOID", "total", "male", "female","under17","a18to34","a18to24","a25to64", "a65up")
    
    # calculating the % of sex and age groups, pasting them to the end of the data frame
    age_dfcut$percentmale <- 100*(age_dfcut$male/age_dfcut$total)
    age_dfcut$percentfemale <- 100*(age_dfcut$female/age_dfcut$total)
    age_dfcut$percentunder17 <- 100*(age_dfcut$under17/age_dfcut$total)
    age_dfcut$percent18to34 <- 100*(age_dfcut$a18to34/age_dfcut$total)
    age_dfcut$percent18to24 <- 100*(age_dfcut$a18to24/age_dfcut$total)
    age_dfcut$percent25to64 <- 100*(age_dfcut$a25to64/age_dfcut$total)
    age_dfcut$percent65up <- 100*(age_dfcut$a65up/age_dfcut$total)
  })

  merged <- reactive({
    geo_join(BOUNDARY(), age_dfcut, "GEOID", "GEOID")
  })
  
  output$demo <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("OpenStreetMap.BlackAndWhite") %>%
      addPolygons(data = merged())
  })
  
  # Specify filename to save output to
  savename <- reactive({
    paste("MI", 
          input$DATA_YEAR, 
          "ACS_5yr", 
          input$GEOGRAPHY, 
          "DemogData.shp", 
          sep = "_")
  })
  
  # eventReactive(input$downloadSHP,{
  #   st_write(merged(), savename(), driver = "ESRI Shapefile")
  # })
  # eventReactive(input$downloadCSV,{
  #   write.csv(age_dfcut, savename())
  # })

}

# Run the app ----
shinyApp(ui = ui, server = server)

