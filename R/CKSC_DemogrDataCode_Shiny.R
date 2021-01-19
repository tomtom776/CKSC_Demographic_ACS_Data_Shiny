# Author: Thomas Quintero
# Date Created: 9/15/2020
# Date Updated: 12/30/2020
# Description: To create a Shapefile of ACS data by Census Tract, Zip Code, Minor Civil Division, or Parish Boundary

#************************#
# Libraries
#************************#
library(tigris)
library(acs)
library(stringr)
library(sf)
library(dplyr)
library(shiny)

#************************#
# API Key Input
#************************#
# set your API key (retrieve your personal API key from http://api.census.gov/data/key_signup.html)
api.key.install('92a362005671c1729d433cdc54b9f49ba399300c', file = 'key.rda')
options(tigris_use_cache = TRUE)

#************************#
# Geographies, Polygons
#************************#
#zip code filtered to those starting with 48 and 49
zipcode <- reactive({
  zctas(state = "MI", starts_with = c('48','49'), cb = TRUE, year = 2019)
})
#tracts filtered to counties
tracts <- reactive({tracts(state = "MI", county = c(87,99,115,125,147,163), cb = TRUE, year = 2019)
})

#************************#
# Geographies, ACS Query
#************************#
ACS_zip <- geo.make(zip.code = '*')
ACS_tracts <- reactive({
  geo.make(state = "MI", county = c(87,99,115,125,147,163), tract="*")
})
# Define UI ----
ui <- fluidPage(
  selectInput(inputId = "DATA_YEAR", 
              "Choose a Year",
              c("2009" = 2009,
                "2014" = 2004,
                "2019" = 2019)),
  radioButtons(inputId = "DATA_SOURCE",
               "ACS",
               c("ACS" = "ACS")),
  radioButtons(inputId = "ACS_YEAR_SPAN",
               "Year Span of ACS Data Collection",
               c("5 Year" = "5")),
  radioButtons(inputId = "GEOGRAPHY",
               "Geographic Boundaries of Data",
               c("Census Tracts" = "tracts",
                 "Zip Codes" = "zipcode")),
  radioButtons(inputId = "BOUNDARY",
               "Geographic Boundaries of Data",
               c("Census Tracts" = tracts,
                 "Zip Codes" = zipcode)),
  radioButtons(inputId = "ACS_BOUNDARY",
               "Geographic Boundaries of Data",
               c("Census Tracts" = ACS_tracts,
                 "Zip Codes" = ACS_zip)),
  checkboxGroupInput(inputId = "COUNTIES",
                     "Select which counties you want data for",
                     c("Lapeer" = 87,
                       "Macomb" = 99,
                       "Monroe" = 115,
                       "Oakland" = 125,
                       "StClaire" = 147,
                       "Wayne" = 163),
                     selected = 163),
  actionButton("goButton", "Go!"),
  br(),
  p("The plot won't update until the button is clicked.",
    " Without the use of ", code("isolate()"),
    " in server.R, the plot would update whenever the slider",
    " changes."),
  tableOutput("dem")
)


# Define server logic ----
server <- function(input, output) {
    #************************#
    # File Name
    #************************#
    # Specify filename to save output to
    savename <- reactive({
      req(input$DATA_YEAR)
      req(input$DATA_SOURCE)
      req(input$GEOGRAPHY)
      paste("MI", input$DATA_YEAR, input$DATA_SOURCE, input$ACS_YEAR_SPAN, input$GEOGRAPHY, "DemogData.shp", sep = "_")
    })
   
    #************************#
    # ----Age and Sex % B01001----
    #************************#
    age <- reactive({
      acs.fetch(endyear = input$DATA_YEAR, span = input$ACS_YEAR_SPAN, geography = input$ACS_BOUNDARY, table.number = "B01001", col.names = "pretty")
    # use of col.names = "pretty" above gives the full column definitions
    # if you want Census variable IDs use col.names="auto".
    
    # to see what's available in the list
    attr(age, "acs.colnames")
    
    # convert to a data.frame for merging
    age_df <- data.frame(paste0(str_pad(age@geography$state, 
                                        2, "left", pad ="0"), 
                                str_pad(age@geography$county, 
                                        3, "left", pad ="0"),
                                str_pad(age@geography$countysubdivision, 
                                        5, "left", pad = "0"),
                                str_pad(age@geography$tract, 
                                        6, "left", pad = "0"),
                                str_pad(age@geography$blockgroup, 
                                        1, "left", pad = "0"),
                                str_pad(age@geography$zipcodetabulationarea, 
                                        5, "left", pad = "0")
    ), 
    age@estimate[,c(1:49)], 
    stringsAsFactors = FALSE)
    
    # adding together age intervals to get specified definitions of youth, young adults, adults, and seniors
    age_df$AgeUnder17 <- as.numeric(apply(age_df[,c(4:7,28:31)],1,sum))
    age_df$Age18to34 <- as.numeric(apply(age_df[,c(8:13,32:37)],1,sum))
    age_df$Age18to24 <- as.numeric(apply(age_df[,c(8:11,32:35)],1,sum))
    age_df$Age25to64 <- as.numeric(apply(age_df[,c(12:20,36:44)],1,sum))
    age_df$Age65Up <- as.numeric(apply(age_df[,c(21:26,45:50)],1,sum))
    
    # cutting out the original intervals of ages
    age_dfcut <- subset(age_df, select = -c(4:26,28:50))
    
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
    # #************************#
    # # ----Race B02001----
    # #************************#
    # race <- reactive({
    #   acs.fetch(endyear = input$DATA_YEAR, span = input$ACS_YEAR_SPAN, geography = input$ACS_BOUNDARY, table.number = "B02001", col.names = "pretty")
    # # use of col.names = "pretty" above gives the full column definitions
    # # if you want Census variable IDs use col.names="auto".
    # 
    # # to see what's available in the list
    # attr(race, "acs.colnames")
    # 
    # # convert to a data.frame for merging
    # race_df <- data.frame(paste0(str_pad(race@geography$state,
    #                                      2, "left", pad ="0"),
    #                              str_pad(race@geography$county,
    #                                      3, "left", pad ="0"),
    #                              str_pad(race@geography$countysubdivision,
    #                                      5, "left", pad = "0"),
    #                              str_pad(race@geography$tract,
    #                                      6, "left", pad = "0"),
    #                              str_pad(race@geography$blockgroup,
    #                                      1, "left", pad = "0"),
    #                              str_pad(race@geography$zipcodetabulationarea,
    #                                      5, "left", pad = "0")),
    #                       race@estimate[,c(1:10)],
    #                       stringsAsFactors = FALSE)
    # 
    # # renaming the columns to the correct titles
    # race_df <- select(race_df, c(1:11))
    # rownames(race_df)<-1:nrow(race_df)
    # names(race_df)<-c("GEOID", "total", "White", "Black","AmIndian","Asian","Pacific","OtherOne", "TwoorMore", "TwoorMoreOther", "ThreeorMore")
    # 
    # # calculating the % of sex and age groups, pasting them to the end of the data frame
    # race_df$prcWhite <- 100*(race_df$White/race_df$total)
    # race_df$prcBlack <- 100*(race_df$Black/race_df$total)
    # race_df$prcAmIn <- 100*(race_df$AmIndian/race_df$total)
    # race_df$prcAsian <- 100*(race_df$Asian/race_df$total)
    # race_df$prcPac <- 100*(race_df$Pacific/race_df$total)
    # race_df$prcOther1 <- 100*(race_df$OtherOne/race_df$total)
    # race_df$prc2More <- 100*(race_df$TwoorMore/race_df$total)
    # race_df$prc2MoreOth <- 100*(race_df$TwoorMoreOther/race_df$total)
    # race_df$prc3More <- 100*(race_df$ThreeorMore/race_df$total)
    # })
    # #************************#
    # # ----Hispanic Origin B03001----
    # #************************#
    # hisp <- reactive({
    #   acs.fetch(endyear = input$DATA_YEAR, span = input$ACS_YEAR_SPAN, geography = input$ACS_BOUNDARY, variable = acs.lookup(endyear=2014, span=5, table.number="B03001"), col.names = "pretty")
    # # use of col.names = "pretty" above gives the full column definitions
    # # if you want Census variable IDs use col.names="auto".
    # 
    # # to see what's available in the list
    # attr(hisp, "acs.colnames")
    # 
    # # convert to a data.frame for merging
    # hisp_df <- data.frame(paste0(str_pad(hisp@geography$state,
    #                                      2, "left", pad ="0"),
    #                              str_pad(hisp@geography$county,
    #                                      3, "left", pad ="0"),
    #                              str_pad(hisp@geography$countysubdivision,
    #                                      5, "left", pad = "0"),
    #                              str_pad(hisp@geography$tract,
    #                                      6, "left", pad = "0"),
    #                              str_pad(hisp@geography$blockgroup,
    #                                      1, "left", pad = "0"),
    #                              str_pad(hisp@geography$zipcodetabulationarea,
    #                                      5, "left", pad = "0")),
    #                       hisp@estimate[,c(1:3)],
    #                       stringsAsFactors = FALSE)
    # 
    # # renaming the columns to the correct titles
    # hisp_df <- select(hisp_df, c(1:4))
    # rownames(hisp_df)<-1:nrow(hisp_df)
    # names(hisp_df)<-c("GEOID", "total", "NotHisp", "Hisp")
    # 
    # # calculating the % of sex and age groups, pasting them to the end of the data frame
    # hisp_df$prcNotHisp <- 100*(hisp_df$NotHisp/hisp_df$total)
    # hisp_df$prcHisp <- 100*(hisp_df$Hisp/hisp_df$total)
    # })
    # #************************#
    # # ----Nativity B05002----
    # #************************#
    # nativity <- reactive({
    #   acs.fetch(endyear = input$DATA_YEAR, span = input$ACS_YEAR_SPAN, geography = input$ACS_BOUNDARY, table.number = "B05002", col.names = "pretty")
    # # use of col.names = "pretty" above gives the full column definitions
    # # if you want Census variable IDs use col.names="auto".
    # 
    # # to see what's available in the list
    # attr(nativity, "acs.colnames")
    # 
    # # convert to a data.frame for merging
    # nativity_df <- data.frame(paste0(str_pad(nativity@geography$state,
    #                                          2, "left", pad ="0"),
    #                                  str_pad(nativity@geography$county,
    #                                          3, "left", pad ="0"),
    #                                  str_pad(nativity@geography$countysubdivision,
    #                                          5, "left", pad = "0"),
    #                                  str_pad(nativity@geography$tract,
    #                                          6, "left", pad = "0"),
    #                                  str_pad(nativity@geography$blockgroup,
    #                                          1, "left", pad = "0"),
    #                                  str_pad(nativity@geography$zipcodetabulationarea,
    #                                          5, "left", pad = "0")),
    #                           nativity@estimate[,c(1:2,13)],
    #                           stringsAsFactors = FALSE)
    # 
    # # renaming the columns to the correct titles
    # nativity_df <- select(nativity_df, c(1:4))
    # rownames(nativity_df)<-1:nrow(nativity_df)
    # names(nativity_df)<-c("GEOID", "total", "Native", "Foreign")
    # 
    # # calculating the % of sex and age groups, pasting them to the end of the data frame
    # nativity_df$prcNative <- 100*(nativity_df$Native/nativity_df$total)
    # nativity_df$prcForeign <- 100*(nativity_df$Foreign/nativity_df$total)
    # })
    # #************************#
    # # ----Household Type B11001----
    # #************************#
    # house <- reactive({
    #   acs.fetch(endyear = input$DATA_YEAR, span = input$ACS_YEAR_SPAN, geography = input$ACS_BOUNDARY, table.number = "B11001", col.names = "pretty")
    # # use of col.names = "pretty" above gives the full column definitions
    # # if you want Census variable IDs use col.names="auto".
    # 
    # # to see what's available in the list
    # attr(house, "acs.colnames")
    # 
    # # convert to a data.frame for merging
    # house_df <- data.frame(paste0(str_pad(house@geography$state,
    #                                       2, "left", pad ="0"),
    #                               str_pad(house@geography$county,
    #                                       3, "left", pad ="0"),
    #                               str_pad(house@geography$countysubdivision,
    #                                       5, "left", pad = "0"),
    #                               str_pad(house@geography$tract,
    #                                       6, "left", pad = "0"),
    #                               str_pad(house@geography$blockgroup,
    #                                       1, "left", pad = "0"),
    #                               str_pad(house@geography$zipcodetabulationarea,
    #                                       5, "left", pad = "0")),
    #                        house@estimate[,c(1:2,7)],
    #                        stringsAsFactors = FALSE)
    # 
    # # renaming the columns to the correct titles
    # house_df <- select(house_df, c(1:4))
    # rownames(house_df)<-1:nrow(house_df)
    # names(house_df)<-c("GEOID", "total", "Fam", "NonFam")
    # 
    # # calculating the % of sex and age groups, pasting them to the end of the data frame
    # house_df$prcFam <- 100*(house_df$Fam/house_df$total)
    # house_df$prcNonFam <- 100*(house_df$NonFam/house_df$total)
    # })
    # #************************#
    # # ----Educational Attainment #15002----
    # #************************#
    # ed <- reactive({
    #   acs.fetch(endyear = input$DATA_YEAR, span = input$ACS_YEAR_SPAN, geography = input$ACS_BOUNDARY, table.number = "B15002", col.names = "pretty")
    # # use of col.names = "pretty" above gives the full column definitions
    # # if you want Census variable IDs use col.names="auto".
    # 
    # # to see what's available in the list
    # attr(ed, "acs.colnames")
    # 
    # # convert to a data.frame for merging
    # ed_df <- data.frame(paste0(str_pad(ed@geography$state,
    #                                    2, "left", pad ="0"),
    #                            str_pad(ed@geography$county,
    #                                    3, "left", pad ="0"),
    #                            str_pad(ed@geography$countysubdivision,
    #                                    5, "left", pad = "0"),
    #                            str_pad(ed@geography$tract,
    #                                    6, "left", pad = "0"),
    #                            str_pad(ed@geography$blockgroup,
    #                                    1, "left", pad = "0"),
    #                            str_pad(ed@geography$zipcodetabulationarea,
    #                                    5, "left", pad = "0")),
    #                     ed@estimate[,c(1:35)],
    #                     stringsAsFactors = FALSE)
    # ed_df$elem <- as.numeric(apply(ed_df[,c(3:5,20:22)],1,sum))
    # ed_df$mid <- as.numeric(apply(ed_df[,c(6,23)],1,sum))
    # ed_df$highdrop <- as.numeric(apply(ed_df[,c(7:10,24:27)],1,sum))
    # ed_df$high <- as.numeric(apply(ed_df[,c(11,28)],1,sum))
    # ed_df$colldrop <- as.numeric(apply(ed_df[,c(12:13,29:30)],1,sum))
    # ed_df$assoc <- as.numeric(apply(ed_df[,c(14,31)],1,sum))
    # ed_df$bach <- as.numeric(apply(ed_df[,c(15,32)],1,sum))
    # ed_df$abovebach <- as.numeric(apply(ed_df[,c(16:18,33:35)],1,sum))
    # 
    # ed_df_c <- subset(ed_df, select = -c(3:36))
    # 
    # # renaming the columns to the correct titles
    # ed_df_c <- select(ed_df_c, c(1:10))
    # rownames(ed_df_c)<-1:nrow(ed_df_c)
    # names(ed_df_c)<-c("GEOID", "total","elem","mid","highdrop","high","colldrop","assoc","bach","abovebach")
    # 
    # # calculating the % of sex and age groups, pasting them to the end of the data frame
    # ed_df_c$prcelem <- 100*(ed_df_c$elem/ed_df_c$total)
    # ed_df_c$prcmid <- 100*(ed_df_c$mid/ed_df_c$total)
    # ed_df_c$prchighdrop <- 100*(ed_df_c$highdrop/ed_df_c$total)
    # ed_df_c$prchigh <- 100*(ed_df_c$high/ed_df_c$total)
    # ed_df_c$prccolldrop <- 100*(ed_df_c$colldrop/ed_df_c$total)
    # ed_df_c$prcassoc <- 100*(ed_df_c$assoc/ed_df_c$total)
    # ed_df_c$prcbach <- 100*(ed_df_c$bach/ed_df_c$total)
    # ed_df_c$prcabovebach <- 100*(ed_df_c$abovebach/ed_df_c$total)
    # })
    # #************************#
    # # ----Poverty Status B17001----
    # #************************#
    # pov <- reactive({
    #   acs.fetch(endyear = input$DATA_YEAR, span = input$ACS_YEAR_SPAN, geography = input$ACS_BOUNDARY, table.number = "B17001", col.names = "pretty")
    # # use of col.names = "pretty" above gives the full column definitions
    # # if you want Census variable IDs use col.names="auto".
    # 
    # # to see what's available in the list
    # attr(pov, "acs.colnames")
    # 
    # # convert to a data.frame for merging
    # pov_df <- data.frame(paste0(str_pad(pov@geography$state,
    #                                     2, "left", pad ="0"),
    #                             str_pad(pov@geography$county,
    #                                     3, "left", pad ="0"),
    #                             str_pad(pov@geography$countysubdivision,
    #                                     5, "left", pad = "0"),
    #                             str_pad(pov@geography$tract,
    #                                     6, "left", pad = "0"),
    #                             str_pad(pov@geography$blockgroup,
    #                                     1, "left", pad = "0"),
    #                             str_pad(pov@geography$zipcodetabulationarea,
    #                                     5, "left", pad = "0")),
    #                      pov@estimate[,c(1:2,31)],
    #                      stringsAsFactors = FALSE)
    # 
    # # renaming the columns to the correct titles
    # pov_df <- select(pov_df, c(1:4))
    # rownames(pov_df)<-1:nrow(pov_df)
    # names(pov_df)<-c("GEOID", "total", "Pov","NonPov")
    # 
    # # calculating the % of sex and age groups, pasting them to the end of the data frame
    # pov_df$prcPov <- 100*(pov_df$Pov/pov_df$total)
    # pov_df$prcNonPov <- 100*(pov_df$NonPov/pov_df$total)
    # })
    # #************************#
    # # ----Disability Status B18101----
    # #************************#
    # disab <- reactive({acs.fetch(endyear = input$DATA_YEAR, span = input$ACS_YEAR_SPAN, geography = input$ACS_BOUNDARY, table.number = "B18101", col.names = "pretty")
    # # use of col.names = "pretty" above gives the full column definitions
    # # if you want Census variable IDs use col.names="auto".
    # 
    # # to see what's available in the list
    # attr(disab, "acs.colnames")
    # 
    # # convert to a data.frame for merging
    # disab_df <- data.frame(paste0(str_pad(disab@geography$state,
    #                                       2, "left", pad ="0"),
    #                               str_pad(disab@geography$county,
    #                                       3, "left", pad ="0"),
    #                               str_pad(disab@geography$countysubdivision,
    #                                       5, "left", pad = "0"),
    #                               str_pad(disab@geography$tract,
    #                                       6, "left", pad = "0"),
    #                               str_pad(disab@geography$blockgroup,
    #                                       1, "left", pad = "0"),
    #                               str_pad(disab@geography$zipcodetabulationarea,
    #                                       5, "left", pad = "0")),
    #                        disab@estimate[,c(1,4:5,7:8,10:11,13:14,16:17,19:20,23:24,26:27,29:30,32:33, 35:36, 38:39)],
    #                        stringsAsFactors = FALSE)
    # 
    # disab_df$Disabled <- as.numeric(apply(disab_df[,c(3,5,7,9,11,13,15,17,19,21,23,25)],1,sum))
    # disab_df$NotDisabled <- as.numeric(apply(disab_df[,c(4,6,8,10,12,14,16,18,20,22,24)],1,sum))
    # 
    # # cutting out the original intervals of ages
    # disab_df_c <- subset(disab_df, select = -c(3:26))
    # 
    # # renaming the columns to the correct titles
    # disab_df_c <- select(disab_df_c, c(1:4))
    # rownames(disab_df_c)<-1:nrow(disab_df_c)
    # names(disab_df_c)<-c("GEOID", "total", "disab","nondisab")
    # 
    # # calculating the % of sex and age groups, pasting them to the end of the data frame
    # disab_df_c$prcdisab <- 100*(disab_df_c$disab/disab_df_c$total)
    # disab_df_c$prcNondisab <- 100*(disab_df_c$nondisab/disab_df_c$total)
    # })
    # #************************#
    # # ----Median Household Income B19013----
    # #************************#
    # income <- reactive({
    #   acs.fetch(endyear = input$DATA_YEAR, span = input$ACS_YEAR_SPAN, geography = input$ACS_BOUNDARY, table.number = "B19013", col.names = "pretty")
    # # use of col.names = "pretty" above gives the full column definitions
    # # if you want Census variable IDs use col.names="auto".
    # 
    # # to see what's available in the list
    # attr(income, "acs.colnames")
    # 
    # # convert to a data.frame for merging
    # income_df <- data.frame(paste0(str_pad(income@geography$state,
    #                                        2, "left", pad ="0"),
    #                                str_pad(income@geography$county,
    #                                        3, "left", pad ="0"),
    #                                str_pad(income@geography$countysubdivision,
    #                                        5, "left", pad = "0"),
    #                                str_pad(income@geography$tract,
    #                                        6, "left", pad = "0"),
    #                                str_pad(income@geography$blockgroup,
    #                                        1, "left", pad = "0"),
    #                                str_pad(income@geography$zipcodetabulationarea,
    #                                        5, "left", pad = "0")),
    #                         income@estimate[,c(1)],
    #                         stringsAsFactors = FALSE)
    # 
    # # renaming the columns to the correct titles
    # income_df <- select(income_df, c(1:2))
    # rownames(income_df)<-1:nrow(income_df)
    # names(income_df)<-c("GEOID", "MedianIncome")
    # })
    # #************************#
    # # ----Veteran Status B21001----
    # #************************#
    # vet <- reactive({acs.fetch(endyear = input$DATA_YEAR, span = input$ACS_YEAR_SPAN, geography = input$ACS_BOUNDARY, table.number = "B21001", col.names = "pretty")
    # # use of col.names = "pretty" above gives the full column definitions
    # # if you want Census variable IDs use col.names="auto".
    # 
    # # to see what's available in the list
    # attr(vet, "acs.colnames")
    # 
    # # convert to a data.frame for merging
    # vet_df <- data.frame(paste0(str_pad(vet@geography$state,
    #                                     2, "left", pad ="0"),
    #                             str_pad(vet@geography$county,
    #                                     3, "left", pad ="0"),
    #                             str_pad(vet@geography$countysubdivision,
    #                                     5, "left", pad = "0"),
    #                             str_pad(vet@geography$tract,
    #                                     6, "left", pad = "0"),
    #                             str_pad(vet@geography$blockgroup,
    #                                     1, "left", pad = "0"),
    #                             str_pad(vet@geography$zipcodetabulationarea,
    #                                     5, "left", pad = "0")),
    #                      vet@estimate[,c(1:3)],
    #                      stringsAsFactors = FALSE)
    # 
    # # renaming the columns to the correct titles
    # vet_df <- select(vet_df, c(1:4))
    # rownames(vet_df)<-1:nrow(vet_df)
    # names(vet_df)<-c("GEOID", "total", "vet","Nonvet")
    # 
    # # calculating the % of sex and age groups, pasting them to the end of the data frame
    # vet_df$prcvet <- 100*(vet_df$vet/vet_df$total)
    # vet_df$prcNonvet <- 100*(vet_df$Nonvet/vet_df$total)
    # })
    # #************************#
    # # ----Employment Status B23001----
    # #************************#
    # employ <- reactive({acs.fetch(endyear = input$DATA_YEAR, span = input$ACS_YEAR_SPAN, geography = input$ACS_BOUNDARY, table.number = "B23001", col.names = "pretty")
    # # use of col.names = "pretty" above gives the full column definitions
    # # if you want Census variable IDs use col.names="auto".
    # 
    # # to see what's available in the list
    # attr(employ, "acs.colnames")
    # 
    # # convert to a data.frame for merging
    # employ_df <- data.frame(paste0(str_pad(employ@geography$state,
    #                                        2, "left", pad ="0"),
    #                                str_pad(employ@geography$county,
    #                                        3, "left", pad ="0"),
    #                                str_pad(employ@geography$countysubdivision,
    #                                        5, "left", pad = "0"),
    #                                str_pad(employ@geography$tract,
    #                                        6, "left", pad = "0"),
    #                                str_pad(employ@geography$blockgroup,
    #                                        1, "left", pad = "0"),
    #                                str_pad(employ@geography$zipcodetabulationarea,
    #                                        5, "left", pad = "0")),
    #                         employ@estimate[,c(1,8,15,22,29,36,43,50,57,64,71,76,81,86,94,101,108,115,122,129,136,143,150,157,162,167,172)],
    #                         stringsAsFactors = FALSE)
    # 
    # employ_df$Unemployed <- as.numeric(apply(employ_df[,c(3:28)],1,sum))
    # 
    # # cutting out the original intervals of ages
    # employ_df_c <- subset(employ_df, select = -c(3:28))
    # 
    # # renaming the columns to the correct titles
    # employ_df_c <- select(employ_df_c, c(1:3))
    # rownames(employ_df_c)<-1:nrow(employ_df_c)
    # names(employ_df_c)<-c("GEOID", "total", "unemploy")
    # 
    # # calculating the % of sex and age groups, pasting them to the end of the data frame
    # employ_df_c$prcunemploy <- 100*(employ_df_c$unemploy/employ_df_c$total)
    # })
    #************************#
    # ----Merging to 1 file----
    #************************
    merge_me = reactive({
      age_dfcut %>% 
      full_join(employ_df_c, by = 'GEOID') %>%
      full_join(hisp_df, by = 'GEOID') %>%
      full_join(income_df,by = 'GEOID') %>%
      full_join(nativity_df,by = 'GEOID') %>%
      full_join(pov_df, by = 'GEOID') %>%
      full_join(race_df, by = 'GEOID') %>%
      full_join(vet_df, by = 'GEOID') %>%  
      full_join(disab_df_c, by = 'GEOID') %>%
      full_join(house_df, by = 'GEOID') %>%
      full_join(ed_df_c, by = 'GEOID')
    })
    
    merged <- reactive({
      geo_join(input$BOUNDARY, merge_me, "GEOID", "GEOID")
      })
    
    output$dem <- renderPlot({
      age
      })
    
    #************************#
    # ----Exporting to Shapefile----
    #************************#
    #st_write(merged, savename, driver = "ESRI Shapefile")
  
}

# Run the app ----
shinyApp(ui = ui, server = server)

