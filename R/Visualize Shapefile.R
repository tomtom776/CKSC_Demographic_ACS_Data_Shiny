library(shiny)
library(readr)
library(RColorBrewer)
library(leaflet)
library(leaflet.esri)

#https://github.com/byollin/ShinyLeaflet/blob/master/CrimeMap/app.R

map_service_url = 'https://services6.arcgis.com/PGzqfIQhd8muL96B/arcgis/rest/services/MI_2018_ACS_5_ZipCode_DemogData_TriCty/FeatureServer/0'


# ui object
ui = navbarPage(
  title = 'Detroit Demographic Data', 
  tabPanel("Map",
           sidebarLayout(
             sidebarPanel(
               selectInput("colors", "Color Scheme",
                           rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
               ),
               selectInput(inputId = "attribute",
                           "Select the demographic to visualize",
                           c("% of Men" = "prcntml",
                             "% of Women" = "prcntfm") #prcnt17,prc1834,prc1824,prc2564,prcnt65,unemply,prcnmpl,NotHisp,Hisp,prcNtHs,prcHisp,MdnIncm,Native,Foreign,prcNatv,prcFrgn,Pov,NonPov,prcPov,prcNnPv,White,Black,AmIndin,Asian,Pacific,OtherOn,TwoorMr,TwrMrOt,ThrerMr,prcWhit,prcBlck,prcAmIn,prcAsin,prcPac,prcOth1,prc2Mor,prc2MrO,prc3Mor,vet,Nonvet,prcvet,prcNnvt,disab,nondisb,prcdisb,prcNnds,Fam,NonFam,prcFam,prcNnFm,elem,mid,highdrp,high,colldrp,assoc,bach,abovbch,prcelem,prcmid,prchghd,prchigh,prcclld,prcassc,prcbach,prcbvbc)
               )
             ),
             mainPanel(leafletOutput('map')
             )
    )
  )
)

# server()
server = function(input, output) {
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorQuantile(input$colors,
                  n=4,
                  domain = 0:100)
  })
  
  field <- reactive({
    input$attribute
  })
  
  fld <- field()
  
  output$map = renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -83.24, lat = 42.33, zoom = 10) %>%
      addEsriFeatureLayer(map_service_url,
                          featureLayerOptions(
                            fields = "GEOID", fld
                          )
                          )
  })
}

# shinyApp()
shinyApp(ui = ui, server = server)