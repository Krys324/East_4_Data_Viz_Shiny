
# Lets load the needed libraries

library(shiny)
library(stringr)
library(tidyverse)
library(sf)
library(ggmap)
library(leaflet)
library(stringr)
library(mapview)
library(tidycensus)
library(tigris)
options(tigris_use_cache = TRUE)

# We register the key
census_api_key("8fbe314ac36be89fd50d19e1738681f359d96fc0")


# Let's load the schools data
sb <- get_acs(state = "IN", county = "St. Joseph County", geography = "tract",
              variables = "B05009_020", geometry = TRUE)
sb <- sb %>% st_transform(crs = "+init=epsg:4326")
schools <- st_read('School_Boundaries.shp',stringsAsFactors = F)


# Let's load the public facilities data
facilities <- read.csv("Public_Facilities.csv", stringsAsFactors = F)
facilities <- facilities %>% separate(POPL_ADDR1, c("POPL_ADDR1","ADD2"), "\\(")



# Let's load Abandoned Properties data
abandoned <- st_read("Abandoned_Property_Parcels.shp", stringsAsFactors = F)
abandoned <- abandoned %>% mutate(Outcome_St = ifelse(is.na(Outcome_St), 'Unknown', Outcome_St))
abandoned$Date_of_Ou <- as.Date(abandoned$Date_of_Ou, format = "%Y-%m-%d")


# Let's load Street Lights data
sl <- read.csv("Street_Lights.csv", stringsAsFactors = F)
sl_read <- sl %>% 
  st_as_sf(coords = c("Lon","Lat")) %>% 
  st_set_crs(value = 4326)

# Cleaning up empty/blank values as Unknown
sl_read <- sl_read %>% mutate(Pole_Type=ifelse(Pole_Type=="", "Unknown", Pole_Type))
sl_read$Pole_Type <- sl_read$Pole_Type %>% str_replace_all(pattern = " ", replacement = "Unknown")
sl_read <- sl_read %>% mutate(Ownership=ifelse(Ownership=="", "Unknown", Ownership))


# Define UI for application that draws a histogram
ui <- navbarPage("City of South Bend",
                 tabPanel("Public Facilities",             
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(inputId = "POPL_TYPE",
                                          label = "Choose a facility",
                                          choices = c("ALL", unique(facilities$POPL_TYPE)),
                                          selected = "ALL"),# End 
                            ),
                            mainPanel(leafletOutput(outputId = "mymap_HB"))#end mainpanel
                          )#end sidebarlayout
                 ),#end tabpanel#1
                 
                 tabPanel("Schools", 
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(inputId = "SchoolType",
                                          label = "Choose a School Type:",
                                          choices = c("ALL", unique(schools$SchoolType)),
                                          selected = "ALL")
                            ),#End SidebarLayout
                            mainPanel(leafletOutput('mymap'))#end mainpanel
                          )#end sidebarlayout
                 ),#end tabpanel#2
                 
                 tabPanel("Abandoned Properties", 
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(inputId = "Outcome_St",
                                          label = "Choose a Property Status:",
                                          choices = c("ALL", unique(abandoned$Outcome_St)),
                                          selected = "ALL")
                            ),
                            mainPanel(leafletOutput(outputId = "mymap_mel"))
                          )#end sidebar layout
                 ), # end of tabpanel #3
                 
                 tabPanel("Street Lights",
                          tabsetPanel(tabPanel(title = "By Ownership",
                                               sidebarLayout(
                                                 sidebarPanel(
                                                   selectInput(inputId = "Ownership",
                                                               label = "Choose a Streetlight Ownership Type:",
                                                               choices = c("ALL", unique(sl_read$Ownership)),
                                                               selected = "ALL")
                                                 ),
                                                 mainPanel(leafletOutput(outputId = "own"))
                                               )
                          ),
                          tabPanel(title = "By Pole Type",
                                   sidebarLayout(
                                     sidebarPanel(
                                       selectInput(inputId = "Pole_Type",
                                                   label = "Choose a Streetlight Pole Type:",
                                                   choices = c("ALL", unique(sl_read$Pole_Type)),
                                                   selected = "ALL")
                                     ),
                                     mainPanel(leafletOutput(outputId = "pole"))
                                   )),
                          mainPanel(leafletOutput(outputId = "ownl"))
                          ))# end of tabpanel #4
                 
                 
)#End UI

# Define server logic required to draw a histogram
server <- function(input, output) {
  schools.subset <- reactive({
    if (input$SchoolType == "ALL"){
      rtrn <- schools
    }
    else{
      rtrn <- schools[schools$SchoolType == input$SchoolType,]
      
    }
    return(rtrn)
  })
  
  
  output$mymap <- renderLeaflet({
    mybins = c(0,300,600,900,1200,1500,1800,2100)
    pal <- colorFactor(c("cadetblue", "sienna2"), domain = c("Private","Public"))
    pal2 <- colorBin(palette = 'YlGn', domain = sb$estimate, bins=mybins)
    leaflet() %>% 
      addTiles() %>% 
      addPolygons(data = sb,
                  color= ~pal2(estimate),
                  popup = ~estimate,
                  stroke = FALSE,
                  weight = 4, 
                  smoothFactor = 1,
                  opacity = 1.0, 
                  fillOpacity = 0.75) %>%
      addPolygons(data = schools.subset(),
                  color = ~pal(SchoolType),
                  popup = ~School,
                  weight = 1, 
                  smoothFactor = 0.5,
                  opacity = 1.0, 
                  fillOpacity = 0.5) %>%
      addLegend("bottomright",
                pal = pal, 
                values = schools.subset()$SchoolType, 
                title = 'School Type',
                opacity = 1) %>% 
      addLegend("bottomleft", 
                pal = pal2, 
                values = sb$estimate,
                title = "Population Ages 6 to 17",
                opacity = 1)
    
    
  }) # end of schools
  
  facilities.subset <- reactive({
    if (input$POPL_TYPE == "ALL"){
      rtrn <- facilities
    }
    else{
      rtrn <- facilities[facilities$POPL_TYPE == input$POPL_TYPE,]
      
    }
    return(rtrn)
  })
  
  output$mymap_HB <- renderLeaflet({
    facilities.spatial <- facilities.subset() %>% 
      st_as_sf(coords = c("Lon","Lat")) %>% 
      st_set_crs(value = 4326)
    pal3 <- colorFactor(c("coral", "burlywood4","cadetblue"), domain = c("FIRE STATION", "LIBRARY", "POLICE STATION"))
    
    facilities.spatial$popup <- paste("<b>",facilities.spatial$POPL_NAME,"</b><br>",
                                      facilities.spatial$POPL_ADDR1, sep ="")
    leaflet()  %>%
      addTiles()  %>%
      addCircleMarkers(data = facilities.spatial,radius = 3,
                       color = ~pal3(POPL_TYPE),
                       stroke = FALSE, fillOpacity = 1, popup = ~popup) %>% 
      addLegend("bottomright",pal=pal3, values = facilities.spatial$POPL_TYPE, title = "Type of Public Facilities",opacity = 1)
  }) #end of facilities
  
  
  abandoned.subset <- reactive({
    if (input$Outcome_St == "ALL"){
      rtrn <- abandoned
    }
    else{
      rtrn <- abandoned[abandoned$Outcome_St == input$Outcome_St,]
      
    }
    return(rtrn)
  })
  
  abandoned$popup <- paste("Outcome Status: ",abandoned$Outcome_St,"<br>",
                           "Date of Outcome: ",abandoned$Date_of_Ou,"<br>",
                           "Council District: ",abandoned$Council_Di,"<br>",
                           "Code Enforcement : ",abandoned$Code_Enfor,"<br>",
                           "Structures: ",abandoned$Structures,"<br>")
  
  
  output$mymap_mel <- renderLeaflet({
    abandoned.spatial <- abandoned.subset() %>% 
      st_as_sf(coords = c("Lon","Lat")) %>% 
      st_set_crs(value = 4326)
    pal4 <- colorFactor(c("coral3", "burlywood4","cadetblue","grey33","sienna2","goldenrod"), domain = c("Deconstructed", "Demolished", "Occupied & Not Repaired", "Repaired","Repaired & Occupied","Unknown"))
    leaflet()  %>%
      addTiles()  %>%
      addPolygons(data = abandoned.spatial, color = ~pal4(Outcome_St), popup = ~popup ,weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5) %>%
      addLegend("bottomright",pal=pal4, values = abandoned.spatial$Outcome_St, title = "Property Status",opacity = 1)
  }) # end of Abandoned Properties
  
  
  # leafletOutput(outputId = "map")
  
  
  
  lights.subset1 <- reactive({
    if (input$Ownership == "ALL"){
      rtrn <- sl_read
    }
    else{
      rtrn <- sl_read[sl_read$Ownership == input$Ownership,]
      
    }
    return(rtrn)
  })
  lights.subset2 <- reactive({
    if (input$Pole_Type == "ALL"){
      rtrn <- sl_read
    }
    else{
      rtrn <- sl_read[sl_read$Pole_Type == input$Pole_Type,]
      
    }
    return(rtrn)
  })
  sl_read$popup <- paste("Pole Number: ",sl_read$Pole_Num_1,"<br>",
                         "Ownership: ",sl_read$Ownership,"<br>",
                         "Type : ",sl_read$Pole_Type,"<br>",
                         "Service: ",sl_read$Service,"<br>",
                         "Bulb Type: ",sl_read$Bulb_Type,"<br>",
                         "Wattage: ",sl_read$Wattage,"<br>",
                         "Lumens: ",sl_read$Lumens,sep ="")
  output$pole <-  renderLeaflet({
    
    pal5 <- colorFactor(c("coral3", "cadetblue","goldenrod","grey33","sienna2","burlywood4"), domain = c("Wood", "Metal", "Fiberglass", "Concrete", "Aluminum", "Unknown"))
    
    
    leaflet(lights.subset2())  %>%
      addTiles()  %>%
      addCircleMarkers(radius = 1.5,
                       stroke = FALSE, fillOpacity = 1, color = ~pal5(lights.subset2()$Pole_Type), popup = ~ popup)  %>%
      addControl("<b>Streetlights in South Bend</b><br>Click on Dots for Details", 
                 position = "topright") %>%
      addLegend("bottomright", pal = pal5, values = ~lights.subset2()$Pole_Type, title = "Type of Streetlight Poles",opacity = 1)
  })  
  output$own <-  renderLeaflet({
    pal4 <- colorFactor(c("coral3", "cadetblue", "burlywood4","grey33"), domain = c("AEP", "City of South Bend", "Other", "Unknown"))
    
    leaflet(lights.subset1())  %>%
      addTiles()  %>%
      addCircleMarkers(    radius = 1.5,
                           stroke = FALSE, fillOpacity = 1, color = ~pal4(lights.subset1()$Ownership), popup = ~ popup)  %>%
      addControl("<b>Streetlights in South Bend</b><br>Click on Dots for Details", 
                 position = "topright") %>%
      addLegend("bottomright", pal = pal4, values = ~lights.subset1()$Ownership, title = "Ownership of Streetlights",opacity = 1)
    
    
  }) #end of street lights
  
  
}# end of the server


# Run the application 
shinyApp(ui = ui, server = server)
