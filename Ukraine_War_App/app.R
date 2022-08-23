library(shiny)
library(tidyverse)
library(leaflet)
library(shinythemes)
library(acled.api)
library(markdown)
library(htmlwidgets)
library(leaflegend)

df <- read_csv("war.csv")

# copy the data so multiple calls don't have to be made
war <- df |>
  mutate(across(c(latitude, longitude), as.numeric))

# create a new data frame filtered by sub_event_type
df_sub <- war %>% 
  filter(sub_event_type %in%
           c("Shelling/artillery/missile attack"
             , "Armed clash")
         & actor1 %>% 
           str_detect(pattern = 
                        "Russia|Ukraine")) %>%
  mutate(color = case_when(
    sub_event_type == "Shelling/artillery/missile attack" ~ alpha('red', 0.15)
    , sub_event_type == "Armed clash" ~ alpha('blue', 0.15)
  )
  , size = sqrt(fatalities/pi)*4)

# df w/ Russian Forces
ru <- df_sub %>% filter(actor1 %>% str_detect("Russia"))


# df w/ Ukrainian Forces
zsu <- df_sub %>% filter(actor1 %>% str_detect("Ukraine"))

# colors for legend
colors <- df_sub$color %>% unique() %>% sort()

#-------------------------------------------------------------------------------
# UI CODE
#-------------------------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- navbarPage("Ukraine’s War Against Russian Occupation"
                 
                 ,theme = shinytheme("paper")
                 
                 , tabPanel("Interactive Map"
                 
                 , leafletOutput("ukr_war_map"
                                 , width = "100%"
                                 , height = "100vh")
                 
                 , absolutePanel(id = "controls"
                                 , class = "panel panel-default"
                                 , fixed = TRUE
                                 , draggable = TRUE
                                 , top = 100
                                 , left = 100
                                 , right = "auto"
                                 , bottom = "auto"
                                 , width = 300
                                 , height = "auto"
                                 , dateRangeInput("date"
                                               , label = "Select Date Range"
                                              , start = min(df_sub$event_date)
                                              , end = max(df_sub$event_date)
                                              , width = 350)
                 ))
                 
                 , tabPanel(title = "Event Analysis"
                            
                            , sidebarPanel(width = 3
                                           , dateRangeInput("line_date"
                                                            , label = "Select Date Range"
                                                            , start = min(war$event_date)
                                                            , end = max(war$event_date))
                                           , includeMarkdown("intro.md"))
                            
                            , mainPanel(plotOutput("line_graph", height = 600)))
                 )

#-------------------------------------------------------------------------------
# SERVER CODE
#-------------------------------------------------------------------------------
server <- function(input, output) {
  
  output$ukr_war_map <- renderLeaflet({
    
    # first layer
    leaflet() %>%
      addProviderTiles(provider = "CartoDB.Positron") %>% 
      addCircleMarkers(data = ru |> filter(event_date >= input$date[1]
                                           & event_date <= input$date[2])
                       , lng = ~longitude
                       , lat = ~latitude
                       , radius = ~size
                       , color = ~color
                       , label = ~str_c("Initiator: "
                                        , actor1
                                        ," (","Date:"
                                        , event_date
                                        ,") "
                                        , "# of Fatalities: "
                                        , ru$fatalities)
                       , group = c("Russian Forces")) %>%
      
      addProviderTiles(provider = "CartoDB.Positron") %>% 
      addCircleMarkers(data = zsu |> filter(event_date >= input$date[1]
                                            & event_date <= input$date[2])
                       , lng = ~longitude
                       , lat = ~latitude
                       , radius = ~size
                       , color = ~color
                       , label = ~str_c("Initiator: "
                                        , actor1
                                        ," (","Date:"
                                        , event_date
                                        ,") "
                                        , "# of Fatalities: "
                                        , zsu$fatalities)
                       , group = c("Armed Forces of Ukraine")) %>%
      
      addLayersControl(overlayGroups = c("Armed Forces of Ukraine"
                                         ,"Russian Forces")) %>% 
      
      addLegend(pal = colorFactor(colors
                                  , domain = c("Armed clash"
                                               , "Shelling/artillery/missile attack"))
                , values = c("Armed clash"
                             , "Shelling/artillery/missile attack")) %>%
      
      setView(lng = 32, lat = 48, zoom = 6)
  })
  
  output$line_graph <- renderPlot({
    war |> 
      filter(!event_type %in% c("Protests", "Riots")) |>
      filter(event_date >= input$line_date[1] 
             & event_date <= input$line_date[2]) |>
      group_by(event_date, event_type) |> 
      tally() |>
      arrange(desc(n)) |> 
      ggplot(aes(event_date, n, color = event_type)) +
      geom_line(alpha = 0.4, lwd = 1.25) + 
      stat_smooth(method = "loess", se = F) +
      theme(axis.title = element_blank()
            , legend.position = "none"
            , panel.background = element_rect(fill = "white")
            , panel.grid = element_line(colour = alpha("grey", 0.25))
            , plot.title = element_text(size = 20)
            , strip.text = element_text(size = 14)) +
      scale_color_manual(values = c("#0057B7"
                                    , "#EF3340"
                                    , "#552583"
                                    , "#00cc00")) +
      ggtitle("Number of Daily Recorded Events") + 
      scale_x_date(breaks = '1 month'
                   , date_labels = "%b-%y") +
      facet_wrap(~event_type)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
