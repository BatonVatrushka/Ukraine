library(shiny)
library(tidyverse)
library(leaflet)
library(shinythemes)
library(acled.api)
library(markdown)
library(htmlwidgets)
library(leaflegend)
library(randomForest)
library(caret)
library(shinyjs)

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
# MODEL
#-------------------------------------------------------------------------------
# load in the model
load("model_rf.rda")

# create a new field where the actors are flattened
war <- war |> 
  mutate("initiator" = case_when(
    actor1 |> str_detect("Ukraine") ~ "U"
    , actor1 |> str_detect("Russia|NAF|Wagner") ~ "R"
    , T ~ "O"
  ))

# create a df to select variables for the model
df_model <- war |> 
  # replace nas in admin1 w/ the location field
  mutate(admin1 = coalesce(admin1, location)) |>
  # select the fields for modeling
  select(event_type
         , sub_event_type
         , latitude
         , longitude
         , admin1
         , fatalities) |>
  # turn character fields to factors
  mutate_if(is.character, as.factor) 

#-------------------------------------------------------------------------------
# sub-event types
#-------------------------------------------------------------------------------
battle_subs <- df |> filter(event_type == "Battles") |> 
  select(sub_event_type) |> distinct() |> pull()

explosion_subs <- df |> filter(event_type == "Explosions/Remote violence") |> 
  select(sub_event_type) |> distinct() |> pull()

p_subs <- df |> filter(event_type == "Protests") |> 
  select(sub_event_type) |> distinct() |> pull()

strat_subs <- df |> filter(event_type == "Strategic developments") |> 
  select(sub_event_type) |> distinct() |> pull()

civ_subs <- df |> filter(event_type == "Violence against civilians") |> 
  select(sub_event_type) |> distinct() |> pull()

r_subs <- df |> filter(event_type == "Riots") |> select(sub_event_type) |> 
  distinct() |> pull()
#-------------------------------------------------------------------------------
# UI CODE
#-------------------------------------------------------------------------------
# Define UI for application that draws a histogram
ui <- navbarPage("Ukraine’s War Against Russian Occupation"
                 
                 ,theme = shinytheme("paper")
                 
                 , shinyjs::useShinyjs()
                 
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
                 
                 , tabPanel(title = "Initiator Prediction Model"
                            
                            , titlePanel("Predict the Initiator")
                            
                            , sidebarLayout(position = "left"
                              , sidebarPanel(checkboxGroupInput(inputId = "event_type"
                                                     , label = "Event Type"
                                                     , choices = unique(df_model$event_type)
                                                     , selected = character(0))
                              , conditionalPanel(condition = "input.event_type == 'Battles'"
                                                 , selectInput("battle_subs"
                                                               , "Sub-Event Type for Battles"
                                                               , choices = battle_subs))
                              , conditionalPanel(condition = "input.event_type == 'Explosions/Remote violence'"
                                                 , selectInput("explosion_subs"
                                                               , "Sub-Event Type for Explosions/Remote violence"
                                                               , choices = explosion_subs))
                              , conditionalPanel(condition = "input.event_type == 'Protests'"
                                                 , selectInput("p_subs"
                                                               , "Sub-Event Type for Protests"
                                                               , choices = p_subs))
                              , conditionalPanel(condition = "input.event_type == 'Strategic developments'"
                                                 , selectInput("strat_subs"
                                                               , "Sub-Event Type for Strategic Developments"
                                                               , choices = strat_subs))
                              , conditionalPanel(condition = "input.event_type == 'Violence against civilians'"
                                                 , selectInput("civ_subs"
                                                               , "Sub-Event Type for Violence Against Civilians"
                                                               , choices = civ_subs))
                              , conditionalPanel(condition = "input.event_type == 'Riots'"
                                                 , selectInput("riots"
                                                               , "Sub-Event Type for Riots"
                                                               , choices = r_subs))
                              , numericInput(inputId = "latitude"
                                             , label = "Latitude"
                                             , value = 50.45466
                                             , step = 0.1
                                             , min = min(df_model$latitude)
                                             , max = max(df_model$latitude))
                              , numericInput(inputId = "longitude"
                                             , label = "Longitude"
                                             , value = 30.5238
                                             , step = 0.1
                                             , min = min(df$longitude)
                                             , max = max(df$longitude))
                              , selectInput(inputId = "admin1"
                                                   , label = "Oblast"
                                                   , choices = unique(df_model$admin1)
                                                   , selected = character(0))
                              , numericInput(inputId = "fatalities"
                                             , label = "Fatalities"
                                             , value = 0
                                             , step = 1
                                             , min = min(df_model$fatalities)
                                             , max = max(df_model$fatalities))
                              , actionButton("predButton"
                                             , "Predict"
                                             , class = "btn-success")
                              , actionButton("reset"
                                             , "Clear Inputs")
                            )
                            , mainPanel(shinyjs::hidden(textOutput("pred"))
                                        , tags$head(
                                        tags$style(
                                        "#pred{font-size: 28px;
                                        font-style: italic;}")))
                            )))

#-------------------------------------------------------------------------------
# SERVER CODE
#-------------------------------------------------------------------------------
server <- function(input, output, session) {
  
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
  
  data <- reactive({
    req(input$event_type)
    
    data.frame(event_type = as.factor(input$event_type)
              , sub_event_type = if (input$event_type == "Battles") {input$battle_subs}
              else if (input$event_type == "Explosions/Remote violence") {input$explosion_subs}
              else if (input$event_type == "Protests") {input$p_subs}
              else if (input$event_type == "Strategic developments") {input$strat_subs}
              else if (input$event_type == "Violence against civilians") {input$civ_subs}
              else {r_subs}
              , latitude = input$latitude
              , longitude = input$longitude
              , admin1 = as.factor(input$admin1)
              , fatalities = input$fatalities)
  })
  
  prediction <- reactive({
    input$predButton
    isolate(predict.train(model_rf, data()))
  })
  
  output$pred <- renderText({
    
    if_else(prediction() == "U", "Prediction: Ukraine initiated the action"
            , "Prediction: Russia initiated the action")
  })
  
  observeEvent(input$reset
               , {shinyjs::reset("event_type")
                 shinyjs::reset("admin1")
                 shinyjs::hide("pred")})
  
  observeEvent(input$predButton
               , {shinyjs::show("pred")})
  
}

# Run the application 
shinyApp(ui = ui, server = server)
