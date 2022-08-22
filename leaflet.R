library(pacman)
p_load(tidyverse, maps, scales, ggmap)
p_load(acled.api, gt, usethis, rgdal, sp)
p_load(leaflet)

# load in the data
df <- acled.api(
  email.address = Sys.getenv("ACLED_EMAIL")
  , access.key = Sys.getenv("ACLED_API_KEY")
  , country = "Ukraine"
  , start.date = "2022-02-24"
  , end.date = Sys.Date()
  , all.variables = T
)

# copy the data so multiple calls don't have to be made
war <- df |>
  mutate(across(c(latitude, longitude), as.numeric))

# glimpse
war %>% glimpse

# I'm interested in the event_type
war %>% group_by(event_type) %>% tally() %>% arrange(desc(n)) %>%
  mutate(prop = n/sum(n))

# dig deeper
(sub_event <- war %>% group_by(sub_event_type) %>% tally() %>% arrange(desc(n)) %>%
    mutate(prop = n/sum(n)))

# let's look at just the Russians
df_ru <- war %>% 
  filter(actor1 %>% str_detect(pattern = "Military Forces of Russia"))

# make sure the filtering worked
df_ru$actor1 %>% unique() %>% sort()

# look at sub_event_type just for Russian Forces
(ru_sub_event <- df_ru %>% 
    group_by(sub_event_type) %>% 
    tally() %>%
    arrange(desc(n)) %>% 
    mutate(prop = n/sum(n)))

# =======================
# LEAFLET
# =======================
names(providers) 
# use CartoDB
leaflet() %>% 
  addProviderTiles(provider = "CartoDB")

# use Esri
leaflet() %>% 
  addProviderTiles(provider = "Esri")
# change lat/long to numeric
df_ru <- df_ru %>%
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

# plot those circles boy
leaflet() %>%
  addProviderTiles(provider = "CartoDB") %>%
  addCircleMarkers(data = df_sub 
                   , lng = ~longitude
                   , lat = ~latitude
                   , radius = ~size
                   , color = ~color)

# -----------
# multi-layer
# -----------
# df w/ Russian Forces
ru <- df_sub %>% filter(actor1 %>% str_detect("Russia"))
# first layer
m1 <- leaflet() %>%
  addProviderTiles(provider = "CartoDB") %>% 
  addCircleMarkers(data = ru
                   , lng = ~longitude
                   , lat = ~latitude
                   , radius = ~size
                   , color = ~color
                   , label = ~str_c("Initiator: "
                                    , actor1
                                    ," (","Date:"
                                    , event_date
                                    ,")")
                   , group = c("Russian Forces"))

# df w/ Ukrainian Forces
zsu <- df_sub %>% filter(actor1 %>% str_detect("Ukraine"))

# colors for legend
colors <- df_sub$color %>% unique() %>% sort()

# second layer
m1 <- m1 %>%
  addProviderTiles(provider = "CartoDB") %>% 
  addCircleMarkers(data = zsu
                   , lng = ~longitude
                   , lat = ~latitude
                   , radius = ~size
                   , color = ~color
                   , label = ~str_c("Initiator: "
                                    , actor1
                                    ," (","Date:"
                                    , event_date
                                    ,")")
                   , group = c("Armed Forces of Ukraine")) %>%
  addLayersControl(overlayGroups = c("Armed Forces of Ukraine"
                                     ,"Russian Forces")) %>% 
  addLegend(pal = colorFactor(colors, domain = c("Armed clash"
                                                 , "Shelling/artillery/missile attack"))
            , values = c("Armed clash"
                         , "Shelling/artillery/missile attack"))




