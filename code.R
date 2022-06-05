

colors.war <- case_when(str_detect(war$actor1, "Russia") == TRUE ~ '#DA291C'
                        , str_detect(war$actor1, "Ukraine") == TRUE ~ '#0057B8'
                        , str_detect(war$actor1, "Novorossiya") == TRUE ~ "purple"
                        , TRUE ~ "orange")

# create a blank map
(kyiv_map <- get_stamenmap(bb2bbox(attr(kyiv, "bb"))
                           , maptype = "toner-background"
                           , zoom = 6) %>% ggmap() +
    theme(axis.title = element_blank()
          , axis.text = element_blank()
          , axis.ticks = element_blank()))

# plot the points!
(point_map <- kyiv_map + 
    geom_point(aes(as.numeric(longitude), as.numeric(latitude))
               , data = war
               , size = rescale(sqrt(war$fatalities/pi), to = c(1,10))
               , color = alpha(colors.war, 0.5)))

# =============================
# ROads and Rails
# =============================
# import the roads shapefile
roads <- readOGR("ukraine_war/roads/roads.shp")
roads@proj4string # used Dep. Proj.4 rep in CRS below

# turn the roads data into a df
roads_df <- spTransform(roads,CRS("+proj=longlat +datum=WGS84 +no_defs")) %>% 
  broom::tidy()

# import the railways shapefile
rail <- readOGR("rails/railways.shp")
rail@proj4string # used Dep. Proj.4 rep in CRS below

# turn the rail data into a df
rail_df <- spTransform(rail, CRS("+proj=longlat +datum=WGS84 +no_defs")) %>%
  broom::tidy()

# plot the map
kyiv_map + 
  # add the roads onto the map
  geom_path(data = roads_df, mapping = aes(long, lat, group = group)
            , color = "grey") +
  # add the rail lines on the map
  geom_path(data = rail_df, mapping = aes(long, lat, group = group)) +
  # plot the points onto the map
  geom_point(data = war
             , mapping = aes(as.numeric(longitude)
                             , as.numeric(latitude))
             , size = rescale(sqrt(war$fatalities/pi), c(1,8))
             , color = alpha(colors.war, 0.5)) + 
  # remove the axis labels
  theme(axis.title = element_blank()
        , axis.ticks = element_blank()
        , axis.text = element_blank())



war <- war %>% dplyr::mutate(event_date = as.Date(event_date)
                             , longitude = as.numeric(longitude)
                             , latitude = as.numeric(latitude)
                             , "color" = case_when(str_detect(war$actor1
                                                              , "Russia") == TRUE ~ '#DA291C'
                                                   , str_detect(war$actor1
                                                                , "Ukraine") == TRUE ~ '#0057B8'
                                                   , TRUE ~ "orange")
                             , "size" = sqrt(fatalities/pi))



