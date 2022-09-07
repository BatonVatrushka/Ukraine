library(pacman)
p_load(acled.api, cronR, writexl, tidyverse, miniUI, shiny, shinyFiles)
# load in the data for Ukraine
df_ukr <- acled.api(
  email.address = Sys.getenv("ACLED_EMAIL")
  , access.key = Sys.getenv("ACLED_API_KEY")
  , country = "Ukraine"
  , start.date = "2022-02-24"
  , end.date = Sys.Date()
  , all.variables = T
)
# For Russia
df_ru <- acled.api(
  email.address = Sys.getenv("ACLED_EMAIL")
  , access.key = Sys.getenv("ACLED_API_KEY")
  , country = "Russia"
  , start.date = "2022-02-24"
  , end.date = Sys.Date()
  , all.variables = T
)
# bind the data frames making sure to only pull in the records
# from df_ru where ZSU is actor1
war <- df_ukr |> 
  rbind(df_ru |> filter(actor1 |> str_detect("Ukraine")))
# write to the directory
war |> write_csv('war.csv')
# write to the shiny directory
war |> write_csv('./Ukraine_War_App/war.csv')



