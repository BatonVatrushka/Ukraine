library(pacman)
p_load(tidyverse, lubridate, directlabels)

# read in the data
war <- read_csv('Ukraine_War_App/war.csv')

# get a count of event type
war |>
  group_by(event_type) |>
  tally() |>
  mutate(prop = n/sum(n)) |>
  arrange(desc(n))

# group by date and event_type then create a line graph
war |> 
  filter(!event_type %in% c("Protests", "Riots")) |>
  group_by(event_date, event_type) |> 
  tally() |>
  arrange(desc(n)) |> 
  ggplot(aes(event_date, n, color = event_type)) +
  geom_line(alpha = 0.4) + 
  stat_smooth(method = "loess", se = F, lwd = .65) +
  #geom_dl(aes(label=event_type), method = list("last.bumpup")) +
  theme(axis.title = element_blank()
        , legend.position = "none"
        , panel.background = element_rect(fill = "white")
        , panel.grid = element_line(colour = alpha("grey", 0.25))) +
  #facet_wrap(~event_type) +
  scale_color_manual(values = c("#0057B7"
                                , "#EF3340"
                                , "#552583"
                                , "#00cc00")) +
  ggtitle("Number of Recorded Events") + 
  scale_x_date(breaks = '1 month'
    , date_labels = "%b-%y") +
  facet_wrap(~event_type)

# look at the number of fatalities by month
war |> 
  mutate("month" = month(event_date, label = T)) |>
  group_by(month) |>
  summarise(fatalities = sum(fatalities)) |> 
  ggplot(aes(month, fatalities)) +
  geom_col() +
  theme(axis.title = element_blank()
        , axis.ticks = element_blank()) +
  ggtitle("Recorded Fatalities")
