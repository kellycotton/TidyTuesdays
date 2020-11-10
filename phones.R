# Setup----
library(tidyverse)
library(ggthemes)

mobile <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-10/mobile.csv')

mean_mobile <- mobile %>% 
  filter(year == 2017) %>% 
  group_by(continent) %>% 
  summarise(mobile_subs= mean(mobile_subs, na.rm = TRUE)) %>% 
  mutate(entity = continent, year = 2017, continent = continent)

mobile %>% 
  select(c(continent, mobile_subs, entity, year)) %>% 
  filter(year == 2017) %>% 
  rbind(mean_mobile) %>% 
  drop_na(mobile_subs) %>% 
  mutate(entity = gsub(" and ", " & ", entity)) %>% 
  mutate(type = ifelse(entity %in% mean_mobile$continent, 2, 1)) %>% 
  mutate(entity = case_when(
    entity == "Saint Vincent & the Grenadines" ~ "St. Vincent &\nthe Grenadines",
    entity == "Saint Kitts & Nevis" ~ "St. Kitts &\nNevis",
    entity == "Sao Tome & Principe" ~ "Sao Tome &\nPrincipe",
    entity == "Syrian Arab Republic" ~ "Syria",
    entity == "Bosnia & Herzegovina" ~ "Bosnia &\nHerzegovina",
    entity == "Democratic Republic of Congo" ~ "D.R.C.",
    entity == "Dominican Republic" ~ "Dominican\nRepublic",
    entity == "Trinidad & Tobago" ~ "Trinidad &\nTobago",
    entity == "United Arab Emirates" ~ "U.A.E.",
    entity == "Solomon Islands" ~ "Solomon\nIslands",
    TRUE ~ as.character(entity)
  )) %>% 
  ggplot(aes(x = reorder(entity, mobile_subs), y = mobile_subs, color = factor(type), alpha = type)) +
  geom_segment(aes(xend = entity, y = 0, yend = mobile_subs), size = 1.5) +
  geom_point(size = 3.5) +
  geom_hline(yintercept = 100, linetype = "dotted") +
  scale_y_continuous(labels = scales::comma) +
  scale_alpha(range = c(.85, 1)) +
  coord_flip() +
  facet_wrap(~continent, scales = "free", nrow = 1) +
  theme_economist() + scale_color_economist() +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text.y = element_text(hjust = 1, vjust = .5, lineheight = .7)
    ) +
  labs(title = "Global Mobile Phone Adoption",
       subtitle = "Number of mobile phone subscriptions per 100 people in 2017",
       caption = "Created by @kllycttn, Data from OurWorldInData.org, #TidyTuesday"
       )

#ggsave("Plots/phoneplot.png", width = 15, height = 10)
