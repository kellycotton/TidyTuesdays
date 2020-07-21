# Setup----

library(tidyverse)
library(tidytuesdayR)
library(showtext)
library(colorblindr)

font_add_google("Montserrat","Montserrat")
showtext_auto()

# Get data
tuesdata <- tidytuesdayR::tt_load('2020-07-14')
tuesdata <- tidytuesdayR::tt_load(2020, week = 29)

astronauts <- tuesdata$astronauts 

# Set arrow coordinates
arrows <- 
  tibble(
    x1 = c(1.9, 1.7, 1.7),
    x2 = c(2.05, .95, 1.1),
    y1 = c(72.5, 28.5, 28.5), 
    y2 = c(77, 25, 26)
  )

# Plots----
# Age of first and last mission by gender
#png("ageplot.png",units="in",width=6,height=6,res=300)
astronauts %>%
  group_by(number) %>% # number = astronaut number
  slice(c(1,n())) %>% # select the first and last entries for each astronaut
  ungroup() %>%
  distinct(id,.keep_all = TRUE) %>% # not everyone has multiple missions and the previous step adds duplicates, so keep only the unique IDs 
  mutate(age= year_of_mission - year_of_birth) %>% # create a new age variable 
  mutate(mission = ifelse(mission_number>1,"Last","First")) %>% # rename first and last missions
  ggplot(aes(x=mission,y=age,group=sex,col=sex)) + 
  geom_point(aes(color=sex),position=position_dodge(0.3),alpha=.2, size = 2.5) +
  stat_summary(fun = mean, geom = "point", size = 5,position=position_dodge(0.3)) +
  scale_color_OkabeIto(labels=c("Female Astronauts", "Male Astronauts")) +
  scale_x_discrete(labels=c("First Mission", "Last Mission")) +
  ggtitle("Astronaut Age at First and Last Mission") +
  theme_light(base_family = "Montserrat") +
  theme(
    axis.title.x = element_blank(), 
    legend.title = element_blank(),
    legend.position = c(.15,.85),
    axis.title.y = element_blank(),
    ) +
  annotate(
    "text", x = 2, y = 72, 
    size = 2.5,
    label = "Oldest: John Glenn (77)"
  ) +
  annotate(
    "text", x = 1.7, y = 29, 
    size = 2.5,
    label = "Youngest: Gherman Titov and Valentina Tereshkova (26)"
  ) +
  geom_curve(
    data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.07, "inch")), size = 0.4,
    color = "gray20", curvature = -0.3, 
    inherit.aes = FALSE
  ) 
#dev.off()