# Setup----
library(tidyverse)
library(viridis)
library(dplyr)
library(ggrepel)
library(usmap)
library(scales)

#Get the data
beer_states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_states.csv')
tuition_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/tuition_cost.csv')
milk_states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-29/state_milk_production.csv')
states <- readr::read_csv('https://raw.githubusercontent.com/jasonong/List-of-US-States/master/states.csv')
ufo_sightings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-25/ufo_sightings.csv")

options(scipen=999) #Remove scientific notation

#Reformat the data
colnames(states) <- c("statename","state")
colnames(milk_states) <- c("region","statename","year","milk_produced")
milk_states <- merge(states,milk_states,by="statename")
ufo_sightings$state <- toupper(ufo_sightings$state)

#Find the state "regions" from the milk data
regions <- milk_states %>%
  group_by(state,region) %>%
  summarize()

#Add regions to the beer datasets
beer_states <- merge(beer_states,regions,by="state")

#Manually reorder regions semi-geographically
beer_states$region <- factor(beer_states$region, 
                              levels = c("Northeast","Appalachian","Southeast","Corn Belt",
                                         "Delta States","Lake States","Northern Plains",
                                         "Southern Plains","Mountain","Pacific"))

#Beer Production by State----
#Find mean beer production by state
beer <- beer_states %>%
  group_by(state,region) %>%
  summarize(mean_barrels = mean(barrels,na.rm = TRUE))

#Plot average annual beer production for each state
png("beermap.png",units="in",width=9,height=4,res=300)
print (
  plot_usmap(data = beer, values = "mean_barrels") + 
    scale_fill_viridis(name="Barrels Produced",label = scales::comma) + 
    theme(legend.position = "right") + ggtitle("Annual Barrels of Beer Produced")
)
dev.off()

#Beer and Tuition----
#Find the mean tuition by state
tuition <- tuition_cost %>%
  group_by(state_code) %>%
  summarize(mean_tuition = mean(in_state_tuition)) %>%
  rename(state=state_code)
  
#Combine beer and tuition dataframes
beer_tuition <- merge(beer,tuition,by="state")

#Plot average tuition vs. average amount of beer produced by state and region
png("beertution1.png",units="in",width=9,height=5,res=300)
print (
ggplot(beer_tuition) + aes(x=mean_barrels,y=mean_tuition,color=region) +
  geom_jitter(size=4) + 
  scale_x_log10(label = scales::comma) +
  labs(y="Annual In-State Tuition ($)",x="Annual Barrels of Beer Produced",
       title="Average College Tuition vs. How Much Beer is Being Produced",
       color="Region") +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust=0.5),
        plot.margin = margin(0.5,1,.5,.5,"cm")) +
  scale_color_viridis(discrete = TRUE) +
  geom_text_repel(aes(label = state),
                  data = subset(beer_tuition, (mean_barrels <= 5000 | mean_barrels >= 5e+6 | mean_tuition <= 7000 | mean_tuition >= 25000)),
                  box.padding   = 0.4, 
                  point.padding = 0.7,
                  segment.color = 'grey50',
                  color="black") 
)
dev.off()
#Plot average tuition for each state, average amount of beer produced is the size of the dot
png("beertuition2.png",units="in",width=9,height=7,res=300)
print (
ggplot(beer_tuition) + aes(x=reorder(state,mean_tuition),y=mean_tuition,size=mean_barrels,color=region) +
  geom_point()  + coord_flip() + scale_color_viridis(discrete=TRUE) +
  scale_size_continuous(name="Annual Barrels Produced",label = scales::comma) +
  labs(y="Annual In-State Tuition ($)",x="State", color="Region",
       title="Average College Tuition vs. How Much Beer is Being Produced") +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust=0.5)) 
)
dev.off()

#Plot this data for kegs only 

#Find mean beer production (kegs)
kegs <- beer_states %>%
  group_by(state,region) %>%
  subset(type=="Kegs and Barrels") %>%
  summarize(mean_barrels = mean(barrels,na.rm = TRUE))

#Combine tuition and keg dataframes
kegs_tuition <- merge(kegs,tuition,by="state")

#Plot average tuition vs. average amount of beer kegs produced by state and region
png("kegtuition.png",units="in",width=9,height=4,res=300)
print(
ggplot(kegs_tuition) + aes(x=mean_barrels,y=mean_tuition,color=region) +
  geom_point(size=4)  + 
  scale_x_log10(label = scales::comma) +
  scale_color_viridis(discrete = TRUE) +
  labs(y="Mean Annual In-State Tuition ($)",x="Annual Beer Produced (Barrels)",
       title="Average College Tuition vs. How Much Beer is Being Produced",color="Region")+
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust=0.5),
        plot.margin = margin(0.5,1,.5,.5,"cm")) +
  geom_text_repel(aes(label = state),
                  data = subset(kegs_tuition, (mean_barrels <= 5000 | mean_barrels >= 1.5e+6 | mean_tuition <= 7000 | mean_tuition >= 25000)),
                  box.padding   = 0.4, 
                  point.padding = 0.7,
                  segment.color = 'grey50',
                  color="black") 
)
dev.off()
#Compare the beer data to dairy production

#Find mean milk production
milk <- milk_states %>%
  group_by(state) %>%
  summarize(mean_milk = mean(milk_produced,na.rm = TRUE))

milk_beer <- merge(milk,beer,by="state")

png("milkbeer.png",units="in",width=9,height=4,res=300)
print (
ggplot(milk_beer) + aes(x=mean_barrels,y=mean_milk,color=region) +
  geom_point(size=4) +
  scale_x_log10(label = scales::comma) +
  scale_y_log10(label = scales::comma) +
  scale_color_viridis(discrete=TRUE) +
  labs(y="Annual Milk Produced (Pounds)",x="Annual Beer Produced (Barrels)",
       title="Milk and.Beer Production",color="Region") +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust=0.5),
        plot.margin = margin(0.5,1,.5,.5,"cm")) +
  geom_text_repel(aes(label = state),
                  data = subset(milk_beer, (mean_barrels <= 5e3 | mean_barrels >= 5e6 | mean_milk <= 1e8 | mean_milk >= 1e10)),
                  box.padding   = 0.4, 
                  point.padding = 0.7,
                  segment.color = 'grey50',
                  color="black") 
)
dev.off()

# And UFOs...
ufo_states <- ufo_sightings %>%
  subset(country=="us") %>%
  count(state) %>%
  rename(sightings=n)

ufo_beer <- merge(ufo_states,beer,by="state")

png("ufobeer.png",units="in",width=9,height=4,res=300)
print (
ggplot(ufo_beer) + aes(x=mean_barrels,y=sightings,color=region) +
  geom_point(size=3) + 
  scale_x_log10(label = scales::comma) +
  scale_color_viridis(discrete=TRUE) +
  labs(y="Number of UFO Sightings",x="Annual Barrels of Beer Produced",
       title="Alien Activity and Beer Production",color="Region") +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust=0.5),
        plot.margin = margin(0.5,1,.5,.5,"cm")) +
  geom_text_repel(aes(label = state),
                  data = subset(ufo_beer, (sightings >=2500)),
                  box.padding   = 0.5, 
                  point.padding = 0.5,
                  segment.color = 'grey50',
                  color="black") 
)
  dev.off()
