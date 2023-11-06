library(tidyverse)
library(ggpubr)
library(lubridate)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(plotly)

Dataset<-read.csv("scuba_diving_logbook - cleaned_data.csv")
glimpse(Dataset)

#cleaning
Dataset <- Dataset |> mutate(depth.m. = as.numeric(depth.m.)) |>
  mutate(duration.min. = as.numeric(duration.min.)) |> 
  mutate(date = as.Date(date))
print(unique(Dataset$gps_position_lat))
print(unique(Dataset$gps_position_long))

D<-Dataset |> select(country, id) |> group_by(country) |> 
  tally(name = "no.dives") |>
  rename("region" = "country")

country.dives<-ggplot(D, mapping = aes(x = region, y = no.dives, fill = region))+
  geom_col()+
  ylab("Number of Dives")+
  xlab("Country")+
  ggtitle("Number of Dives in Each Country")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5), legend.position = "none")
print(country.dives)
#im guessing he's french, 

#How long underwater:
total.bottom.time<-sum(Dataset$duration.min.,na.rm = TRUE)
types.of.entries<- Dataset |> group_by(stand) |> tally(name = "entry.type")
entry.plot<-ggplot(types.of.entries, mapping = aes(x= "", y = entry.type, fill = stand))+
  geom_col(stat = "identity")+
  coord_polar("y", start = 0)
print(entry.plot)

#Geom_polygon map
mapdata<-map_data("world")
md2<-left_join(mapdata, D, by ="region")
#md2<- md2 |> filter(!is.na(no.dives))
map1<-ggplot(md2, mapping = aes(x = long, y = lat, group = group))+
  geom_polygon(aes(fill=no.dives), colour = "black")+
  geom_point(mapping = aes(x =gps_position_long, y = gps_position_lat))
  ggtitle("Map Showing where this person has dove")+
  theme(axis.text.x = element_blank(),axis.text.y = element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
        rect = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())+
  scale_fill_gradient(name = "number of dives", low = "cyan", high = "blue", na.value = "white")
map1<-ggplotly(map1)
print(map1)

#sf map
theme_set(theme_bw())
world<-ne_countries(scale = "medium", returnclass = "sf")
D<-Dataset |> select(country, id) |> group_by(country) |> 
  tally(name = "no.dives") |>
  rename("name" = "country")
map.data<-left_join(world, D, by="name")
fill.map<-ggplot(map.data, aes(fill = no.dives, text = name))+
  geom_sf()+
  ggtitle("Places this person has dived")+
  scale_fill_gradient(name = "number of dives", low = "cyan", high = "blue", na.value = "white")
fill.map<-ggplotly(fill.map, tooltip = "text")
print(fill.map)


#plo_ly map?
map3<-plot_geo(data = D, z = ~no.dives,locationmode = "country names", 
               locations = ~name, marker = list(line = list(color = "red", width = 0.6)))
print(map3)
map4<-plot_ly(data = D, type = "choropleth", locations = ~name, z = ~no.dives, locationmode = "country names")
print(map4)

#Linear Regression
print(ggdensity(Dataset$duration.min.))
test<-ggplot(Dataset, mapping = aes(x = depth.m., y = duration.min., colour = gas))+
  geom_point()
print(test)
depth.time<-lm(data = Dataset, formula = duration.min. ~ depth.m.)
print(summary(depth.time))