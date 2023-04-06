library(tidyverse)
library(RColorBrewer)
library(patchwork)
install.packages("tidyverse")
install.packages("RColorBrewer")
install.packages("patchwork")
require(RColorBrewer)

beetles <- read.csv("dung_beetles.csv") %>%
  rename_with(tolower, everything()) %>%
  select(!starts_with("X")) %>%
  rename_with( ~ gsub("opis", "opris", .x), matches("Copis")) %>% 
  pivot_longer(cols = contains("_"), names_to = "spp", values_to = "values") %>%
  separate_wider_delim("spp", "_",
                       names = c("genus",
                                 "species"))
View(beetles)

#group by/summarise,
# we can start to look for some broad patterns in the data. What about seasonality?
#mutate can go along rows but also columns
#Group by the months and count all of them up as follows:

total_pcm = beetles %>%
  group_by(month) %>%
  summarize(total = sum(values))

total_pcm$month <- factor(total_pcm$month, levels = month.name)

#plot1
plot1 <- total_pcm %>% ggplot(aes(x = month, y = total)) +
  geom_col()

#heatmaps:
#group by months too
#group the beetles table by genus name as well as month. Plot all of these together using geom_raster
#note, you’ll need to use themes to fit those month names in properly

total2 = beetles %>%
  group_by(month, genus) %>%
  summarize(total = mean(values))
total2$month <- factor(total2$month, levels = month.name)

plot2 <- total2 %>% ggplot(aes(x = month, y = genus, fill = total)) +
  geom_raster()

#setting scale limits
#let’s just cut that scale off at ten. You’ll need to set up a scale for ‘fill’
#with a top limit of 10. This will remove all values over ten.
total2$total <- as.numeric(total2$total)

plot3 = total2 %>% ggplot(aes(x = month, y = genus, fill = total, na.)) +
  geom_raster() +
  scale_fill_gradient(limits = c(0,10), na.value = "#56B1F7")

#axes

guinea_world <- read.csv("number-of-reported-guinea-worm-dracunculiasis-cases.csv") %>% 
  rename(cases = Reported.cases.of.Guinea.worm.disease,
         country = Entity) %>%
  rename_with(tolower) %>% 
  filter(country == "World")

guinea <- guinea_world %>%
  ggplot(aes(x = year, y = cases)) +
  geom_point() +
  geom_line() 
guinea + plot_annotation(title = "Guinea worm worldwide cases 1986-2021")

#use log scale
guinea2 <- guinea_world %>%
  ggplot(aes(x = year, y = cases)) +
  geom_point() +
  geom_line() +
  scale_y_log10( limits = c(1,1e6), breaks = c(10,100, 1000, 10000, 100000, 1000000),
                 labels = c("10","100", "1,000","10,000", "100,000" ,"1,000,000"))

guinea3 <- guinea_world %>%
  ggplot(aes(x = year, y = cases)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(trans='log2', limits = c(1,1000000))

scale_y_continuous(trans='log2', limits = c(1,1000000), breaks = c(10,100, 1000, 1000000))

(breaks = seq(0, 100, by = 20))
#good plots , labels inserted into plot, colour used for variable
#use summary tables and group by, complimentaery data

guinea_countries <- read.csv("number-of-reported-guinea-worm-dracunculiasis-cases.csv") %>% 
  rename(cases = Reported.cases.of.Guinea.worm.disease,
         country = Entity) %>%
  rename_with(tolower) %>%
  mutate(country = gsub("Central African Republic",
                        "C.A.R.",
                        country)) %>% 
  filter(country != "World")

endemic_countries <- c("Angola","C.A.R.","Kenya","Yemen",                   
                       "Pakistan","Chad","Cameroon","Senegal","Ethiopia",
                       "Mauritania","Cote d'Ivoire","Mali","Togo","India",
                       "Benin","Niger","Burkina Faso","Uganda","Ghana",
                       "Sudan","South Sudan","Nigeria")

guinea_countries <- guinea_countries %>% filter(country %in% endemic_countries)

guinea_countries$country <- factor(guinea_countries$country,levels=endemic_countries)

#highlight a subset of your data

gp1 = guinea_countries %>% 
  ggplot(aes(x = year, y = cases, fill = country)) +
  geom_col() 

high_burden_countries <- c("Burkina Faso","Ghana","Niger","Nigeria",
                           "South Sudan","Sudan","Uganda")

ggplot(guinea_countries,aes(year,cases,group=country)) + 
  geom_col(fill="light grey") +
  geom_col(fill="dark red",
           data=guinea_countries %>% filter(country %in% high_burden_countries)) +
  ggtitle("Guinea worm worldwide cases 1986-2021 (high-burden countries)")  

#use only high burden countries
ggplot(guinea_countries%>% filter(country %in% high_burden_countries) ,aes(year,cases,group=country, fill = country)) + 
  geom_col() +
  ggtitle("Guinea worm worldwide cases 1986-2021 (high-burden countries)")

ggplot(guinea_countries,aes(year,cases,group=country,fill=country)) + 
  geom_col() + xlim(2012.5,NA)+
  ggtitle("Guinea worm worldwide cases 2012-2021")

# countries with any cases after 2011
persistent_countries <- c("Angola","Cameroon","Chad","Ethiopia",
                          "Mali","South Sudan","Sudan")
ggplot(guinea_countries%>% filter(country %in% high_burden_countries) ,aes(year,cases,group=country, fill = country)) + 
  geom_col() +
  ggtitle("Guinea worm worldwide cases 1986-2021 (high-burden countries)")

# use scales
gwcols <- rep("grey",length(endemic_countries))
names(gwcols) <- endemic_countries

#remove persistent countries that are also high-burden countries
persistent_countries <- persistent_countries[!persistent_countries %in% high_burden_countries]

#get a vector of 'blues' the same length as persistent_countries:
oranges <- brewer.pal(length(high_burden_countries),name="oranges")
blues <- brewer.pal(length(persistent_countries),name="Blues")

#shuffle blue colours so similar colours aren't next to each other
blues <- sample(blues)

#assign blue colours to gwcols
gwcols[persistent_countries] <- blues

#view our plot with the 
ggplot(guinea_countries,aes(year,cases,group=country,fill=country)) + 
  geom_col() + 
  scale_fill_manual(values=gwcols) +
  ggtitle("Guinea worm worldwide cases 1986-2021")

ggplot(guinea_countries,aes(year,cases,group=country,fill=country)) + 
  geom_col() + 
  scale_fill_manual(values=gwcols) +
  ggtitle("Guinea worm worldwide cases 1986-2021")

blues <- brewer.pal(length(persistent_countries),name="Blues")
??brewer.pal

## countries with any cases after 2011
persistent_countries <- c("Angola","Cameroon","Chad","Ethiopia",
                          "Mali","South Sudan","Sudan")
#remove persistent countries that are also high-burden countries
persistent_countries <- persistent_countries[!persistent_countries %in% high_burden_countries]

#get a vector of 'blues' the same length as persistent_countries:
blues <- brewer.pal(length(persistent_countries),name="Blues")

#shuffle blue colours so similar colours aren't next to each other
blues <- sample(blues)

#assign blue colours to gwcols
gwcols[persistent_countries] <- blues

#view our plot with the 
ggplot(guinea_countries,aes(year,cases,group=country,fill=country)) + 
  geom_col() + 
  scale_fill_manual(values=gwcols) +
  ggtitle("Guinea worm worldwide cases 1986-2021")

oranges <- brewer.pal(length(persistent_countries),name="Oranges")

#remove persistent countries that are also high-burden countries
persistent_countries <- persistent_countries[!persistent_countries %in% high_burden_countries]

#get a vector of 'blues' the same length as persistent_countries:
blues <- brewer.pal(length(persistent_countries),name="Blues")

#shuffle blue colours so similar colours aren't next to each other
blues <- sample(blues)

#assign blue colours to gwcols
gwcols[persistent_countries] <- oranges

#view our plot with the 
ggplot(guinea_countries,aes(year,cases,group=country,fill=country)) + 
  geom_col() + 
  scale_fill_manual(values=gwcols) +
  ggtitle("Guinea worm worldwide cases 1986-2021")


