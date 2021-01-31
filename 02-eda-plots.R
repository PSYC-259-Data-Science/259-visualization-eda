#Load packages
library(tidyverse)
library(vroom)
library(here)
library(janitor)
library(DataExplorer)
library(visdat)

#Load the 4 data files and fix the y scaling
files <- list.files(here("data_raw"), pattern = ".txt")
ds <- vroom(here("data_raw",files), id = "file", delim = " ", skip = 6) 
ds <- ds %>% rename(scene_time = `sceneQTtime(d:h:m:s.tv/ts)`, 
                    por_time = `porQTtime(d:h:m:s.tv/ts)`)
ds <- ds %>% rename_with(make_clean_names)
ds <- ds %>% mutate(file = str_extract(file, "\\d\\d\\d_(search|walk)")) %>% 
  separate(file, into = c("id", "cond")) %>% 
  mutate(cond = as.factor(cond))
  
#Let's first use the DataExplorer package to give us a snapshot of the entire dataset
plot_intro(ds)
plot_bar(ds)
plot_histogram(ds)

#Vis_dat and vis_miss also give us useful snapshots
vis_dat(ds)
vis_miss(ds)

#Vis_expect is a little more customizable
#Visualize both expected values and missing at the same time
vis_expect(select(ds, por_x), ~.x > 0 & .x < 640)
vis_expect(select(ds, avg_fps), ~.x %in% c(29.96, 29.97))

#Let's plot our own histogram

#Simplest histogram
ggplot(data = ds, mapping = aes(x = por_x)) + geom_histogram()
ds %>% ggplot(mapping = aes(x = por_x)) + geom_histogram() #Or pipe data in

#Piping data in let's us filter first
ds %>% filter(por_x < 640 & por_x > 0) %>% 
  ggplot(mapping = aes(x = por_x)) + geom_histogram()

#Or we can adjust the graph axes to ignore the out of range data
ds %>% ggplot(mapping = aes(x = por_x)) + geom_histogram() + xlim(0, 640)

#Altering the bin width might be helpful rather than relying on the default
ds %>% filter(por_x < 640 & por_x > 0) %>% 
  ggplot(mapping = aes(x = por_x)) + geom_histogram(binwidth = 10)

#A boxplot might be a better first glance (note change from x to y for mapping)
ds %>% ggplot(mapping = aes(y = por_x)) + geom_boxplot()
ds %>% filter(por_x < 640 & por_x > 0) %>% ggplot(mapping = aes(y = por_x)) + geom_boxplot()

#At this point, I'm convinced -- let's set the impossible por_x and por_y values to NA 
#so that we can stop filtering
ds <- ds %>% mutate(
  por_x = ifelse(por_x >= 0 & por_x < 640, por_x, NA),
  por_y = ifelse(por_y >= 0 & por_y < 480, por_y, NA)
)

#Might be better to check by participant
#Change the mapping to include x for id to divide data
ds %>% ggplot(mapping = aes(x = id, y = por_x)) + geom_boxplot()
#Now we can better see the outliers and distributions in the remaining data

#Or use freq_poly to overlay multiple histogram lines
ds %>%  ggplot(mapping = aes(color = id, x = por_x)) + geom_freqpoly()

#Normalize counts with y = stat(density)
ds %>%  ggplot(mapping = aes(color = id, x = por_x, y = stat(density))) + geom_freqpoly()

#Scatter plot
ds %>% ggplot(mapping = aes(x = por_x, y = por_y)) + geom_point()

#for overlapping data, try bin2d
ds %>%  ggplot(mapping = aes(x = por_x, y = por_y)) + geom_bin2d()

#facets are incredibly useful for eda
#when your plot commands start to get long, make sure the + ends each line 
ds %>% filter(por_x < 640 & por_x > 0) %>% 
  ggplot(mapping = aes(x = por_x, y = por_y)) + 
  geom_bin2d() + 
  facet_wrap(~ id) +
  xlim(0, 640) + 
  ylim(0,480)

#But if you have more than 5-6 individual graphs, 
#it might be better to save them to a file to inspect one at a time
ids <- unique(ds$id)
for (i in ids) {
  ds %>% filter(por_x < 640 & por_x > 0, id == i) %>% 
    ggplot(mapping = aes(x = por_x, y = por_y)) + 
    geom_bin2d() + 
    xlim(0, 640) + 
    ylim(0,480)
  ggsave(here("eda","individual_xy_plots",paste0(i,".png")))
}

ds %>% filter(por_x < 640 & por_x > 0) %>% 
  ggplot(mapping = aes(x = group, y = por_y)) + stat_summary(fun = "mean_se")

ggplot(ds, aes(id, por_x)) + 
  stat_summary(fun.data = mean_se, geom = "pointrange")

ggplot(ds, aes(id, por_y)) + 
  stat_summary(fun.data = mean_se, geom = "bar") +
  stat_summary(fun.data = mean_se, geom = "errorbar")

ggplot(ds, aes(x= id, y = stat(prop), group = 1)) + geom_bar()
ggplot(ds, aes(x= id, y = stat(count), group = 1)) + geom_bar()


