#Load packages
library(tidyverse)
library(vroom)
library(here)
library(janitor)
library(DataExplorer)

#Load the 4 data files and fix the y scaling
files <- list.files(here("data_raw"), pattern = ".txt")
ds <- vroom(here("data_raw",files), id = "file", delim = " ", skip = 6) 
ds <- ds %>% rename(scene_time = `sceneQTtime(d:h:m:s.tv/ts)`, 
                    por_time = `porQTtime(d:h:m:s.tv/ts)`)
ds <- ds %>% rename_with(make_clean_names)
ds$id <- factor(ds$file, labels = c("1","2","3","4"))

#Simplest histogram
ggplot(data = ds, mapping = aes(x = por_x)) + geom_histogram()

#Or pipe data in
ds %>% ggplot(mapping = aes(x = por_x)) + geom_histogram()

#Or pipe data in, so that you can filter first
ds %>% filter(por_x < 640 & por_x > 0) %>% ggplot(mapping = aes(x = por_x)) + geom_histogram()

#Altering the bin width might be helpful rather than relying on the default
ds %>% filter(por_x < 640 & por_x > 0) %>% ggplot(mapping = aes(x = por_x)) + geom_histogram(binwidth = 10)

#A boxplot might be a better first glance (note change from x to y for mapping)
ds %>% ggplot(mapping = aes(y = por_x)) + geom_boxplot()
ds %>% filter(por_x < 640 & por_x > 0) %>% ggplot(mapping = aes(y = por_x)) + geom_boxplot()

#Might be better to check by participant
#Change the mapping to include x for id to divide data
ds %>% ggplot(mapping = aes(x = id, y = por_x)) + geom_boxplot()
ds %>% filter(por_x < 640 & por_x > 0) %>% 
  ggplot(mapping = aes(x = id, y = por_x)) + geom_boxplot()

#Or use freq_poly to overlay multiple histogram lines
ds %>% filter(por_x < 640 & por_x > 0) %>% 
  ggplot(mapping = aes(color = id, x = por_x)) + geom_freqpoly()

#Normalize counts with y = ..density..
ds %>% filter(por_x < 640 & por_x > 0) %>% 
  ggplot(mapping = aes(color = id, x = por_x, y = ..density..)) + geom_freqpoly(binwidth = 20)

#Point graph
ds %>% filter(por_x < 640 & por_x > 0) %>% ggplot(mapping = aes(x = por_x, y = por_y)) + geom_point()

#for overlapping data, try bin2d or contour2d
ds %>% filter(por_x < 640 & por_x > 0) %>% 
  ggplot(mapping = aes(x = por_x, y = por_y)) + geom_bin2d()

#Better EDA should include actual limits on axes
ds %>% filter(por_x < 640 & por_x > 0) %>% 
  ggplot(mapping = aes(x = por_x, y = por_y)) + geom_bin2d() +
  xlim(0, 640) + ylim(0,480)

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


introduce(ds)
plot_intro(ds)
plot_bar(ds)
plot_histogram(ds)

#vis_expect
