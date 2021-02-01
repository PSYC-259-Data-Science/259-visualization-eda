#Load packages
library(tidyverse)
library(vroom)
library(here)
library(janitor)

#Load the 4 data files
files <- list.files(here("data_raw"), pattern = ".txt")
ds <- vroom(here("data_raw",files), id = "file", delim = " ", skip = 6) 
ds <- ds %>% rename(scene_time = `sceneQTtime(d:h:m:s.tv/ts)`, 
                    por_time = `porQTtime(d:h:m:s.tv/ts)`)
ds <- ds %>% rename_with(make_clean_names)
ds <- ds %>% mutate(file = str_extract(file, "\\d\\d\\d_(search|walk)")) %>% 
  separate(file, into = c("id", "cond")) %>% 
  mutate(cond = as.factor(cond))
  
#Let's first use the DataExplorer package to give us a snapshot of the entire dataset
library(DataExplorer)
plot_intro(ds) #Basic stats about columns and rows and missing values
plot_bar(ds) #Frequencies of all categorical variables
plot_histogram(ds) #Histograms of all continuous variables

#Vis_dat and vis_miss also give us useful snapshots
library(visdat)
vis_dat(ds) #similar to plot_intro, but gives a snapshot of each column
vis_miss(ds) #visualiza missing

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

ds %>% count(cut_width(por_y, 50))

#A boxplot might be a better first glance (note change from x to y for mapping)
ds %>% ggplot(mapping = aes(y = por_x)) + geom_boxplot()
ds %>% filter(por_x < 640 & por_x > 0) %>% ggplot(mapping = aes(y = por_x)) + geom_boxplot()

#At this point, I'm convinced -- let's set the impossible por_x and por_y values to NA 
#so that we can stop filtering
ds_cleaned <- ds %>% mutate(
  por_x = ifelse(por_x >= 0 & por_x < 640, por_x, NA),
  por_y = ifelse(por_y >= 0 & por_y < 480, por_y, NA)
)

#Might be better to check by participant
#Change the mapping to include x for id to divide data
ds_cleaned %>% ggplot(mapping = aes(x = id, y = por_x)) + geom_boxplot()
#Now we can better see the outliers and distributions in the remaining data

#Or use freq_poly to overlay multiple histogram lines
ds_cleaned %>%  ggplot(mapping = aes(color = id, x = por_x)) + geom_freqpoly()

#Normalize counts with y = stat(density)
ds_cleaned %>%  ggplot(mapping = aes(color = id, x = por_x, y = stat(density))) + geom_freqpoly()

#Scatter plot
ds_cleaned %>% ggplot(mapping = aes(x = por_x, y = por_y)) + geom_point()

#for overlapping data, try bin2d
ds_cleaned %>%  ggplot(mapping = aes(x = por_x, y = por_y)) + geom_bin2d()

#facets are incredibly useful for eda
#when your plot commands start to get long, make sure the + ends each line 
ds_cleaned %>% 
  ggplot(mapping = aes(x = por_x, y = por_y)) + 
  geom_bin2d() + 
  facet_wrap(~ id) +
  xlim(0, 640) + 
  ylim(0, 480)

#Saving your last ggplot to disk is easy
ggsave(here("eda","all_ppts_xy_plots.pdf"))

#But if you have more than 5-6 individual graphs, 
#it might be better to save them to a file to inspect one at a time
ids <- unique(ds_cleaned$id)
for (i in ids) {
  ds_cleaned %>% filter(id == i) %>% 
    ggplot(mapping = aes(x = por_x, y = por_y)) + 
    geom_bin2d() + 
    facet_wrap(~ cond) + 
    xlim(0, 640) + 
    ylim(0,480)
  ggsave(here("eda","individual_xy_plots",paste0(i,".png")))
}

#Writing a lot of individual files can get to be annoying, 
#but it's easy to put together different graphs with the patchwork package
library(patchwork)

ids <- unique(ds_cleaned$id)
for (i in ids) {
  
  #Plots can be stored to objects
  p1 <- ds_cleaned %>% filter(id == i) %>% 
    ggplot(mapping = aes(x = por_x, y = por_y)) + 
    geom_bin2d() + 
    facet_wrap(~ cond) + 
    xlim(0, 640) + 
    ylim(0,480)
  
  #Store plot 2
  p2 <- ds_cleaned %>% filter(id == i) %>% 
    ggplot(mapping = aes(x = por_x)) + 
    geom_histogram() +
    facet_wrap(~ cond) + 
    xlim(0, 640) 
  
  #Store plot 3
  p3 <- ds_cleaned %>% filter(id == i) %>% 
    ggplot(mapping = aes(x = por_y)) + 
    geom_histogram() +
    facet_wrap(~ cond) + 
    xlim(0, 480)
  
  #Divide means put plots one on top of another
  #Add means put them side by side
  #Any guesses what this does?
  p1/(p2 + p3)
  
  #p1 + p2 + p3 means panel horizontally
  #p1/p2/p3 means panel vertically

  ggsave(here("eda","individual_composite",paste0(i,".png")))
}
