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
plot_bar(ds) #Frequencies of all categorical variables
plot_histogram(ds) #Histograms of all continuous variables

#Vis_dat and vis_miss also give us useful snapshots
library(visdat)
vis_dat(ds) #snapshot of each column
vis_miss(ds) #visualize missing

#Vis_expect is a little more customizable
#Visualize both expected values and missing at the same time
vis_expect(select(ds, por_x), ~ .x > 0 & .x < 640)
#The ~ .x part is like what we used in map. It specifies a formula where .x stands in for the column we are visualizing


#Let's plot our own histogram with ggplot

#Simplest histogram
ggplot(data = ds, aes(x = por_x)) + geom_histogram()
ds %>% ggplot(aes(x = por_x)) + geom_histogram() #Or pipe data in

#Piping data in let's us filter first
ds %>% filter(por_x < 640 & por_x > 0) %>% 
  ggplot(aes(x = por_x)) + geom_histogram()

#Or we can adjust the graph axes to ignore the out of range data
ds %>% ggplot(aes(x = por_x)) + geom_histogram() + xlim(0, 640)
#xlim(lower_bound, upper_bound)

#A boxplot might be a better first glance
ds %>% filter(por_x < 640 & por_x > 0) %>% 
  ggplot(aes(y = por_x)) + geom_boxplot()

#What if we wanted to change the orientation of the boxplot?

#At this point, I'm convinced -- let's set the impossible por_x and por_y values to NA 
#so that we can stop filtering
ds_cleaned <- ds %>% mutate(
  por_x = ifelse(por_x >= 0 & por_x < 640, por_x, NA),
  por_y = ifelse(por_y >= 0 & por_y < 480, por_y, NA)
)

#Might be better to check by participant and condition
#Adding aes elements for id (x) and cond (fill) will change how the data are grouped
ds_cleaned %>% ggplot(aes(x = cond, y = por_x)) + geom_boxplot()
#Now we can better see the outliers and distributions in the remaining data

#Reference lines can also help
#when your plot commands start to get long, make sure the + ends each line
#Just like with the pipe, starting a line with + will lead to a syntax error
ds_cleaned %>% 
  ggplot(aes(x = id, fill = cond, y = por_x)) + 
  geom_hline(yintercept = 320) + 
  geom_boxplot() + 
  ylim(0, 640)

#For categorical data, geom_bar will automatically plot counts by category
ds_cleaned %>%
  ggplot(aes(id)) + geom_bar()

#Scatter plot - use geom_point
ds_cleaned %>% ggplot(aes(x = por_x, y = por_y)) + geom_point()

#For overlapping or dense data, try bin2d
ds_cleaned %>%  ggplot(mapping = aes(x = por_x, y = por_y)) + geom_bin2d()

#facets are incredibly useful for eda
ds_cleaned %>% 
  ggplot(mapping = aes(x = por_x, y = por_y)) + 
  geom_point() + 
  facet_wrap(c("id","cond")) +
  xlim(0, 640) + 
  ylim(0, 480)

#Saving your last ggplot to disk is easy
ggsave(here("eda","all_ppts_xy_plots.pdf"))

#But if you have more than 5-6 individual graphs, 
#it might be better to save them to a file to inspect one at a time
ids <- unique(ds_cleaned$id) #What does this do?
for (i in ids) {
  ds_cleaned %>% filter(id == i) %>% 
    ggplot(aes(x = por_x, y = por_y)) + 
    geom_bin2d() + 
    facet_wrap("cond") + 
    xlim(0, 640) + 
    ylim(0,480)
  ggsave(here("eda","individual_xy_plots",paste0(i,".png")))
}


### TOO LONG OF AN EXAMPLE FOR CLASS, BUT LEAVING IT FOR REFERENCE
# Faceting is a nice tool if you want to panel the same figure together
# If you want to panel different types of figures together, I suggest patchwork


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
