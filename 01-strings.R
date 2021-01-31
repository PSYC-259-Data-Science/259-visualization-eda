#Load packages
library(tidyverse)
library(vroom)
library(here)
library(janitor)

#Load the 4 data files and fix the y scaling
files <- list.files(here("data_raw"), pattern = ".txt")
ds <- vroom(here("data_raw",files), id = "file", delim = " ", skip = 6) 
ds <- ds %>% rename(scene_time = `sceneQTtime(d:h:m:s.tv/ts)`, 
                    por_time = `porQTtime(d:h:m:s.tv/ts)`)
ds <- ds %>% rename_with(make_clean_names)

#Clean up file name with str_remove
ds %>% mutate(file = str_remove(file, fixed("/Users/johnfranchak"))) %>% select(file)

#Let's not "hard code" this
dir_path <- here()
ds %>% mutate(file = str_remove(file, dir_path)) %>% select(file)

dir_path_better <- paste0(dir_path, "/data_raw/")
ds %>% mutate(file = str_remove(file, dir_path_better)) %>% select(file)

#Yes, that works -- let's make it permanent
ds <- ds %>% mutate(file = str_remove(file, dir_path_better)) %>% select(file)
