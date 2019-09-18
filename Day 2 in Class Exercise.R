#day 2 in class exercise
rm(list = ls())
library(lubridate)
library(reshape2)
library(tidyverse)

generation_dp <- read.csv(here::here("data/ca_energy_generation.csv"), stringsAsFactors = F)
imports_dp <- read.csv(here::here("data/ca_energy_imports.csv"), stringsAsFactors = F)

generation_dp$datetime = as_datetime(generation_dp$datetime) # from package 'lubridate': convert data type
imports_dp$datetime = as_datetime(imports_dp$datetime)


##reshaping data
head(generation_dp)
long_gen = melt(generation_dp, id.vars = 'datetime', variable.name = 'source',value.name = 'usage') # change to long-format

#merge function
merged_energy = merge(generation_dp, imports_dp, by = 'datetime')
long_merged_energy = melt(merged_energy, id.vars = 'datetime', variable.name = 'source',value.name = 'usage')

#data manipulation
merged_energy %>% filter(imports > 7000 , natural_gas < 7000) %>% nrow()

long_merged_energy %>% mutate(log_usage = log(usage), usage_sq = usage^2) %>% head()

long_merged_energy %>% summarise(total = sum(usage, na.rm = T), mean = mean(usage,na.rm = T))

long_merged_energy %>% 
    filter(source == 'geothermal') %>% 
    select(-datetime) %>% 
    mutate(log_usage = log(usage)) %>% 
    summarise(mean_log_usage = mean(log_usage,na.rm = T))


merged_energy %>% 
    select(-datetime) %>% 
    mutate(total_usage = rowSums(., na.rm = T)) %>% 
    summarize(total_usage = sum(total_usage, na.rm = T))

merged_energy %>% 
    select(contains("hydro")) %>% 
    mutate(total_hydro = rowSums(., na.rm = T)) %>% ##remember to use '.'
    summarise(mean_usage = mean(total_hydro, na.rm = T))

long_merged_energy %>% 
    group_by(source) %>% 
    summarize(sum_usage = sum(usage, na.rm = T))

long_merged_energy %>% group_by(source) %>% 
    summarise(ave_usage = mean(usage,na.rm = T)) %>% 
    filter(source %in% c('small_hydro','large_hydro','biogas','biomass'))

merged_energy %>% select(small_hydro,large_hydro,biogas,biomass) %>% 
    summarise_all(mean)


#################
#Part 2 data.table
#################
# import data.table library
library(data.table)
data_file <- here::here("data", "ca_energy_generation.csv")

# read in two versions of data, one as a data.frame and one as a data.table
generation_df <- read.csv(data_file, stringsAsFactors = F)
generation_dt <- fread(data_file)

View(generation_df)
View(generation_dt)
generation_df
generation_dt
str(generation_df)
str(generation_dt)

# i operator
generation_dt[wind > 4400]
generation_dt[wind > 4400 & mday(datetime) == 7]
generation_dt[natural_gas <= 5000 & large_hydro > 2000]
generation_dt[coal > 10 & solar > median(solar)]

# j operator
generation_dt[,wind + solar]
generation_dt[,3 * wind + solar * biogas/2]

#directly add to a data table  := is in-place
generation_dt[,newcol := 3*wind + solar*biogas/2] 

#export to a new data table  .( export data
generation_dt[,.(newcol = 3*wind + solar*biogas/2)] 

#delete a column
generation_dt[,newcol := NULL]

#practice
generation_dt[, total_hydro := small_hydro+large_hydro]
generation_dt[,.(mean(nuclear), mean(biogas)) ]  #why do we need a period here? -- to export data?
generation_dt[solar==0,.(datetime, total_thermal = natural_gas+coal)]

# group_by operator
generation_dt[,mean(nuclear), by = mday(datetime)]
generation_dt[,.(mean_nuc = mean(nuclear), mean_wind = mean(wind)), 
              by = mday(datetime)]
generation_dt[hour(datetime) > 19,
              .(mean_nuc = mean(nuclear), mean_wind = mean(wind)), 
              by = mday(datetime)]
#practice
generation_dt[,.(median(solar)), by = hour(datetime)]  #Q:when do we need to add period?
generation_dt[,median(solar), by = hour(datetime)]

generation_dt[solar>0, .(max(natural_gas)), by = mday(datetime)]
generation_dt[solar>0, max(natural_gas), by = mday(datetime)]

#practice2
all_generation_long[,day := as_date(datetime)]
all_generation_long[,log_output := log(value)]
all_generation_long[,per_output := value/sum(value), by = day]
#if use only one command
all_generation_long[,`:=`(day2 = as_date(datetime), 
                          log_output2 = log(value), 
                          per_output2 = value/sum(value)), 
                    by = day]


# for .N: convenient
all_generation_long[,.N] 
all_generation_long[,.N, by = type] # the number of rows by type

# for .I: more advanced syntax
all_generation_long[,.I]

# check the current key
key(generation_dt)

# set key
setkey(generation_dt, datetime)

imports_dt <- fread(here::here("data", "ca_energy_imports.csv"))

imports_dt

imports_dt[generation_dt, on = "datetime"]
key(generation_dt)
