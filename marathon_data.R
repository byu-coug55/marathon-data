library(tidyverse)
library(rvest)
library(furrr)
library(tictoc)
library(lubridate)


marathon_data = read_csv("Downloads/marathon_data.csv") %>% filter(!is.na(`Want to read more?`))

key_question = "At which marathon did you get your first BQ?"

get_index = function(x){
  result = str_which(read_html(x) %>% 
                     html_nodes("p") %>% html_text(), key_question) %>% head(n=1)
  if (length(result)==1){
    return(result)
  }else{
    return(0)
  }
}

plan(strategy = "multisession")

tic()

index = as_tibble(future_map_dbl(.x = marathon_data$`Want to read more?`, .f = get_index))

toc()

names(index) = "index"

marathon_data_index = bind_cols(marathon_data,index) %>% filter(index != 0)

get_text = function(x,y){
  text1 = read_html(x) %>% html_nodes("p") %>% html_text() %>% nth(y)
  text2 = read_html(x) %>% html_nodes("p") %>% html_text() %>% nth(y+1)
  return(paste0(text1,text2, collapse = " "))
}

tic()
text = as_tibble(future_map2_chr(.x = marathon_data_index$`Want to read more?`, .y = marathon_data_index$index, .f = get_text))
toc()

names(text) = "text"

marathon_data_text = bind_cols(marathon_data_index,text)

get_time = function(x){
  trimws(str_sub(x, str_locate(x,"[:digit:]:[:digit:]")[1], str_locate(x,"[:digit:]:[:digit:]")[1]+3))
}

time = as_tibble(map_chr(.x = marathon_data_text$text, .f = get_time))

names(time)="time"

marathon_data_sub = bind_cols(marathon_data_text,time) %>% filter(!is.na(time))

marathon_data_time = marathon_data_sub %>% mutate(hours = as.double(str_sub(time,1,1))) %>%
  mutate(minutes = as.double(str_sub(time,3,4))) %>%
  mutate(time_hours = round(hours+minutes/60,2)) %>%
  mutate(seconds = time_hours*3600) %>% filter(hours>1, hours<5)

marathon_data_select = marathon_data_time %>%
  select('Name', 'Sex:', 'Age (at the time of first BQ):','Height (in inches):',
         'Weight (in lbs at the time of first BQ):','BMI','time_hours','seconds') %>%
  filter(!is.na(`Weight (in lbs at the time of first BQ):`))

my_data= data.frame("Lance", "M", 30, 76, 250, 30.4, 5.55,19980)

names(my_data) = names(marathon_data_select)

marathon_data_withme = bind_rows(marathon_data_select,my_data)

marathon_data_power = marathon_data_withme %>% mutate(weight_kg = `Weight (in lbs at the time of first BQ):`/2.2) %>%
  mutate(force_n = weight_kg*9.8) %>%
  mutate(work_per_mile = formatC(force_n*1609.34, format = "e", digits = 2)) %>%
  mutate(work_total = formatC(force_n*42195, format = "e", digits = 2)) %>%
  mutate(power_per_mile = force_n*1609.34/(seconds/26.2))

marathon_data_bins = marathon_data_power %>%
  mutate(height_bin = cut(`Height (in inches):`, breaks = 4)) %>%
  mutate(weight_bin = cut(`Weight (in lbs at the time of first BQ):`, breaks = 4)) %>%
  mutate(bmi_bin = cut(BMI, breaks = 4)) %>%
  mutate(age_bin = cut(`Age (at the time of first BQ):`, breaks = 4))

marathon_data_bins %>% ggplot(aes(x=height_bin,y=time_hours)) + geom_boxplot() + geom_point()
marathon_data_bins %>% ggplot(aes(x=weight_bin,y=time_hours)) + geom_boxplot()
marathon_data_bins %>% ggplot(aes(x=bmi_bin,y=time_hours)) + geom_boxplot()
marathon_data_bins %>% ggplot(aes(x=age_bin,y=time_hours)) + geom_boxplot()
marathon_data_bins %>% ggplot(aes(x=`Weight (in lbs at the time of first BQ):`,y=time_hours)) + geom_point()

marathon_data_rank = marathon_data_bins %>% arrange(time_hours) %>% mutate(time_rank = row_number())

write_csv(marathon_data_rank, "marathon_data.csv")
