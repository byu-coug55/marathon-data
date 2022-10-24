# marathon-data
Trying out Shiny!

On Oct 1 this year, I ran my first marathon! I competed with 5000 other athletes at the 45th annual St. George Marathon in St. George, UT. My goal was just survival - all I wanted to do was cross the finish line. After training months for this race, I completed the marathon in 5 hours 33 minutes and decided I probably should have trained more 😂. 

Even though I was happy with my time and ability to complete the marathon, I couldn't help looking at the leaders' times. The fastest runner this year was JJ Santana who ran the marathon in 2 hours 15 minutes! They finished the marathon in less than half of my time. This got me thinking, how does the amount of work I completed and the power I generated compare to the time leaders? 

Time to gather and analyze some data! I quickly found out that I could not find any data other than the runners' times for the St. George Marathon. In order to compare work and power, I needed to find weight data. After a thorough Google search, I was able to find one source of data: https://miloandthecalf.com/the-bq-questionnaire/. This data comes from a blog entitled "Milo and the Calf". The blog's author (Sean) is a runner and was also interested in marathon runners' demographics and body types. He put out a survey to runners who had qualified for the Boston Marathon and compiled the data on his website. So thank you @miloandthecalf [https://twitter.com/MiloandtheCalf] for collecting this data!

Sean compiled the data he collected into a google sheet: https://docs.google.com/spreadsheets/d/1yc2jk2zv7iSTBSZd_FIU2zNarJ61-GxZTGEs4xr2F_A/edit#gid=0. The one thing missing from this sheet is a column to identify the runner's time when the qualified for Boston. So that was my first task: compiling runner times.

After some data gathering and cleaning, I then take this data and turn it into an interactive web application here: https://byu-coug55.shinyapps.io/Marathon_Data_App/. With this web application you can enter your data into the app and so how you compare to me and ~100 Boston-qualified marathon runners!

## Data Gathering/Cleaning

Here I'll run through my analysis and code to get to the finished product. First: load libraries and load the downloaded dataset from the google sheet above:
```
library(tidyverse)
library(rvest)
library(furrr)
library(tictoc)
library(lubridate)


marathon_data = read_csv("Downloads/marathon_data.csv") %>% filter(!is.na(`Want to read more?`))
```

The next thing I needed to do was compile the runner times. Although the runner times were not in the google sheet, Sean did have the runner times in the individual interview pages. Luckily he listed a link to that page in the last column of the google sheet. Using that link, I was able to use R's rvest package to find each runner's time.

```
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
```

The above code finds the index of the html node where the key_question is located. I recommend using furrr's future_map as it takes advantage of parallel processing. I was then able to use that index to return the text data:

```
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
```

The next step was to extract the time out of the text. I did this using stringr's str_locate:

```
get_time = function(x){
  trimws(str_sub(x, str_locate(x,"[:digit:]:[:digit:]")[1], str_locate(x,"[:digit:]:[:digit:]")[1]+3))
}

time = as_tibble(map_chr(.x = marathon_data_text$text, .f = get_time))

names(time)="time"

marathon_data_sub = bind_cols(marathon_data_text,time) %>% filter(!is.na(time))

marathon_data_time = marathon_data_sub %>% mutate(hours = as.double(str_sub(time,1,1))) %>%
  mutate(minutes = as.double(str_sub(time,3,4))) %>%
  mutate(time_hours = round(hours+minutes/60,2)) %>%
  mutate(seconds = time_hours*3600) %>% filter(hours>1, hours<5) #filtering for realistic times (any time starting with a 1 was usually a half marathon time and anything starting with a 5 was usually a mile time)
```

The rest of it is pretty easy. I add my data to the dataset, calculate work and power, then split the data into bins to create boxplots and save the data to use for the web app.

```
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
marathon_data_bins %>% ggplot(aes(x=`Height (in inches):`,y=time_hours)) + geom_point() + geom_smooth(method = "lm")

marathon_data_rank = marathon_data_bins %>% arrange(time_hours) %>% mutate(time_rank = row_number())

write_csv(marathon_data_rank, "marathon_data.csv")
```

Here is an example of one of the boxplots:

<img width="697" alt="image" src="https://user-images.githubusercontent.com/56312233/197585634-2651be48-7478-498f-99e1-b46522190010.png">

And the scatterplot for height:

<img width="699" alt="image" src="https://user-images.githubusercontent.com/56312233/197585918-5cdcb1d5-d98e-46df-b14e-7d2fa420a841.png">

Interestingly, in this dataset, as height increases, the marathon time decreased! I don't believe this is generally true, but an interesting tidbit from this dataset.

## Build the Shiny App

Now that we have the dataset ready to use, it's time to build the app! This is done using R's shiny package. The three most important functions to this package are: ui(), server(), and shinyApp(). Before we get started though, let's load the libraries and data:

```
library(tidyverse)
library(shinydashboard)
library(shiny)
library(gghighlight)
library(shinythemes)


marathon_data = read_csv("marathon_data.csv") %>% select(1:13) %>% select(-`Sex:`) %>%
  select(Name, Age = "Age (at the time of first BQ):", Height = "Height (in inches):", Weight = "Weight (in lbs at the time of first BQ):",
         Weight_kg = "weight_kg", Weight_N = force_n, BMI, Time_hrs = time_hours, Time_s = seconds,
         Work_per_mile = work_per_mile, Work_total = work_total, Power = power_per_mile) %>%
  arrange(Time_hrs) %>% mutate(time_rank = row_number())
```

Within the ui() function, you include functions that organize your content, add input parameters, and add outputs.

```

ui <- dashboardPage(
  
  dashboardHeader(title = "Marathon Data Analysis"),
  dashboardSidebar(
    HTML("<h3>Input parameters</h3>"),
    textInput("name","Name", value = "Michael"),
    numericInput("height","Height in Inches", value = 73, min = 59, max = 84),
    numericInput("weight","Weight in Lbs", value = 190, min = 100, max = 350),
    numericInput("age","Age", value = 27, min = 15, max = 105),
    numericInput("marathon_time","Marathon Time (Hours)", value = 4.5, min = 1.75, max = 8),
    selectInput("selection","Chart Selection", choices = c("Age","Height","Weight","BMI","Work"="Work_total","Power"),
                selected = "Power")
  ),
  dashboardBody(
    wellPanel("Marathon Data Scatterplot",plotOutput(outputId = "scatter"),style = "overflow-y:scroll"),
    fluidRow(
      tabBox(
        title = "Rankings",
        # The id lets us use input$tabset1 on the server to find the current tab
        id = "tabset1", height = "600px",
        tabPanel("User Info", "User Info", tableOutput("user_info")),
        tabPanel("Race Times", "Race Times", tableOutput("time_leaders")),
        tabPanel("Work ", "Work Completed (Nm): Work=Force*distance",tableOutput("work_leaders")),
        tabPanel("Power", "Power Consumption (W): Power=Work/time",tableOutput("power_leaders"))
      ),
      tabBox(
        title = "Distribution",
        # The id lets us use input$tabset1 on the server to find the current tab
        id = "tabset2", height = "600px",
        tabPanel("Height Dist", "Height Dist", plotOutput("height_box")),
        tabPanel("Weight Dist", "Weight Dist",plotOutput("weight_box")),
        tabPanel("Age Dist", "Age Dist",plotOutput("age_box"))
      ))
  )
)
```

This is the frontend user interface the website visitor interacts with. Inputs are sent to the backend server where they are transformed and then brought back as outputs. For example, the user inputs their data into the side bar of the app, that data is sent to the server where I use it to calculate other paramters like BMI, work, and power. The transformed data is then brought back to the ui and is displayed as tableOutput("user_info").

![image](https://user-images.githubusercontent.com/56312233/197590255-00dbf689-a9dd-439d-9f95-8188fd32eb4f.png)

Here is the server's code:

```
server <- function(input, output, session) {
  
  user_name = reactive(input$name)
  user_age = reactive(input$age)
  user_height = reactive(input$height)
  user_weight = reactive(input$weight)
  user_time = reactive(input$marathon_time)
  
  
  user_data = reactive({data.frame(`Name`= user_name(), `Age` = user_age(), `Height` = user_height(),
                                   `Weight` = user_weight(), `Weight_kg` = user_weight()/2.2, `Weight_N` = user_weight()/2.2*9.8,`BMI` = 703*user_weight()/(user_height())^2,
                                   Time_hrs = user_time(), Time_s = user_time()*3600, Work_per_mile = user_weight()/2.2*9.8*1609.34, 
                                   Work_total = user_weight()/2.2*9.8*42195, Power = user_weight()/2.2*9.8*42195/(user_time()*3600))})
  
  marathon_data_rank = reactive(marathon_data %>% select(-time_rank) %>% bind_rows(user_data()) %>%
                                  arrange(Time_hrs) %>% mutate(time_rank = row_number()))
  
  user_select = reactive(input$selection)
  
  head_time = reactive(head(marathon_data_rank() %>% select(rank = time_rank,Name,Time_hrs),n=10))
  
  head_time_func = function(x) {if (str_detect(head_time()[2],x)){
    return(head_time())
  } else{
    return(bind_rows(head_time(),marathon_data_rank() %>% filter(Name == x) %>%
                       select(rank = time_rank, Name, Time_hrs)))
  }}
  
  head_work = reactive(head(marathon_data_rank() %>% arrange(desc(Work_total)) %>%
                              mutate(rank = row_number()) %>% select(rank, Name,"Total Work" = Work_total, time_rank),n=10))
  
  head_work_func = function(x) {if (str_detect(head_work()[2],x)){
    return(head_work())
  } else{
    return(bind_rows(head_work(),marathon_data_rank() %>% arrange(desc(Work_total)) %>%
                       mutate(rank = row_number()) %>% filter(Name == x) %>% select(rank, Name,"Total Work" = Work_total, time_rank)))
  }}
  
  head_power = reactive(head(marathon_data %>% arrange(desc(Power)) %>%
                               mutate(rank = row_number()) %>% select(rank, Name,Power = Power, time_rank),n=10))
  
  head_power_func = function(x) {if (str_detect(head_power()[2],x)){
    return(head_power())
  } else{
    return(bind_rows(head_power(),marathon_data_rank() %>% arrange(desc(Power)) %>%
                       mutate(rank = row_number()) %>% filter(Name == x) %>% select(rank, Name, Power = Power, time_rank)))
  }}
  
  marathon_data_bins = reactive(marathon_data_rank() %>%
                                  mutate(height_bin = cut(`Height`, breaks = 4)) %>%
                                  mutate(weight_bin = cut(`Weight`, breaks = 4)) %>%
                                  mutate(bmi_bin = cut(BMI, breaks = 4)) %>%
                                  mutate(age_bin = cut(`Age`, breaks = 4)))
  
  output$user_info = renderTable(marathon_data_rank() %>% filter(Name == user_name()) %>%  pivot_longer(cols = !Name ,names_to = "Data", values_to = "Values") %>% select(-Name))
  output$time_leaders = renderTable(bind_rows(head_time_func("Lance"),head_time_func(user_name())) %>% distinct() %>% arrange(rank))
  output$work_leaders = renderTable(bind_rows(head_work_func("Lance"),head_work_func(user_name())) %>% distinct() %>% arrange(rank))
  output$power_leaders = renderTable(bind_rows(head_power_func("Lance"),head_power_func(user_name())) %>% distinct() %>% arrange(rank))
  output$height_box = renderPlot(marathon_data_bins() %>% ggplot(aes(x=height_bin,y=Time_hrs)) + geom_boxplot() + 
                                   geom_point() + gghighlight(Name %in% c("Lance", user_name()), label_key = Name, use_group_by = FALSE) + theme_minimal())
  output$weight_box = renderPlot(marathon_data_bins() %>% ggplot(aes(x=weight_bin,y=Time_hrs)) + geom_boxplot() + 
                                   geom_point() + gghighlight(Name %in% c("Lance", user_name()), label_key = Name, use_group_by = FALSE) + theme_minimal())
  output$age_box = renderPlot(marathon_data_bins() %>% ggplot(aes(x=age_bin,y=Time_hrs)) + geom_boxplot() + 
                                geom_point() + gghighlight(Name %in% c("Lance", user_name()), label_key = Name, use_group_by = FALSE) + theme_minimal())
  output$scatter = renderPlot(marathon_data_bins() %>% ggplot(aes_string(x=user_select(),y="Time_hrs")) +
                                geom_point() + geom_smooth(data = subset(marathon_data_bins(), !Name %in% c("Lance",user_name())),method = "lm", se = FALSE) + gghighlight(Name %in% c("Lance", user_name()), label_key = Name, use_group_by = FALSE) + theme_minimal())
  
}
```

This is where all the magic happens. At the beginning I am creating reactive functions that contain the user's input data. These have to be reactive functions because I want the web app to respond reactively anytime the user inputs new data. Anytime a reactive function is used, you need to include "()" after it. For example, "user_weight()". I then add the user's data to the dataset.

Now these are the things I wanted to include as outputs in my app:
- A table showing the user's data with the new calculcated parameters (BMI, work, power, etc)
- Tables ranking the user's data alongside the top 10 of each category: time, work, and power
  - However, I also wanted the user's data to show up even if outside the top 10
  - I use the custom head functions to achieve this
- Boxplots to show the distribution of data across categories
- An interactive scatterplot where the user can choose what to plot

You'll see that inputs are called using "input$". Outputs are likewise created using "output$". The name of the output comes after "output$". This name is then used in the ui function to display. You use "render" ("renderPlot", "renderTable") everytime an output is defined in order to create a reactive object.

At the end of the script you call the shinyApp() function:
```
shinyApp(ui, server)
```

And here is the final product: https://byu-coug55.shinyapps.io/Marathon_Data_App/

## Conclusion

I like the interactivity and how the user can click through the various tabs to explore the data. It actually also displays well on mobile, which was an unexpected gift!

If I had more time I would have played with the layout and colors more. However, I am very pleased with the end result. And as you can see from the "Work Rankings" tab, I completed way more work than everyone else (simply because I weigh much more). The "Power Rankings" data is where the true test is though, and I was happy to see that even though my time was very slow, my power output was mid-range compared to these Boston Qualifiers.





