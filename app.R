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



## ---- app ----

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

shinyApp(ui, server)



