
library(shiny)
library(shinydashboard)
library(tidyverse)
library(caret)
library(randomForest)
library(gbm)

## import dataset
getwd()
setwd("D:/Statistics/ST 558/Project 3/project-3")
data <- read_csv("D:/Statistics/ST 558/Project 3/project-3/U.S._Life_Expectancy_at_Birth_by_State_and_Census_Tract_-_2010-2015.csv")

## remove NA
my_data <- data %>% drop_na()

##Define UI
ui <- fluidPage(
  # Application title
  titlePanel("My Shiny App"), 
  navbarPage("About", 
  textInput("a", "1. This purpose of the app is to investigate U.S. life expectancy at birth by state and census tract for the period 2010 - 2015,
   2. This dataset is from Center for Disease Control and Prevention (CDC), link is https://data.cdc.gov/NCHS/U-S-Life-Expectancy-at-Birth-by-State-and-Census-T/5h56-n989. The variables in this dataset include State, County, Census Tract Number, Life Expectancy, Life Expectancy Range, Life Expectancy Standard Error,
  3. The main contents in this App contain an About page-a briefly introduction about the dataset and Shiny App, a Data exploration page - summary and plots of dataset, a Modeling page - there are three models to use for life expectancy prediction. and a Data page - view dataset and do some data manipulation in ths page.
  # 4.life expantancy picture is as bellow.")), 
  
  Link("D:/Statistics/ST 558/Project 3/project-3/life_expectancy"),
  
  # define the sidebar with inputs
  navbarPage("Data Exploration", 
  sidebarPanel(
    checkboxGroupInput("table", 
    choice1 = table(my_data$State, my_data$`Life Expectancy`),
    choice2 = table(my_data$County, my_data$`Life Expectancy`)), 
    checkboxGroupInput("plot",
    choice1 = g_state <- ggplot(my_data, aes(x = `State`)), 
    g + geom_bar + 
      labs(x = "State", title = "Bar plot of State for life expantancy"),
    choice2 = g_county <- ggplot(my_data, aes(x = `County`)), 
    g + geom_bar + 
      labs(x = "county", title = "Bar plot of State for life expantancy"),
    )
  )
),

# page of Modeling
navbarPage("Modeling",
      tabPanel("Modeling Info",
       textInput("b", "Three supervised learning models were fitted, multiple linear regression, regression, and a random forest model")),
      tabPanel("Model Fitting",
      set.seed(123),
      dataIndex <- createDataPartition (((my_data$`Life Expectancy`, p = 0.8), list = FALSE)), ## split data 
      dataTrain <- my_data[dataIndex, ], ## training data set
      dataTest <- my_data[-dataIndex, ], ## test data set
      sliderInput("proportion", lable = c("dataTrain", "dataTest"),
   value = p, min =0, max = 1),   
      
      ##classificatin tree
  trCtrlNew <- trainControl(method = "repeatedcv", number = 5, repeats = 3),
  class_Tree <- train(`Life Expectancy` ~ ., data = dataTrain, 
                        method = "rpart", 
                        trControl = trCtrlNew,
                        preProcess = c("center", "scale"),
                        tuneGrid = data.frame(cp = seq(from = 0, to = 0.1,                          by = 0.001))),
  ## random forest model 
  random_forest <- train(`Life Expectancy` ~ `State` + `County`, data = dataTrain, method = "rf", trControl=trainControl(method="cv", number=10),preProcess = c("center", "scale"), tuneGrid=data.frame(mtry=(1:8))), 
  
## multiple linear regression
  MLR <- lm(`Life Expectancy` ~ `State`+`County`, data = data = dataTrain )
      ), 
actionButton("action", label = "model fitting"), 

summary(class_Tree),
summary(random_forest),
summary(MLR),

## prediction
    tabPanel("Prediction",
    class_Pred <- predict(class_Tree, newdata = dataTest), 
    MLR_pred <- predict(MLR, newdata = dataTest), 
    random_pred<-predict(random_forest, newdata = dataTest),
    checkboxGroupInput("modelselect", label= c("class_Pred", "MLR_pred", "random_pred")),
    )
),


## page of "Data"
navbarPage("Data",
        sliderInput("dataInput", min=min(my_data$`Life Expectancy`), max = max(my_data$`Life Expectancy`), step = 2),
        data_row <- my_data %>% filter(),
        data_column <- my_data %>% select(),
        numericInput("rowInput", value = data_row, min = min(n(data_row)), max = max(n(data_row))),
        numericInput("columnInput", value = data_column, min = min(n(data_column)), max = max(n(data_column))),
        
        data_row_file <- write_csv("D:/Statistics/ST 558/Project 3/project-3/data_row.csv"),
        data_column_file <- write_csv("D:/Statistics/ST 558/Project 3/project-3/data_row.csv"),
        fileInput("file", label = c("rowfile", "columnfile"), data_row_file, data_column_file) 
      ),

# mainpanel
mainPanel(textOutput("a", "b"),
          tableOutput("table"),
          plotOutput("plot"),
          uiOutput("proportion", "modelselect", "dataInput", "rowInput", "columnInput"),
          htmlOutput("file")

) )


## server file 
server <- function(input, output) {
  output$a <- renderText({
  })
  
}


shinyApp(ui = ui, server = server)

runGitHub("project-3", "nina068")