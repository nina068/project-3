
## read in package
library(shiny)
library(shinydashboard)
library(tidyverse)
library(caret)
library(randomForest)
library(gbm)
library(tree)
library(rgl)

## import dataset
getwd()
data <- read_csv("U.S._Life_Expectancy_at_Birth_by_State_and_Census_Tract_-_2010-2015.csv")

## remove NA
my_data <- data %>% drop_na()

# Define UI for application 
shinyUI(fluidPage(

    # Application title
    titlePanel("My Shiny App"),
    
    # About page
    navbarPage("About", 
               textInput("a", "1. This purpose of the app is to investigate U.S. life expectancy at birth by state and census tract for the period 2010 - 2015,
   2. This dataset is from Center for Disease Control and Prevention (CDC), link is https://data.cdc.gov/NCHS/U-S-Life-Expectancy-at-Birth-by-State-and-Census-T/5h56-n989. The variables in this dataset include State, County, Census Tract Number, Life Expectancy, Life Expectancy Range, Life Expectancy Standard Error,
  3. The main contents in this App contain an About page-a briefly introduction about the dataset and Shiny App, a Data exploration page - summary and plots of dataset, a Modeling page - there are three models to use for life expectancy prediction. and a Data page - view dataset and do some data manipulation in ths page.
  # 4.life expantancy picture is as bellow.")), 
    a(href = "https://www.titlemax.com/media/life-expectancy-united-states-4.png", "life expactancy by state image"), 
    
    # Data exploration page
    # define the sidebar with inputs
 navbarPage("Data Exploration", 
            sidebarPanel(
             checkboxGroupInput("table", choices = list("choice 1", "choice 2")
                                    ),
                 
            checkboxGroupInput("plot", choices = list("choice 1", "choice 2")
                                   )
                            )
            ),
# page of Modeling
navbarPage("Modeling",
           tabPanel("Modeling Info",
                    textInput("b", "Three supervised learning models were fitted, multiple linear regression, classfication trees, and a random forest model. For the three models, two character strings (State and County) were used as predictors and one numerical `Life Expectancy` was used as response.")
                    ),
           tabPanel("Model Fitting",
                    set.seed(123), ## split data 
         dataIndex <- createDataPartition(my_data$`Life Expectancy`, p = 0.8),
                    dataTrain <- my_data[dataIndex, ], ## training data set
                    dataTest <- my_data[-dataIndex, ], ## test data set
                 sliderInput("proportion", lable = c("dataTrain", "dataTest"),
                                value = p, min =0, max = 1),  
          ##classification tree
    fluidRow(
    trCtrlNew <- trainControl(method = "repeatedcv", number = 5,repeats = 3),
            column(2, "classfication tree model",
                  class_Tree <- train(`Life Expectancy` ~ ., data = dataTrain,
                                       method = "rpart", 
                                       trControl = trCtrlNew,
                                       preProcess = c("center", "scale"),
            tuneGrid = data.frame(cp = seq(from = 0, to = 0.1, by = 0.001)
                                  )
            )
            ),
 ## random forest model 
      column(6, "random forrest model", 
             random_forest <- train(`Life Expectancy` ~ `State` + `County`, 
                                    data = dataTrain, method = "rf", 
                   trControl = trainControl(method="cv", number=10), 
          preProcess = c("center", "scale"), tuneGrid=data.frame(mtry=(1:8)
                                                                 )
          )
          ), 
      
      ## multiple linear regression
      column(4, "multiple linear model",
      MLR <- lm(`Life Expectancy` ~ `State`+`County`, data = dataTrain)
      ),
      
      actionButton("action", label = "model fitting")
 )
 ), 
 
 ## prediction
 tabPanel("Prediction",
          class_Pred <- predict(class_Tree, newdata = dataTest), 
          MLR_pred <- predict(MLR, newdata = dataTest), 
          random_pred<-predict(random_forest, newdata = dataTest),
          checkboxGroupInput("modelselect", label= "model prediction")
                             )
          )
 ),
 
 ## page of "Data"
 navbarPage("Data",
            sliderInput("dataInput", "number of data"),
                  
             numericInput("rowInput"),
             numericInput("columnInput"),
           fileInput("file", label = row_column,
                     data_row_file , data_column_file)
                       ), 

 # mainpanel
 
 mainPanel(
   navbarsetpage(type = "navbarpage",
                 navbarpage("About",
                            texOutput("a")),
                 navbarpage("Data Exploration",
                            tableOutput("table"),
                            plotOutput("plot")),
                 navbarpage("Modeling",
                            tabsetPanel(type = "tabs",
                            tabPanel("Modeling Info", textOutput("b")), 
                            tabPanel("Model Fitting", 
                            outputOptions("proprotion", "summary", "action")), 
                       tabPanel("Prediction", uiOutput("modelselect")
                                )
                       )
                       ),
                 navbarpage("Data",
         outputOption("dataInput", "rowInput", "columnInput", "file")
         )
         )
   )
)

