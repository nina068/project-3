

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


# Define server logic
shinyServer(function(input, output) {

  output$plot <- renderPlot({ 
    g_state <- ggplot(my_data, aes(x = `State`))  
    g_state + geom_bar + 
      labs(x = "State", title = "Bar plot of State for life expantancy")
    
    g_county <- ggplot(my_data, aes(x = `County`)) 
    g + geom_bar + 
      labs(x = "county", title = "Bar plot of State for life expantancy")}) 
  
  output$table <- renderTable({
    table(my_data$State, my_data$`Life Expectancy`)
    table(my_data$County, my_data$`Life Expectancy`)})
  output$a <- renderText()
  output$b <- renderText()
  output$proportion <- renderPrint(p = seq(from = 0, to = 1, by = 0.05))
  output$summary <- renderPrint(
    summary(class_Tree),
    summary(random_forest),
    summary(MLR))
  output$action <- renerUI(class_Tree, random_forest, MLR)
  output$file <- renderUI(data_row_file <- write_csv("data_row.csv"),
                          data_column_file <- write_csv("data_row.csv"))
  output$rowInput <- renderPrint( min = min(n(data_row)), 
                                  max = max(n(data_row)))
  output$columnInput <- renderPrint(min = min(n(data_column)), 
                                    max = max(n(data_column)))
  output$dataInput <- renderPrint(min = min(n(my_data$`Life Expectancy`), 
                                            max = max(n(my_data$`Life Expectancy`)), step = 2))
  output$modelselect <- renderUI(class_Pred, MLR_pred, random_pred)
}
)


