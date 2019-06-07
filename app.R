#install.packages("shiny")
library(shiny)
library(ggplot2)
library(dplyr)

### set working directory
setwd("/Users/suniljoshi/wq-exploratory-code/sjoshi/CTG/SHINY")


### read test results data
data <-read.csv("test.csv")
#names(data)
library(plyr)


### create shiny UI

## consider slider as sample_year as we want to see the chemical_value for every year
#### radio buttons input as chemical_name mainly phosporus, chloride and nitrogen
ui <- fluidPage(
  titlePanel("test_results_dataset"),
  sidebarLayout(
     sidebarPanel(
       sliderInput("Year", "SAMPLE_YEAR", 1980, 2017, c(1990,2000)),
       radioButtons("typeInput", "Characteristic_Name",
                    choices = c("Phosphorus","Chloride", "Total Nitrogen, mixed forms"),
                    selected = "Phosphorus"),
       uiOutput("chemicalOutput")),
       mainPanel(
         plotOutput("coolplot"),
         br(), br(),
         tableOutput("results")
       )

     )
)


### create server fucnton to visylaize the tables and graphs

## create basic histogram to visualize the data
server <- function(input,output){
  output$chemicalOutput <- renderUI({
    selectInput("county_input" , "County",
                sort(unique(data$County)))
  })
  filtered <-  reactive({
    if (is.null(input$county_input)){
      return (NULL)
    }

    data %>%
      filter(SAMPLE_YEAR >= input$Year[1],
             SAMPLE_YEAR >= input$Year[2],
            Characteristic_Name == input$typeInput,
            County == input$county_input)

  })
### visualize the output with histograms and tables
  output$coolplot <- renderPlot({

    if (is.null(filtered())) {
      return ()
    }
    ggplot(filtered(),aes(Result.Value))+ geom_histogram()
  })

  output$results <- renderTable({
    filtered()
  })
}

shinyApp(ui=ui, server = server)



