#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/

library(shiny)
library(ggplot2)
library(DT)
library(dplyr)


data <- read.csv("Auschwitz_data.csv")

ui <- fluidPage(
  titlePanel("Visualization of Auschwitz Holocaust Victims"),
  sidebarLayout(
    sidebarPanel(
      selectInput("categories",
                  "Choose a Category:",
                  choices = c("Religion", "Birthplace", "Residence")),
      uiOutput("category_checkbox")
    ),
    mainPanel(
      plotOutput("graph"),
      DTOutput("table")
    )
  )
)

server <- function(input, output, session) {
  
 
  output$category_checkbox <- renderUI({
    choices <- NULL
    if (input$categories == "Religion") {
      choices <- unique(data$Religion)
    } else if (input$categories == "Birthplace") {
      choices <- unique(data$Birthplace)
    } else if (input$categories == "Residence") {
      choices <- unique(data$Residence)
    }
    if (!is.null(choices)) {
      checkboxGroupInput("ChosenCategories",
                         label = paste("Select", input$categories),
                         choices = choices)
    }
  })
  
  
  filtered_data <- reactive({
    req(input$categories, input$ChosenCategories) 
    
    category <- input$categories
    chosen <- input$ChosenCategories
    
    data %>% 
      filter(.[[category]] %in% chosen)
  })
  
  # Render the graph based on the selected category and checkboxes
  output$graph <- renderPlot({
    data_to_plot <- filtered_data()
    if (nrow(data_to_plot) > 0) {
      ggplot(data_to_plot, aes(x = get(input$categories), fill = get(input$categories))) +
        geom_bar() +
        theme_classic() +
        labs(x = input$categories, y = "Number of Victims", title = paste("Auschwitz Victims by", input$categories)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
  
  
  output$table <- renderDT({
    datatable(filtered_data())
  })
}


shinyApp(ui = ui, server = server)
