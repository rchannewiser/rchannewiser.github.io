library(shiny)
library(ggplot2)
library(dplyr)


ui <- fluidPage(
  
  
  # App title ----
  titlePanel("Beer data Analysis!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Choose Beer CSV File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      fileInput("file2", "Choose Breweries CSV File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      radioButtons("plottype", "Plot type:",
                   c("Boxplot" = "box",
                     "Histogram" = "hist")),
      
      radioButtons("reg_flag", "Regression Line:",
                   c("Yes" = "y",
                     "No" = "n"))
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Data file ----
      plotOutput("ABVplot"),
      plotOutput("IBUplot"),
      plotOutput("Scatterplot")
    )
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  
  output$ABVplot <- renderPlot({
    req(input$file1)
    
    beer <- read.csv(input$file1$datapath,header = TRUE)
    
    beer %>% ggplot(aes(x=ABV)) + geom_histogram() + ggtitle("ABV Histogram") -> abvhp
    beer %>% ggplot(aes(x=ABV)) + geom_boxplot() + ggtitle("ABV Boxplot") -> abvbp
    switch(input$plottype, 'box' = abvbp, 'hist' = abvhp )
    
  })
  
  output$IBUplot <- renderPlot({
    req(input$file1)
    
    beer <- read.csv(input$file1$datapath,header = TRUE)
    
    beer %>% ggplot(aes(x=IBU)) + geom_histogram() + ggtitle("IBU Histogram") -> ibuhp
    beer %>% ggplot(aes(x=IBU)) + geom_boxplot() + ggtitle("IBU Boxplot") -> ibubp
    switch(input$plottype, 'box' = ibubp, 'hist' = ibuhp )
  })
  
  output$Scatterplot <- renderPlot({
    req(input$file1)
    
    beer <- read.csv(input$file1$datapath,header = TRUE)
    
    beer %>% ggplot(aes(x=IBU, y=ABV)) + geom_point() + geom_smooth(method = 'lm') + ggtitle("IBU Vs ABV") -> sclm
    beer %>% ggplot(aes(x=IBU, y=ABV)) + geom_point() + ggtitle("IBU Vs ABV") -> scwlm
    switch(input$reg_flag, 'y' = sclm, 'n' = scwlm )
  })
  
}


shinyApp(ui = ui, server = server)
