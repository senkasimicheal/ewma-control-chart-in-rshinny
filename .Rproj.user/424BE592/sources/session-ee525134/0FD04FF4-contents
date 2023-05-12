library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Exponentially Weighted Moving Average Control Chart"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          fileInput("datafile", "Upload dataset (CSV file)",
                    accept = c("text/csv", "text/comma-separated-values,text/plain",
                               ".csv")),
          numericInput("lambda", "Enter lambda:", value = 0),
          numericInput("target_value_mean", "Enter target value of the mean:", value = 0),
          numericInput("factor_L", "Enter width of the control limits:", value = 0),
          numericInput("Sigma", "Enter the value of sigma:", value = 0)
),
        mainPanel(
          tabPanel("Plot", plotOutput("plot"))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  data <- reactive({
    file <- input$datafile
    if (is.null(file)) {
      return(NULL)
    }
    read.csv(file$datapath)
  })
  
  output$plot <- renderPlot({
    if (is.null(data())) {
      return()
    }
    
    z = c();
    
    upper_control_limits = c();
    lower_control_limits = c();
    ucl = 0;
    lcl = 0;
    
    z_calculation = input$lambda*data()$x[1] + (1-input$lambda)*input$target_value_mean;
    z = append(z,z_calculation)
    
    for(i in 1:length(data()[[1]])){
      ucl=input$target_value_mean + input$factor_L*input$Sigma*(sqrt((input$lambda/(2-input$lambda))*(1-((1-input$lambda)^(2*i)))))
      lcl=input$target_value_mean - input$factor_L*input$Sigma*(sqrt((input$lambda/(2-input$lambda))*(1-((1-input$lambda)^(2*i)))))
      upper_control_limits = append(upper_control_limits,ucl)
      lower_control_limits = append(lower_control_limits,lcl)
    }
    
    for(i in 2:length(data()[[1]])){
      z_calculation = input$lambda*data()$x[i] + (1-input$lambda)*z[i-1]
      z = append(z,z_calculation) 
    }
    
    plot(1:length(data()[[1]]),z, type = "b",
         main="EWMA Control Chart", xlab = "Observation",
         ylab = "EWMA", ylim = c(min(lower_control_limits),max(upper_control_limits)));
    abline(h=input$target_value_mean)
    lines(1:length(data()[[1]]),upper_control_limits)
    lines(1:length(data()[[1]]),lower_control_limits)
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
