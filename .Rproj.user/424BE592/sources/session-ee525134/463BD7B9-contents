library(shiny)

ui <- fluidPage(
  titlePanel("The g and h control charts"),
  sidebarLayout(
    sidebarPanel(
      fileInput("datafile", "Upload dataset (CSV file)",
                accept = c("text/csv", "text/comma-separated-values,text/plain",
                           ".csv")),
      radioButtons("plot_type", "Select chart type:",
                   choices = c("g chart", "h chart")),
      numericInput("alpha", "Enter the value of alpha:", value = 0)
      
    ),
    mainPanel(
      tabPanel("Plot", plotOutput("plot")),
      textOutput("upper_control_limit"),
      textOutput("center_line"),
      textOutput("lower_control_limit")
    )
  )
)

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
    
    if (input$plot_type == "g chart") {
      totals = c();
      sum = 0;
      
      for(i in 1:length(data()[[1]])){
        for(j in 1:ncol(data())){
          sum = sum + data()[[j]][[i]];
        }
        totals = append(totals,sum);
        sum = 0;
      }
      
      grand_sum = 0;
      
      for(i in 1:length(totals)){
        grand_sum = grand_sum + totals[i];
      }
      
      t_bar = grand_sum/length(totals);
      x_bb = t_bar/ncol(data());
      
      gUCL = t_bar + 3*(sqrt((ncol(data()))*(x_bb - input$alpha)*(x_bb - input$alpha + 1)));
      gCL = t_bar;
      gLCL = t_bar - 3*(sqrt((ncol(data()))*(x_bb - input$alpha)*(x_bb - input$alpha + 1)));
      
      if(gLCL<0){
        gLCL=0
      }
      
      ##the g chart
      plot(1:length(data()[[1]]),totals, type = "b",
           main="The g Chart", xlab = "Subgroup Number",
           ylab = "Total of the observations",
           ylim = c(0,gUCL+5));
      points(1:length(data()[[1]]),totals, pch = 16, col = ifelse(totals >= gUCL | totals <= gLCL, "red", "green"));
      
      abline(h = gUCL, lty = 2)
      abline(h = gCL)
      abline(h = gLCL, lty = 2)
      
      identify(1:length(data()[[1]]), totals, labels = totals, plot = FALSE, n = 1)
      
      output$upper_control_limit <- renderText({
        paste("Upper Control Limit:", round(gUCL,2))
      })
      
      output$center_line <- renderText({
        paste("Center Line:", round(gCL,2))
      })
      
      output$lower_control_limit <- renderText({
        paste("Lower Control Limit::", round(gLCL,2))
      })
    
    }else{
      totals = c();
      sum = 0;
      
      for(i in 1:length(data()[[1]])){
        for(j in 1:ncol(data())){
          sum = sum + data()[[j]][[i]];
        }
        totals = append(totals,sum);
        sum = 0;
      }
      
      grand_sum = 0;
      
      for(i in 1:length(totals)){
        grand_sum = grand_sum + totals[i];
      }
      
      t_bar = grand_sum/length(totals);
      x_bb = t_bar/ncol(data());
      
      ##control limits for h chart
      hUCL = x_bb + 3*(sqrt((1/ncol(data()))*(x_bb-input$alpha)*(x_bb-input$alpha+1)));
      hCL = x_bb;
      hLCL = x_bb - 3*(sqrt((1/ncol(data()))*(x_bb-input$alpha)*(x_bb-input$alpha+1)));
      
      if(hLCL<0){
        hLCL=0
      }
      
      averages = c(totals/ncol(data()));
      
      ##plotting the h chart
      plot(1:length(data()[[1]]), averages,
           main = "The h chart", xlab = "Subgroup Number",
           ylab = "Average of the observations", type = "b",
           ylim = c(0,hUCL+5));
      points(1:length(data()[[1]]),averages, pch = 16, col = ifelse(averages >= hUCL | averages <= hLCL, "red", "green"));
      
      ##plotting the control limits
      abline(h=hUCL, lty = 2);
      abline(h=hCL);
      abline(h=hLCL, lty = 2);
      
      output$upper_control_limit <- renderText({
        paste("Upper Control Limit:", round(hUCL,2))
      })
      
      output$center_line <- renderText({
        paste("Center Line:", round(hCL,2))
      })
      
      output$lower_control_limit <- renderText({
        paste("Lower Control Limit::", round(hLCL,2))
      })
    }
  })
  
}

shinyApp(ui, server)
