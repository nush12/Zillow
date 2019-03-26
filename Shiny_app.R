library(shiny)
library(ggplot2)
library(plotly)
library(readr)
library(dplyr)
library(scales)

app_data <-  load("data/app_data.RData")


# UI Function -------------------------------------------------------------


ui <- fluidPage(

sidebarLayout(
  
    sidebarPanel(
   
    wellPanel(
      
      h5("Drag the slider to change years"),
      
      sliderInput(inputId = "z", 
                  label = "Select Year:",
                  min = 1, max = 44,
                  value = "35")

    )
      

      
      
    ),
    

    mainPanel(
      plotOutput(outputId = "payback_period")
      
    )
  )
)

# Define server function required to create the scatterplot
server <- function(input, output) {
  
  input_func <- reactive({
    req(input$z)
    year <-  as.character(input$z)
  })
  
  
  
  # Create the scatterplot object the plotOutput function is expecting
output$payback_period <- renderPlot({
    
    year <- input_func()
    year <- as.numeric(year)
    merged_ijoin %>% 
    select(zipcode,median_rent_zip, investment, annual_payback) %>%
    mutate(payback = ((annual_payback * year) - investment )) %>%
    mutate(recovery = ifelse(payback >= 0, "Complete", "Incomplete")) %>%
    ggplot(aes(x = reorder(zipcode, payback), y = payback,
               fill = as.factor(recovery))) +
    geom_bar(stat = "identity", width = 0.6) +
      labs(title = "When will the properties be profitable?",
           subtitle = "ROI over years", 
           legend = "Payback") +
      scale_y_continuous(labels = comma)+
      xlab("zipcode") +
      ylab("Payback amount") +
    scale_fill_manual(values=c("#ff5a5f", "#66b2b2"),
                      labels = c("Complete", "Incomplete"),
                      name="Payback")+
    theme_minimal()+
    theme(text = element_text(size = 10),
          plot.title = element_text(size = 18,color = "#ff5a5f", face = "bold",
                                      margin = margin(b = 7)),
          plot.subtitle = element_text(size = 14, color = "darkslategrey", 
                                       margin = margin(b = 7)))
 
    
  })
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)

