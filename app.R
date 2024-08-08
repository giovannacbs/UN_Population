library(shiny)
library(dplyr)
library(shinyWidgets)
library(plotly)

data <- read.csv("Base_Pop_EM_24.csv")

data$Time <- as.integer(data$Time)
data$Ano <- as.integer(data$Ano)

age_order <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", 
               "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", 
               "75-79", "80-84", "85-89", "90-94", "95-99", "100+")

data$AgeGrp <- factor(data$AgeGrp, levels = age_order)

anos_disponiveis <- sort(unique(data$Ano))
anos_time <- sort(unique(data$Time))

ui <- fluidPage(
  titlePanel("Population (UN)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("location1", "Select first group:", choices = unique(data$Location)), 
      selectInput("location2", "Select second group:", choices = unique(data$Location)), 
      sliderTextInput("time", "Select reference year:", 
                      choices = anos_time, selected = min(anos_time), animate = animationOptions(interval = 300)),
      sliderTextInput("ano", "Select year of revisions updates:", 
                      choices = anos_disponiveis, selected = min(anos_disponiveis), animate = animationOptions(interval = 300))
    ),
    mainPanel(
      fluidRow(
        column(6, plotlyOutput("pyramidPlot1")),
        column(6, plotlyOutput("pyramidPlot2"))
      ),
      tags$footer(
        tags$p("Source: UN Population Projection (medium estimates)", style = "text-align: left;")
      )
    )
  )
)

server <- function(input, output) {
  renderPyramidPlot <- function(filtered_data, showlegend = TRUE) {
    total_male <- sum(filtered_data$PopMale)
    total_female <- sum(filtered_data$PopFemale)
    
    filtered_data <- filtered_data %>%
      mutate(PctMale = (PopMale / total_male) * 100,
             PctFemale = (PopFemale / total_female) * 100,
             PctFemale = -PctFemale)  # Invert the female percentage for the pyramid
    
    male_plot <- plot_ly(
      data = filtered_data, 
      x = ~PctMale, 
      y = ~AgeGrp, 
      type = 'bar', 
      orientation = 'h', 
      name = 'Male',
      hoverinfo = 'text',
      hovertext = ~paste('Male percentage:', formatC(PctMale, format = "f", digits = 2), '%', 
                         '<br>Age group:', AgeGrp, 'years'),
      marker = list(color = 'lightskyblue')
    )
    
    female_plot <- plot_ly(
      data = filtered_data, 
      x = ~PctFemale, 
      y = ~AgeGrp, 
      type = 'bar', 
      orientation = 'h', 
      name = 'Female',
      hoverinfo = 'text',
      hovertext = ~paste('Female percentage:', formatC(-PctFemale, format = "f", digits = 2), '%', 
                         '<br>Age group:', AgeGrp, 'years'),
      marker = list(color = 'pink')
    )
    
    p <- subplot(
      female_plot, 
      male_plot,
      shareY = TRUE, 
      titleX = TRUE, 
      widths = c(0.5, 0.5)) %>% 
      layout(
        xaxis = list(title = '', showtick = TRUE, title_standoff = 20,range = c(-20,0)),
        xaxis2 = list(title = '', showtick = FALSE, title_standoff = 20, range = c(0,20)),
        yaxis = list(title = 'Age group', categoryorder = "array", categoryarray = age_order, title_standoff = 20),
        title = paste(filtered_data$Time[1],"population prospect", "for", filtered_data$Location[1], "-", filtered_data$Ano[1], "revision"),
        bargap = 0.1,
        bargroupgap = 0,
        showlegend = showlegend  # Control the legend display
      ) %>%
      config(displayModeBar = FALSE)
    
    p
  }
  
  output$pyramidPlot1 <- renderPlotly({
    filtered_data <- data %>%
      filter(Location == input$location1,
             Time == input$time,
             Ano == input$ano)
    
    renderPyramidPlot(filtered_data)
  })
  
  output$pyramidPlot2 <- renderPlotly({
    filtered_data <- data %>%
      filter(Location == input$location2,
             Time == input$time,
             Ano == input$ano)
    
    renderPyramidPlot(filtered_data)
  })
}

shinyApp(ui = ui, server = server)
