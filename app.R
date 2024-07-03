library(shiny)
library(dplyr)
library(shinyWidgets)
library(plotly)

data <- read.csv("Base_Pop_Latam.csv")

data$Time <- as.integer(data$Time)
data$Ano <- as.integer(data$Ano)

age_order <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", 
               "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", 
               "75-79", "80-84", "85-89", "90-94", "95-99", "100+")

data$AgeGrp <- factor(data$AgeGrp, levels = age_order)

anos_disponiveis <- sort(unique(data$Ano))
anos_time <- sort(unique(data$Time))

ui <- fluidPage(
  titlePanel("LatAm Population (UN)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("location", "Select country:", choices = unique(data$Location)), 
      sliderTextInput("time", "Select reference year:", 
                      choices = anos_time, selected = min(anos_time), animate = animationOptions(interval = 300)),
      sliderTextInput("ano", "Select year of revisions updates:", 
                      choices = anos_disponiveis, selected = min(anos_disponiveis), animate = animationOptions(interval = 300))
    ),
    mainPanel(
      plotlyOutput("pyramidPlot")
    )
  )
)

options(repos = c(CRAN = "https://cran.rstudio.com/"))

library(shiny)
library(dplyr)
library(shinyWidgets)
library(plotly)

server <- function(input, output) {
  output$pyramidPlot <- renderPlotly({
    filtered_data <- data %>%
      filter(Location == input$location,
             Time == input$time,
             Ano == input$ano)
    
    female_data <- filtered_data %>%
      mutate(PopFemale = -PopFemale)  # Invert the female population for the pyramid
    
    male_plot <- plot_ly(
      data = filtered_data, 
      x = ~PopMale, 
      y = ~AgeGrp, 
      type = 'bar', 
      orientation = 'h', 
      name = 'Male',
      hoverinfo = 'text',
      hovertext = ~paste('Male population:', formatC(PopMale, format = "f", big.mark = ",", digits = 0), 'millions', 
                         '<br>Age group:', AgeGrp, 'years'),
      marker = list(color = 'lightskyblue')
    )
    
    female_plot <- plot_ly(
      data = female_data, 
      x = ~PopFemale, 
      y = ~AgeGrp, 
      type = 'bar', 
      orientation = 'h', 
      name = 'Female',
      hoverinfo = 'text',
      hovertext = ~paste('Female population:', formatC(-PopFemale, format = "f", big.mark = ",", digits = 0), 'millions', 
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
        xaxis = list(title = 'Female Population (millions)'),
        xaxis2 = list(title = 'Male Population (millions)'),
        yaxis = list(title = 'Age group', categoryorder = "array", categoryarray = age_order),
        title = paste("Age pyramid in", input$location, "for", input$time, "according to the", input$ano, "revision"),
        bargap = 0.1,
        bargroupgap = 0.15 
      ) %>%
      config(displayModeBar = FALSE)
    
    p
  })
}

shinyApp(ui = ui, server = server)

