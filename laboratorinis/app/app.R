library(shiny)
library(shinythemes)
library(tidyverse)

ui <- fluidPage(
  theme = shinytheme("spacelab"),
  titlePanel("Nuosavo arba nuomojamo nekilnojamojo turto nuoma ir eksploatavimas"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput("imones_kodas", "Įmonės kodas", choices = NULL, selected = NULL)),
    mainPanel(
      tabPanel("plot", plotOutput("plot"))
    )
  )
)

server <- function(input, output, session) {
  data <- read_csv("https://github.com/kestutisd/KTU-duomenu-vizualizacija/raw/main/laboratorinis/data/lab_sodra.csv")
  filtered <- data %>% 
    filter(ecoActCode == 682000)
  updateSelectizeInput(session, "imones_kodas", choices = filtered$name, server = TRUE)
    
  output$plot <- renderPlot(
    filtered %>% 
      filter(mean(avgWage, na.rm = T) > 0) %>% 
      filter(name == input$imones_kodas) %>% 
      mutate("month" = as.numeric(substr(month, 5, 6))) %>% 
      ggplot(aes(month, avgWage)) +
      geom_line(aes(col = name), linewidth = 1) +
      scale_x_continuous(breaks = 1:12) +
      theme(legend.position = "NULL")
  )
}

shinyApp(ui, server)
