#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(googlesheets4)
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           tableOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderTable({

      
      # Baca dengan spesifikasi tipe kolom
      lapor_spt <- read_sheet(
        "https://docs.google.com/spreadsheets/d/1CI6PxR_Su_CdTqnESs5AxzhqNXmOJoe-AOwyvGECvKs/edit?gid=759413326#gid=759413326",
        col_types = "c"  # 'c' berarti character untuk semua kolom, atau sesuaikan
      )
      lapor_spt
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
