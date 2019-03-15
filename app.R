library(shiny)
library(dplyr)
library(ggplot2)

#Prep the data (Run this section before running UI and Server section----

#UI----
ui<-fluidPage(
  titlePanel("NLP data by Country"),
  sidebarLayout(
    sidebarPanel(
        selectInput("country", "Select a Country", choices = unique(nlp$CountryCombined)),
        #download button
        downloadButton("downloadData", "Download"),
        br(),
        br(),
        img(src = "logo.png", height = 100, width = 100)
    ),
    mainPanel (
      plotOutput("BarPlot"),
      tableOutput("table")
    )
  )
)


#Server----
server<-function(input,output){
  #Reactive value for selected dataset
          datasetInput<-reactive({
            req(input$country)
            filter(nlp, CountryCombined %in% input$country)
           })
  #Fill in the spot we created for a plot
           output$BarPlot <- renderPlot({
             # Render a barplot
             ggplot(datasetInput(), aes(x=AdmPublishWebNoPassword)) + 
               geom_bar(stat="count")+ theme_bw()+ggtitle(input$country)+
               labs(y="Number of Records", x = "Permission")
             
           })         
  #table of selected dataset
          output$table<- renderTable({
            datasetInput()
          })
          
          #Downloadable csv of selected dataset
          output$downloadData <- downloadHandler(
            filename = function() {
              paste(input$country, ".csv", sep = "")
            },
            content = function(file) {
              write.csv(datasetInput(), file, row.names = FALSE)
            }
          )

}
# Run the app ----
shinyApp(ui = ui, server = server)
