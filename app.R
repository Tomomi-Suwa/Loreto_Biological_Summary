library(shiny)
library(dplyr)
library(ggplot2)

#Prep the data (Run this section before running UI and Server section----
df<-read.csv("data/LoretoPaperBioSummary.csv")

#UI----
ui<-fluidPage(
  titlePanel("Loreto Biological Inventories"),
  sidebarLayout(
    sidebarPanel(
      #select box
      selectInput("riInput", "Select an Inventory", choices = unique(df$RI)),
      #radio button
      radioButtons("YInput", label = h3("Y-axis"),
                   c("# Species" = "NoSpecies", "# Specimen" = "NoSpecimen", "# Occurance" = "NoOccurance"), 
                   selected = "NoSpecies"),
     #slider bar
     sliderInput("slider", label = h3("Select Y-axis Maximum Scale"), min = 0, 
                 max = 2000, value = 15000),
       #download button
      downloadButton("downloadData", "Download"),
      br(),
      br(),
      img(src = "logo.png", height = 100, width = 100)
    ),
    mainPanel (
      plotOutput("BarPlot"),
      plotOutput("PieChart"),
      tableOutput("table")
    )
  )
)


#Server----
server<-function(input,output){
  #Reactive value for selected dataset
  datasetInput<-reactive({
    df %>%
      filter(RI %in% input$riInput) %>%
      select(RI, Category, input$YInput)%>%
      mutate(Percent = 100 * !! rlang::sym (input$YInput)/sum(!! rlang::sym (input$YInput), na.rm=TRUE ))%>%
      as.data.frame()
  })
  
  #Render bar plot
  output$BarPlot <- renderPlot({
    # print it out to the console.
    print(datasetInput()) 
      datasetInput() %>% ggplot(aes_string(x="Category", y=input$YInput, fill="Category")) + 
      geom_bar(stat="identity")+ theme_bw()+ggtitle(paste(input$YInput,"of Rapid Inventory",input$riInput))+
      labs(y="Number of Records", x ="Category" )+coord_cartesian(ylim=c(0,input$slider))
  }) 
  #Pie Chart - broken. something is wrong with "y=input$YInput" part
  output$PieChart<-renderPlot({
    datasetInput() %>% 
      ggplot(aes(x="", y=Percent, fill=Category))+geom_bar(width = 1, stat = "identity")+ coord_polar("y", start=0)
  })
  
   #table of selected dataset
  output$table<- renderTable({
    datasetInput()
  })
  
  #Downloadable csv of selected dataset
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("RI_",input$riInput, "_", input$YInput, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    })
  
}
# Run the app ----
shinyApp(ui = ui, server = server)