shinyNetwork <- function(exb){
  shiny::shinyApp(
    ui= fluidPage(
      fluidRow(dataTableOutput("ExbData")),
      fluidRow(selectInput("vars", "Variablen auswählen", choices = names(exb), multiple = TRUE)),
      fluidRow(actionButton("plotData", "Auswahl plotten")),
      fluidRow(plotOutput("Netzwerk")),
      # #fluidRow(textOutput("Variablen", "Ausgewählte Variablen")),
      # fluidRow(dataTableOutput("clusterData")),
      # fluidRow(textInput("filename", "Enter filename"),
      #          downloadButton("Dcluster", "Download"))

    ),
    server = function(input, output){
      output$ExbData <-  renderDataTable({datatable(exb, filter= "top",selection = list(target = 'row', selected= c(seq(1:nrow(exb)))),options = list(lengthChange = TRUE, autoWidth= TRUE, scrollX= TRUE))})
      NetworkPlot <- eventReactive(input$plotData,{
                 data <- exb[input$ExbData_rows_selected,]
                 com_clust(data, variables = input$vars)})
      output$Netzwerk <- renderPlot({plot(NetworkPlot()[[2]],NetworkPlot()[[3]],vertex.size=10, vertex.label.font=20, family="serif", edge.width=E(NetworkPlot()[[3]])$weight, sub= stringr::str_c("Modularity=",modularity(NetworkPlot()[[2]])))})
    }
  )
}
#
# shinyNetwork <- function(exb){
#   shinyApp(
#     ui = dashboardPage(
#       dashboardHeader(title =  "ExmaraldaR"),
#       dashboardSidebar(
#         sidebarMenu(
#           menuItem("Daten importieren und Analyse", tabName = "daten"),
#         )
#       ),
#       dashboardBody(
#         tabItems(
#           tabItem(tabName = "daten",
#                   fluidRow(dataTableOutput("ExbData")),
#                   fluidRow(selectInput("vars", "Variablen auswählen", choices = names(exb), multiple = TRUE)),
#                   fluidRow(actionButton("plotData", "Auswahl plotten")),
#                   fluidRow(plotOutput("Netzwerk")),
#                   #fluidRow(textOutput("Variablen", "Ausgewählte Variablen")),
#                   fluidRow(dataTableOutput("clusterData")),
#                   fluidRow(textInput("filename", "Enter filename"),
#                            downloadButton("Dcluster", "Download"))
#
#           )
#         )
#       )
#     ),
#
#     server <- function(input, output){
#       output$ExbData <-  renderDataTable({datatable(exb, filter= "top",selection = list(target = 'row', selected= c(seq(1:nrow(exb)))),options = list(lengthChange = TRUE, autoWidth= TRUE, scrollX= TRUE))})
#       output$download <- downloadHandler(filename = function(){paste(sub('\\.exb$', '', input$file$name),".csv", sep="")}, content = function(file){write.csv(exb, file, row.names = FALSE)})
#       output$test <- renderPrint(str(input$file))
#       NetworkPlot <- eventReactive(input$plotData,{
#         data <- exb[input$ExbData_rows_selected,]
#         com_clust(data, variables = input$vars)})
#       output$Netzwerk <- renderPlot({plot(NetworkPlot()[[2]],NetworkPlot()[[3]],vertex.size=10, vertex.label.font=20, family="serif", edge.width=E(NetworkPlot()[[3]])$weight, sub= stringr::str_c("Modularity=",modularity(NetworkPlot()[[2]])))})
#       output$clusterData <- renderDataTable({datatable(NetworkPlot()[[1]],options = list(lengthChange = TRUE, autoWidth= TRUE, scrollX= TRUE))})
#       output$Dcluster <- downloadHandler(filename = function(){paste(input$filename,".csv", sep="")}, content = function(file){write.csv(NetworkPlot()[[1]], file, row.names = FALSE)})
#
#     }
#
#   )
#
# }
