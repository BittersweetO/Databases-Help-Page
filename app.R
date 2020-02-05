library(shiny)
library(shinydashboard)
library(DT)
library(RODBC)
#reading server and databases names and creating dataframe of it
SQLcon12 <- odbcDriverConnect('driver={SQL Server}; server=s-kv-center-s12')
SQLcon64 <- odbcDriverConnect('driver={SQL Server}; server=s-kv-center-s64')
Databases12 <- sqlQuery(SQLcon12, 'SELECT name AS DatabasesName FROM sys.databases d WHERE d.database_id > 4 ORDER BY name')
Databases64 <- sqlQuery(SQLcon64, 'SELECT name AS DatabasesName FROM sys.databases d WHERE d.database_id > 4 ORDER BY name')
odbcClose(SQLcon12)
odbcClose(SQLcon64)

ChooseDatabase <-data.frame(
  ServerName = c(rep("s-kv-center-s12", nrow(Databases12)), rep("s-kv-center-s64", nrow(Databases64))),
  DatabaseName = rbind(Databases12, Databases64)
)

ui <- dashboardPage(
  dashboardHeader(
    title = "Server Databases",
    dropdownMenu(
      type = "notifications", 
      icon = icon("question-circle"),
      badgeStatus = NULL,
      headerText = "See also:")
  ),
  dashboardSidebar(
    selectInput(inputId = "SQLserver", label = "Server",   choices = sort(ChooseDatabase$ServerName), selected = 1),
    uiOutput("Database"),
    textInput(inputId = "Table",       label = "Table or View name"),
    textAreaInput(inputId = "Columns", label = "Columns description"),
    textInput(inputId = "Description", label = "Database descripption"),
    actionButton(inputId = "Add",      label = "Add")
  ),
  dashboardBody(
    DT::dataTableOutput(outputId = "table")
  )
)

server <- function(input, output){
  output$Database <- renderUI({selectInput(inputId = "Database",  label = "Database", choices = sort(ChooseDatabase$DatabasesName[ChooseDatabase$ServerName == input$SQLserver]), selected = 1)})
  values <- reactiveValues()
  values$DB <- read.csv(file = "Databases.csv")
  observeEvent(input$Add, {
    New <-data.frame(ServerName = input$SQLserver, DatabasesName = input$Database, Name = input$Table , Columns = input$Columns, Description = input$Description)
    values$DB <- rbind(values$DB, New)
    ServerDatabase <<- values$DB
    write.csv(ServerDatabase, file = "Databases.csv", row.names = F)
  })
  
  output$table <- DT::renderDataTable({
    DT::datatable(values$DB, options = list(paging = F), editable = T)
  })
  proxy <- dataTableProxy(outputId = "table")
  observeEvent(input$table_cell_edit, {
    info <- input$table_cell_edit
    i <- info$row
    j <- info$col
    v <- info$value
    values$DB[i,j] <<- DT::coerceValue(v, values$DB[i,j])
    replaceData(proxy, values$DB, resetPaging = F, rownames = F)
    ServerDatabase <<- values$DB
    write.csv(ServerDatabase, file = "Databases.csv", row.names = F)
  })
}

shinyApp(ui, server)