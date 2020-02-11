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
    selectInput(  inputId = "SQLserver",   label = "Server",   choices = sort(ChooseDatabase$ServerName), selected = 1),
    uiOutput("Database"),
    uiOutput("Table"),
    textAreaInput(inputId = "Description", label = "Database descripption"),
    actionButton( inputId = "Add",         label = "Add",    icon("plus")),
    actionButton( inputId = "Delete",      label = "Delete", icon("trash-alt")),
    actionButton( inputId = "Details",     label = "Details")
  ),
  dashboardBody(
    tabsetPanel(
    tabPanel( title = "Tables List",
    DT::dataTableOutput(outputId = "table")
    ),
    tabPanel( title = "Details",
    DT::dataTableOutput(outputId = "coltable"))
    )
  )
)

server <- function(input, output){
  values <- reactiveValues()
  output$Database <- renderUI({selectInput(inputId = "Database",  label = "Database", choices = sort(ChooseDatabase$DatabasesName[ChooseDatabase$ServerName == input$SQLserver]), selected = 1)})
  TableSelect <- reactive({
    SQLcon12 <- odbcDriverConnect('driver={SQL Server}; server=s-kv-center-s12')
    SQLcon64 <- odbcDriverConnect('driver={SQL Server}; server=s-kv-center-s64')
     if(input$SQLserver == "s-kv-center-s12"){
     tempdf <-  sqlQuery(SQLcon12, paste("SELECT TABLE_NAME FROM ",input$Database,".INFORMATION_SCHEMA.TABLES;", sep = ""))
    }else{
     tempdf <-  sqlQuery(SQLcon64, paste("SELECT TABLE_NAME FROM ",input$Database,".INFORMATION_SCHEMA.TABLES;", sep = ""))
      }
    odbcClose(SQLcon12)
    odbcClose(SQLcon64)
    tempdf
    })
  output$Table <- renderUI({selectInput(inputId = "Table", label = "Table or View name", choices = TableSelect())})
  
  values$DB <- read.csv(file = "Databases.csv")
  observeEvent(input$Add, {
    New <-data.frame(ServerName = input$SQLserver, DatabasesName = input$Database, TableName = input$Table, Description = input$Description)
    values$DB <- rbind(values$DB, New)
    ServerDatabase <<- values$DB
    write.csv(ServerDatabase, file = "Databases.csv", row.names = F)
  })
  
  output$table <- DT::renderDataTable({
    DT::datatable(values$DB, options = list(paging = F), editable = list(target = "cell", disable = list(columns = c(0:3))), selection = 'single', class = 'cell-border stripe', style = 'bootstrap' )
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
  observeEvent(input$Delete, {
    req(input$table_rows_selected)
    i <- input$table_rows_selected
    if(!is.null(input$input$table_rows_selected)){
        values$DB <- values$DB[-as.numeric(input$table_rows_selected),]
    }
    values$DB <- values$DB[-i,]
    ServerDatabase <<- values$DB
    write.csv(ServerDatabase, file = "Databases.csv", row.names = F)
  })
  observeEvent(input$Details,{
    req(input$table_rows_selected)
    i <- input$table_rows_selected
    SQLcon <- odbcDriverConnect(paste('driver={SQL Server}; server=',values$DB[i,1],'', sep = ""))
    collist <- sqlQuery(SQLcon, paste("Select COLUMN_NAME as ColumnName FROM ",values$DB[i,2],".INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME ='",values$DB[i,3],"'", sep = ""))
    DetailsTable <- data.frame(Column = collist, Description = NA)
    output$coltable <- DT::renderDataTable({
      DT::datatable(DetailsTable, options = list(paging = F), selection = 'single', class = 'cell-border stripe', style = 'bootstrap',  editable = list(target = "cell", disable = list(columns = c(0:1))) )
    })
  })
}
 
shinyApp(ui, server)