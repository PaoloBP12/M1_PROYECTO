library(shiny)
library(dplyr)
library(sqldf)
library(RMySQL)
library(plotly)
source("www/presentacion.R")
source("www/recoleccion.R")
source("www/transformacion.R")
source("www/modelo.R")
source("www/exploracion.R")



ui <- fluidPage(
  ui <- navbarPage(title = "Proyecto  de Ciencia de Datos",
                   tabPanel("Presentacion",presentacion),
                   tabPanel( "Recoleccion",recoleccion),
                   tabPanel("Transformacion",transformacion),
                   tabPanel("Visualizacion",exploracion),
                   tabPanel("Modelo",modelo),
                   tabPanel("Interpretacion")
                   
  )
  
)


server <- function(input, output) {
  
  
  #####################################################Recoleccion###############################################
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
  })
  ##################################################### Fin Recoleccion ###############################################
  ##################################################### transformacion ###############################################
  dt<-read.csv("dataset/orden_compra.csv")
  output$resDplyr<-renderDataTable({
    selector<-input$transDplyr
    if(is.null(selector)){
      return (NULL)
    }
    if(selector=="1"){
      return (distinct(dt, PROVEEDOR, ENTIDAD))
    }
    if(selector=="2"){
      return (select (dt,ORDEN_ELECTRÓNICA,ESTADO_ORDEN_ELECTRÓNICA,TOTAL))
    }
    if(selector=="3"){
      return (select(dt, RUC_PROVEEDOR, PROVEEDOR, ORDEN_ELECTRÓNICA,SUB_TOTAL, IGV, TOTAL))
    }
    if(selector=="4"){
      return (filter(dt, ESTADO_ORDEN_ELECTRÓNICA=='ACEPTADA' & TIPO_PROCEDIMIENTO=='Compra ordinaria'))
    }
    if(selector=="5"){
      return (arrange(dt, desc(RUC_PROVEEDOR), PROVEEDOR)%>%select(-FECHA_PROCESO, -(RUC_ENTIDAD:TIPO_PROCEDIMIENTO), -(ORDEN_ELECTRÓNICA_GENERADA:ACUERDO_MARCO)))
    }
    if(selector=="6"){
      return (dt%>%filter(ESTADO_ORDEN_ELECTRÓNICA=='ACEPTADA'))
    }
    if(selector=="7"){
      return (dt%>%filter(TIPO_PROCEDIMIENTO=='Gran Compra' & ENTIDAD=='BANCO CENTRAL DE RESERVA DEL PERÚ'))
    }
    if(selector=="8"){
      return (dt%>%group_by(ENTIDAD)%>%select(dt, RUC_ENTIDAD, PROVEEDOR)%>%summarise(media.total = mean(TOTAL, na.rm = TRUE)) %>%
                filter (media.total > 123 & media.total < 680))
    }
    if(selector=="9"){
      return (grupos<-group_by(dt, ))
    }
    if(selector=="10"){
      return (grupos<-group_by(dt, ))
    }
    
    
  })
  output$resSQLDF<-renderDataTable({
    selector<-input$transSQLDF
    if(is.null(selector)){
      return (NULL)
    }
    if(selector=="1"){
      query1<-sqldf('select PROVEEDOR, RUC_PROVEEDOR, ACUERDO_MARCO from dt where ESTADO_ORDEN_ELECTRÓNICA = "PAGADA" group by proveedor;', drv="SQLite")
      return (query1)
    }
    if(selector=="2"){
      query2<-sqldf('select PROVEEDOR, ACUERDO_MARCO, ESTADO_ORDEN_ELECTRÓNICA, count(ESTADO_ORDEN_ELECTRÓNICA) from dt where total > 100 group by PROVEEDOR having ESTADO_ORDEN_ELECTRÓNICA = "PAGADA";', drv="SQLite")
      return (query2)
    }
    if(selector=="3"){
      query3<-sqldf('select PROVEEDOR, CANTIDAD_PRODUCTOS_PEDIDOS, ESTADO_ORDEN_ELECTRÓNICA, TOTAL from dt where CANTIDAD_PRODUCTOS_PEDIDOS between 10 and 20 group by PROVEEDOR having ESTADO_ORDEN_ELECTRÓNICA = "PAGADA" order by TOTAL desc;', drv="SQLite")
      return (query3)
    }
    if(selector=="4"){
      query4<-sqldf('select PROVEEDOR, ACUERDO_MARCO, CANTIDAD_PRODUCTOS_PEDIDOS, count(ESTADO_ORDEN_ELECTRÓNICA) from dt where IGV > 21 group by PROVEEDOR order by CANTIDAD_PRODUCTOS_PEDIDOS desc;', drv="SQLite")
      return (query4)
    }
    if(selector=="5"){
      query5<-sqldf('select PROVEEDOR, ENTIDAD, ESTADO_ORDEN_ELECTRÓNICA, IGV from dt where CANTIDAD_PRODUCTOS_PEDIDOS < 15 group by RUC_ENTIDAD having ESTADO_ORDEN_ELECTRÓNICA <> "PAGADA" order by CANTIDAD_PRODUCTOS_PEDIDOS desc;', drv="SQLite")
      return (query5)
    }
    if(selector=="6"){
      query6<-sqldf('select PROVEEDOR, RUC_PROVEEDOR, ACUERDO_MARCO from dt where ESTADO_ORDEN_ELECTRÓNICA = "PAGADA" group by proveedor;', drv="SQLite")
      return (query6)
    }
    if(selector=="7"){
      query7<-sqldf('select PROVEEDOR, ACUERDO_MARCO, ESTADO_ORDEN_ELECTRÓNICA, count(ESTADO_ORDEN_ELECTRÓNICA) from dt where total > 100 group by PROVEEDOR having ESTADO_ORDEN_ELECTRÓNICA = "PAGADA";', drv="SQLite")
      return (query7)
    }
    if(selector=="8"){
      query8<-sqldf('select PROVEEDOR, CANTIDAD_PRODUCTOS_PEDIDOS, ESTADO_ORDEN_ELECTRÓNICA, TOTAL from dt where CANTIDAD_PRODUCTOS_PEDIDOS between 10 and 20 group by PROVEEDOR having ESTADO_ORDEN_ELECTRÓNICA = "PAGADA" order by TOTAL desc;', drv="SQLite")
      return (query8)
    }
    if(selector=="9"){
      query9<-sqldf('select PROVEEDOR, ACUERDO_MARCO, CANTIDAD_PRODUCTOS_PEDIDOS, count(ESTADO_ORDEN_ELECTRÓNICA) from dt where IGV > 21 group by PROVEEDOR order by CANTIDAD_PRODUCTOS_PEDIDOS desc;', drv="SQLite")
      return (query9)
    }
    if(selector=="10"){
      query10<-sqldf('select PROVEEDOR, ENTIDAD, ESTADO_ORDEN_ELECTRÓNICA, IGV from dt where CANTIDAD_PRODUCTOS_PEDIDOS < 15 group by RUC_ENTIDAD having ESTADO_ORDEN_ELECTRÓNICA <> "PAGADA" order by CANTIDAD_PRODUCTOS_PEDIDOS desc;', drv="SQLite")
      return (query10)
    }
    
  })
  ##################################################### exploracion ####################################################################
  output$resGgplot2<-renderPlot({
    selector<-input$expGgplot
    if(is.null(selector)){
      return (NULL)
    }
    if(selector=="1"){
      gr1<- ggplot(dt,aes(x=C1))+geom_bar()
      return (gr1)
    }
    if(selector=="2"){
 
    }
    if(selector=="3"){
      
    }
    
  })
  ######################################################################################################################################
  output$resPlotly<-renderPlotly({
    selector<-input$expPlotly
    if(is.null(selector)){
      return (NULL)
    }
    if(selector=="1"){
      trace_0 <- rnorm(100, mean = 5)
      trace_1 <- rnorm(100, mean = 0)
      trace_2 <- rnorm(100, mean = -5)
      x <- c(1:100)
      
      data <- data.frame(x, trace_0, trace_1, trace_2)
      
      fig <- plot_ly(data, x = ~x)
      fig <- fig %>% add_trace(y = ~trace_0, name = 'trace 0',mode = 'lines')
      fig <- fig %>% add_trace(y = ~trace_1, name = 'trace 1', mode = 'lines+markers')
      fig <- fig %>% add_trace(y = ~trace_2, name = 'trace 2', mode = 'markers')
      
      return (fig)
      
    }
    if(selector=="2"){
      fig <- plot_ly(data = dt, y =  ~TIPO_PROCEDIMIENTO, x = ~TOTAL, color = 'rgba(152, 0, 0, .8)', colors = "Set1")
      
      return (fig)
    }
    if(selector=="3"){
      fig <- plot_ly(data = dt, y=  ~TOTAL, x = ~FECHA_PROCESO, color = ~FECHA_FORMALIZACIÓN, colors = "Set1")
      
      return (fig)
    }
    if(selector=="4"){
      fig <- plot_ly(data = dt, y =  ~TOTAL, x = ~FECHA_FORMALIZACIÓN, color = ~FECHA_FORMALIZACIÓN, colors = "Set1")
      
      return (fig)
    }
    if(selector=="3"){
      d <- dt[sample(nrow(diamonds), 1000), ]
      
      fig <- plot_ly(
        d, x = ~carat, y = ~price,
        color = ~carat, size = ~carat
      )
      
      fig
    }
   
    
    
  
  })
  
  
}


shinyApp(ui = ui, server = server)


