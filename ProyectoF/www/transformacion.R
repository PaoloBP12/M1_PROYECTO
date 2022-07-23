transformacion<-{
  navlistPanel(
    tabPanel("Consultas en dplyr", 
             h4("DPLYR"),selectInput("transDplyr","Consultas exploratorias 1",choices = c("Proveedores por entidad"='1',"Estado de la orden"='2',"Datos de orden de compra"='3',"Ordenes aceptadas con procedimiento ordinario"='4',"Consulta 5"='5',"Consulta 6"='6',"Consulta 7"='7',"Consulta 8"='8',"Consulta 9"='9',"Consulta 10"='10')),
             dataTableOutput("resDplyr")
             
             
    ),
    tabPanel("Consultas en sqldf", 
             h4("SQLDF"),selectInput("transSQLDF","Consultas exploratorias 2",choices = c("Acuerdos por proveedor"='1',"Consulta 2"='2',"Cantidad dee productos por porveedor"='3',"Consulta 4"='4',"Consulta 5"='5',"Consulta 6"='6',"Consulta 7"='7',"Consulta 8"='8',"Consulta 9"='9',"Consulta 10"='10')),
             dataTableOutput("resSQLDF")
             
    )
  )
}
