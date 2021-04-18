library(dplyr)
library(xlsx)
library(openxlsx)
library(shiny)
library(readxl)
library(DT)
library(shinydashboard)
library(shinythemes)
library(shinyalert)
library(shinyWidgets)


excluidas <- seq(as.Date('2021-04-08'), as.Date('2021-04-29'), by = "day")

    
ui= fluidPage(
  tags$style("#add3 {background-color: #ABB2B9;
               font-weight: bold;
               vertical-align- middle;
               
               
               }"),
  
  
  
  
  tags$footer("Atención telefónica
600 6000 166, Atención de público
Santo Domingo 566 1º piso
9:00 a 14:00 horas", align = "center", style = "
              position:fixed;
              bottom:0;
              width:100%;
              height:50px;   /* Height of the footer */
              color: white;
              padding: 10px;
              background-color: #152131;
              z-index: 1000;
              font-size: 13px;
                "),
  
  
  
    
    tags$style("#add {background-color: #ABB2B9;
               font-weight: bold;
               
               
               }"),
    
    #tags$head(tags$script("alert('Hello!');")),
    
    tags$div(img(src = "https://www.proactivanet.com/images/servel_Chile.jpg",height = '60px', width = '60px')),
    
        
        tags$head(
            
           
            tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Yusei+Magic&display=swap');
     body {
    background-image: linear-gradient(to bottom right, white, rgba(255,0,0,0));
    font-family: arial;
    color: #000000;
    
     }
  
  #text1{color: red;
                                 font-size: 15px;
                                 font-style: italic;
                                 
                                 
                                 }
  
  
  
      h2 {
        font-family: 'Yusei Magic', sans-serif;
      }
      .shiny-input-container {
        color: #474747;
      }"))
        ),
        titlePanel("Calculadora de Fechas de Aportes a Candidaturas Electorales(aportes a terceros)"),
        h4("Elecciones Municipales, Gobernadores Regionales y Convencionales Constituyentes
           Mayo 2021"),
        hr(),
        
        sidebarLayout(
            
            sidebarPanel(
                
                dateInput("date1", label = "Indique fecha de realización del aporte", value = Sys.Date(), format = "yyyy-mm-dd",language = 'es',min = "2021-04-01",
                          max = "2021-05-16", datesdisabled = excluidas),
                br(),
                
                selectInput("select", "Medio de pago por el cual se realizó el aporte",
                            choices = c("Presencial Banco Estado - Ventanilla","Presencial Banco Estado - Cheque","Transbank - Tarjeta de Débito", "Transbank - Tarjeta de Crédito")),
                
                br(),
                #checkboxInput("plot", "El Aporte se realizó antes de las 14:00 hrs", TRUE),
                materialSwitch(
                    inputId = "check1",
                    label = "El Aporte se realizó antes de las 14:00 hrs", 
                    value = TRUE,
                    status = "success"
                ),
                
                br(),
                dateInput("date2", label = "Indique fecha de aprobación por parte del Candidato", value = Sys.Date()+1, format = "yyyy-mm-dd",language = 'es',min = "2021-04-01",
                          max = "2021-05-16"),
                br(),
                
                
                actionButton("add", "Calcula fechas",icon = icon("calculator")),
                
                #br(),br(),
                #HTML('<p>Consulta datos del IRE, más los aportes por candidato 2021 <a href="https://rpubs.com/Servel_2021v2/745354">Aquí</a></p>')
                
               
                
            ),
            
            
            
            mainPanel(
               
                h4(strong("Instrucciones: ")),
                
                p(em(" Para calcular la fecha de recaudación(abono a cuenta SERVEL), debe ingresar fecha de realización del aporte, modalidad de pago y si el pago se efectuó antes o después de horario bancario. ")),    
                
                p(em(" Si usted desea calcular la fecha de abono a la cuenta del candidato, debe ingresar la información solicitada anteriormente, más la fecha de aceptación por parte del candidato.")),    
                br(),br(),
                
                DT::DTOutput('tbl'),
                
                br(),
                
                
               
                uiOutput("text1")
                
              
               
                
                
                
            )
            
        ) 
    )




###############################################################
    
    server=function(input, output, session){
        
      
        
        fechas_2021 <- read_excel("date_2021.xlsx")
        
        fechas_2021$Fecha = as.Date(as.POSIXct(fechas_2021$Fecha, 'UTC'))
        
        
        
        cal_data<- read_excel("data_cal.xlsx")
        
        cal_data$Fechas = as.Date(as.POSIXct(cal_data$Fechas, 'UTC'))
        
        cal_data$Habil_Siguiente = as.Date(as.POSIXct(cal_data$Habil_Siguiente, 'UTC'))
        
        cal_data$Habil_mas_1 = as.Date(as.POSIXct(cal_data$Habil_mas_1, 'UTC'))
        
        
        observeEvent( input$add,{
            
            fecha1 <- as.Date(input$date1,format = "%Y-%m-%d")
            
            fecha2 <- as.Date(input$date2,format = "%Y-%m-%d")
            
            diff_fechas <- fecha2 - fecha1
            
            num_diff <- as.numeric(diff_fechas)
            
            dataset_2021_d1 <- reactive({
                fechas_2021 %>% filter(Fecha == as.Date(input$date1,format = "%Y-%m-%d"))
            })
            
            dataset_2021_d2 <- reactive({
                fechas_2021 %>% filter(Fecha == as.Date(input$date2,format = "%Y-%m-%d"))
            })
            
            conta_rows_d1 <- nrow(dataset_2021_d1())
            
            conta_rows_d2 <- nrow(dataset_2021_d2())
            
            
        if(conta_rows_d1 > 0 & conta_rows_d2 > 0){
                
            
            
            
    #################validación de fechas 2
            
            
            if(num_diff>=0){
                
                #############dependientes del checkbox de la hora
               if(input$check1) {
                   
            
                
                if(input$select=="Transbank - Tarjeta de Crédito" | input$select =="Presencial Banco Estado - Cheque"){
                    
                    datasetInput <- reactive({
                        cal_data %>% filter(Fechas == as.Date(input$date1,format = "%Y-%m-%d")) %>%
                            select(Habil_mas_1)
                        
                    })
                    
                    L2 <- datasetInput() 
                    
                    
                    dat <- L2[[1,1]]
                    
                    dat <- as.Date(dat, format = "%Y-%m-%d")
                    
                    
                    
                    
                }
                else{
                    datasetInput <- reactive({
                        cal_data %>% filter(Fechas == as.Date(input$date1,format = "%Y-%m-%d")) %>%
                            select(Habil_Siguiente)
                    })
                    
                    L2 <- datasetInput() 
                    
                    
                    dat <- L2[[1,1]]
                    
                    dat <- as.Date(dat, format = "%Y-%m-%d")
                    
                    
                    
                    
                }
                   
                   
                   
                   
             }else{
                 
                 if(input$select=="Transbank - Tarjeta de Crédito" | input$select =="Presencial Banco Estado - Cheque"){
                     
                     datasetInput <- reactive({
                         cal_data %>% filter(Fechas == as.Date(input$date1,format = "%Y-%m-%d")) %>%
                             select(Habil_mas_1)
                         
                     })
                     
                     L2 <- datasetInput() 
                     
                     
                     da <- L2[[1,1]]
                     
                     da <- as.Date(da, format = "%Y-%m-%d")
                     
                     
                     jj <-cal_data %>% filter(Fechas == as.Date(da,format = "%Y-%m-%d")) %>%
                         select(Habil_Siguiente)
                     
                     p2 <-jj
                     
                     
                     dat <- p2[[1,1]]
                     
                     dat <- as.Date(dat, format = "%Y-%m-%d")
                     
                     
                     
                     
                 }
                 else{
                     datasetInput <- reactive({
                         cal_data %>% filter(Fechas == as.Date(input$date1,format = "%Y-%m-%d")) %>%
                             select(Habil_Siguiente)
                     })
                     
                     L2 <- datasetInput() 
                     
                     
                     da <- L2[[1,1]]
                     
                     da <- as.Date(da, format = "%Y-%m-%d")
                     
                     jj <-cal_data %>% filter(Fechas == as.Date(da,format = "%Y-%m-%d")) %>%
                         select(Habil_Siguiente)
                     
                     p2 <-jj
                     
                     
                     dat <- p2[[1,1]]
                     
                     dat <- as.Date(dat, format = "%Y-%m-%d")
                     
                     
                     
                     
                 }
                 
                    
                    
                
                
                
            }
#####################calculo de fecha de abono 
                if(fecha2 > dat){
                    
                    mm <- cal_data %>% filter(Fechas == as.Date(fecha2,format = "%Y-%m-%d")) %>%
                        select(Habil_Siguiente)
                    
                    ss <- mm
                    
                    datodf <- mm[[1,1]]
                    
                    datodf <- as.Date(datodf, format = "%Y-%m-%d")
                    
                    
                }  
                else{
                    
                    mm <- cal_data %>% filter(Fechas == as.Date(dat,format = "%Y-%m-%d")) %>%
                        select(Habil_Siguiente) 
                    
                    
                    ss <- mm
                    
                    datodf <- mm[[1,1]]
                    
                    datodf <- as.Date(datodf, format = "%Y-%m-%d")
                    
                    
                }
                
###############################info que sale por pantalla
              f_f = as.Date(dat)
              f_f = format(f_f, "%d/%m/%Y")
              
              
              f_f2 = as.Date(datodf)
              f_f2 = format(f_f2, "%d/%m/%Y")
              
              
              
              
                
                data_df_d <- data.frame(
                    "Fecha_Recaudación_del_Aporte_en_SERVEL" = f_f,
                    "Fecha_Abono_Cuenta_del_Candidato" = f_f2
                    
                )
                
                
        ##########Busca la fecha larga
                textsalida1 <- cal_data %>% filter(Fechas ==  as.Date(dat,format = "%Y-%m-%d")) %>%
                               select(fecha_larga)
                
                
                
                txx1 <- textsalida1[[1,1]]
                
                
                txx1 <- as.character(txx1)
                
                
                textsalida2 <- cal_data %>% filter(Fechas ==  as.Date(datodf,format = "%Y-%m-%d")) %>%
                    select(fecha_larga)
                
                
                
                txx2 <- textsalida2[[1,1]]
                
                
                txx2 <- as.character(txx2)
                
                
                
                
                
            
                
                
                output$text1 <- renderUI({
                  tagList(
                    HTML(paste0(
                      "<h4><strong>", "Resumen de Resultado:", "</strong></h4>",
                      "a.- Fecha de Recaudación a Cuenta Bancaria de SERVEL: ", txx1,
                      "<br>",
                      "b.- Fecha de Abono a Cuenta Bancaria Electoral del Candidato: ", txx2,"<br>","<br>"
                      
                    )),
                    actionButton("add3", "¿Aún no obtienes tu aporte en tu cuenta?",icon = icon("frown-open,
")), br(),br(),br(),br()
                    
                  )
                  
                })
                
                
               
                
                #Envio tabla
                output$tbl= renderDT(data_df_d, extensions = 'Responsive',options = list(paging = FALSE,searching = FALSE, info= FALSE)) 
                
                
                
                
            }
            
            
            else{
                
                showNotification("La fecha de aprobación del aporte por parte del candidato no puede ser menor que la fecha de la realización del aporte", type = "error")
                
            }
            
#####fin validacion 2                
                
                
            }
             else{
                 
                 showNotification("No se pueden ingresar letras", type = "error")    
                 
             }
            
#######fin validacion 1           
            
            
            
            
            
######################################################## fin del observador
            
       
            
             }
        
        )
        
        
        observeEvent(input$add3, {
          showModal(modalDialog(
            title = "Información Relevante sobre Aportes",
            footer = modalButton("Volver"),
            div(
              
              p(em("Si aún no ha recibido su aporte dentro de la fecha calculada, los motivos pueden ser los siguientes:
        
1.-  El aportante indicó erróneamente los datos del candidato en el comprobante de aporte,  

2.-  Por alguna razón externa o interna, el aporte aún no se encuentra abonado en la cuenta bancaria de servel,
3.-  El Cheque fue protestado.

Para Mayor información, escribanos a financiamiento@servel.cl
     "))
              ,style="font-size:130%") 
            
          ))
        })
        
        
        
        
        
        
        
        
        
    }
    
    
    
    
    
    
    



shinyApp(ui = ui, server = server)


