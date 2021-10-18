################################################################################
##########    Librerías    #####################################################
################################################################################
library(MASS) #Distribuciones de Probabilidad
library(actuar) #Modificaciones en la v.a de pérdida X
library(ggplot2) #Plots
library(shiny) #Dashboards
library(bslib) #Temas de shiny

################################################################################
##########    Diseño de tablero    #############################################
################################################################################
ui <- navbarPage("Modificaciones a la Variable Aleatoria",
                theme = bs_theme(bootswatch = "united"),
                tabPanel("Plot", icon = icon("chart-bar"),
                         sidebarLayout(position = "right",
                                       sidebarPanel(
                                         h4("Seleccione las modificaciones de interés"),
                                         checkboxInput("deducible", "Deducible", TRUE),
                                         radioButtons("tipo", label=NULL,
                                                      c("Ordinario" = "ord",
                                                        "Franchise" = "fra")),
                                         sliderInput(inputId = "ded",label=NULL,
                                                     value=0.5,min=0,max=10,step = 0.1),
                                         checkboxInput("limite", "Límite de póliza", TRUE),
                                         sliderInput(inputId = "lim",label=NULL,
                                                     value=1,min=0,max=10,step = 0.1),
                                         checkboxInput("coaseguro", "Coaseguro", TRUE),
                                         sliderInput(inputId = "coa",label=NULL,
                                                     value=0.90,min=0,max=1,step = 0.01),
                                         checkboxInput("inflacion", "Inflación", TRUE),
                                         sliderInput(inputId = "inf",label=NULL,
                                                     value=0.05,min=0,max=1,step = 0.01),
                                                    ),
                                       mainPanel(
                                         tabsetPanel(
                                          tabPanel("V.A. de Pérdida", plotOutput("plot"),icon = icon("car-crash")),
                                          tabPanel("V.A. de Pago", plotOutput("plot2"),icon = icon("hand-holding-usd")), 
                                          type="pills"
                                         )
                                       )
                                       ),
                         wellPanel(h4("Seleccione los parámetros deseados"),
                                   fluidRow(column(3,
                                                   selectInput('dist',label="Distribuciones",
                                                   choices = c("Gamma","Normal","F","Logistic",
                                                               "Log-Logistic","Log-Normal",
                                                               "t de Student","Weibull")),value="Gamma"),
                                            column(3,
                                                   sliderInput(inputId = "p1",label="Parámetro 1",
                                                   value=4,min=0,max=20,step=0.1)),
                                            column(3,
                                                   sliderInput(inputId = "p2",label="Parámetro 2",
                                                   value=3,min=0,max=20,step=0.1)),
                                            column(3, verbatimTextOutput("t"))
                                           )
                                   ),
                          ),
                navbarMenu("Más", icon = icon("info-circle"),
                             tabPanel("Resumen",
                                      h4("Datos"),
                                      verbatimTextOutput("r1"),
                                      h4("Resumen de la densidad de los datos empirícos"),
                                      verbatimTextOutput("r2"),
                                      h4("Resumen de la densidad de los datos modificados en la V.A. de pérdida"),
                                      verbatimTextOutput("r3"),
                                      h4("Resumen de la densidad de los datos modificados en la V.A. de pago"),
                                      verbatimTextOutput("r4")
                                      ),
                             tabPanel("Tabla de V.A. de pérdida", verbatimTextOutput("t1")),
                             tabPanel("Tabla de V.A. de pago", verbatimTextOutput("t2"))
                )
)

################################################################################
##########     SERVIDOR     ####################################################
################################################################################
server<-function(input,output){

  #datos para ambas tabs #######################################################
  
  cded <- reactive({if(input$deducible==TRUE){input$ded}else{0}})
  clim <- reactive({if(input$limite==TRUE){input$lim}else{10}})
  ccoa <- reactive({if(input$coaseguro==TRUE){input$coa}else{1}})
  cinf <- reactive({if(input$inflacion==TRUE){input$inf}else{0}})
  
  cfra <- reactive({if(input$tipo =="fra"){TRUE}else{FALSE}})
  
  a <- seq(from=0, to=10, by = 0.01)
  d <- rep("Empiríca", times = 1001)
  e <- rep("Modificada", times = 1001)

  b<-reactive({
    if(input$dist=="Normal"){
      dnorm(a,input$p1,input$p2)
    }
    else if(input$dist=="Gamma"){
      dgamma(a,input$p1,input$p2)
    }
    else if(input$dist=="F"){
      df(a,input$p1,input$p2)
    }
    else if(input$dist=="Logistic"){
      dlogis(a,input$p1)
    }
    else if(input$dist=="Log-Logistic"){
      dllogis(a,input$p1,input$p2)
    }
    else if(input$dist=="Log-Normal"){
      dlnorm(a,input$p1,input$p2)
    }
    else if(input$dist=="t de Student"){
      dt(a,input$p1,input$p2)
    }
    else if(input$dist=="Weibull"){
      dweibull(a,input$p1,input$p2)
    }
  })

  #datos para tab de pérdida ###################################################
  
  m<-reactive({
    if(input$dist=="Normal"){
      coverage(dnorm, pnorm,
                        deductible = cded(),franchise = cfra(),limit = clim(),
                        coinsurance = ccoa(),inflation = cinf(),per.loss = TRUE)
    }
    else if(input$dist=="Gamma"){
      coverage(dgamma, pgamma,
                        deductible = cded(),franchise = cfra(),limit = clim(),
                        coinsurance = ccoa(),inflation = cinf(),per.loss = TRUE)
    }
    else if(input$dist=="F"){
      coverage(df, pf,
                        deductible = cded(),franchise = cfra(),limit = clim(),
                        coinsurance = ccoa(),inflation = cinf(),per.loss = TRUE)
    }
    else if(input$dist=="Logistic"){
      coverage(dlogis, plogis,
                        deductible = cded(),franchise = cfra(),limit = clim(),
                        coinsurance = ccoa(),inflation = cinf(),per.loss = TRUE)
    }
    else if(input$dist=="Log-Logistic"){
      coverage(dllogis, pllogis,
                        deductible = cded(),franchise = cfra(),limit = clim(),
                        coinsurance = ccoa(),inflation = cinf(),per.loss = TRUE)
    }
    else if(input$dist=="Log-Normal"){
      coverage(dlnorm, plnorm,
                        deductible = cded(),franchise = cfra(),limit = clim(),
                        coinsurance = ccoa(),inflation = cinf(),per.loss = TRUE)
    }
    else if(input$dist=="t de Student"){
      coverage(dt, pt,
                        deductible = cded(),franchise = cfra(),limit = clim(),
                        coinsurance = ccoa(),inflation = cinf(),per.loss = TRUE)
    }
    else if(input$dist=="Weibull"){
      coverage(dweibull, pweibull,
                        deductible = cded(),franchise = cfra(),limit = clim(),
                        coinsurance = ccoa(),inflation = cinf(),per.loss = TRUE)
    }
  })
  
  cm <- reactive(m()(a,4,3))
  x <- reactive(data.frame(x=c(a,a),tipo=c(d,e),datos=c(b(),cm())))
  
  #datos para tab de pago ######################################################
  m2<-reactive({
    if(input$dist=="Normal"){
      coverage(dnorm, pnorm,
               deductible = cded(),franchise = cfra(),limit = clim(),
               coinsurance = ccoa(),inflation = cinf(),per.loss = FALSE)
    }
    else if(input$dist=="Gamma"){
      coverage(dgamma, pgamma,
               deductible = cded(),franchise = cfra(),limit = clim(),
               coinsurance = ccoa(),inflation = cinf(),per.loss = FALSE)
    }
    else if(input$dist=="F"){
      coverage(df, pf,
               deductible = cded(),franchise = cfra(),limit = clim(),
               coinsurance = ccoa(),inflation = cinf(),per.loss = FALSE)
    }
    else if(input$dist=="Logistic"){
      coverage(dlogis, plogis,
               deductible = cded(),franchise = cfra(),limit = clim(),
               coinsurance = ccoa(),inflation = cinf(),per.loss = FALSE)
    }
    else if(input$dist=="Log-Logistic"){
      coverage(dllogis, pllogis,
               deductible = cded(),franchise = cfra(),limit = clim(),
               coinsurance = ccoa(),inflation = cinf(),per.loss = FALSE)
    }
    else if(input$dist=="Log-Normal"){
      coverage(dlnorm, plnorm,
               deductible = cded(),franchise = cfra(),limit = clim(),
               coinsurance = ccoa(),inflation = cinf(),per.loss = FALSE)
    }
    else if(input$dist=="t de Student"){
      coverage(dt, pt,
               deductible = cded(),franchise = cfra(),limit = clim(),
               coinsurance = ccoa(),inflation = cinf(),per.loss = FALSE)
    }
    else if(input$dist=="Weibull"){
      coverage(dweibull, pweibull,
               deductible = cded(),franchise = cfra(),limit = clim(),
               coinsurance = ccoa(),inflation = cinf(),per.loss = FALSE)
    }
  })
  
  cm2 <- reactive(m2()(a,4,3))
  x2 <- reactive(data.frame(x=c(a,a),tipo=c(d,e),datos=c(b(),cm2())))
  
  #Plot ########################################################################
  
  tema <- theme(plot.title = element_text(size = (16), colour="orangered2",hjust = 0.5,face = "bold"), 
                legend.title = element_text(colour = "orangered2"), 
                legend.text = element_text(colour="orangered2"), 
                axis.title = element_text(size = (14), colour = "orangered2",face = "bold"),
                axis.text = element_text(colour = "black", size = (12)),
                legend.position = "right")

  output$plot<-renderPlot({
    ggplot(data = x(), aes(x = x, y = datos, colour = tipo)) +
      geom_line() +
      geom_area(aes(fill = tipo, group = tipo),
                alpha = 0.5, position = 'identity',color="wheat4") +
      scale_fill_manual(values=c("orange", "darkturquoise")) +
      labs(y="Densidad", x = "Datos") +
      ggtitle("Modificaciones en la V.A.") + tema
  })
  
  output$plot2<-renderPlot({
    ggplot(data = x2(), aes(x = x, y = datos, colour = tipo)) +
      geom_line() +
      geom_area(aes(fill = tipo, group = tipo),
                alpha = 0.5, position = 'identity',color="wheat4") +
      scale_fill_manual(values=c("orange", "darkturquoise")) +
      labs(y="Densidad", x = "Datos") +
      ggtitle("Modificaciones en la V.A.") + tema
  })
  
  output$t <- renderPrint({
    "Autor: Brando Alberto Toribio García"
  })
  
  resumen <- reactive(data.frame(Distribución = c(input$dist),
                                 Parámetro_1 = c(input$p1),
                                 Parámetro_2 = c(input$p2),
                                 Deducible = c(input$ded),
                                 Límite = c(input$lim),
                                 Coaseguro = c(input$coa),
                                 Inflación = c(input$inf)
                                 ))
  
  output$r1 <- renderPrint({
    resumen()
  })
  
  output$r2 <- renderPrint({
    summary(b())  
  })
  
  output$r3 <- renderPrint({
    summary(cm())    
  })
  
  output$r4 <- renderPrint({
    summary(cm2())   
  })
  
  output$t1 <- renderPrint({
    cm()   
  })
  
  output$t2 <- renderPrint({
    cm2()  
  })
  
}

################################################################################
##########     PUBLICACIÓN     #################################################
################################################################################
shinyApp(ui=ui,server=server)




