library(shiny)
library(shinythemes)
ui = fluidPage(theme = shinytheme("cerulean"),
  column(3,img(src="1.png",width=300,height=100)),
  column(3,offset=6,a(href="https://es.kaust.edu.sa/Pages/Home.aspx",
           img(src="envstat.png",width=100,height=100))),
  fluidRow(
    column(10,offset=.1,h1("Spectral Representation of the AR2 process")),
    column(6,offset = 0.1,p("This app shows the effect of the spectral parameters on the 
       oscillatory behave of the process path."))
  ),
  a(href="https://es.kaust.edu.sa/Pages/CarolinaEuan.aspx",h5("PhD, Carolina Euan.")),
  hr(),
  sidebarPanel(    helpText("AR2 parameters and spectral parameters: 
                            phi1= (2 cos(2 pi eta))/M and phi2 = -1/M^2."), 
    sliderInput("Eta", "Peak frequency (eta):",
                                 min = 0, max = .5, value = .1,step = .05
  ),
  sliderInput("M", "Spectral Dispersion (M):",
              min = 1.005, max = 2, value = 1.01,step = .005
  ),
  actionButton("Run", "Run Setting"),
  h4(htmlOutput("AR2Par"))),
  mainPanel(plotOutput("Xtplot"),
                  column(6,plotOutput("Covplot")),
                  column(6,plotOutput("Specplot"))
                  )
)


server<-function(input,output){
  parameters<-reactiveValues(Time=1000)
  phi1<-eventReactive(input$Run,{
    (2*cos(2*pi*input$Eta))/input$M
  })
  phi2<-eventReactive(input$Run,{
    (-1)/(input$M^2)
  })
  Xt<-eventReactive(input$Run,arima.sim(parameters$Time,model=list(ar=c(phi1(),phi2()),order=c(2,0,0))))
  output$AR2Par <- renderUI({
      str1 <- "Correponding AR(2) parameters:"
      str2 <- paste("Phi1=",round(phi1(),2),", Phi2:",round(phi2(),2))
      HTML(paste(str1, str2, sep = '<br/>'))
      
    })
  output$Xtplot<-renderPlot({
    plot(1:parameters$Time,Xt(),type="l",main = "AR(2) Simulated Sample",
         xlab="Time",ylab="X(t)")
  })
  output$Covplot<-renderPlot({
    acf(Xt(),main="Estimated Autocorrelation")
  })
  output$Specplot<-renderPlot({
    spectrum(Xt(),spans = c(3,5),main="Estimated Spectrum")
  })
}

shinyApp(ui=ui,server=server)


