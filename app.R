#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(curl)
library(reticulate)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Canary"),
  
  # Show a plot of the generated distribution
  mainPanel(
    textInput("command", label = h3("Run Command"), value = ""), br(), actionButton("executeButton", "Execute Command"),
    verbatimTextOutput("commandOutput"),
    h4("URL"),
    verbatimTextOutput("urlInfo"),
    
    h4("Session"),
    verbatimTextOutput("sessionInfo"),
    
    h4("User"),
    verbatimTextOutput("userInfo"),
    
    h4("OS Release"),
    htmlOutput("osVersion"),
    tableOutput("versions"),
    
    h4("Python Version"),
    htmlOutput("pythonVersion"),
    
    h4("Connect Version"),
    htmlOutput("connectVersion"),
    
    h4("Request Headers"),
    textOutput("clientData"),
    uiOutput("headers"),
    
    h3("Headers passed into Shiny"),
    verbatimTextOutput("summary"),
    
    h3("Value of specified header"),
    verbatimTextOutput("value"),
    
    h3("Monospaced font test"),
    plotOutput("mono"),
    
    h3("System Packages"),
    htmlOutput("packages")
  )
)

# Define server logic required to draw a histogram
server <- function(session, input, output) {
  
  output$connectVersion <- renderText({
    o <- system("/opt/connect/current/bin/connect --version", intern=TRUE)
    paste(o, collapse="<br/>")
  })
  
  output$pythonVersion <- renderText({
    # Use reticulate to access Python and get the version
    sys <- import("sys")
    o <- sys$version
    paste(o, collapse="<br/>")
  })
  
  output$osVersion <- renderText({
    o <- system("grep VERSION /etc/os-release", intern=TRUE)
    paste(o, collapse="<br/>")
  })
  
  output$versions <- renderTable({
    r <- version$version.string
    python <- paste(system("python --version", intern=TRUE), collapse="\n")
    java <- paste(system("java --version", intern=TRUE), collapse="\n")
    julia <- paste(system("julia --version", intern=TRUE), collapse="\n")
    tribble(
      ~language, ~version,
      "R", r,
      "python", python,
      "java", java,
      "julia", julia
    )
  })
  
  # output sessionInfo
  output$sessionInfo <- renderPrint({
    sessionInfo()
  })
  
  # output urlInfo
  output$urlInfo <- renderText({
    paste(sep = "",
          "protocol: ", session$clientData$url_protocol, "\n",
          "hostname: ", session$clientData$url_hostname, "\n",
          "pathname: ", session$clientData$url_pathname, "\n",
          "port: ",     session$clientData$url_port,     "\n",
          "search: ",   session$clientData$url_search,   "\n"
    )
  })
  
  # output userInfo
  output$userInfo <- renderText({
    paste(sep="",
          "user: ", session$user, "\n")
  })
  
  output$mono <- renderPlot({
    dat <- data.frame(
      y = 1:3,
      text = c("This is text", "Text with\nmultiple lines", "Some more text")
    )
    
    p <- ggplot(dat, aes(x=1, y=y)) + 
      scale_y_continuous(limits=c(0.5, 3.5), breaks=NULL) +
      scale_x_continuous(breaks=NULL)
    
    p + geom_text(aes(label=text))
    
    p + geom_text(aes(label=text), family="Times", fontface="italic", lineheight=.8) +
      annotate(geom="text", x=1, y=1.5, label="Annotation text", colour="red",
               size=7, family="mono", fontface="bold", angle=30)
  })
  
  output$clientData <- renderText({
    paste("ClientData$url_hostname = ", session$clientData$url_hostname)
  })
  
  output$summary <- renderText({
    ls(env=session$request)
  })
  
  output$packages <- renderText({
    o <- system("dpkg-query --list", intern=TRUE)
    paste(o, collapse="<br/>")
  })
  
  output$headers <- renderUI({
    selectInput("header", "Header:", ls(env=session$request))
  })
  
  output$value <- renderText({
    if (nchar(input$header) < 1 || !exists(input$header, envir=session$request)){
      return("NULL");
    }
    return (get(input$header, envir=session$request));
  })
  
  runCommand <- eventReactive(input$executeButton, {
    system(input$command, intern=TRUE)
  })
  
  output$commandOutput <- renderPrint({
    runCommand()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
