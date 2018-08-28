library('shiny')

#ui <- fluidPage(

#  fluidRow(h1(tags$strong(textOutput(outputId = "cmonth")))),
#  tabsetPanel(type = "tabs" ,tabPanel("Arab Heavy",
#  fluidRow(column(6,fluidRow(tableOutput(outputId = "Predtable1"))),column(6,fluidRow(tableOutput(outputId = "Prevpred1")))),
#  fluidRow(column(6,plotOutput(outputId = "Plot1")),column(6,plotOutput(outputId = "Plot2"))),
#  fluidRow(column(6,plotOutput(outputId = "Plot3")),column(6,plotOutput(outputId = "Plot4")))), 
            
#  tabPanel("Arab Medium",
#  fluidRow(column(6,fluidRow(tableOutput(outputId = "Predtable2"))),
#  column(6,fluidRow(tableOutput(outputId = "Prevpred2")))),
#  fluidRow(column(6,plotOutput(outputId = "Plot5")),column(6,plotOutput(outputId = "Plot6"))),
#  fluidRow(column(6,plotOutput(outputId = "Plot7")),column(6,plotOutput(outputId = "Plot8"))),
#  fluidRow(column(6,plotOutput(outputId = "Plot8")))), 
            
  
  
#  tabPanel("Arab Light"),
#            tabPanel("Arab Extra Light")
#)
#)

ui <- navbarPage((textOutput(outputId = "cmonth")),
        
        actionButton("show", "Click on the four tabs. Also check the message on bottom right side"),           
        tabPanel("Arab Heavy",tabPanel("Arab Heavy",
        fluidRow(column(6,fluidRow(h4("Prediction"),tableOutput(outputId = "Predtable1"))),column(6,fluidRow(h4("Previous Six months comparison"),tableOutput(outputId = "Prevpred1")))),
        fluidRow(h3("Scatter plots")),
        fluidRow(column(6,plotOutput(outputId = "Plot1")),column(6,plotOutput(outputId = "Plot2"))),
        fluidRow(column(6,plotOutput(outputId = "Plot3")),column(6,plotOutput(outputId = "Plot4"))))),
        
        tabPanel("Arab Medium",
        fluidRow(column(6,fluidRow(h4("Prediction"),tableOutput(outputId = "Predtable2"))),
        column(6,fluidRow(h4("Previous Six months comparison"),tableOutput(outputId = "Prevpred2")))),
        fluidRow(h3("Scatter plots")),
        fluidRow(column(6,plotOutput(outputId = "Plot5")),column(6,plotOutput(outputId = "Plot6"))),
        fluidRow(column(6,plotOutput(outputId = "Plot7")),column(6,plotOutput(outputId = "Plot8"))),
        fluidRow(column(6,plotOutput(outputId = "Plot9")))),
                   
        tabPanel("Arab Light",
        fluidRow(column(6,fluidRow(h4("Prediction"),tableOutput(outputId = "Predtable3"))),
        column(6,fluidRow(h4("Previous six months comparison"),tableOutput(outputId = "Prevpred3")))),
        fluidRow(h3("Scatter plots")),
        fluidRow(column(6,plotOutput(outputId = "Plot10")),
        column(6,plotOutput(outputId = "Plot11")))),

                   
        tabPanel("Arab Extra Light",
        fluidRow(column(6,fluidRow(h4("Prediction"),tableOutput(outputId = "Predtable4"))),
        column(6,fluidRow(h4("Previous six months prediction"),tableOutput(outputId = "Prevpred4")))),
        fluidRow(h3("Scatter plots")),
        fluidRow(column(6,plotOutput(outputId = "Plot12")),column(6,plotOutput(outputId = "Plot13"))),
        fluidRow(column(6,plotOutput(outputId = "Plot14")),column(6,plotOutput(outputId = "Plot15"))))
)
