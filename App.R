library(shiny)
library(shinydashboard)
library("readxl")
library(ggplot2)
library(gridExtra)
library(DT)

# GEneration TIme from TEcan data version 1.2 (11-12-2020)
# Use with R studio
# Be sure to have all the packages listed above installed
# Input file is excel file, direct output from the TECAN, with a little formating
# Change the name of the first column to Time and delete the "s" before using it

#################################
#        Define function        # 
#################################

DERIVATIVE1 <- function(x,y)
{
  l=length(y)
  g=rep("NA",length(y))
  for ( i in 1:l) 
  { 
    X1=x[i]
    X2=x[i+5]
    Y1=y[i]
    Y2=y[i+5]
    TANGa=(Y2-Y1)/(X2-X1)
    g[i]=TANGa
    g=as.numeric(g)
  }
  return(g)
}

DELTA <- function(x)
{
  l=length(x)
  h=rep("NA",length(x))
  for (i in 1:l)
  {
    X1=x[i]
    X2=x[i+1]
    TANGb=(X2-X1)
    h[i]=TANGb
    h=as.numeric(h)
  }
  return(h)
}

Res = data.frame(Pos="e.g. B2", Sample="e.g. s288c", val="e.g. 95", Mu="0.005", Range="100-200", Plateau="e.g. 1.15")

#################################
#    Define User interface      #
#################################

# for the icon : https://fontawesome.com/icons?d=gallery&q=open

ui <- dashboardPage(skin = "red",
  dashboardHeader(title = "Calculation of doubling time from growth curves", titleWidth = 500),
  dashboardSidebar(width=290,
                  box(width = 290, background  ="green",
                     fileInput("file1", "Choose file to upload"),
                  ),
                  box(width = 290,background = "green",
                     selectInput("Sample","Select Sample", choices = ""),
                     textInput("Name","Name the sample",value=""),
                     selectInput("Bkg","Select Blank", choices = ""),
                  )
  ),

  dashboardBody(
    fluidRow(
       column(width = 12,
           box(
              plotOutput("plate", click = "plate_click", dblclick = "plate_b=dblclick"),status="success"
           ),
           box(
              plotOutput("plot", click = "plot_click", dblclick = "plot_dblclick"),status="success"                   
           )
       )
    ),
    fluidRow(
       column(width = 12,
            box(
              dataTableOutput("GTable"), status="danger"
            ),
            box(width = 4,
              sliderInput("slider", "Expo phase", min = 0, max = 1000,value = c(200,800), step = 10, ticks = FALSE),
              tags$hr(),
              verbatimTextOutput("Mu"),
              verbatimTextOutput("Gen"),
              verbatimTextOutput("Plateau"),
              tags$hr(),
              actionButton("valid","validate the calculation",class = "butt1"),
              status="danger"
            ),
            box(width = 2,background = "red",
              downloadButton("SaveGraph","Save graphic as png file",class = "butt1"), tags$head(tags$style(".butt1{background-color:orange;} .butt1{color: black;} .butt1{font-family: Arial}")),
              tags$hr(),
              downloadButton("download","Save result in a txt file",class = "butt1"),
              tags$hr(),
              downloadButton("Boxplot","Export result as a Boxplot",class = "butt1"),
              status = "danger"             
            )
            
       )
    )
  )
)


#################################
#    Define Server action       # 
#################################

server <- function(session, input, output) {  
  
  data <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    as.data.frame(read_excel(inFile$datapath, col_names = TRUE, col_types = "numeric"))
  })
  
  Rmax <- reactive({
    round(max(data()[,1]/60)+100,-2)
  })
 
  Pas <- reactive({
     round(data()[2,1]/60,0)
  #  round((data()[3,1]-data()[2,1])/60,0)    alternate calculation
  })
  
  observe({
    updateSliderInput(session, inputId = "slider", max = Rmax(),value = c(200,400), step = Pas())
  })
  
  
  col_names <- reactive({
    colnames(data())
  })
  
  observe({
    updateSelectInput(session, inputId = "Sample", choices = col_names())
  })
  
  observe({
    updateSelectInput(session, inputId = "Bkg", choices = col_names())
  })
  
  WD <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    as.data.frame(subset(data(),select = c("Time",input$Sample,input$Bkg)))
  })
  
  xmax <- reactive({
    max(WD()[,1]/60)+100
  })
  
  ymin <- reactive({
    min(log10(WD()[,2]))
  })
  
  # truc de bourrin (memo pour margin : c(top, right, bottom, left))
  
  pt1 <- reactive({
    qplot(x=data()[,1], y=data()[,2] ,main="1", ylim = c(0,1.5), xlab = "", ylab ="A", geom = "line") + theme(plot.title = element_text(hjust = 0.5), axis.title.y = element_text(angle=0, vjust = 0.5), axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(0,-0.35,-0.5,0), "cm"))
  })
  
  pt2 <- reactive({
    qplot(data()[,1], data()[,3], main="2", ylim = c(0,1.5), xlab = "", ylab="", geom = "line") + theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(0,-0.35,-0.5,-0.35), "cm"))
  })
  
  pt3 <- reactive({
    qplot(data()[,1], data()[,4], main = "3", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(0,-0.35,-0.5,-0.35), "cm"))
  })
  
  pt4 <- reactive({
    qplot(data()[,1], data()[,5], main = "4", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(0,-0.35,-0.5,-0.35), "cm"))
  })
  
  pt5 <- reactive({
    qplot(data()[,1], data()[,6],main="5", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(0,-0.35,-0.5,-0.35), "cm"))
  })
  
  pt6 <- reactive({
    qplot(data()[,1], data()[,7], main="6", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(0,-0.35,-0.5,-0.35), "cm"))
  })
  
  pt7 <- reactive({
    qplot(data()[,1], data()[,8], main = "7", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(0,-0.35,-0.5,-0.35), "cm"))
  })
  
  pt8 <- reactive({
    qplot(data()[,1], data()[,9], main = "8", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(0,-0.35,-0.5,-0.35), "cm"))
  })
  
  pt9 <- reactive({
    qplot(data()[,1], data()[,10], main="9", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(0,-0.35,-0.5,-0.35), "cm"))
  })
  
  pt10 <- reactive({
    qplot(data()[,1], data()[,11], main="10", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(0,-0.35,-0.5,-0.35), "cm"))
  })
  
  pt11 <- reactive({
    qplot(data()[,1], data()[,12], main = "11", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(0,-0.35,-0.5,-0.35), "cm"))
  })
  
  pt12 <- reactive({
    qplot(data()[,1], data()[,13], main = "12", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(0,-0.35,-0.5,-0.35), "cm"))
  })
  pt13 <- reactive({
    qplot(data()[,1], data()[,14], main="", ylim = c(0,1.5), xlab = "", ylab="B", geom = "line")+ theme(axis.title.y = element_text(angle=0, vjust = 0.5), axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,0), "cm"))
  })
  
  pt14 <- reactive({
    qplot(data()[,1], data()[,15], main="", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })
  
  pt15 <- reactive({
    qplot(data()[,1], data()[,16], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })
  
  pt16 <- reactive({
    qplot(data()[,1], data()[,17], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })
  
  pt17 <- reactive({
    qplot(data()[,1], data()[,18], main="", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })
  
  pt18 <- reactive({
    qplot(data()[,1], data()[,19], main="", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })
  
  pt19 <- reactive({
    qplot(data()[,1], data()[,20], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })
  
  pt20 <- reactive({
    qplot(data()[,1], data()[,21], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })
  
  pt21 <- reactive({
    qplot(data()[,1], data()[,22], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })
  
  pt22 <- reactive({
    qplot(data()[,1], data()[,23], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })
  
  pt23 <- reactive({
    qplot(data()[,1], data()[,24], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })
  
  pt24 <- reactive({
    qplot(data()[,1], data()[,25], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })
  
  pt25 <- reactive({
    qplot(data()[,1], data()[,26], main = "", ylim = c(0,1.5), xlab = "", ylab="C", geom = "line")+ theme(axis.title.y = element_text(angle=0, vjust = 0.5), axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,0), "cm"))
  })
  
  pt26 <- reactive({
    qplot(data()[,1], data()[,27], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })
  
  pt27 <- reactive({
    qplot(data()[,1], data()[,28], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })
  
  pt28 <- reactive({
    qplot(data()[,1], data()[,29], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })
  
  pt29 <- reactive({
    qplot(data()[,1], data()[,30], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })
  
  pt30 <- reactive({
    qplot(data()[,1], data()[,31], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })
  
  pt31 <- reactive({
    qplot(data()[,1], data()[,32], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })
  
  pt32 <- reactive({
    qplot(data()[,1], data()[,33], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })
  
  pt33 <- reactive({
    qplot(data()[,1], data()[,34], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })
  
  pt34 <- reactive({
    qplot(data()[,1], data()[,35], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })
  
  pt35 <- reactive({
    qplot(data()[,1], data()[,36], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })
  
  pt36 <- reactive({
    qplot(data()[,1], data()[,37], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })
  
  pt37 <- reactive({
    qplot(data()[,1], data()[,38], main = "", ylim = c(0,1.5), xlab = "", ylab="D", geom = "line")+ theme(axis.title.y = element_text(angle=0, vjust = 0.5), axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,0), "cm"))
  })
  
  pt38 <- reactive({
    qplot(data()[,1], data()[,39], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })
  
  pt39 <- reactive({
    qplot(data()[,1], data()[,40], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })
  
  pt40 <- reactive({
    qplot(data()[,1], data()[,41], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })
  
  pt41 <- reactive({
    qplot(data()[,1], data()[,42], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })
  
  pt42 <- reactive({
    qplot(data()[,1], data()[,43], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })
  
  pt43 <- reactive({
    qplot(data()[,1], data()[,44], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })
  
  pt44 <- reactive({
    qplot(data()[,1], data()[,45], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })
  
  pt45 <- reactive({
    qplot(data()[,1], data()[,46], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })
  
  pt46 <- reactive({
    qplot(data()[,1], data()[,47], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })
  
  pt47 <- reactive({
    qplot(data()[,1], data()[,48], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })
  
  pt48 <- reactive({
    qplot(data()[,1], data()[,49], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })
  
  pt49 <- reactive({
    qplot(data()[,1], data()[,50], main = "", ylim = c(0,1.5), xlab = "", ylab="E", geom = "line")+ theme(axis.title.y = element_text(angle=0, vjust = 0.5), axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,0), "cm"))
  })
  
  pt50 <- reactive({
    qplot(data()[,1], data()[,51], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })
  
  pt51 <- reactive({
    qplot(data()[,1], data()[,52], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })  
  
  pt52 <- reactive({
    qplot(data()[,1], data()[,53], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })   
  
  pt53 <- reactive({
    qplot(data()[,1], data()[,54], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })   
  
  pt54 <- reactive({
    qplot(data()[,1], data()[,55], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })   
  
  pt55 <- reactive({
    qplot(data()[,1], data()[,56], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })   
  
  pt56 <- reactive({
    qplot(data()[,1], data()[,57], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })   
  
  pt57 <- reactive({
    qplot(data()[,1], data()[,58], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })   
  
  pt58 <- reactive({
    qplot(data()[,1], data()[,59], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })   
  
  pt59 <- reactive({
    qplot(data()[,1], data()[,60], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })   
  
  pt60 <- reactive({
    qplot(data()[,1], data()[,61], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })   
  
  pt61 <- reactive({
    qplot(data()[,1], data()[,62], main = "", ylim = c(0,1.5), xlab = "", ylab="F", geom = "line")+ theme(axis.title.y = element_text(angle=0, vjust = 0.5), axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,0), "cm"))
  })   
  
  pt62 <- reactive({
    qplot(data()[,1], data()[,63], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })   
  
  pt63 <- reactive({
    qplot(data()[,1], data()[,64], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })   
  
  pt64 <- reactive({
    qplot(data()[,1], data()[,65], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })   
  
  pt65 <- reactive({
    qplot(data()[,1], data()[,66], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })   
  
  pt66 <- reactive({
    qplot(data()[,1], data()[,67], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })   
  
  pt67 <- reactive({
    qplot(data()[,1], data()[,68], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })   
  
  pt68 <- reactive({
    qplot(data()[,1], data()[,69], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })   
  
  pt69 <- reactive({
    qplot(data()[,1], data()[,70], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })   
  
  pt70 <- reactive({
    qplot(data()[,1], data()[,71], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })   
  
  pt71 <- reactive({
    qplot(data()[,1], data()[,72], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })   
  
  pt72 <- reactive({
    qplot(data()[,1], data()[,73], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })   
  
  pt73 <- reactive({
    qplot(data()[,1], data()[,74], main = "", ylim = c(0,1.5), xlab = "", ylab="G", geom = "line")+ theme(axis.title.y = element_text(angle=0, vjust = 0.5), axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,0), "cm"))
  })   
  
  pt74 <- reactive({
    qplot(data()[,1], data()[,75], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })   
  
  pt75 <- reactive({
    qplot(data()[,1], data()[,76], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })   
  
  pt76 <- reactive({
    qplot(data()[,1], data()[,77], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })   
  
  pt77 <- reactive({
    qplot(data()[,1], data()[,78], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })   
  
  pt78 <- reactive({
    qplot(data()[,1], data()[,79], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })   
  
  pt79 <- reactive({
    qplot(data()[,1], data()[,80], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })   
  
  pt80 <- reactive({
    qplot(data()[,1], data()[,81], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })   
  
  pt81 <- reactive({
    qplot(data()[,1], data()[,82], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })   
  
  pt82 <- reactive({
    qplot(data()[,1], data()[,83], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })   
  
  pt83 <- reactive({
    qplot(data()[,1], data()[,84], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })   
  
  pt84 <- reactive({
    qplot(data()[,1], data()[,85], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })   
  
  pt85 <- reactive({
    qplot(data()[,1], data()[,86], main = "", ylim = c(0,1.5), xlab = "", ylab="H", geom = "line")+ theme(axis.title.y = element_text(angle=0, vjust = 0.5), axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,0), "cm"))
  })   
  
  pt86 <- reactive({
    qplot(data()[,1], data()[,87], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })   
  
  pt87 <- reactive({
    qplot(data()[,1], data()[,88], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })   
  
  pt88 <- reactive({
    qplot(data()[,1], data()[,89], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })   
  
  pt89 <- reactive({
    qplot(data()[,1], data()[,90], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })   
  
  pt90 <- reactive({
    qplot(data()[,1], data()[,91], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })   
  
  pt91 <- reactive({
    qplot(data()[,1], data()[,92], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })   
  
  pt92 <- reactive({
    qplot(data()[,1], data()[,93], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })   
  
  pt93 <- reactive({
    qplot(data()[,1], data()[,94], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })   
  
  pt94 <- reactive({
    qplot(data()[,1], data()[,95], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })   
  pt95 <- reactive({
    qplot(data()[,1], data()[,96], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })   
  pt96 <- reactive({
    qplot(data()[,1], data()[,97], main = "", ylim = c(0,1.5), xlab = "", ylab="", geom = "line")+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(), plot.margin=unit(c(-0.5,-0.35,-0.5,-0.35), "cm"))
  })   
  
  output$plate <- renderPlot({
    ptlist <- list(pt1(),pt2(),pt3(),pt4())
    if (length(ptlist)==0) return(NULL)
    grid.arrange(pt1(),pt2(),pt3(),pt4(),pt5(),pt6(),pt7(),pt8(),pt9(),pt10(),pt11(),pt12(),
                 pt13(),pt14(),pt15(),pt16(),pt17(),pt18(),pt19(),pt20(),pt21(),pt22(),pt23(),pt24(),
                 pt25(),pt26(),pt27(),pt28(),pt29(),pt30(),pt31(),pt32(),pt33(),pt34(),pt35(),pt36(),
                 pt37(),pt38(),pt39(),pt40(),pt41(),pt42(),pt43(),pt44(),pt45(),pt46(),pt47(),pt48(),
                 pt49(),pt50(),pt51(),pt52(),pt53(),pt54(),pt55(),pt56(),pt57(),pt58(),pt59(),pt60(),
                 pt61(),pt62(),pt63(),pt64(),pt65(),pt66(),pt67(),pt68(),pt69(),pt70(),pt71(),pt72(),
                 pt73(),pt74(),pt75(),pt76(),pt77(),pt78(),pt79(),pt80(),pt81(),pt82(),pt83(),pt84(),
                 pt85(),pt86(),pt87(),pt88(),pt89(),pt90(),pt91(),pt92(),pt93(),pt94(),pt95(),pt96(),
                 ncol = 12)
  })
  
  output$plot <- renderPlot({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    if (input$Bkg == "Time" || input$Sample == "Time")
      return(NULL)
    plot(round(WD()[,1]/60,-1),DERIVATIVE1(WD()[,1]/60, log10(WD()[,2]-WD()[,3])), type = "l", axes=F, col = "blue", xlab = "", ylab = "", main = "", xlim = c(0,xmax()))
    grid(lwd = 1, nx = 25)
    par(new = T)
    plot(round(WD()[,1]/60,-1), log10(WD()[,2]-WD()[,3]), pch = 20, col = "red", main = input$Name, xlab = "Time (min)", ylab = "Log10(OD)", xlim = c(0,xmax()))
    legend(xmax()*0.8,ymin()*1.5, legend = c("log10(OD)","Slope"), col = c("red","blue"), lty = 1:1, cex = 0.8)
    abline(v=input$slider[1])
    abline(v=input$slider[2])
    abline(h=log10(PLTval()), col = "red", lty = 5)
  })

  Y1 <- reactive({
    log10(
      WD()[trunc((round(WD()$Time/60,0))/Pas())*Pas()==input$slider[1],2]
      -
      WD()[trunc((round(WD()$Time/60,0))/Pas())*Pas()==input$slider[1],3]
    )
  })
  
  Y2 <- reactive({
    log10(
      WD()[trunc((round(WD()$Time/60,0))/Pas())*Pas()==input$slider[2],2]
      -
      WD()[trunc((round(WD()$Time/60,0))/Pas())*Pas()==input$slider[2],3]
    )
  })
  
  Mumax <- reactive({
    round(
      (Y2()-Y1())
      /
        (input$slider[2]-input$slider[1])
      ,5)
  })
  
  DT <- reactive({
    round(log10(2)/Mumax(),2)
  })
  
  output$Mu <- renderText({
    inFile <- input$file1
    if (input$Sample == input$Bkg)
      return("NULL")
    paste0("µmax = ",Mumax())
  })
  
  output$Gen <- renderText({
    inFile <- input$file1
    if (input$Sample == input$Bkg)
      return("NULL")
    paste0("Doubling Time = ",DT()," min")
  })

  PLT1 <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    as.data.frame(subset(WD(),WD()[,1]/60>600))
    })
  
  PLT2 <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    as.data.frame(subset(PLT1(),DELTA(PLT1()[,2])<0.005))
  })
  
  PLTval <- reactive({
#    round(log10(median(PLT2()[,2]-PLT2()[,3])),5) formule alternative
    median(PLT2()[,2]-PLT2()[,3])
      })
  
  output$Plateau <- renderText({
    inFile <- input$file1
    if (input$Sample == input$Bkg)
      return("NULL")
#    if (PLTval() !="")
#      return("no plateau detected")
#    paste0("Plateau detected at  = ",PLTval(), "  from ",round(min(PLT2()[,1])/60,0), " to ", round(max(PLT2()[,1])/60,0))
     paste0("Plateau detected at  OD = ",PLTval(), "  (graph value :",round(log10(PLTval()),3),")")
    
      })
  
  output$GTable <- renderDataTable(df())
  df <- eventReactive(input$valid, {
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    if(input$Sample != "Time"  && DT() != "" && input$Name != "" && PLTval() !=""){
      newrow = data.frame(Pos=input$Sample, Sample=input$Name, val=as.character(DT()), Mu=as.character(Mumax()), Range=paste(as.character(input$slider[1]),"_",as.character(input$slider[2])), Plateau=as.character(PLTval()))
      Res <<- rbind(Res, newrow)
    }
    Res
  }, ignoreNULL = FALSE)
  
  output$download <- downloadHandler(
    filename = "Res.txt",
    content = function(file) {
      write.table(Res[-1,], file, row.names = FALSE, sep = "\t", quote = FALSE)
    })
  
  exportPlot <- reactive({
    plot(round(WD()[,1]/60,-1),DERIVATIVE1(WD()[,1]/60, log10(WD()[,2]-WD()[,3])), type = "l", axes=F, col = "blue", xlab = "", ylab = "", main = "", xlim = c(0,xmax()))
    grid(lwd = 1, nx = 25)
    par(new = T)
    plot(round(WD()[,1]/60,-1), log10(WD()[,2]-WD()[,3]), pch = 20, col = "red", main = input$Name, xlab = "Time (min)", ylab = "Log10(OD)", xlim = c(0,xmax()))
    legend(xmax()*0.8,ymin()*1.5, legend = c("log10(OD)","Slope"), col = c("red","blue"), lty = 1:1, cex = 0.8)
    abline(v=input$slider[1])
    abline(v=input$slider[2])
  })
  
  output$SaveGraph <- downloadHandler(
    filename <- function(){paste0(input$Name,".png")},
    content = function(file) {
      ggsave(file,exportPlot())
    }
  )

  Res2 <- reactive({
    as.data.frame(Res[-1,])
  })
  
 exportBoxPlot <- reactive({
#    plot(as.numeric(Res2()[,2]),Res2()[,3])
    ggplot(Res2(), aes(x=Sample, y=as.numeric(as.character(val)))) + geom_boxplot() + geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5) + xlab("Strain") + ylab("Generation Time (min)")
  })
  
  output$Boxplot <- downloadHandler(
    filename = "Boxplot.pdf",
    content = function(file) {
      ggsave(file,exportBoxPlot(), device = "pdf")
    }
  )

}


#################################
#        Launch in local        # 
#################################

shinyApp(ui = ui, server = server)
