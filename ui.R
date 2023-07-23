library(jpeg)
library(imager)
library(shinydashboard)
library(grid)
library(gridExtra)
library(OpenImageR)
library(shiny)
library(DT)
library(pROC)
library(class)
library(e1071)


shinyUI(
  dashboardPage(skin = "yellow",
                dashboardHeader(
                  title =  span("MY APP",target = "_blank"),
                  #title = span(img(src = "logo3.png", height = 35), "MY APP"),
                  titleWidth = 400,
                  dropdownMenu(
                    type = "notifications", 
                    headerText = strong("GMAIL"), 
                    icon = icon("envelope"), 
                    badgeStatus = NULL,
                    notificationItem(
                      text = HTML("hsu48982669@gmail.com"),
                      href = "mailto:hsu48982669@gmail.com",
                      icon = icon("envelope")
                    )
                  ),
                  tags$li(
                    a(
                      strong("MY CODE"),
                      height = 40,
                      href = "https://github.com/KAILi-Hsu/my_app",
                      title = "",
                      target = "_blank"
                    ),
                    class = "dropdown"
                  )
                ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("胸部X光左心室功能障礙預測", tabName = "X_tab"),
      menuItem("識別草帽一夥人!", tabName = "ONE_tab"),
      menuItem("分類器比較", tabName = "Class_tab"),
      menuItem("Panel Function", tabName = "Function_tab")
    )
  ),
  
  dashboardBody(
    tabItems(
      # X tab content
      tabItem(tabName = "X_tab",
              fileInput("files", label = h4("請上傳你的X光片:"), multiple = FALSE, accept = "image"),
              helpText("Note: 你需要等待一下(等待正在預測的時間)."),
              br(), 
              plotOutput("plot"),
              br(), 
              verbatimTextOutput("summary"),
              verbatimTextOutput("C_index")
      ),
      
      # ONE tab content
      tabItem(tabName = "ONE_tab",
              fileInput("files2", label = h4("上傳一張圖片吧!"), multiple = FALSE, accept = "image"),
              helpText("Note: 你需要等待一下(等待正在預測的時間)."),
              br(), 
              plotOutput("plot2"),
              br(), 
              verbatimTextOutput("summary2")
      ),
      
      # Class tab content
      tabItem(tabName = "Class_tab",
              
              sidebarPanel(
                    fileInput("files3", label = h4("Upload your data file:"), multiple=FALSE),
                    helpText("Note:you only can upload the .csv file. "),
                    uiOutput("choose_columns5"),
                    uiOutput("choose_columns6"),
                    uiOutput("choose_columns7"),
                    
                    selectInput('method', 'Methods', choices = c(
                      'Linear Discriminant'=1,
                      'Logistic regression'=2 ,
                      'Naive Bayes Classification'=3,
                      'k-nearest neighbor algorithm'=4 ))
              ),
              mainPanel(
                br(), 
                verbatimTextOutput('summary3'),
                br(),
                plotOutput("roc_test1", height = 500, width = 500)
              )
      ),
      
      # Function tab content
      tabItem(tabName = "Function_tab",
              
              fluidRow(
                box(
                  title = "Inputs", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE, width = 4,
                  #"Press ctrl for multiple selection", br(), 
                  #state.name <- c("Option1", "Option2", "Option3", "Option4", "Option5"),
                  selectInput('in1', 'multiple choice1:', state.name <- c("Option1", "Option2", "Option3", "Option4", "Option5", "Option6"), 
                              multiple=TRUE, selectize=FALSE),
                  "Press ctrl for multiple selection",
                  verbatimTextOutput('out1'),
                  selectInput('in2', 'multiple choice2:', state.name, multiple=TRUE, selectize=TRUE),
                  verbatimTextOutput('out2')
                  
                ),
                
                box(
                  title = "Inputs", status = "warning", solidHeader = TRUE,
                  width = 4,
                  "Try anything and see the output", br(), 
                  sliderInput("in3", "Slider input:", 1, 100, 50),
                  textInput("in4", "Text input:"),
                  verbatimTextOutput("out3"),
                  verbatimTextOutput("out4")
                  
                ),
                
                box(
                  title = "Inputs", status = "danger", solidHeader = TRUE,
                  width = 4,
                  checkboxGroupInput("in5", label = h3("Multiple choice3:"), 
                                     choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),
                                     selected = 1),
                  verbatimTextOutput("out5"),
                  hr(),
                  radioButtons("in6", label = h3("Radio buttons"),
                               choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), 
                               selected = 1),
                  verbatimTextOutput("out6")
                  
                )
              ),
              
              fluidRow(
                tabBox(
                  title = tagList(shiny::icon("gear"), "tabBox status"),
                  tabPanel("Tab1_Date",
                           dateInput("in7", label = h3("Date input"), value = "2014-01-01"),
                           verbatimTextOutput("out7"),
                           dateRangeInput("in8", label = h3("Date range")),
                           verbatimTextOutput("out8")
                  ),
                  tabPanel("Tab2_button",
                           h3("Action button"),
                           actionButton("in9", label = "Action"),
                           p("Current Value:", style = "color:#888888;"),
                           verbatimTextOutput("out9")
                  )
                ),

                tabBox(
                  side = "right", height = "250px", 
                  selected = "Slider",
                  tabPanel("Reference",
                           tags$b(h4("Here are the reference site where I wrote this APP:")),
                           hr(),
                           tags$b("1.Shiny Official Site Gallery", HTML("<a href='https://shiny.posit.co/r/gallery/' target='_blank'>click</a>")),
                           hr(),
                           tags$b("2.The structure of shinydashboard", HTML("<a href='https://rstudio.github.io/shinydashboard/structure.html' target='_blank'>click</a>"))
                  ),
                  tabPanel("Slider",
                           sliderInput("in10", label = h3("Slider Range"), min = 0,
                                       max = 100, value = c(40, 60)),
                           verbatimTextOutput("out10")
                  ),
                  tabPanel("TEXT", "For these functions, you can click MY CODE on the upper 
                           right to view the original code and find the function you want to copy.")
                ),
                
                tabBox(
                  title = "Input data, Show it and Download", side = "left", width = 12,
                
                  tabPanel("input_data",
                           fileInput("files4", label = h4("Upload A CSV file:"), multiple=FALSE),
                           uiOutput("choose_columns8")
                           
                  ),
                  
                  tabPanel("table_show",
                           tabPanel("mytable", DT::dataTableOutput("mytable1")
                           
                  )
                  
                  ),
                  
                  tabPanel("download", "You can export the data of the variables you have selected.",
                           downloadButton("downloadData", "Download")
                  )
                
                

        )

      )
      
    )
    
  )
)
)
)
