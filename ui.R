library(shiny)
library(shinydashboard)

library(jpeg)
library(imager)
library(grid)
library(gridExtra)
library(OpenImageR)

library(DT)
library(class)
library(e1071)
library(pROC)

shinyUI(
  
  dashboardPage(skin = "yellow",
                dashboardHeader(
                  title =  span(tags$b("MY APP (Designer: KAI-HSU)"),target = "_blank"),
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
                    menuItem(tags$b("胸部X光左心室功能障礙預測"), tabName = "X_tab"),
                    menuItem(tags$b("識別草帽一夥人!"), tabName = "ONE_tab"),
                    menuItem(tags$b("二元分類器比較"), tabName = "Class_tab"),
                    menuItem(tags$b("Panel Function"), tabName = "Function_tab"),
                    menuItem(tags$b("The feedback"), tabName = "feedback_tab"),
                    menuItem(tags$b("My works"), tabName = "work_tab")
                  )
                ),
                
                dashboardBody(
                  tabItems(
                    # X tab content
                    tabItem(tabName = "X_tab",
                            fileInput("files", label = h4("請上傳你的X光片:"), multiple = FALSE, accept = "image"),
                            helpText("Note1 :  圖片只能是JPEG檔案(.jpg/.jpeg)"),
                            helpText("Note2 :  你需要等待一下正在預測的時間~"),
                            uiOutput("plot1", height = "400px"),
                            #plotOutput("plot"),
                            br(),
                            verbatimTextOutput("summary"),
                            uiOutput("feedback1"),
                            fluidRow(
                              column(1, uiOutput("button1")),
                              tags$style(type='text/css', '#f1_out {color: rgba(227,71,71)}'),
                              column(6, uiOutput("f1_out"))    
                            )
                            
                            #verbatimTextOutput("C_index")
                    ),
                    
                    # ONE tab content
                    tabItem(tabName = "ONE_tab",
                            fileInput("files2", label = h4(tags$b("上傳一張圖片吧！")), multiple = FALSE, accept = "image"),
                            tags$b(helpText("Note1 :  圖片只能是JPEG檔案(.jpg/.jpeg)")),
                            tags$b(helpText("Note2 :  你需要等待一下正在預測的時間~")),
                            uiOutput("plot2", height = "400px"),
                            #plotOutput("plot2"),
                            br(), 
                            tags$style(type='text/css', '#summary2 {background-color: rgba(252,251,242,0.8); color: rgba(8,46,81)}'),
                            tags$b(verbatimTextOutput("summary2")),
                            uiOutput("feedback2"),
                            fluidRow(
                              column(1, uiOutput("button2")),
                              tags$style(type='text/css', '#f2_out {color: rgba(227,71,71)}'),
                              column(9, uiOutput("f2_out"))   
                            )
                            
                    ),
                    
                    # Class tab content
                    tabItem(tabName = "Class_tab",
                            
                            sidebarPanel(
                              fileInput("files3", label = h4("Upload your data file:"), multiple=FALSE),
                              helpText("Note:you only can upload the .csv file. "),
                              uiOutput("choose_columns5"),
                              uiOutput("choose_columns6"),
                              uiOutput("choose_columns7")
                              
                              # selectInput('method', 'Methods', choices = c(
                              #   'Linear Discriminant'=1,
                              #   'Logistic regression'=2 ,
                              #   'Naive Bayes Classification'=3,
                              #   'k-nearest neighbor algorithm'=4 ))
                            ),
                            mainPanel(
                              br(), 
                              verbatimTextOutput('summary3'),
                              br(),
                              plotOutput("roc_test1", height = 550, width = 550)
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
                                            multiple=TRUE, selectize=FALSE, selected = c("Option2", "Option3")),
                                "Press ctrl for multiple selection",
                                verbatimTextOutput('out1'),
                                selectInput('in2', 'multiple choice2:', state.name, multiple=TRUE, selectize=TRUE, selected = c("Option3", "Option4", "Option5")),
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
                                                   selected = c(1, 2)),
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
                                         tabPanel("mytable", DT::dataTableOutput("mytable1"))
                                ),
                                tabPanel("download", "You can export the data of the variables you have selected.",
                                         downloadButton("downloadData", "Download")
                                )
                                
                              )
                              
                            )
                            
                    ),
                    
                    # feedback tab content
                    tabItem(tabName = "feedback_tab",
                            h3(HTML("&nbsp;&nbsp;&nbsp;"), tags$b("讓我們了解一下使用者對於預測狀況的反饋 (◑‿◐)")),
                            
                            br(),
                            box(
                              h4(HTML("&nbsp;"), tags$b("▼ 胸部X光左心室功能障礙預測")), status = "primary", solidHeader = TRUE, width = 5,
                                #collapsible = TRUE,
                                br(),
                                tableOutput("X_table")),
                            box(
                              h4(HTML("&nbsp;"), tags$b("▼ 識別草帽一夥人")), status = "warning", solidHeader = TRUE,width = 4,
                                br(),
                                tableOutput("ONE_table"))
                           
                    ),
                    
                    tabItem(tabName = "work_tab",
                            uiOutput("work_portfolio")
                    )
                    
                  )
                )
  )
)

