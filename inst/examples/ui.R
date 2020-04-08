
source("infobox.R",local = TRUE)
source("override.R", local = TRUE) 


shinyUI(
  dashboardPage( title=  "Mr. Bean" , 
                 #--------------------------------  dashboardHeader  -------------------------------------------------------------
                 
                 dashboardHeader(title = tagList(img(src="beans3.png", height = 28, width = 32), " Mr.Bean") ,titleWidth = 200,
                                 tags$li(class="dropdown",tags$a(href="https://github.com/apariciojohan", icon("github"), "Code", target="_blank")),
                                 dropdownMenu(type = "message",messageItem(from = "Model",message = "This app has a database loaded by default",icon = icon("cogs"),time = Sys.time()),badgeStatus = "danger"),
                                 dropdownMenu(type = "tasks",taskItem(value=80,color="red",text = "Progress application"),
                                              taskItem(value=100,color="green",text = "SpATS"),
                                              taskItem(value=90,color="blue",text = "Lme4"),icon = icon("info-circle"),badgeStatus = "warning")
                 ),
                 #------------------------------  dashboardSidebar  --------------------------------------------------------------
                 
                 dashboardSidebar(       sidebarMenu(id = "tabs",
                                                     div(class="hide_when_sidebar_collapsed", br()),
                                                     div(class="hide_when_sidebar_collapsed",  img(src="CIAT4.png", height = 100, width = 199), style="text-align: center;"),
                                                     div(class="hide_when_sidebar_collapsed", br()),
                                                     menuItem("Home", tabName = "home", icon=icon("home")),
                                                     menuItem("Data", tabName = "data", icon=icon("database")),
                                                     menuItem("Spatial MLM", tabName = "model",icon = icon("braille"),
                                                              menuSubItem("SpATS", tabName = "modelo",icon = icon("arrow-circle-right")),
                                                              menuSubItem("BLUPs/BLUEs", tabName = "blupspat",icon = icon("arrow-circle-right")),
                                                              menuSubItem("Residuals", tabName = "resispat",icon = icon("arrow-circle-right"))),
                                                     menuItem("MLM",tabName = "mix",icon=icon("yelp"),
                                                              menuSubItem("Lme4", tabName = "mixed",icon = icon("arrow-alt-circle-right")),
                                                              menuSubItem("BLUPs/BLUEs", tabName = "blupsalfa",icon = icon("arrow-alt-circle-right"))),
                                                     menuItem("Comparison",tabName = "Comparison", icon=icon("glyphicon glyphicon-random",lib = "glyphicon")),
                                                     menuItem("About", tabName = "IB", icon = icon("leaf")),
                                                     br(),
                                                     div(class="hide_when_sidebar_collapsed" , tags$em("J.aparicio@cgiar.org"), style="text-align: center;")
                                                     
                 ),  
                 tags$script(HTML("$('body').addClass('sidebar-mini');"))),
                 #----------------------------------   dashboardBody -------------------------------------------------------------                 
                 
                 dashboardBody(tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
                                         tags$link(rel = "shortcut icon", type="image/x-icon", href="favicon.ico")),
                               useShinyjs(),useShinyalert(),introjsUI(),useToastr(),
                               setShadow("info-box"),
                               # information about value
                               v1,v2,v3,v4,v5,v6,v7,v8,
                               # --------------------------------- Home   TAB -----------------------------------------------------------------
                               tabItems(
                                 tabItem(tabName = "home",
                                         fluidRow(box(
                                           HTML(" <img src='beans3.png' width='52' vspace='10' hspace='10' height='48' align='left'> <font size='7'>Mr.Bean</font>
                                                "),
                                           br(),
                                           
                                           includeHTML("encabezado.html"),
                                           
                                           
                                           h3("Let's start"),
                                           width = 9,status = "primary",solidHeader = TRUE, title = tagList(shiny::icon("home"), "Home")
                                         ),
                                         
                                         fluidRow( 
                                           infoBox("Spatial",value =  "SpATS",fill=TRUE,subtitle = "P-Splines",width = 3,color = "blue",
                                                   href = "https://cran.r-project.org/web/packages/SpATS/SpATS.pdf",icon = icon("braille")),
                                           infoBox("Lme4", fill=TRUE,value =  "lmer",width = 3, icon = icon("line-chart"),color = "red",
                                                   subtitle = "Mixed-Model",href = "https://cran.r-project.org/web/packages/lme4/lme4.pdf"),
                                           infoBox("Effects", fill=TRUE,value = "BLUPs/BLUEs",width = 3,subtitle = "Selection" ,icon = icon("sort-numeric-up"),color = "yellow"),
                                           infoBox("Visualization", fill=TRUE,value = "ggplot2",subtitle = "plotly",width = 3, icon = icon("bar-chart"),color = "blue"),
                                           box(title = "Jump",status = "danger",width = 3,solidHeader = TRUE,
                                               actionLink(inputId = "toAwesome1", label = "Data", icon = icon("database"), style = "color: #d9534f"),br(),
                                               actionLink(inputId = "toAwesome2", label = "Spatial", icon = icon("braille"), style = "color: #d9534f"),br(),
                                               actionLink(inputId = "toAwesome3", label = "Lme4", icon = icon("bar-chart-o"), style = "color: #d9534f"),br(),
                                               actionLink(inputId = "toAwesome4", label = "Comparison", icon =icon("glyphicon glyphicon-random",lib = "glyphicon"), style = "color: #d9534f"),br()
                                           )
                                         )
                                         )
                                 ),
                                 
                                 #------------------------ Data -----------------------------------------
                                 
                                 tabItem(tabName = "data",
                                         fluidRow(box(title = tagList(shiny::icon("upload"), "Import data")   ,solidHeader = TRUE,status = "primary",
                                                      
                                                      radioGroupButtons(
                                                        inputId = "Id004",
                                                        choices = c("Example Data"=1, "Import Data"=2),
                                                        status = "primary",selected = 1
                                                      ),
                                                      conditionalPanel("input.Id004==1", h6('Use the example database to try the different modules of Mr. Bean')),
                                                      width = 3,prettyRadioButtons(inputId = "choice",label = "Choose an option", outline = TRUE,fill = FALSE,shape = "square",inline = TRUE,
                                                                                   choices=c("Dataset" = 1,"Plot" = 2, "Info-Box" = 3 ),icon = icon("check"),animation = "tada" )
                                         ),
                                         
                                         conditionalPanel("input.Id004==2", 
                                                          box(title = "Import Data",solidHeader = TRUE,
                                                              fileInput(inputId='file1',
                                                                        label='Load your database',
                                                                        accept = c(
                                                                          'text/csv',
                                                                          'text/comma-separated-values',
                                                                          'text/tab-separated-values',
                                                                          'text/plain',
                                                                          '.csv',
                                                                          '.tsv','xlsx'
                                                                        )),helpText("Default max. file size is 5MB"),
                                                              prettyCheckbox(
                                                                inputId = "header", label = "Header?", icon = icon("check"),outline = TRUE,fill = FALSE,shape="square",
                                                                animation = "tada", value=TRUE,status = "primary"
                                                              ),width = 3, status = "primary"
                                                          ),
                                                          shinyjs::hidden(
                                                            div(id = "when_file1",
                                                                box(title = "Attributes",solidHeader = TRUE,
                                                                    radioButtons(inputId="miss",label="Missing value character: ",choices = list("NA",'Empty'),inline = T),
                                                                    selectInput(inputId="sep",
                                                                                label = "Cell separation character:", 
                                                                                choices = list(Tab='\t', Comma=',',
                                                                                               Semicolon=';', 'Space'=' '),
                                                                                selected = ';'),
                                                                    uiOutput("oshet"), # sheet
                                                                    width = 3, status = "primary"))),
                                                          # SUbset DATA
                                                          shinyjs::hidden(
                                                            div(id = "when_file2",
                                                                box(title = "Subset",solidHeader = TRUE,
                                                                    prettyCheckbox(
                                                                      inputId = "subset", label = "Subset of data?", icon = icon("check"),outline = TRUE,fill = FALSE,shape="square",
                                                                      animation = "tada", value=FALSE,status = "primary"
                                                                    ),
                                                                    selectInput(inputId="varsubset",
                                                                                label= tagList( "Subset variable",tags$a(icon("exclamation-circle"))),
                                                                                choices=""),
                                                                    selectInput(inputId="levelessub",
                                                                                label= tagList( "Which level?",tags$a(icon("exclamation-circle"))),
                                                                                choices=""),
                                                                    width = 3, status = "primary")))
                                                          
                                         )
                                         ),
                                         
                                         conditionalPanel(condition="input.choice==1",
                                                          fluidRow(box(withSpinner( DT::dataTableOutput("Rawdata"),type = 5,color = "#337ab7" ),style = "overflow-x: scroll;",
                                                                       width = 12,title = "DATA",status = "primary",solidHeader = TRUE,collapsible = TRUE) ),br() ),
                                         
                                         conditionalPanel(condition="input.choice==2",
                                                          fluidRow( box( 
                                                            dropdown(
                                                              
                                                              tags$h3("List of Input"),
                                                              
                                                              uiOutput("varx"),
                                                              uiOutput("vary"),uiOutput("factor2"),
                                                              actionButton("actionplot", label = "Plot",style="display:rigth ;color: white  ; background-color: #337ab7") ,
                                                              
                                                              animate = animateOptions(
                                                                enter = animations$fading_entrances$fadeInLeftBig,
                                                                exit = animations$fading_exits$fadeOutLeftBig
                                                              ),
                                                              style = "unite", icon = icon("gear"),
                                                              status = "warning", width = "300px"
                                                            ),withSpinner( plotlyOutput("plot"),type = 5,color = "#337ab7"),
                                                            width = 6,title = "Scatterplot",status = "primary",solidHeader = TRUE),
                                                            box(dropdown(
                                                              
                                                              tags$h3("List of Input"),
                                                              uiOutput("varresponse"),
                                                              uiOutput("box"),
                                                              actionButton("actionbox", label = "Boxplot",style="display:rigth ;color: white  ; background-color: #337ab7"),
                                                              animate = animateOptions(
                                                                enter = animations$fading_entrances$fadeInRightBig,
                                                                exit = animations$fading_exits$fadeOutRightBig
                                                              ),
                                                              style = "unite", icon = icon("gear"),
                                                              status = "warning", width = "300px"
                                                            ),
                                                            withSpinner( plotlyOutput("boxplot"),type = 5,color = "#337ab7"),
                                                            width = 6,title = "Boxplot",status = "primary",solidHeader = TRUE) ),br() ),
                                         conditionalPanel(condition="input.choice==3",
                                                          fluidRow(
                                                            box(width = 3,status = "danger",
                                                                pickerInput(
                                                                  inputId = "Id088",
                                                                  label = "Select variable", 
                                                                  choices = "",
                                                                  options = list( style = "btn-danger",size = 5)))),
                                                          
                                                          fluidRow(  valueBoxOutput("resp",width = 3),valueBoxOutput("min",width = 3),
                                                                     valueBoxOutput("obs",width = 3), valueBoxOutput("IQR", width = 3)),
                                                          
                                                          fluidRow(valueBoxOutput("na", width = 3),valueBoxOutput("sd", width = 3), 
                                                                   valueBoxOutput("mean",width = 3), valueBoxOutput("median", width = 3))
                                         )
                                         
                                 ),
                                 
                                 # ------------------ Modelo TAB ------------------------------------------------------
                                 
                                 
                                 tabItem(tabName = "modelo",
                                         fluidRow( infoBox("Spatial Model",value = " ",subtitle = "Mixed Model with P-Splines",color = "blue",icon("cogs"),width = 12)   ),
                                         
                                         fluidRow(
                                           
                                           
                                           box( width = 3,status = "primary", solidHeader = TRUE,
                                                actionButton("btn", tagList(icon=icon("question-circle"), "Guide"),style= " color: white ; background-color: #dd4b39"),hr(),
                                                introBox(
                                                  
                                                  selectInput(inputId="variable",
                                                              label= tagList( "Response variable",tags$a(icon("exclamation-circle"),id="q1")),
                                                              choices=""),
                                                  data.step = 1,data.intro = "Select the column that contains the phenotypic response variable.",data.position = "right",color="red") ,
                                                introBox(
                                                  selectInput(inputId="genotipo",
                                                              label=tagList( "Genotype",tags$a(icon("exclamation-circle"),id="q2")),
                                                              choices=""),
                                                  checkboxInput(inputId='res_ran',
                                                                label='Random Genotype?', 
                                                                value=TRUE),data.step = 2,data.intro = "Select the column that contains the genotype IDs.
                                                                                         Check/Uncheck the box if you want to treat this as a random/fixed
                                                                                              effect factor in the MLM.",data.position = "right",color="red"   ) ),
                                           
                                           
                                           box(width = 2, status = "primary", solidHeader = TRUE,collapsible = TRUE ,
                                               title=tagList(icon=icon("th"), "Coords"),
                                               introBox(
                                                 selectInput(inputId="column",label=tagList( "Column",tags$a(icon("exclamation-circle"),id="q3")),  choices=""),
                                                 selectInput(inputId="fila",
                                                             label=tagList( "Row",tags$a(icon("exclamation-circle"),id="q4")),
                                                             choices=""),data.step = 3,data.intro = "Select the columns in your dataset that contain the Row and Column coordinates for the plots in your trial.",data.position = "right" ) ),
                                           
                                           
                                           box(width = 2, title =  'Factors',status = "primary", solidHeader = TRUE,collapsible = TRUE ,
                                               introBox(
                                                 selectizeInput("show_fixed", "Fixed ",
                                                                choices = "", multiple = TRUE),
                                                 
                                                 selectizeInput("show_random", "Random",
                                                                choices = "", multiple = TRUE),
                                                 data.step = 4,data.intro = "In case you want to include additional qualitative variables in the MLM, select them here as either fixed or random effect factors.",data.position = "bottom",color="red"),
                                               introBox(
                                                 selectizeInput("covariate", "Covariate",
                                                                choices = "", multiple = TRUE,selected=NULL),
                                                 data.step = 5,data.intro = "In case you want to include additional quantitative variables in the MLM.",data.position = "bottom",color="red")),
                                           box(width = 2, status = "primary",solidHeader = TRUE, title=tagList(icon=icon("tasks"), "Model"),collapsible = TRUE ,
                                               introBox(
                                                 actionButton("action", label = "Run Model",style="display:rigth; color: white ; background-color: #337ab7"),
                                                 actionButton("inf",label = "Info-Box",style="display:rigth"),
                                                 br(),br(),
                                                 actionButton("spatial",label = "Spatial Trend",style="display:rigth"),
                                                 br(),hr(),
                                                 actionButton("tabBut", "View BLUPs"),
                                                 data.step=6,data.intro = "Use this control panel to run the model and display the 3D spatial trend.",data.hint = "Good",data.position = "bottom-middle-aligned")
                                           ),
                                           bsModal("modalExample", "BLUPs/BLUEs", "tabBut", size = "large",
                                                   withSpinner(dataTableOutput("distTable"),type = 6,color = "#337ab7"),
                                                   br(), br(),
                                                   downloadButton("downloadData", "Download BLUPs/BLUEs",style= " color: white ; background-color: #337ab7; float:left"),br()),
                                           
                                           box(width = 3, status = "primary",solidHeader = TRUE,title = "Segments and Report",collapsible = TRUE ,
                                               introBox(
                                                 materialSwitch("able",label = "Enable",status = "primary"),
                                                 uiOutput("segcol"),uiOutput("segrow"),data.step=7,data.intro="Enable this box in case you have a large-scale trial (nColumns > 100 ; nRows > 100)
                                                   to reduce the number of segments for smoothing the spatial components in the MLM.",data.position = "bottom" ),
                                               radioButtons('format', 'Report format', c('PDF', 'HTML', 'Word'), inline = TRUE),
                                               downloadButton('downloadReport'))
                                           
                                         ),
                                         
                                         fluidRow(valueBoxOutput("heritability",width = 3),valueBoxOutput("maxline",width = 3),
                                                  valueBoxOutput("minline",width = 3), valueBoxOutput("cv",width = 3)),
                                         
                                         fluidRow(
                                           
                                           box(status = "primary",width = 6, collapsible = TRUE,collapsed = FALSE,title=tagList(icon=icon("braille"), "Model"),solidHeader = TRUE,
                                               tabBox(width = 12,selected = "Summary",
                                                      tabPanel("Summary", h5("First upload your data and fill the required fields."), withSpinner(verbatimTextOutput("summary2"),type = 5,color = "#337ab7"),icon = icon("arrow-circle-right")),
                                                      tabPanel("Plot",
                                                               dropdown(
                                                                 prettyRadioButtons(inputId = "typefile",label = "Filetype for download plot", outline = TRUE,fill = FALSE,shape = "square",inline = TRUE,
                                                                                    choices = list("png","pdf"),icon = icon("check"),animation = "tada" ),
                                                                 conditionalPanel(condition="input.typefile=='png'", sliderInput(inputId="png.wid",min = 200,max = 2000,value = 900,label = "Width pixels") ,
                                                                                  sliderInput(inputId="png.hei",min = 200,max = 2000,value = 600,label = "Height pixels")),
                                                                 conditionalPanel(condition="input.typefile=='pdf'", sliderInput(inputId="pdf.wid",min = 2,max = 20,value = 10,label = "Width") ,
                                                                                  sliderInput(inputId="pdf.hei",min = 2,max = 20,value = 8,label = "Height")),
                                                                 
                                                                 downloadButton("descargar", "Download the plot",style= " color: white ; background-color: #337ab7"), br() ,
                                                                 animate = animateOptions(
                                                                   enter = animations$fading_entrances$fadeInLeftBig,
                                                                   exit = animations$fading_exits$fadeOutLeftBig
                                                                 ),
                                                                 style = "unite", icon = icon("gear"),
                                                                 status = "warning", width = "300px"
                                                               ),
                                                               withSpinner(plotOutput("plot_spats"),type = 5,color = "#337ab7"),icon = icon("th")),
                                                      tabPanel("Var-Components",icon = icon("signal"), withSpinner(plotlyOutput("varcomp",height = "500px"),type = 5,color = "#337ab7"))
                                               )
                                           ),
                                           box(status = "primary",width = 6,collapsible = TRUE,collapsed = FALSE,title = "Spatial Trend" ,solidHeader = TRUE,
                                               withSpinner(plotlyOutput("trend"),type = 5,color = "#337ab7"))
                                                  
                                         )       
                                         
                                 ),
                                 
                                 # --------------------- BLUPS spatial Model ---------------------------------------
                                 
                                 tabItem(tabName = "blupspat",fluidRow(infoBox("Spatial Model",value = " ",subtitle = "BLUPs / BLUEs",color = "blue",icon("cogs"),width = 12)   ),
                                         fluidRow(column(12, box(withSpinner(plotlyOutput("plotblup2"),type = 5,color = "#337ab7"),title = textOutput("efectos"), solidHeader =  TRUE,status = "primary",width = 12))), 
                                         fluidRow(column(12,
                                                         fluidRow(column(box(withSpinner(DT::dataTableOutput("blups"),type = 5,color = "#337ab7"),width = 12,title = "Effects",status = "danger",solidHeader = TRUE,
                                                                             downloadButton("downloadData2",label = "Download")),width = 6),
                                                                  column(box(withSpinner(plotlyOutput("hist"),type=5,color = "#337ab7"),width = 12,title = "Histogram",status = "primary",solidHeader = TRUE),width = 6 ))
                                                         
                                         ))
                                         
                                         
                                 ),
                                 
                                 # --------------------- RESIDUAL spatial Model ---------------------------------------
                                 
                                 tabItem(tabName = "resispat",
                                         fluidRow(column(width=6,
                                                         box(title = "Raw-data",status = "danger",width = 12,solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,
                                                             
                                                             prettyRadioButtons(inputId = "out",label = "Outliers methods", outline = TRUE,fill = FALSE,shape = "curve",inline = TRUE,
                                                                                choices = list("MAD","IQR"),icon = icon("check"),animation = "tada" ),
                                                             withSpinner( plotlyOutput("plotati"),type = 5,color = "#337ab7")
                                                         ),
                                                         box(withSpinner(DT::dataTableOutput("OUT"),type = 5,color = "#337ab7"),width = 12,style = "overflow-x: scroll;",
                                                             status = "primary",title = "Outliers Raw-data",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE),
                                                         
                                                         checkboxGroupButtons(
                                                           inputId = "Id059",
                                                           label = "Download dataset without outliers in:",
                                                           choices = c("MAD", 
                                                                       "IQR", "Residuals"),
                                                           status = "primary",
                                                           selected = c("MAD"),
                                                           checkIcon = list(
                                                             yes = icon("ok", 
                                                                        lib = "glyphicon"),
                                                             no = icon("remove",
                                                                       lib = "glyphicon"))
                                                         ),downloadButton("downloadout", "Download Data cleaned",style= " color: white ; background-color: #337ab7")
                                         )
                                         ,
                                         column(width=6,
                                                box(width = 12,solidHeader = TRUE,status = "primary",title = "Residuals",collapsible = TRUE,collapsed = TRUE,
                                                    tabBox(width = 12,selected = "Residual vs Index",
                                                           tabPanel("Residual vs Index", helpText("First run the model Spats"),
                                                                    withSpinner(plotlyOutput("plotati2"),type = 5,color = "#337ab7"),icon = icon("arrow-circle-right")),
                                                           tabPanel("Plot", withSpinner(plotlyOutput("mapout"),type = 5,color = "#337ab7"),icon = icon("th")),
                                                           tabPanel("Residual vs Fitted", withSpinner(plotlyOutput("fitted"),type = 5,color = "#337ab7"),icon = icon("ellipsis-h")),
                                                           tabPanel("QQplot",withSpinner(plotlyOutput("qqplot"),type = 5,color = "#337ab7"),icon = icon("ellipsis-h"))
                                                    )),
                                              
                                                box(withSpinner(DT::dataTableOutput("OUT2"),type = 5,color = "#337ab7"),width = 12,style = "overflow-x: scroll;",
                                                    status = "primary",title = "Outliers Residuals",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE)
                                         )
                                         )
                                         
                                         
                                         
                                 ),
                                 
                                 
                                 #------------------------  Mixed Model   -------------------------------------
                                 
                                 tabItem(tabName = "mixed",
                                         fluidRow( infoBox("Mixed Model",value = " ",subtitle = "Experimentals designs",color = "green",icon("cogs"),width = 8),
                                                   valueBoxOutput("rep",width = 2),valueBoxOutput("block",width = 2)),
                                         fluidRow(
                                           
                                           box(title =  tagList(shiny::icon("question-circle"), "Help"), 
                                               solidHeader = TRUE, footer =  "Let's start",width =4,status = "danger",
                                               h3("Factors in Lme4 package"),
                                               hr(),
                                               HTML("<ul>
                                            <li><strong>Fixed:</strong> Rep + Block  </li>
                                            <li><strong>Random:</strong> (1|Rep) + (1|Block) </li>
                                            <li><strong>Interaction Fixed:</strong> Rep:Block </li>
                                            <li><strong>Interaction Random:</strong> (1|Rep:Block) </li>
                                            </ul>  ")),
                                           box( width = 2,status = "primary", solidHeader = TRUE,
                                                background = "blue",selectInput(inputId="variable2",
                                                                                label="Response variable",choices=""),
                                                
                                                selectInput(inputId="genotipo2",
                                                            label="Genotype",
                                                            choices=""),
                                                checkboxInput(inputId='res_ran2',
                                                              label='Random Genotype?', 
                                                              value=TRUE)),
                                           box( width = 2,status = "primary",solidHeader = TRUE,title = "Model",
                                                prettyRadioButtons(
                                                  inputId = "Id039",
                                                  label = "Choose:", 
                                                  choices = c("Lattice"=1, "RCBD"=2, "Other model"=3),
                                                  icon = icon("check"), 
                                                  bigger = TRUE,
                                                  status = "primary",
                                                  animation = "jelly"
                                                ),
                                                actionButton("run",label = "Run",icon = icon("check")),
                                                actionButton("infomix",label = "Info Model",style="display:rigth ;color: white  ; background-color: #00a65a",icon = icon("info"))),
                                           conditionalPanel(condition="input.Id039==1|input.Id039==2",
                                                            box( width = 4,status = "primary",solidHeader = TRUE,title="Components", 
                                                                 pickerInput(
                                                                   inputId = "Id086",
                                                                   label = "Replicate", 
                                                                   choices = c(""),
                                                                   options = list(
                                                                     title = "Rep",size = 5)
                                                                 ),
                                                                 conditionalPanel(condition="input.Id039==1", pickerInput(
                                                                   inputId = "Id087",
                                                                   label = "Block", 
                                                                   choices = c(""),
                                                                   options = list(
                                                                     title = "Block",size = 5)
                                                                 )),
                                                                 selectizeInput("Id089", "Covariate ",
                                                                                choices = "", multiple = TRUE)
                                                            ) ),
                                           bsModal(id = "mixedmodal",title = "Information about Mixed Model",trigger = "infomix",size = "large",
                                                   fluidRow( valueBoxOutput("varres",width = 4),
                                                             valueBoxOutput("vargen",width = 4),
                                                             valueBoxOutput("hcullis",width = 4)),
                                                   DT::dataTableOutput("glance"), 
                                                   withSpinner(verbatimTextOutput("printranova"),type = 5,color = "#337ab7"), 
                                                   DT::dataTableOutput("anovamix") ,br()  
                                           ),
                                           conditionalPanel(condition="input.Id039==3",
                                                            fluidRow(
                                                              box(title = "Formula",status = "primary",solidHeader = TRUE , 
                                                                  textInput("formula",label = h5("Write the formula for the mixed model"),
                                                                            value = "" , placeholder = " (1|Line) + (1|Rep) + (1|Rep:Block)"), 
                                                                  infoBoxOutput("form",width = 12),   width = 4)))
                                           
                                         ),
                                         
                                         fluidRow( box(width = 6,status = "primary", solidHeader = TRUE, 
                                                       title=tagList(icon=icon("leaf"), "Summary Model"),
                                                       withSpinner(verbatimTextOutput("summaryalpha"),type = 6,color = "#337ab7")),
                                                   box(width = 6,status = "primary", solidHeader = TRUE, style = "overflow-x: scroll;",
                                                       title=tagList(icon=icon("sort-numeric-up "), "Effects"),
                                                       withSpinner(DT::dataTableOutput("blups_mixed"),type = 6,color = "#337ab7"),
                                                       downloadButton("desc_mixed", "Download table",style= " color: white ; background-color: #337ab7")))
                                         
                                 ),
                                 
                                 tabItem(tabName = "blupsalfa",
                                         fluidRow(box(withSpinner(plotlyOutput("plot_effects"),type = 6,color = "#337ab7"),  
                                                      width = 12,title = tagList(shiny::icon("sort-numeric-up"), "Effects"),status = "primary",solidHeader = TRUE) ),
                                         
                                         fluidRow(column(width=6,
                                                         box(width = 12,solidHeader = TRUE,status = "primary",title = tagList(shiny::icon("bar-chart-o"), "Residuals"),collapsible = TRUE,collapsed = TRUE,
                                                             tabBox(width = 12,selected = "Residual vs Index",
                                                                    tabPanel("Residual vs Index", helpText("First run the Mixed Model"),
                                                                             withSpinner(plotlyOutput("plotati3"),type = 5,color = "#337ab7"),icon = icon("arrow-circle-right")),
                                                                    tabPanel("Residual vs Fitted", withSpinner(plotlyOutput("fitted2"),type = 5,color = "#337ab7"),icon = icon("ellipsis-h")),
                                                                    tabPanel("QQplot",withSpinner(plotlyOutput("Normality"),type = 5,color = "#337ab7"),icon = icon("ellipsis-h"))
                                                             ))
                                                         
                                                         
                                         ),
                                         column(width=6, box(withSpinner(DT::dataTableOutput("OUT3"),type = 5,color = "#337ab7"),width = 12,style = "overflow-x: scroll;",
                                                             status = "primary",title = "Outliers Residuals",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE))
                                         
                                         )
                                         ),
                                 
                                 
                                 # -------  Comparison  ------------------------------------------------------------------------
                                 
                                 tabItem(tabName = "Comparison",
                                         fluidRow(
                                           box(fileInput(inputId='file2',
                                                         label='Load your database',
                                                         accept = c(
                                                           'text/csv',
                                                           'text/comma-separated-values',
                                                           'text/tab-separated-values',
                                                           'text/plain',
                                                           '.csv',
                                                           '.tsv','xlsx','xls'
                                                         )),helpText("Default max. file size is 5MB"),
                                               prettyCheckbox(
                                                 inputId = "header2", label = "Header?", icon = icon("check"),outline = TRUE,fill = FALSE,shape="square",
                                                 animation = "tada", value=TRUE,status = "primary"
                                               ),
                                               radioButtons(inputId="miss2",label="Missing value character: ",choices = list("NA",'Empty'),inline = T),
                                               selectInput(inputId="sep2",
                                                           label = "Cell separation character:", 
                                                           choices = list(Tab='\t', Comma=',',
                                                                          Semicolon=';', 'Space'=' '),
                                                           selected = ';'),width = 4, status = "primary",title = tagList(shiny::icon("upload"), "Import data") ,
                                               solidHeader = TRUE,collapsible = TRUE
                                           ),
                                           box(DT::dataTableOutput("pru"),style = "overflow-x: scroll;", status = "primary",title = "Data imported",solidHeader = TRUE,
                                               collapsible = TRUE,collapsed = FALSE,width = 5),
                                           box(width = 3,title = "Action",solidHeader = TRUE,status = "danger",collapsible = TRUE,
                                               prettyRadioButtons(
                                                 inputId = "pca",
                                                 label = "Choose:", 
                                                 choices = c("Scree Plot"=1, "BiPlot"=2, "Graph of Variables"=3,"Graph of Individuals"=4),
                                                 icon = icon("check"), 
                                                 bigger = TRUE,
                                                 status = "primary",
                                                 animation = "jelly"
                                               ) ,
                                               actionButton("compa", label = "Plot",style="display:rigth ;color: white  ; background-color: #337ab7"))),
                                         fluidRow(
                                           box(solidHeader = TRUE,status = "primary",title = "Correlation",width = 6,collapsible = TRUE,collapsed = TRUE,
                                               tabBox(title = " ",width = 12,
                                                      tabPanel("Plot",
                                                               dropdown(
                                                                 
                                                                 tags$h3("List of Input"),
                                                                 
                                                                 uiOutput("varx2"),
                                                                 uiOutput("vary2"),
                                                                 actionButton("corr", label = "Cor",style="display:rigth ;color: white  ; background-color: #337ab7") ,
                                                                 
                                                                 animate = animateOptions(
                                                                   enter = animations$fading_entrances$fadeInLeftBig,
                                                                   exit = animations$fading_exits$fadeOutLeftBig
                                                                 ),
                                                                 style = "unite", icon = icon("gear"),
                                                                 status = "warning", width = "300px"
                                                               ),withSpinner( plotOutput("pares"),type = 5,color = "#337ab7")  )
                                                      ,
                                                      tabPanel("Matriz",
                                                               withSpinner( plotOutput("correlation",height = "600px"),type = 5,color = "#337ab7")  )
                                               )),
                                           
                                           box(dropdown(
                                             prettyRadioButtons(inputId = "typefile_comp",label = "Filetype for download plot", outline = TRUE,fill = FALSE,shape = "square",inline = TRUE,
                                                                choices = list("png","pdf"),icon = icon("check"),animation = "tada" ),
                                             conditionalPanel(condition="input.typefile_comp=='png'", sliderInput(inputId="png.wid_comp",min = 200,max = 2000,value = 900,label = "Width pixels") ,
                                                              sliderInput(inputId="png.hei_comp",min = 200,max = 2000,value = 600,label = "Height pixels")),
                                             conditionalPanel(condition="input.typefile_comp=='pdf'", sliderInput(inputId="pdf.wid_comp",min = 2,max = 20,value = 10,label = "Width") ,
                                                              sliderInput(inputId="pdf.hei_comp",min = 2,max = 20,value = 8,label = "Height")),
                                             
                                             downloadButton("descargar_comp", "Download the plot",style= " color: white ; background-color: #337ab7"), br() ,
                                             animate = animateOptions(
                                               enter = animations$fading_entrances$fadeInRight,
                                               exit = animations$fading_exits$fadeOutRight
                                             ),
                                             style = "unite", icon = icon("gear"),
                                             status = "warning", width = "300px"
                                           ),
                                           withSpinner( plotOutput("biplot",height = "600px"),type = 5,color = "#337ab7"),
                                           title = "Principal Components Analysis",status = "primary",collapsed = TRUE,collapsible = TRUE,solidHeader = TRUE))
                                 ),
                                 
                                 
                                 
                                 
                                 #-----------------  Info TAB  ------------------------------------------------------
                                 
                                 tabItem(tabName = "IB", 
                                         navbarPage(title=div(icon("jsfiddle"),"Rsession"),fluid = TRUE),
                                         verbatimTextOutput("Rsession"),
                                         br()     
                                 )
                               ),
                               
                               # ---------- FOOTER PIE ---------------
                               
                               tags$footer(HTML("© 2019 | All Rights Reserved | <a style='text-decoration:none' href='https://github.com/apariciojohan'> Aparicio Johan </a>  "), align = "center", style = "
                                      position:absolute;
                                      bottom:0%;
                                      width:100%;
                                      height:40px;   /* Height of the footer */
                                      color: grey ;
                                      padding: 10px;
                                      background-color: ht-blue;
                                      z-index: 1000;")))
  
)





