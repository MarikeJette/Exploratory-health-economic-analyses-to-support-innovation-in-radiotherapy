#####################################################################################################################
#                                                                                                                   #
#    Shiny application Exploratory health economic analyses to support innovation in radiotherapy                   #
#                                                                                                                   #
#                                               by: Marike Ulehake                                                  #  
#                                                                                                                   #  
#####################################################################################################################

rm(list = ls())

## Load packages 
if (!require('dplyr')) install.packages('dplyr'); library(dplyr)
if (!require('shiny')) install.packages('shiny'); library(shiny)
if (!require('shinydashboard')) install.packages('shinydashboard'); library(shinydashboard)
if (!require('ggplot2')) install.packages('ggplot2'); library(ggplot2)
if (!require('shinycssloaders')) install.packages('shinycssloaders'); library(shinycssloaders)
if (!require('shinyBS')) install.packages('shinyBS'); library(shinyBS)

## Load functions 
source("Calc_shiny.R")


## Define UI for application 
ui <- function(request) {
  dashboardPage(
    dashboardHeader(
      tags$li(class = "dropdown",
              tags$style(".main-header {max-height: 100px}"),
              tags$style(".main-header .logo {height: 80px}")),
      title = "Exploratory health economic analyses to support innovation in radiotherapy",
      titleWidth = '100%'
    ),
                  
    # Sidebar with sliders for input
    dashboardSidebar(width = 300, 
      sidebarMenu(
        br(), 
        br(),
        actionButton("reset_input", "Reset"), 
        actionButton("go", "Calculate", style = 'padding:10px ; font-size:100%'), 
        h4(style='padding:20px;color:#404040;', "Conventional treatment"),
                sliderInput("f_L", 
                    "Number of fractions per patient",
                    min = 0, max = 50, ticks = TRUE, 
                    value = 20), 
        sliderInput("tf_L", 
                    "Time per fraction (minutes)",
                    min = 0, max = 60, ticks = T, 
                    value = 15), 
#        numericInput("a_t_L", 
#                     "Maximum active time per year (hours)",
#                     step = 1,
#                     value = 1813),
#        sliderInput("occ_t_L", 
#                    "Occupancy rate coventional linac",
#                    min = 0, max = 1, ticks = T, 
#                    value = 1),
        h4(style='padding:20px;color:#404040;', "MR-Linac treatment"),
        sliderInput("f_MRL", 
                    "Number of fractions per patient",
                    min = 0, max = 50, ticks = TRUE, 
                    value = 5),
        sliderInput("tf_MRL", 
                    "Time per fraction (minutes)",
                    min = 0, max = 90, ticks = T, 
                    value = 60), 
#        numericInput("a_t_MRL", 
#                     "Maximum active time per year (hours)",
#                     step = 1,
#                     value = 1813),
#        sliderInput("occ_t_MRL", 
#                    "Occupancy rate",
#                    min = 0, max = 1, ticks = T, 
#                    value = 0.7),
        br(),
        sliderInput("wtp", 
                    "Cost-effectiveness threshold",
                    min = 0, max = 100000, ticks = 10000, 
                    value = 80000),
        startExpanded = TRUE)
    ),
        
    # Main panel for displaying outputs
    dashboardBody(#F5F5F5
      tags$head(tags$style(HTML('
                             .result-p {
                                                  font-size: 30px; color: black; font-family: Arial;
                                                             }
    
                            /* changes the colour of the bars */ 
                            .js-irs-0  .irs-single, .js-irs-0  .irs-bar-edge, .js-irs-0 .irs-bar {
                                                  background: #3c8dbc;
                                                  border-top: 1px solid #3c8dbc ;
                                                  border-bottom: 1px solid #3c8dbc ;}

                            /* changes the colour of the number tags */
                           .irs-from, .irs-to, .irs-single { background: #3c8dbc; color: #FFFFFF }
                           
                           .irs-min, .irs-max, .irs-grid, .irs-grid-text {
                                                    color: #000000;}
                                                
                                                  
                                                    
                           .sidebar .irs-min, .sidebar .irs-max{
                                                    color: #000000; background: #ffffff}
                           
                           
                             /* changes the colour of the bars */ 
                            .js-irs-1  .irs-single, .js-irs-1  .irs-bar-edge, .js-irs-1 .irs-bar {
                                                  background: #3c8dbc;
                                                  border-top: 1px solid #3c8dbc ;
                                                  border-bottom: 1px solid #3c8dbc ;}
                                                  
                                                       /* changes the colour of the bars */ 
                            .js-irs-2  .irs-single, .js-irs-2  .irs-bar-edge, .js-irs-2 .irs-bar {
                                                  background: #3c8dbc;
                                                  border-top: 1px solid #3c8dbc ;
                                                  border-bottom: 1px solid #3c8dbc ;}
                                                  
                                                    .js-irs-3  .irs-single, .js-irs-3  .irs-bar-edge, .js-irs-3 .irs-bar {
                                                  background: #3c8dbc;
                                                  border-top: 1px solid #3c8dbc ;
                                                  border-bottom: 1px solid #3c8dbc ;}
    
    
    
    
    
    
    
                              .tabbable > .nav > li > a                  { background-color: white;  color:black}
       
       
                            /* logo */
                                .skin-blue .main-header .logo {
                                background-color: #3c8dbc;
                                }
                                /* logo when hovered */
                                .skin-blue .main-header .logo:hover {
                                background-color: #3c8dbc;
                                }
                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                background-color: #93383F;
                                }
                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color: #dedbd3;
                             
                                }
                               
                                .skin-blue .sidebar {
                                color: #404040;
                                }
                                /* active selected tab in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                background-color: #333b43;
                                }
                                /* other links in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                background-color: #333b43;
                                color: #333b43;
                                }
                                /* other links in the sidebarmenu when hovered */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                                background-color: #333b43;
                                }
                                /* toggle button when hovered  */
                                .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                background-color: #333b43;
                                }
                                /* body */
                                .content-wrapper, .right-side {
                                background-color: #FFFFFF;
                                }
                                
                                .info-icon {
                                font-size: 12px;
                                margin-left: 5px;
                                vertical-align: middle
                                
                                }
                            '))),
      
      
      h6("Authors: Marike J Ulehake; Ellen JL Brunenberg; Marcel Verheij; Janneke PC Grutters"),
      h6("Cite this article as: [not published yet]"),
      br(), 
      
      tags$hr(),
      tags$style(HTML("hr {border-top: 1px solid #000000;}")),
      
      
      tabsetPanel(type = "tabs",
                  
                  # START TAB ----------
                  tabPanel("Information",
                           # Background sectie
                           h4("Background"),
                           br(),
                           p("There is an increasing need to understand the value of radiotherapeutic innovations before they are widely adopted. For multi-purpose innovations such as the MR-Linac, determining its value within a specific clinical context does not conclusively answer the broader question of whether the innovation justifies the required investment. The MR-Linac is expensive to acquire, necessitating significant initial investments in procurement, construction, staff training, and ongoing maintenance. In the early stages of innovation, evidence of added value is often lacking, prompting hospitals and society to question whether such investments in new technologies like the Magnetic Resonance Linear Accelerator (MR-Linac) are worthwhile and offer sufficient return on investment."),
                           p("Exploratory analyses can be particularly informative during the early development and initial use of such innovations. With this tool, we aim to stimulate early, informed dialogues among stakeholders to ensure that decisions regarding the adoption and implementation of MR-guided radiotherapy (MRgRT) are based on both clinical and economic considerations."),
                           br(), 
                           # instruction of how to use the tool
                           h4("How to use the tool"),
                           br(),
                           p("Explore the additional costs and effects of MRgRT compared to conventional treatment, where conventional treatment refers to image-guided radiation therapy without plan adaptation on a linear accelerator with cone-beam CT."),
                           p("Adjust the input variables based on your scenario by modifying the default values in the left sidebar and input tabs as needed. Click on the 'Calculate' button in the left corner to generate tables and graphs showing the relationship between variables such as the number of fractions, time per fraction, acquisition costs, total available time, and occupancy rate, and the cost differences between MR-Linac and conventional treatment. Each graph illustrates how changes in a specific variable impact the cost difference and the required health benefits (in QALYs)."),
                           p("By default, the tool uses hypothetical but realistic values based on the Dutch context. Use the reset button to clear all inputs. For a detailed description of the tool, please refer to the publication [not published yet]."),
                           br(), 
                           br() 
                    
                    
                  ),

                  ### COST TAB
                  tabPanel("Medical devices input",
                           wellPanel(style = "background: white",
                                     h5("By default, input for the Dutch clinical context is used."),
                                     br(),
                                     fluidRow(
                                       column(12,
                                              h4("Medical devices purchase costs"),
                                              br(),
                                              fluidRow(
                                                column(6, 
                                                       h4("Conventional linac"),
                                                       br(),
                                                       numericInput("c_aanschaf_L",
                                                                    "Acquisition costs (€)",
                                                                    step = 5000,
                                                                    value = round(2000000, 0)),
                                                       numericInput("c_bunk_L",
                                                                    "Bunker construction (€)",
                                                                    step = 1000,
                                                                    value = round(500000, 0)),
                                                       numericInput("c_onderhoud_L",
                                                                    "Annual maintenance costs (€)",
                                                                    step = 100,
                                                                    value = round(140000, 0)),
                                                       numericInput("c_qaequip_L",
                                                                    "Costs of additional equipment (for instance QA equipment) (€)",
                                                                    step = 100,
                                                                    value = round(0, 0)),
                                                ),
                                                column(6,
                                                       h4("MR-Linac"),
                                                       br(),
                                                       numericInput("c_aanschaf_MRL",
                                                                    "Acquisition costs (€)",
                                                                    step = 5000,
                                                                    value = round(9500000, 0)),
                                                       numericInput("c_bunk_MRL",
                                                                    "Bunker construction (€)",
                                                                    step = 5000,
                                                                    value = round(2500000, 0)),
                                                       numericInput("c_onderhoud_MRL",
                                                                    "Annual maintenance costs (€)",
                                                                    step = 100,
                                                                    value = round(665000, 0)),
                                                       numericInput("c_qaequip_MRL",
                                                                    "Costs of additional equipment (for instance QA equipment) (€)",
                                                                    step = 100,
                                                                    value = round(0, 0))
                                                ), 
                                                column(6,
                                                       br(),
                                                       h4("Depreciation input"),
                                                       br(),
                                                       numericInput("t_afschr_app",
                                                                    "Useful life years equipment",
                                                                    step = 1,
                                                                    value = round(12, 0)),
                                                       numericInput("t_afschr_bunk",
                                                                    "Useful life years bunker" ,
                                                                    step = 1,
                                                                    value = round(20, 0)),
                                                       numericInput("p_rente",
                                                                    "Interest (%)",
                                                                    step = 0.1,
                                                                    value = round(2.5, 1))
                                                )
                                              )
                                       )
                                     ),
                                     br(),
                                     br(),
                                     fluidRow(
                                       column(12,
                                              h4("Operational information"),
                                              p("By default, the total available minutes for the conventional treatment and the MR-Linac are based on a total of 50 working weeks of 5 working days each, with a total of 9.5 working hours minus 135 minutes for maintenance, breaks, and other tasks."),
                                              br(),
                                              fluidRow(
                                                column(6,      
                                                       numericInput("a_t_L",
                                                                    "Maximum active time per year (hours)",
                                                                    step = 1,
                                                                    value = 1813)
                                                ),
                                                column(6)  
                                              )
                                       )
                                     ),
                                     br(),
                                     fluidRow(
                                       column(6,
                                              sliderInput("occ_t_L",
                                                          "Occupancy rate conventional linac",
                                                          min = 0,
                                                          max = 1,
                                                          value = 1,
                                                          step = 0.05)
                                       ),
                                       column(6,
                                              sliderInput("occ_t_MRL",
                                                          "Occupancy rate MR-Linac",
                                                          min = 0,
                                                          max = 1,
                                                          value = 1,
                                                          step = 0.05)         
                                       )
                                     )
                           )
                  ), 
                  
                  ### PERSONNEL COSTS (HOURLY RATES AND TIME)
                  tabPanel("Personnel cost input",
                           fluidPage(
                             # Personnel Time Input
                             wellPanel(style = "background: white",
                                       h5("By default, input for the Dutch clinical context is used."),
                                       br(),
                                       fluidRow(
                                         column(6,
                                                h4("Conventional treatment"),
                                                br(),
                                                h4("Pre-treatment time (minutes)", 
                                                   span(icon("info-circle", class = "info-icon"), id = "info1")),
                                                bsTooltip(id = "info1", title = "Consider time for tasks such as contouring, planning, and consultation", placement = "right", trigger = "hover"),
                                                numericInput("tv_labo_L",
                                                             "Total time RTT(s)",
                                                             step = 1,
                                                             value = round(210, 0)),
                                                numericInput("tv_arts_L",
                                                             "Total time radiation oncologist(s)",
                                                             step = 1,
                                                             value = round(135, 0)),
                                                numericInput("tv_fys_L",
                                                             "Total time medical physicist(s)",
                                                             step = 1,
                                                             value = round(15, 0)),
                                                br(),
                                                h4("Staffing available during treatment",
                                                   span(icon("info-circle", class = "info-icon"), id = "info2")),
                                                bsTooltip(id = "info2", title = "This indicates the number of staff members available and their availability percentage during treatment.", placement = "right", trigger = "hover"),
                                                fluidRow(
                                                  column(3,
                                                         selectInput("n_labo_L",
                                                                     "RTT(s)",
                                                                     choices = 0:5,
                                                                     selected = 3)),
                                                  column(3,
                                                         selectInput("perc_labo_L",
                                                                     "% Available",
                                                                     choices = seq(0, 100, by = 5),
                                                                     selected = 100))
                                                ),
                                                fluidRow(
                                                  column(3,
                                                         selectInput("n_arts_L",
                                                                     "Radiation oncologists",
                                                                     choices = 0:5,
                                                                     selected = 1)),
                                                  column(3,
                                                         selectInput("perc_arts_L",
                                                                     "% Available",
                                                                     choices = seq(0, 100, by = 5),
                                                                     selected = 20))
                                                ),
                                                fluidRow(
                                                  column(3,
                                                         selectInput("n_fys_L",
                                                                     "Clinical physicists",
                                                                     choices = 0:5,
                                                                     selected = 1)),
                                                  column(3,
                                                         selectInput("perc_fys_L",
                                                                     "% Available",
                                                                     choices = seq(0, 100, by = 5),
                                                                     selected = 20))
                                                )
                                         ),
                                         
                                         column(6,
                                                h4("MR-Linac treatment"),
                                                br(),
                                                h4("Pre-treatment time (minutes)",
                                                   span(icon("info-circle", class = "info-icon"), id = "info3")),
                                                bsTooltip(id = "info3", title = "Consider time for tasks such as contouring, planning, and consultation", placement = "right", trigger = "hover"),
                                                numericInput("tv_labo_MRL",
                                                             "Total time RTT(s)",
                                                             step = 1,
                                                             value = round(210, 0)),
                                                numericInput("tv_arts_MRL",
                                                             "Total time radiation oncologist(s)",
                                                             step = 1,
                                                             value = round(135, 0)),
                                                numericInput("tv_fys_MRL",
                                                             "Total time medical physicist(s)",
                                                             step = 1,
                                                             value = round(15, 0)),
                                                br(),
                                                h4("Staffing available during treatment",
                                                   span(icon("info-circle", class = "info-icon"), id = "info4")),
                                                bsTooltip(id = "info4", title = "This indicates the number of staff members available and their availability percentage during treatment (including replanning)", placement = "right", trigger = "hover"),
                                                fluidRow(
                                                  column(3,
                                                         selectInput("n_labo_MRL",
                                                                     "RTT(s)",
                                                                     choices = 0:5,
                                                                     selected = 3)),
                                                  column(3,
                                                         selectInput("perc_labo_MRL",
                                                                     "% Available",
                                                                     choices = seq(0, 100, by = 5),
                                                                     selected = 100))
                                                ),
                                                fluidRow(
                                                  column(3,
                                                         selectInput("n_arts_MRL",
                                                                     "Radiation oncologists",
                                                                     choices = 0:5,
                                                                     selected = 1)),
                                                  column(3,
                                                         selectInput("perc_arts_MRL",
                                                                     "% Available",
                                                                     choices = seq(0, 100, by = 5),
                                                                     selected = 20))
                                                ),
                                                fluidRow(
                                                  column(3,
                                                         selectInput("n_fys_MRL",
                                                                     "Clinical physicists",
                                                                     choices = 0:5,
                                                                     selected = 1)),
                                                  column(3,
                                                         selectInput("perc_fys_MRL",
                                                                     "% Available",
                                                                     choices = seq(0, 100, by = 5),
                                                                     selected = 20))
                                                )
                                         )
                                       )
                             ),
                             
                             # Personnel Costs
                             wellPanel(style = "background: white",
                                       fluidRow(
                                         column(6,
                                                h4("Hourly rates for personnel (gross)"),
                                                br(),
                                                numericInput("c_arts",
                                                             "Radiation oncologist (€)",
                                                             step = 1,
                                                             value = round(105, 0)),
                                                numericInput("c_fys",
                                                             "Clinical physicists (€)",
                                                             step = 1,
                                                             value = round(93, 0)),
                                                numericInput("c_labo",
                                                             "RTT(s) (€)",
                                                             step = 1,
                                                             value = round(49, 0))
                                         ),
                                         column(6,
                                                h4("Costs for MRI Planning"),
                                                br(),
                                                numericInput("c_IMD_L",
                                                             "Planning MRI conventional treatment (€)",
                                                             step = 1,
                                                             value = round(358, 0)),
                                                numericInput("c_IMD_MRL",
                                                             "Planning MRI MR-Linac treatment (€)",
                                                             step = 1,
                                                             value = round(358, 0)))
                                         )
                                       )
                             )
                           ),
                  
                  

                  ## RESULTS TAB: NUMBER OF FRACTIONS   ------
                  
                  tabPanel("Number of fractions ",
                           ui <- fluidPage(
                             wellPanel(
                               style = "background: white",
                               br(),
                               p("The table and figure illustrate how variations in the number of fractions of MR-Linac treatment impact both the additional cost and the QALYs needed to compensate for those costs per patient.
                                 If the number of fractions MR-Linac is negative, MR-Linac treatment is less costly per patient compared to conventional treatment."),
                               br(),
                               conditionalPanel(
                                 condition = "input.go == 0"
                               ),
                               conditionalPanel(
                                 condition = "input.go != 0",
                                 fluidRow(
                                   column(6,
                                          wellPanel(
                                            style = "background: white; border: 1px solid #ccc; padding: 10px;",
                                            tableOutput("table_fractions")
                                          )
                                   ),
                                   column(6,
                                          wellPanel(
                                            style = "background: white; border: 1px solid #ccc; padding: 10px;",
                                            plotOutput("plot_fractions"), 
                                            br(), 
                                            textOutput("snijwaarde_fractions"))
                                   )
                                 )
                               )
                               
                             ),
                             
                             #verbatimTextOutput("a"),
                             #verbatimTextOutput("b"),
                             #verbatimTextOutput("c"),
                             #verbatimTextOutput("d"),
                             #verbatimTextOutput("e"),
                             #verbatimTextOutput("f"),
                             #verbatimTextOutput("g"),
                             #verbatimTextOutput("h"),
                             #verbatimTextOutput("j"),
                             #verbatimTextOutput("k"),
                             #verbatimTextOutput("l"),
                             #verbatimTextOutput("m"),
                             #verbatimTextOutput("n"),
                             #verbatimTextOutput("o"),
                             #verbatimTextOutput("p"),
                             #verbatimTextOutput("q"),
                             #verbatimTextOutput("r"),
                             #verbatimTextOutput("s"),
                             #verbatimTextOutput("t"),
                             #verbatimTextOutput("u"),
                             #verbatimTextOutput("v"),
                             #verbatimTextOutput("w"),
                             #verbatimTextOutput("x"),
                             #verbatimTextOutput("y"),
                             #verbatimTextOutput("z"),
                             #verbatimTextOutput("aa"),
                             #verbatimTextOutput("ab"),
                             #verbatimTextOutput("ac"),
                             #verbatimTextOutput("ad"),
                             #verbatimTextOutput("ae"),
                             #verbatimTextOutput("af"),
                             #verbatimTextOutput("ag"),
                             #verbatimTextOutput("ah"),
                             #verbatimTextOutput("ai"),
                             #verbatimTextOutput("aj"),
                             #verbatimTextOutput("ak"),
                             #verbatimTextOutput("al"),
                             #verbatimTextOutput("am")
                           )
                  )
                  , 
      
      ## RESULTS TAB: TIME PER FRACTION   ------
      
      tabPanel("Time per fraction ",
               ui <- fluidPage(
                 wellPanel(
                   style = "background: white",
                   br(),
                   p("The table and figure below illustrate how variations in time per fraction impact both the additional cost and the QALYs needed to compensate for those costs per patient.
                     If the cost difference per patient is negative, MR-Linac treatment is less costly per patient compared to conventional treatment"), 
                   br(),
                   conditionalPanel(
                     condition = "input.go == 0"
                   ),
                   conditionalPanel(
                     condition = "input.go != 0",
                     fluidRow(
                       column(
                         6,
                         wellPanel(
                           style = "background: white; border: 1px solid #ccc; padding: 10px;",
                           tableOutput("table_time")
                         )
                       ),
                       column(
                         6,
                         wellPanel(
                           style = "background: white; border: 1px solid #ccc; padding: 10px;",
                           plotOutput("plot_time"), 
                           br(), 
                           textOutput("snijwaarde_time"))),
                       
                     )
                   )
                 ), 
                                )
      ),
             
      ## RESULTS TAB: UPFRONT INVESTMENT COST   ------
      
      tabPanel("Investment cost",
               ui <- fluidPage(
                 wellPanel(
                   style = "background: white",
                   br(),
                   p("The table and figure below illustrate how variations in investment costs (acquisition costs of MR-Linac and equipment) impact both the additional cost and the QALYs needed to compensate for those costs per patient.
                     If the cost difference per patient is negative, MR-Linac treatment is less costly per patient compared to conventional treatment"), 
                   br(),
                   conditionalPanel(
                     condition = "input.go == 0"
                   ),
                   
                   conditionalPanel(
                     condition = "input.go != 0",
                     fluidRow(
                       column(
                         6,
                         wellPanel(
                           style = "background: white; border: 1px solid #ccc; padding: 10px;",
                           tableOutput("table_invest")
                         )
                       ),
                       column(
                         6,
                         wellPanel(
                           style = "background: white; border: 1px solid #ccc; padding: 10px;",
                           plotOutput("plot_invest"), 
                           br(), 
                           textOutput("snijwaarde_invest"))),
                       
                     )
                   )
                 ), 
               )
      ),
      
      ## RESULTS TAB: AVAILABLE TIME   ------
      
      tabPanel("Available time",
               ui <- fluidPage(
                 wellPanel(
                   style = "background: white",
                   br(),
                   p("The table and figure below illustrate how variations in the available MR-Linac time impact both the additional cost and the QALYs needed to compensate for those costs per patient.
                     If the cost difference per patient is negative, MR-Linac treatment is less costly per patient compared to conventional treatment"), 
                   br(),
                   conditionalPanel(
                     condition = "input.go == 0"
                   ),
                   
                   conditionalPanel(
                     condition = "input.go != 0",
                     fluidRow(
                       column(
                         6,
                         wellPanel(
                           style = "background: white; border: 1px solid #ccc; padding: 10px;",
                           tableOutput("table_maxtime")
                         )
                       ),
                       column(
                         6,
                         wellPanel(
                           style = "background: white; border: 1px solid #ccc; padding: 10px;",
                           plotOutput("plot_maxtime"), 
                           br(), 
                           textOutput("snijwaarde_maxtime"))),
                       
                     )
                   )
                 ), 
               )
      ),
      
      ## RESULTS TAB: OCCUPANCY RATE MR-LINAC   ------
      
      tabPanel("Occupancy rate",
               ui <- fluidPage(
                 wellPanel(
                   style = "background: white",
                   br(),
                   p("The table and figure below illustrate how variations in the occupancy rate of the MR-Linac impact both the additional cost and the QALYs needed to compensate for those costs per patient.
                     If the cost difference per patient is negative, MR-Linac treatment is less costly per patient compared to conventional treatment"), 
                   br(),
                   conditionalPanel(
                     condition = "input.go == 0"
                   ),
                   
                   conditionalPanel(
                     condition = "input.go != 0",
                     fluidRow(
                       column(
                         6,
                         wellPanel(
                           style = "background: white; border: 1px solid #ccc; padding: 10px;",
                           tableOutput("table_occ")
                         )
                       ),
                       column(
                         6,
                         wellPanel(
                           style = "background: white; border: 1px solid #ccc; padding: 10px;",
                           plotOutput("plot_occ"), 
                           br(), 
                           textOutput("snijwaarde_occ"))),
                       
                     )
                   )
                 ), 
               )
      ),
      
                    
                  ### COST PER MINUTE ------        
      tabPanel("Cost breakdown",
               wellPanel(style = "background: white",
                         br(),
                         p("The figures below illustrate the breakdown of costs per minute for conventional and MR-Linac treatment."), 
                         br(), 
                         conditionalPanel(
                           condition = "input.go != 0",
                           fluidRow(
                             column(6, 
                                    wellPanel(
                                      style = "background: white; border: 1px solid #ccc; padding: 10px;",
                                      plotOutput("pie_plot_L")
                                    )
                             ),
                             column(6, 
                                    wellPanel(
                                      style = "background: white; border: 1px solid #ccc; padding: 10px;",
                                      plotOutput("pie_plot_MRL")
                                    )
                             )
                           )
                         )
               )
      )
                  
                  
     
                  
      ),
               
                   
               
                 
       fluidRow(
        tags$hr(),
        h6(style='padding:20px;', "Developed by Radboudumc, v1. For questions please contact: marike.ulehake@radboudumc.nl")),
      
      fluidRow(
        tags$img(src="radboudumc.jpg", height = 28, width = 210, align='right')),
      
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br() 
    ))
}
     

# Define server logic 
server <- function(input, output, session) {
  
  reactive_values <- reactiveValues()
  
  observeEvent(input$reset_input, {
    session$reload()
  })     
  
  # Alle input in renderPrint om te controleren
  output$a <- renderPrint(input$f_L)
  output$b <- renderPrint(input$tf_L)
  output$c <- renderPrint(input$a_t_L)
  output$d <- renderPrint(input$occ_t_L)
  output$e <- renderPrint(input$tf_MRL)
#  output$f <- renderPrint(input$a_t_MRL)
  output$g <- renderPrint(input$occ_t_MRL)
  output$h <- renderPrint(input$wtp)
  output$j <- renderPrint(input$c_aanschaf_L)
  output$k <- renderPrint(input$c_bunk_L)
  output$l <- renderPrint(input$c_onderhoud_L)
  output$m <- renderPrint(input$c_qaequip_L)
  output$n <- renderPrint(input$c_aanschaf_MRL)
  output$o <- renderPrint(input$c_bunk_MRL)
  output$p <- renderPrint(input$c_onderhoud_MRL)
  output$q <- renderPrint(input$c_qaequip_MRL)
  output$r <- renderPrint(input$c_arts)
  output$s <- renderPrint(input$c_fys)
  output$t <- renderPrint(input$c_labo)
  output$u <- renderPrint(input$n_arts_L)
  output$v <- renderPrint(input$n_fys_L)
  output$w <- renderPrint(input$tv_labo_MRL)
  output$x <- renderPrint(input$tv_fys_MRL)
  output$y <- renderPrint(input$tv_arts_MRL)
  output$ab <- renderPrint(input$tv_labo_L)
  output$ac <- renderPrint(input$tv_fys_L)
  output$ad <- renderPrint(input$tv_arts_L)
  output$ae <- renderPrint(input$c_IMD_L)
  output$af <- renderPrint(input$c_IMD_MRL)
  output$ag <- renderPrint(input$perc_arts_MRL)
  output$ah <- renderPrint(input$perc_fys_MRL)
  output$ai <- renderPrint(input$perc_labo_MRL)
  output$aj <- renderPrint(input$perc_arts_L)
  output$ak <- renderPrint(input$perc_fys_L)
  output$al <- renderPrint(input$perc_labo_L)
  output$am <- renderPrint(input$f_MRL)
  
                             
                             
  observeEvent(input$go, {
    # Berekeningen uitvoeren en resultaten opslaan in reactiveValues
    reactive_values$results <- Calc.shiny(
      f_L = as.numeric(input$f_L),
      f_MRL = as.numeric(input$f_MRL), 
      tf_L = as.numeric(input$tf_L),
      a_t_L = as.numeric(input$a_t_L),
      occ_t_L = as.numeric(input$occ_t_L),
      tf_MRL = as.numeric(input$tf_MRL),
#     a_t_MRL = as.numeric(input$a_t_MRL),
      occ_t_MRL = as.numeric(input$occ_t_MRL),
      wtp = as.numeric(input$wtp),
      c_aanschaf_L = as.numeric(input$c_aanschaf_L),
      c_bunk_L = as.numeric(input$c_bunk_L),
      c_onderhoud_L = as.numeric(input$c_onderhoud_L),
      c_qaequip_L = as.numeric(input$c_qaequip_L),
      c_aanschaf_MRL = as.numeric(input$c_aanschaf_MRL),
      c_bunk_MRL = as.numeric(input$c_bunk_MRL),
      c_onderhoud_MRL = as.numeric(input$c_onderhoud_MRL),
      c_qaequip_MRL = as.numeric(input$c_qaequip_MRL),
      c_arts = as.numeric(input$c_arts),
      c_fys = as.numeric(input$c_fys),
      c_labo = as.numeric(input$c_labo),
      n_arts_MRL = as.numeric(input$n_arts_MRL),
      n_fys_MRL = as.numeric(input$n_fys_MRL),
      n_labo_MRL = as.numeric(input$n_labo_MRL),
      n_arts_L = as.numeric(input$n_arts_L),
      n_fys_L = as.numeric(input$n_fys_L),
      n_labo_L = as.numeric(input$n_labo_L),
      tv_labo_MRL = as.numeric(input$tv_labo_MRL),
      tv_fys_MRL = as.numeric(input$tv_fys_MRL),
      tv_arts_MRL = as.numeric(input$tv_arts_MRL),
      tv_labo_L = as.numeric(input$tv_labo_L),
      tv_fys_L = as.numeric(input$tv_fys_L),
      tv_arts_L = as.numeric(input$tv_arts_L),
      c_IMD_L = as.numeric(input$c_IMD_L),
      c_IMD_MRL = as.numeric(input$c_IMD_MRL), 
      perc_arts_MRL = as.numeric(input$perc_arts_MRL),
      perc_fys_MRL = as.numeric(input$perc_fys_MRL),
      perc_labo_MRL = as.numeric(input$perc_labo_MRL),
      perc_arts_L = as.numeric(input$perc_arts_L),
      perc_fys_L = as.numeric(input$perc_fys_L),
      perc_labo_L = as.numeric(input$perc_labo_L), 
      t_afschr_app = as.numeric(input$t_afschr_app),   
      t_afschr_bunk = as.numeric(input$t_afschr_bunk),  
      p_rente = as.numeric(input$p_rente)  
      
    )
    
    #check reactive_values
    print(reactive_values$results)
    
    # Resultaten renderen in de UI
    output$table_fractions <- renderTable(reactive_values$results$table_fractions)
    output$plot_fractions <- renderPlot(reactive_values$results$plot_fractions)
    output$snijwaarde_fractions <- renderText(reactive_values$results$snijwaarde_fractions)
    
    output$table_time <- renderTable(reactive_values$results$table_time)
    output$plot_time <- renderPlot(reactive_values$results$plot_time)
    output$snijwaarde_time <- renderText(reactive_values$results$snijwaarde_time)
    
    output$table_invest <- renderTable(reactive_values$results$table_invest)
    output$plot_invest <- renderPlot(reactive_values$results$plot_invest)
    output$snijwaarde_invest <- renderText(reactive_values$results$snijwaarde_invest)
    
    output$table_maxtime <- renderTable(reactive_values$results$table_maxtime)
    output$plot_maxtime <- renderPlot(reactive_values$results$plot_maxtime)
    output$snijwaarde_maxtime <- renderText(reactive_values$results$snijwaarde_maxtime)
    
    output$table_occ <- renderTable(reactive_values$results$table_occ)
    output$plot_occ <- renderPlot(reactive_values$results$plot_occ)
    output$snijwaarde_occ <- renderText(reactive_values$results$snijwaarde_occ)
    
    output$data <- renderTable(reactive_values$results$data)
    output$pie <- renderPlot(reactive_values$results$pie)
    output$costspie <- renderText(reactive_values$results$costspie)
    
    output$pie_plot_L <- renderPlot(reactive_values$results$pie_plot_L)
    output$pie_plot_MRL <- renderPlot(reactive_values$results$pie_plot_MRL)

    
})
}

# Run the application 
shinyApp(ui, server)