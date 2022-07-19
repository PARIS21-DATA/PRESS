# Initializing all packages

library(shinydashboard)
library(dplyr)
library(bupaR)
library(shiny)
library(processmapR)
library(processmonitR)
library(DT)
library(DiagrammeR)
library(shinyalert)

# setwd(dir = "D:\\Users\\luis.s.oliveira\\Desktop\\Mentoria\\Data\\RDatas")
load("./Data/Gender dashboard/paradata_log_anonymized_dsc_mensagem_split.RData")
load("./Data/Intermediate/crs03_full_en.rds")
options(scipen=999)

teste <- filter(t_paradados_log_split, (t_paradados_log_split$TIPO_PARADADO == 2))

OTC <- teste %>%  select(DSC_CONTEXTO,NUM_IMEI,NUM_MATRICULA,ID,DSC_SUBCONTEXTO,TIMESTAMP) %>% 
  mutate(Key_Event = paste(DSC_CONTEXTO,NUM_IMEI,NUM_MATRICULA,ID,DSC_SUBCONTEXTO,TIMESTAMP))

OTC$Order <- c(1:812398)
OTC <- OTC[,-9]
OTC <- relocate(OTC,  Order, .before="Key_Event")
OTC <- OTC[sample(nrow(OTC),5000),]

names(OTC) <- c("Event","Case_ID","Resource","Event_ID","Status","Timestamp","Order","Key_Event")

OTC$Resource <- as.character(OTC$Resource)

# OTC$Resource[OTC$Resource == "r1"] <- "Katherine"     # Renaming the resources
# OTC$Resource[OTC$Resource == "r2"] <- "Jonathan"
# OTC$Resource[OTC$Resource == "r3"] <- "Andrea"
# OTC$Resource[OTC$Resource == "r4"] <- "Steven"
# OTC$Resource[OTC$Resource == "r5"] <- "Julia"
# OTC$Resource[OTC$Resource == "r6"] <- "Amy"
# OTC$Resource[OTC$Resource == "r7"] <- "Jerry"

Sys.setlocale(category = "LC_TIME", "English")

OTC$Timestamp = as.POSIXct(OTC$Timestamp, 
                           format = "%d%b%Y:%H:%M:%OS")
# 
# data$endtimestamp = as.POSIXct(OTC$TIMESTAMP, 
#                                format = "%d%b%Y:%H:%M:%OS")


Sys.setlocale(category = "LC_TIME", "")


OTC <- unique.data.frame(OTC)

data(mtcars)
data(iris)
library(ggplot2)
data(diamonds)

# UI side design
ui <- fluidPage(
  title = "Comparison of gender markers PRESS",
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        'input.dataset === "diamonds"',
        checkboxGroupInput("show_vars", "Columns in diamonds to show:",
                           names(diamonds), selected = names(diamonds))
      ),
      conditionalPanel(
        'input.dataset === "mtcars"',
        helpText("Click the column header to sort a column.")
      ),
      conditionalPanel(
        'input.dataset === "iris"',
        helpText("Display 5 records by default.")
      )
    ),
    mainPanel(
      tabsetPanel(
        id = 'dataset',
        tabPanel("diamonds", DT::dataTableOutput("mytable1")),
        tabPanel("mtcars", DT::dataTableOutput("mytable2")),
        tabPanel("iris", DT::dataTableOutput("mytable3"))
      )
    )
  )
)

server <- function(input, output) {
  
  # choose columns to display
  diamonds2 = diamonds[sample(nrow(diamonds), 1000), ]
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(diamonds2[, input$show_vars, drop = FALSE])
  })
  
  # sorted columns are colored now because CSS are attached to them
  output$mytable2 <- DT::renderDataTable({
    DT::datatable(mtcars, options = list(orderClasses = TRUE))
  })
  
  # customize the length drop-down menu; display 5 rows per page by default
  output$mytable3 <- DT::renderDataTable({
    DT::datatable(iris, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
  })
  
}

shinyApp(ui, server)






ui <- dashboardPage(skin = "red", 
                    
                    dashboardHeader(title = "Comparison of gender markers PRESS", 
                                    titleWidth = 450,
                                    
                                    dropdownMenu(type = "notifications", badgeStatus = "danger",
                                                 
                                                 notificationItem(text = "12 new users today",
                                                                  icon("users")),
                                                 
                                                 notificationItem(text = "31 items delivered",
                                                                  icon("truck"),
                                                                  status = "success"),
                                                 
                                                 notificationItem(text = "Server load at 34%",
                                                                  icon("exclamation-triangle"),
                                                                  status = "warning")
                                    ),
                                    
                                    dropdownMenu(type = "tasks", badgeStatus = "success",
                                                 
                                                 taskItem(value = 90, color = "green","Documentation"),
                                                 
                                                 taskItem(value = 17, color = "aqua","Omega BI implementation"),
                                                 
                                                 taskItem(value = 78, color = "yellow","Server deployment"),
                                                 
                                                 taskItem(value = 84, color = "red", "Project progress")
                                                 
                                    )
                    ),
                    
                    dashboardSidebar(disable = TRUE),
                  
                    dashboardBody( 
                      
                      tags$head(tags$style(HTML('
                                                .main-header .logo {
                                                font-family: "Georgia", Times,
                                                "Times New Roman",
                                                font-weight: bold;
                                                font-size: 24px;
                                                font-style: italic;
                                                }
                                                '))),
                      
                      fluidRow(
                        
                        tabBox(height = "1100px", width = "1000px",
                               
                               tabPanel(title = tagList(icon("project-diagram", class = "fas fa-project-diagram")
                                                        
                                                        , "General Information"),
                                        
                                        box(grVizOutput("Pr_map"), status = "primary", solidHeader = TRUE,
                                            
                                            title = "PROCESS MAP", width = 8, height = 612, collapsible = TRUE),
                                        
                               ),
                               
                               
                               tabPanel(title = tagList(icon("th-list", lib = "glyphicon"), "A - Text detection"),
                                        
                                        box(title = "Pre XGBoost classification comparison",status = "primary",solidHeader = TRUE, width = 600,
                                            
                                            collapsible = TRUE, DT::dataTableOutput("table")),
                                        
                                        box(title = "Importance matrix",status = "primary",solidHeader = TRUE, width = 600,
                                            
                                            collapsible = TRUE, DT::dataTableOutput("table")),
                                        
                                        box(title = "Post XGBoost classification comparison ",status = "primary",solidHeader = TRUE, width = 600,
                                            
                                            collapsible = TRUE, DT::dataTableOutput("table")),
                               ),
                               
                               tabPanel(title = tagList(icon("th-list", lib = "glyphicon"), "B - Text detection & gen marker"),
                                        
                                        box(title = "Pre XGBoost classification comparison",status = "primary",solidHeader = TRUE, width = 600,
                                            
                                            collapsible = TRUE, DT::dataTableOutput("table")),
                                        
                                        box(title = "Importance matrix",status = "primary",solidHeader = TRUE, width = 600,
                                            
                                            collapsible = TRUE, DT::dataTableOutput("table")),
                                        
                                        box(title = "Post XGBoost classification comparison ",status = "primary",solidHeader = TRUE, width = 600,
                                            
                                            collapsible = TRUE, DT::dataTableOutput("table")),
                               ),
                               
                               tabPanel(title = tagList(icon("th-list", lib = "glyphicon"),  "C - Text detection & channelcode"),
                                        
                                        box(title = "Pre XGBoost classification comparison",status = "primary",solidHeader = TRUE, width = 600,
                                            
                                            collapsible = TRUE, DT::dataTableOutput("table")),
                                        
                                        box(title = "Importance matrix",status = "primary",solidHeader = TRUE, width = 600,
                                            
                                            collapsible = TRUE, DT::dataTableOutput("table")),
                                        
                                        box(title = "Post XGBoost classification comparison ",status = "primary",solidHeader = TRUE, width = 600,
                                            
                                            collapsible = TRUE, DT::dataTableOutput("table")),
                               ),
                               
                               tabPanel(title = tagList(icon("th-list", lib = "glyphicon"),  "D - Text detection & purposecode"),
                                        
                                        box(title = "Pre XGBoost classification comparison",status = "primary",solidHeader = TRUE, width = 600,
                                            
                                            collapsible = TRUE, DT::dataTableOutput("table")),
                                        
                                        box(title = "Importance matrix",status = "primary",solidHeader = TRUE, width = 600,
                                            
                                            collapsible = TRUE, DT::dataTableOutput("table")),
                                        
                                        box(title = "Post XGBoost classification comparison ",status = "primary",solidHeader = TRUE, width = 600,
                                            
                                            collapsible = TRUE, DT::dataTableOutput("table")),
                               ),
                              
                               tabPanel(title = tagList(icon("th-list", lib = "glyphicon"),  "E - Text detection & gen SDG"),
                                        
                                        box(title = "Pre XGBoost classification comparison",status = "primary",solidHeader = TRUE, width = 600,
                                            
                                            collapsible = TRUE, DT::dataTableOutput("table")),
                                        
                                        box(title = "Importance matrix",status = "primary",solidHeader = TRUE, width = 600,
                                            
                                            collapsible = TRUE, DT::dataTableOutput("table")),
                                        
                                        box(title = "Post XGBoost classification comparison ",status = "primary",solidHeader = TRUE, width = 600,
                                            
                                            collapsible = TRUE, DT::dataTableOutput("table")),
                               ),
                               
                               tabPanel(title = tagList(icon("th-list", lib = "glyphicon"),  "F - Text detection, channel code, purpose code"),
                                        
                                        box(title = "Pre XGBoost classification comparison",status = "primary",solidHeader = TRUE, width = 600,
                                            
                                            collapsible = TRUE, DT::dataTableOutput("table")),
                                        
                                        box(title = "Importance matrix",status = "primary",solidHeader = TRUE, width = 600,
                                            
                                            collapsible = TRUE, DT::dataTableOutput("table")),
                                        
                                        box(title = "Post XGBoost classification comparison ",status = "primary",solidHeader = TRUE, width = 600,
                                            
                                            collapsible = TRUE, DT::dataTableOutput("table")),
                               )
                               
                        )
                      )
                    )) 

# Server side design
server <- function(input, output) {
  output$table = DT::renderDataTable({
    OTC %>% filter(Event == "CNEFE")
  })
}


server <- function(input, output) {
  
  output$mytabs <- renderUI({
    
    nTabs = input$nTabs
    
    myTabs = lappy(paste("Tab", 1:nTabs), tabPanel)
    
    do.call(tabsetPanel, myTabs)
    
  })
  
  output$table <- renderDataTable({ OTC %>% filter(Event == "CNEFE") })
  
  observeEvent(input$PLOT, {
    
    progress <- shiny::Progress$new()
    
    on.exit(progress$close())
    
    progress$set(message = "Computing plot values", value = 0)
    
    n <- 10
    
    for (i in 1:n) {
      
      progress$inc(1/n, detail = paste(i*10,"% completed"))
      
      Sys.sleep(0.5)
      
    } 
    
    progress$set(message = "Plot values computed successfully!")
    
    
    Resource_filtered <- input$R_ID
    
    Time <- input$TIME
    
    Event_filtered <- input$EVENT
    
    Activity_frequency_level <- input$LEVEL
    
    Activity_unit_level <- input$UNITS
    
    Trace_type <- input$TYPE
    
    OTC_Filtered <- OTC
    
    if(Resource_filtered != "All") {
      
      OTC_Filtered <- OTC %>% filter(Resource == Resource_filtered)
      
    }
    
    if(Event_filtered != "All") {
      
      OTC_Filtered <- OTC %>% filter(Event == Event_filtered)
      
    }
    
    OTC_Filtered <- as.data.frame(OTC_Filtered)
    
    O2C <- bupaR::eventlog(eventlog = OTC_Filtered,
                           case_id = "Case_ID",
                           activity_id = "Event",
                           activity_instance_id = "Key_Event",
                           timestamp = "Timestamp",
                           lifecycle_id = "Status",
                           resource_id = "Resource",
                           order = "auto")
    
    O2C <- O2C %>% filter_time_period(interval = Time)
    
    if(nrow(O2C) == 0) {
      
      output$act_frq <- renderPlot(plot.new())
      output$Pr_map <- renderGrViz(plot.new())
      output$prcn_Matr <- renderPlot(plot.new())
      output$table <- renderDataTable(plot.new())
      
      shinyalert("Data not sufficient. Make some other selection!", type = "warning")
      
    }
    
    else {
      
      
      output$table <- renderDataTable({ O2C[1:6] })
      
      output$activity_frequency <- renderPlot(
        
        if(nrow(O2C) == 0) {
          
          plot.new()
          
        }
        
        else {
          
          validate(
            
            need(Activity_frequency_level == c("log", "trace","case","activity"), "Please select LEVEL OF ANALYSIS as either log, trace, case or activity!")
            
          )
          
          O2C %>% activity_frequency(level = Activity_frequency_level) %>% plot()
          
        }
        
      )
      
      output$Pr_map <- renderGrViz({
        
        O2C %>% process_map(rankdir = "TB", fixed_edge_width = FALSE, render = TRUE)
        
      })
      
      output$prcn_Matr <- renderPlot({
        
        O2C %>% precedence_matrix() %>% plot()
        
      })
      
      output$n_activities <- renderText({
        
        O2C %>% n_activities()
        
      })
      
      output$n_activity_instances <- renderText({
        
        O2C %>% n_activity_instances()
        
      })
      
      output$n_cases <- renderText({
        
        O2C %>% n_cases()
        
      })
      
      output$n_events <- renderText({
        
        O2C %>% n_events()
        
      })
      
      output$n_resources <- renderText({
        
        O2C %>% n_resources()
        
      })
      
      output$n_traces <- renderText({
        
        O2C %>% n_traces()
        
      })
      
      output$idle_time <- renderPlot({
        
        validate(
          
          need(Activity_frequency_level == c("resource", "log", "case"), "Please select LEVEL OF ANALYSIS as resource, log or case!")
          
        )
        
        O2C %>% idle_time(level = Activity_frequency_level, units = Activity_unit_level) %>% plot()
        
      })
      
      output$process_time <- renderPlot({
        
        # patients$employee <- as.character(patients$employee)
        # 
        # # patients$employee[patients$employee == "r1"] <- "Katherine"     # Renaming resources again
        # patients$employee[patients$employee == "r2"] <- "Jonathan"
        # patients$employee[patients$employee == "r3"] <- "Andrea"
        # patients$employee[patients$employee == "r4"] <- "Steven"
        # patients$employee[patients$employee == "r5"] <- "Julia"
        # patients$employee[patients$employee == "r6"] <- "Amy"
        # patients$employee[patients$employee == "r7"] <- "Jerry"
        
        O2C %>% processing_time(level = Activity_frequency_level, units = Activity_unit_level) %>% plot() 
        
      })
      
      output$throughput_time <- renderPlot({
        
        validate(
          
          need(Activity_frequency_level == c("log","case"), "Please select LEVEL OF ANALYSIS as either log or case!")
          
        )
        
        O2C %>% group_by(resource_id = Resource) %>% throughput_time(level = Activity_frequency_level, units = Activity_unit_level) %>% plot()
        
      })
      
      output$resource_frequency <- renderPlot({
        
        validate(
          
          need(Activity_frequency_level == c("log","resource","case","resource-activity","activity"), "Please select LEVEL OF ANALYSIS as either log, resource, resource-activity, activity or case!")
          
        )
        
        O2C %>% resource_frequency(level = Activity_frequency_level) %>% plot()
        
      })
      
      output$resource_map <- renderGrViz({
        
        O2C %>% resource_map()
        
      })
      
      output$resource_specialization <- renderPlot({
        
        validate(
          
          need(Activity_frequency_level == c("resource","activity","log"), "Please select LEVEL OF ANALYSIS as either resource, log or activity!")
          
        )
        
        O2C %>% resource_specialisation(level = Activity_frequency_level) %>% plot()
        
      })
      
      output$start_activity <- renderPlot({
        
        validate(
          
          need(Activity_frequency_level == c("resource","resource-activity","activity"), "Please select LEVEL OF ANALYSIS as either resource, resource-activity, or activity!")
          
        )
        
        O2C %>% start_activities(level = Activity_frequency_level) %>% plot()
        
      })
      
      output$end_activity <- renderPlot({
        
        validate(
          
          need(Activity_frequency_level == c("resource","resource-activity","activity"), "Please select LEVEL OF ANALYSIS as either resource, resource-activity, or activity!")
          
        )
        
        O2C %>% end_activities(level = Activity_frequency_level) %>% plot()
        
      })
      
      output$activity_presence <- renderPlot({
        
        O2C %>% activity_presence() %>% plot()
        
      })
      
      output$trace_explorer <- renderPlot({
        
        O2C %>% trace_explorer(type = Trace_type) %>% plot()
        
      })
      
      output$trace_coverage <- renderPlot({
        
        validate(
          
          need(Activity_frequency_level == c("trace","log","case"), "Please select LEVEL OF ANALYSIS as either log, trace or case!")
          
        )
        
        O2C %>% trace_coverage(level = Activity_frequency_level) %>% plot()
        
      })
      
      output$trace_length <- renderPlot({
        
        validate(
          
          need(Activity_frequency_level == c("case","log","trace"), "Please select LEVEL OF ANALYSIS as either log, trace or case!")
          
        )
        
        O2C %>% trace_length(level = Activity_frequency_level) %>% plot()
        
      })
      
      output$table <- renderDataTable({ OTC %>% filter(Event == "CNEFE") })
      
    }
    
  })
  output$table <- renderDataTable({ OTC %>% filter(Event == "CNEFE") })
  
}

shinyApp(ui = ui, server = server)
          