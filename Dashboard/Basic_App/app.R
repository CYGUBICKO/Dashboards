##### ----- Building Dashboards ---------------
#### Date: 08-06-2018
#### By: Steve Bicko

# Clear the environment
rm(list = ls())

# Load required packages
library(shiny)
library(shinydashboard)
library(ggplot2)
library(haven)
library(data.table)
library(dplyr)

library(shinyjs)
library(shinyFilters)

# Load data
caregiver <- read_dta("caregiver_labeled.dta")
caregiver <- as_factor(caregiver)
working_df <- setDT(caregiver)


#### ------ Define functions to be used -------

# Frequency tables
tabFunc <- function(var){
  df <- working_df %>% tbl_df()
  freq <- as.data.frame(table(df[, var]))
  names(freq) <- c("Labels", "Frequency")
  return(freq)
}

# Two-way frequency tables
tab2Func <- function(var1, var2){
  df <- working_df %>% tbl_df()
  dat <- df[, c(var1, var2)]
  freq <- data.frame(table(dat))
  names(freq) <- c(var1, var2, "Frequency")
  return(freq)
}


# create filterset in global section of app
filterSet <- newFilterSet('FS1') %>%
  # give each filter unique id and tell it which column to filter on
  addSelectFilter('Interviewer','fi_code_new') %>%
  addSelectFilter('County','cg_qcounty')
 # addSliderFilter('disp',value=c(0,500)) %>%
 # addSelectFilter('carb') %>%
  #addCustomSliderFilter('hp',value=seq(0,500,50))


#### ---- Define the User Interface -----

#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "APHRC")
#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
  sidebarUserPanel("CMAM Survey",
                   #subtitle = a(href = "#", icon("circle", class = "text-success"), "Online"),
                   # Image file should be in www/ subdir
                   image = "aphrc.jpg"),
  menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
  menuItem("Visit-us", icon = icon("send",lib='glyphicon'), 
           href = "http://aphrc.org/")
)

frow1 <- fluidRow(
  valueBoxOutput("value1", width = 3)
  ,valueBoxOutput("value2", width = 3)
  ,valueBoxOutput("value3", width = 3)
  ,valueBoxOutput("value4", width = 3)
)

frow2 <- fluidRow(
  
  box(
    title = "Completed interviews per interviewer"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("interviewer", height = "300px")
  )
  
  ,box(
    title = "Completed per County"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("completedbycounty", height = "300px")
  ) 
  ,box(
    title = "Malnutrition status - 1st reading"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("malstatuscounty1", height = "300px")
  ) 
  ,box(
    title = "Malnutrition status - 2nd reading"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("malstatuscounty2", height = "300px")
  ) 
  ,box(
    title = "Compare the MUAC readings - Boxplot"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("muaccompbox", height = "300px")
  ) 
  ,box(
    title = "Compare the MUAC readings - Density plot"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("muaccompdensity", height = "300px")
  )
)



# combine the two fluid rows to make the body
body <- dashboardBody(frow1, frow2)

ui <- fluidPage(
  #header,
  #sidebar,
  #body,
  dashboardPage(title = 'This is my Page title', header, sidebar, body, skin='red'),
  #shinyjs is required to show/hide filters
  #useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      filterInputs(filterSet),
      #action but to reset filters
      hr(),
      filterMaster(filterSet),
      filterResetButton(filterSet)
      # actionButton('resetFilter','resetFilter')
    ),
    mainPanel(
      DT::dataTableOutput("data")
    )
  )
)

#completing the ui part with dashboardPage
#ui <- dashboardPage(title = 'This is my Page title', header, sidebar, body, skin='red')



# Define server logics / Back-end codes
# create the server functions for the dashboard  
server <- function(input, output, session) { 
  
  #some data manipulation to derive the values of KPI boxes
  total_respondents <- nrow(working_df)
  total_res_dup_id <- working_df[cg_q1p7_auto_dup>0, ] %>% nrow
  total_res_miss_id <- working_df[cg_q1p7_auto==""|is.na(cg_q1p7_auto), ] %>% nrow
  total_non_issue <- working_df[cg_q1p7_auto_dup==0 & (cg_q1p7_auto!=""|!is.na(cg_q1p7_auto)), ] %>% nrow
  
  # Completed per interviewer
  interviewer_complete <- tabFunc("fi_code_new")
  colnames(interviewer_complete) <- c("Interviewer", "Frequency")
  setDF(interviewer_complete)
  setorder(interviewer_complete, -Frequency)
  
  # Interviewer per county
  interviewer_conty <- tab2Func("fi_code_new", "cg_qcounty")
  colnames(interviewer_conty) <- c("Interviewer", "County", "Frequency")
  interviewer_conty <- setDF(interviewer_conty)
  setorder(interviewer_conty, -Frequency)
  
  # Nutrition status per county - 1st reading
  malnu_conty <- tab2Func("anth_q4p1p1_grouped", "cg_qcounty")
  colnames(malnu_conty) <- c("Mal_Status", "County", "Frequency")
  malnu_conty <- setDF(malnu_conty)
  setorder(malnu_conty, -Frequency)
  
  
  # Nutrition status per county - 2nd reading
  malnu_conty2 <- tab2Func("anth_q4p1p2_grouped", "cg_qcounty")
  colnames(malnu_conty2) <- c("Mal_Status", "County", "Frequency")
  malnu_conty2 <- setDF(malnu_conty2)
  setorder(malnu_conty2, -Frequency)

  # Compare MUAC readings
  reading1 <- data.frame(measurement=working_df$anth_q4p1p1)
  reading1$round <- rep("1st Reading", nrow(reading1))
  reading2 <- data.frame(measurement=working_df$anth_q4p1p2)
  reading2$round <- rep("2st Reading", nrow(reading2))
  measure_df <- rbind(reading1, reading2)
  measure_df <- setDT(measure_df)
  measure_df1 <- measure_df[measurement<=20]

  
    
  #creating the valueBoxOutput content
  output$value1 <- renderValueBox({
    valueBox(
      formatC(total_respondents, format="d", big.mark=',')
      , "No. of Surveyed Respondents"
      ,icon = icon("stats",lib='glyphicon')
      ,color = "purple")
    
    
  })
  
  
  output$value2 <- renderValueBox({

    valueBox(
      formatC(total_res_dup_id, format="d", big.mark=',')
      ,'No. Duplicate Mother IDs'
      ,icon = icon("stats",lib='glyphicon')
      ,color = "orange")

  })

  output$value3 <- renderValueBox({

    valueBox(
      formatC(total_res_miss_id, format="d", big.mark=',')
      ,"Number of missing mother ID"
      ,icon = icon("stats",lib='glyphicon')
      ,color = "red")

  })
  
  output$value4 <- renderValueBox({
    
    valueBox(
      formatC(total_non_issue, format="d", big.mark=',')
      ,"Approved Surveys"
      ,icon = icon("stats",lib='glyphicon')
      ,color = "green")
    
  })
  
  output$interviewer <- renderPlot({
    ggplot(interviewer_complete, aes(x=reorder(Interviewer, -Frequency), y=Frequency)) +
      geom_col(fill="green") + coord_flip() +
      ylab("Completed Interviews") + xlab("Interviewers") + 
      geom_text(size=3, aes(label=Frequency, y=Frequency), position = position_stack(vjust = 0.5)) + 
      ggtitle("Completed per interviewer")
  })


  output$completedbycounty <- renderPlot({
    ggplot(data=interviewer_conty, aes(x=reorder(Interviewer, -Frequency), y=Frequency, fill=County))+
      geom_col(position = position_stack()) + 
      scale_fill_discrete(name="County", 
                          breaks=c("Isiolo", "Turkana"),
                          labels=c("Isiolo", "Turkana")) + 
      xlab("Interviewers") + ylab("Completed Interviews") + coord_flip()+
      geom_text(size=3, aes(label=Frequency, y=Frequency), position = position_stack(vjust = 0.5))
  })
  
  
  
  output$malstatuscounty1 <- renderPlot({
    ggplot(data=malnu_conty, aes(x=reorder(Mal_Status, -Frequency), y=Frequency, fill=County))+
      geom_col(position = position_stack()) + 
      scale_fill_discrete(name="County", 
                          breaks=c("Isiolo", "Turkana"),
                          labels=c("Isiolo", "Turkana")) + 
      xlab("Malnutrition status") + ylab("Completed Interviews") + coord_flip()+
      geom_text(size=3, aes(label=Frequency, y=Frequency), position = position_stack(vjust = 0.5))
  })
  
  output$malstatuscounty2 <- renderPlot({
    ggplot(data=malnu_conty2, aes(x=reorder(Mal_Status, -Frequency), y=Frequency, fill=County))+
      geom_col(position = position_stack()) + 
      scale_fill_discrete(name="County", 
                          breaks=c("Isiolo", "Turkana"),
                          labels=c("Isiolo", "Turkana")) + 
      xlab("Malnutrition status") + ylab("Completed Interviews") + coord_flip()+
      geom_text(size=3, aes(label=Frequency, y=Frequency), position = position_stack(vjust = 0.5))
  })
  
  output$muaccompbox <- renderPlot({
    ggplot(measure_df1, aes(round, measurement)) + geom_boxplot(notch = T, na.rm = T, fill="green") + 
      xlab("Reading") + ylab("Measurement")
  })
  
  output$muaccompdensity <- renderPlot({
    ggplot(measure_df1, aes(measurement)) + 
      geom_density(aes(fill=factor(round)), alpha=0.8) + 
      labs(x="Measurement",
           fill="Readings")
  })
  
  # wrap data in reactive expression in case you
  # need to do something to the data before filtering
  data <- reactive(working_df[, .(key, fi_code_new, cg_qcounty, q_sub_county, q_village, 
                                  q_comm_unit, cg_chv, cg_q1p6, cg_q1p7_auto,
                                  cg_contact, cg_q2p10, child_dob_age_months, 
                                  cg_child_id_auto)])
  
  # initialize the filter set
  filterSet <- initializeFilterSet(filterSet, data)
  
  # the output is a reactive data.frame
  output$data <- DT::renderDataTable(filterSet$output(), options = list(lengthMenu = c(5, 30, 50), pageLength = 10))
  
}
# Run the application 
shinyApp(ui = ui, server = server)

