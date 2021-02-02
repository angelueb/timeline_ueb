### Interactive Product Roadmap
### Stephen Howe
### August 15, 2019

### This RShiny app generates an interactive, JavaScript-based timeline
### in which to view the roadmap for multiple teams.
### Data is sourced from the Excel spreadsheet referenced in the code.

#libraries ----
library(timevis)  # timeline visualization
library(readxl)  # read Excel data
library(xlsx)
library(shiny)  # build Shiny app
library(shinyjs)
library(googledrive)
library(googlesheets4)

#define CSS styles for roadmap
styles <- "
.vis-item.EL { background-color: MidnightBlue; color: White; }
.vis-item.ELUA { background-color: SteelBlue; color: White; }
.vis-item.UA { background-color: MediumBlue; color: White; }
.vis-item.milestones { background-color: DarkOrange; color: White; }

.vis-labelset .vis-label.milestones { color: Black; }
.vis-labelset .vis-label.EL { color: Black; }
.vis-labelset .vis-label.UA { color: Black; }"

# Shiny UI
ui <- fluidPage(
  #titlePanel("Cross-Team Product Roadmap"),
  
  #sidebarLayout(
    
    #sidebarPanel(
      #define input selector
      #checkboxGroupInput("group", "Select Product Team(s):",
       #                  c("Team 1" = "team_1",
       #                    "Team 2" = "team_2",
       #                    "Team 3" = "team_3",
       #                    "Team 4" = "team_4",
       #                    "Team 5" = "team_5",
       #                    "Team 6" = "team_6"))
    #),
    
    #mainPanel(
      
      #set CSS style
      tags$style(styles, type="text/css"),
      
      #output timeline
      #timevisOutput("timeline")
    #)
  #)
  fluidRow(
        column(
          12,
          div(id = "Action_label",
                  class = "optionsSection",
                  tags$h4("Actions:")
                  
          )
        )
  ),
  fluidRow(
        column(
          3,
          div(id = "prefix_div",
                  class = "optionsSection",
                  textInput("prefix", "Prefix")
                  
          )
        ),
        column(style="display: inline-block;vertical-align:bottom;",
          3,
          div(id = "saveActions",
                  class = "optionsSection",
                  disabled(actionButton("save", "Save changes"))
                  #actionButton("save", "Save changes")
                  #actionButton("changed", "Changed!")
          )
        )
  ),
  fluidRow(column(12,
            timevisOutput("timeline")
          ))
)

server <- function(input, output, session) {
  #get data
  #ranged <- read_excel(path ="timelineUEB_1612264248.xlsx",
                         #sheet = "Ranged")
  #milestones <- read_excel(path = "timelineUEB_1612264248.xlsx",
                           #sheet = "Milestones")
  #groups <- read_excel(path ="timelineUEB_1612264248.xlsx",
                       #sheet = "Groups")

  #trying auth with Google
  options(gargle_oauth_cache = "config")
  #options(gargle_quiet = FALSE)
  drive_auth(cache = "config", email = "angelskunkworks@gmail.com")
  sheets_auth(token = drive_token())


  #goooglesheets4 inputs
  ss <- '1zwX6k60yTzolEqOhD61384TAERmqTy1BLRLNzpGksmU'

  ranged <- read_sheet(ss, sheet = 'Ranged')
  milestones <- read_sheet(ss, sheet = 'Milestones')
  groups <- read_sheet(ss, sheet = 'Groups')
  

  
  data = rbind(ranged, milestones)

  y <- reactiveVal(data)
observeEvent(y(), {
  enable("save")
})


  
  #create timeline output
  output$timeline <- renderTimevis({
    #timevis(data = subset(data, data$group %in% input$group | data$group == "milestones"), 
            #groups = subset(groups, groups$id %in% input$group | groups$id == "milestones"))
    config <- list(
      editable = TRUE,
      multiselect = TRUE
    )
    timevis(data = data, 
            groups = groups, options = config)
  })

  observeEvent(input$save, {
    milestones = input$timeline_data[which(input$timeline_data$className == "milestones"), ]
    ranged = input$timeline_data[which(input$timeline_data$className != "milestones"), ]
    
    #Write using xlsx
    #xlsfname <- paste0('timelineUEB_',as.integer(Sys.time()),".xlsx")
    #oldOpt <- options()
    #options(xlsx.date.format="yyyy-MMM-dd")
    #write.xlsx(as.data.frame(ranged), xlsfname, sheetName="Ranged", col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)
    #write.xlsx(as.data.frame(milestones), xlsfname, sheetName="Milestones", col.names=TRUE, row.names=FALSE, append=TRUE, showNA=FALSE)
    #write.xlsx(as.data.frame(groups), xlsfname, sheetName="Groups", col.names=TRUE, row.names=FALSE, append=TRUE, showNA=FALSE)
    #options(oldOpt)
    #disable("save")

    #Write using googlesheets4
    write_sheet(as.data.frame(ranged), ss, sheet = "Ranged")
    write_sheet(as.data.frame(milestones), ss, sheet = "Milestones")
    write_sheet(as.data.frame(groups), ss, sheet = "Groups")
  })
}

shinyApp(ui = ui, server = server)
