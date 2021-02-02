

#libraries ----
library(timevis)  # timeline visualization
library(readxl)  # read Excel data
library(xlsx)
library(shiny)  # build Shiny app
library(shinyjs)
library(googledrive)
library(googlesheets4)

#Some helper code

randomID <- function() {
  paste(sample(c(letters, LETTERS, 0:9), 16, replace = TRUE), collapse = "")
}

#define CSS styles
styles <- "
.vis-item.EL { background-color: Gold; color: White; }
.vis-item.ELUA { background-color: #CD212A; color: White; }
.vis-item.UA { background-color: #00A170; color: White; }
.vis-item.milestones { background-color: DarkOrange; color: White; }

.vis-labelset .vis-label.milestones { color: Black; font-size: 0.9em; }
.vis-labelset .vis-label.EL { background: Gold; color: Black; font-size: 0.9em; font-weight: bold}
.vis-labelset .vis-label.ELUA { background: #CD212A; color: Black; font-size: 0.75em; font-weight: bold}
.vis-labelset .vis-label.UA { background: #00A170; color: Black; font-size: 0.9em; font-weight: bold}

.optionsSection {
  border: 1px solid #EEE;
  border-radius: 3px;
  background: #FCFCFC;
  padding: 10px;
  margin-bottom: 20px;
}

#timeline {
  box-shadow: 0px 0px 3px #444;
}

#timeline .vis-item {
  #border-color: #F991A3;
  #background-color: pink;
  #font-size: 15pt;
  #color: purple;
  box-shadow: 3px 3px 5px rgba(100,100,100, 0.6);
}

#timeline .vis-timeline {
  border: 1px solid;
  font-size: 12pt;
  background: white;
}

#timeline .vis-background .vis-minor.vis-odd {
  background: #e7e5e4;
}

#timeline .vis-time-axis .vis-grid.vis-minor {
  border-width: 2px;
  border-color: #e7e5e4;
}

#timeline .vis-time-axis .vis-grid.vis-major {
  border-width: 2px;
  border-color: Black;
}"

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
        column(style="display: inline-block;vertical-align:bottom;",
          2,
          div(id = "saveActions",
                  class = "optionsSection",
                  disabled(actionButton("save", "Save changes"))
                  #actionButton("save", "Save changes")
                  #actionButton("changed", "Changed!")
          )
        )
  ),
  fluidRow(

          column(
              2,
              div(class = "optionsSection",
                  tags$h4("Add Entry:"),
                  textInput("addTitle", "Add Title:", "New Title"),
                  textInput("addText", "Add Content:", "New item"),
                  dateInput("addLDate", NULL, "2021-01-01"),
                  dateInput("addRDate", NULL, "2021-01-03"),
                  selectInput("addGroup", "Group:", c("EOSC-Life" = "EL", "EOS-Life + UEB Admin" = "ELUA", "UEB Admin" = "UA")),
                  actionButton("addBtn", "Add")
              ),
              div(id = "interactiveActions",
                  class = "optionsSection",
                  tags$h4("More Actions:"),
                  actionButton("fit", "Fit all items"),
                  actionButton("center", "Center around Today")
              )
          ),

          column(10,
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
      #multiselect = TRUE
      orientation = 'top'
    )
    timevis(data = data, 
            groups = groups, options = config)
  })

  #Add item event
  observeEvent(input$addBtn, {
    addItem("timeline",
            data = list(id = randomID(),
                        content = input$addText,
                        start = input$addLDate,
                        end = input$addRDate,
                        group = input$addGroup,
                        className = input$addGroup,
                        title = input$addTitle))
  })
  observeEvent(input$fit, {
    fitWindow("timeline")
  })
  observeEvent(input$center, {
    centerTime("timeline", format(Sys.Date(), format="%Y-%m-%d"))
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
