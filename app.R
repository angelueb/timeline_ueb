#First attemp of Shiny timeline app. As quick and dirty as they come.

#TODO:
# - Auto saving (decide on eny changed, timed, etc. It will depend on the storage backend).
# - Reactively edition of selected item (on the existing space of the selected item table or any other better place).
# - Fix/add milestones, currently doesn't work but that's not breaking any other fucntionality.
# - Field validation
# - Error with selected item qhen there are no more entries in a group
# - CSS issue with top group, space on top the same height as the label of the group
# - Of course, migrate to Shiny server, nginx and ssl (as containerized as practically possible) 

#libraries ----
library(timevis)  # timeline visualization
library(readxl)  # read Excel data
library(xlsx)
library(shiny)  # build Shiny app
library(shinyjs)
library(googledrive)
library(googlesheets4)

#Little helpers

randomID <- function() {
  paste(sample(c(letters, LETTERS, 0:9), 16, replace = TRUE), collapse = "")
}

prettyDate <- function(d, mode) {
  if (is.null(d)) return()
  posix <- as.POSIXct(d, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
  corrected <- lubridate::with_tz(posix, tzone = Sys.timezone())
  #else(mode == 'onlydate', format(corrected, "%Y-%m-%d %Z"), format(corrected, "%Y-%m-%d %H:%M:%OS %Z"))
  if (mode=='onlydate') {
    format(corrected, "%Y-%m-%d %Z")
  } else{
    format(corrected, "%Y-%m-%d %H:%M:%OS %Z")
  }

}

#define CSS styles
styles <- "
.vis-item.EL { background-color: #ffbf00; color: White; border-color: #ffbf00;}
.vis-item.ELUA { background-color: #CD212A; color: White; border-color: #CD212A; }
.vis-item.UA { background-color: #00A170; color: White; border-color: #00A170; }


.vis-foreground .vis-group.EL { border-bottom: 2px solid #e7e5e4;}
.vis-foreground .vis-group.ELUA { border-bottom: 2px solid #e7e5e4;}
#.vis-foreground .vis-group.UA { border-bottom: 1px solid Black;}

.vis-labelset .vis-label.EL { background: White; color: #ffbf00; font-size: 0.9em; font-weight: bold; border-bottom: 2px solid #e7e5e4;}
.vis-labelset .vis-label.ELUA { background: White; color: #CD212A; font-size: 0.75em; font-weight: bold; border-bottom: 2px solid #e7e5e4;}
.vis-labelset .vis-label.UA { background: White; color: #00A170; font-size: 0.9em; font-weight: bold; }

.optionsSection {
  border: 1px solid #EEE;
  border-radius: 3px;
  background: #FCFCFC;
  padding: 10px;
  margin-bottom: 20px;
}

#selectedInfo {
  font-size: 10pt
}

#timeline {
  
}

#timeline .vis-item {
  box-shadow: 3px 3px 5px rgba(100,100,100, 0.6);
}

#timeline .vis-timeline {
  border: 1px solid #cfcbc9;
  border-radius: 5px;
  font-size: 12pt;
}

#timeline .vis-background .vis-minor.vis-odd {
  background: #FCFCFC;
}

#timeline .vis-time-axis {
  
}

#timeline .vis-time-axis .vis-grid.vis-minor {
  border-width: 2px;
  border-color: #e7e5e4;
}

#timeline .vis-time-axis .vis-grid.vis-major {
  border-width: 1px;
  border-color: Black;
}"

# Shiny UI
ui <- fluidPage(
      
  #Set CSS style
  tags$style(styles, type="text/css"),
      
  #Fluid rows and columns based interface (quick and dirty)
  fluidRow(
        column(style="display: inline-block;vertical-align:bottom;",
          2,
          div(id = "saveActions",
                  class = "optionsSection",
                  tags$h4("Save (don't re-click quickly!):"),
                  actionButton("save", "Save changes")
          )
        ),
        column(style="display: inline-block;vertical-align:bottom;",
          10,
          div(id = "selectedInfo",
                  class = "optionsSection",
                  #tags$h5("Selected Item:"),
                  tableOutput("selected_datatable")
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
                  textInput("addLink", "Add Link", "No", placeholder="Enter something"),
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
  #get data with readxl
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


  #goooglesheets4 data reading and arrangement
  ss <- '1zwX6k60yTzolEqOhD61384TAERmqTy1BLRLNzpGksmU'

  ranged <- read_sheet(ss, sheet = 'Ranged')
  milestones <- read_sheet(ss, sheet = 'Milestones')
  groups <- read_sheet(ss, sheet = 'Groups')

  ranged$docLink <- ifelse(is.na(ranged$docLink), 'No', ranged$docLink)
  milestones$docLink <- ifelse(is.na(milestones$docLink), 'No', milestones$docLink)

  data = rbind(ranged, milestones)
  
  #Create timeline output
  output$timeline <- renderTimevis({
    config <- list(
      editable = TRUE,
      #multiselect = TRUE
      orientation = 'top',
      align = "center",
      margin = htmlwidgets::JS('{ 
                                  axis: 9,
                                  item: {
                                          vertical: 15,
                                          horizontal: 3
                                  }
                              }')
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
                        className = ifelse(as.character(input$addRDate) == '', 'milestones', input$addGroup), #not working
                        title = input$addTitle,
                        #docLink = input$addLink
                        docLink = ifelse(identical(input$addLink, ''), 'No', input$addLink)
                        ))
  })

  #Fill the selected item table
  output$selected_datatable <- renderTable({
    #selected_entry <- input$timeline_data[which(input$timeline_data$id == input$timeline_selected), ]
    selected_entry <- subset(input$timeline_data, input$timeline_data$id == input$timeline_selected)
    data <- selected_entry[, c("title", "content", "start", "end", "group", "docLink")]
    data$start <- prettyDate(data$start, 'onlydate')
    if (!is.null(data$end)) {
      data$end <- prettyDate(data$end, 'onlydate')
    }
    data$group <- if (identical(data$group, 'EL')) 'EOSC-Life' else if (identical(data$group, 'ELUA')) 'EOSC-Life + UEB Admin' else if (identical(data$group, 'UA')) 'UEB Admin'
    data$docLink <- ifelse(data$docLink == 'No', 'No', 
                           paste0('<A href=',data$docLink,' target="_blank" rel="noopener noreferrer">Go</A>'))
    data
  }, sanitize.text.function = function(x) x)

  #More actions' events
  observeEvent(input$fit, {
    fitWindow("timeline")
  })
  observeEvent(input$center, {
    centerTime("timeline", format(Sys.Date(), format="%Y-%m-%d"))
  })

  #Saving to Google Sheet
  observeEvent(input$save, {
    milestones = input$timeline_data[which(input$timeline_data$className == "milestones"), ]
    ranged = input$timeline_data[which(input$timeline_data$className != "milestones"), ]
    
    #Write using xlsx, BUGGY!!!!!
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
