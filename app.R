

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
.vis-item.EL { background-color: #ffbf00; color: White; border-color: #ffbf00; }
.vis-item.ELUA { background-color: #CD212A; color: White; border-color: #CD212A; }
.vis-item.UA { background-color: #00A170; color: White; border-color: #00A170; }
.vis-item.milestones { background-color: DarkOrange; color: White; }

.vis-labelset .vis-label.milestones { color: Black; font-size: 0.9em; }
.vis-labelset .vis-label.EL { background: White; color: #ffbf00; font-size: 0.9em; font-weight: bold}
.vis-labelset .vis-label.ELUA { background: White; color: #CD212A; font-size: 0.75em; font-weight: bold}
.vis-labelset .vis-label.UA { background: White; color: #00A170; font-size: 0.9em; font-weight: bold}

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
  border: 1px solid;
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
      
  #set CSS style
  tags$style(styles, type="text/css"),
      

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
                  textInput("addLink", "Add Link"),
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


  #goooglesheets4 inputs
  ss <- '1zwX6k60yTzolEqOhD61384TAERmqTy1BLRLNzpGksmU'

  ranged <- read_sheet(ss, sheet = 'Ranged')
  milestones <- read_sheet(ss, sheet = 'Milestones')
  groups <- read_sheet(ss, sheet = 'Groups')
  

  
  data = rbind(ranged, milestones)
  
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
                        className = ifelse(input$addRDate=='', 'milestones', input$addGroup), #non functional by now
                        title = input$addTitle,
                        docLink = input$addLink))
  })

  #Fill the selected item table

  output$selected_datatable <- renderTable({
    selected_entry <- input$timeline_data[which(input$timeline_data$id == input$timeline_selected), ]
    data <- selected_entry[, c("title", "content", "start", "end", "docLink")]
    data$start <- prettyDate(data$start, 'onlydate')
    if (!is.null(data$end)) {
      data$end <- prettyDate(data$end, 'onlydate')
    }
    data$docLink <- ifelse(data$docLink == '', '', 
                           paste0('<A href=',data$docLink,' target="_blank" rel="noopener noreferrer">Go</A>'))
    data
  }, sanitize.text.function = function(x) x)


  observeEvent(input$fit, {
    fitWindow("timeline")
  })
  observeEvent(input$center, {
    centerTime("timeline", format(Sys.Date(), format="%Y-%m-%d"))
  })

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
