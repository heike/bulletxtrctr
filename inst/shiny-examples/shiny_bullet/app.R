#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rgl)
library(ggplot2)

hasname_x3p <- assertthat::has_name(shiny.tt, "x3p")
hasname_scanid <- assertthat::has_name(shiny.tt, "scan_id")
hasname_grooves <- assertthat::has_name(shiny.tt, "grooves")
check_names_all <- shiny.tt %>% assertthat::has_name(c("x3p", "crosscut", "grooves", "scan_id")) %>% all()

if(hasname_scanid){ all_scan_id <- shiny.tt$scan_id } else {
    all_scan_id <- NULL
}

if((!hasname_x3p) & (!hasname_grooves)) {stop("NEED X3P OR GROOVES. \n")}

n <- nrow(shiny.tt)

shinyApp(

    ui = fluidPage(
        selectInput("k","Investigate kth land:", selected = 1,
                    choices=1:n),
        conditionalPanel(
            condition = "output.hasname_scanid",
            selectInput("scanID","Investigate according to the Scan ID :",
                        selected = all_scan_id[1],
                        choices=all_scan_id)
        ),

        textOutput("groovelocations"),
        plotOutput("groovePlot", click = "plot_click"),
        actionButton("confirm", "Confirm"),
        actionButton("save", "Save"),
        actionButton("mark", "Mark"),
        actionButton("unmark", "Unmark"),
        conditionalPanel(
            condition = "output.hasname_x3p",
            actionButton("displayx3p", "Display x3p image"),
            textOutput("ccvalue"),
            # textInput("cc", "change crosscut to"),
            numericInput("cc", "change crosscut to:", 100, min = 0, max = 1000),
            sliderInput("cc_slide", "change crosscut with a slider:",
                        min = 0, max = 1000,
                        value = 100, step = 0.1),
            actionButton("updateCC", "Update Crosscut Value"),
            actionButton("drawsig", "Draw Signature"),
            plotOutput("sigPlot"),
            rglwidgetOutput("x3prgl")
        )
    ),

    server = function(input, output, session) {

        # if grooves are provided, draw plots
        if(hasname_grooves){
            output$groovePlot <- renderPlot({
                k <- as.numeric(input$k)
                p <- shiny.tt$grooves[[k]]$plot

                p
            })
            output$groovelocations <- renderText({
                paste("Left Groove: ",shiny.tt$grooves[[as.numeric(input$k)]]$groove[1],
                      " Right Groove: ",shiny.tt$grooves[[as.numeric(input$k)]]$groove[2])
            })
        }

        # if x3p are provided, give all other stuff
        if(hasname_x3p){
            output$sigPlot <- renderPlot({
                k <- as.numeric(input$k)
                ggplot() + geom_blank()
            })
            output$ccvalue <- renderText({
                paste("Current Crosscut Value: ",
                      shiny.tt$crosscut[as.numeric(input$k)] ) })
        }

        # CONFIRM
        observeEvent(input$confirm,{
            cat("Confirmed", input$k, "\n")
            updateSelectInput(session, "k","Investigate kth plot:",
                              selected = as.numeric(input$k)+1,
                              choices=1:length(shiny.tt$grooves))
        })

        # SAVE
        observeEvent(input$save,{
            saveRDS(shiny.tt$grooves, file="data/bullets_output.rds")
            cat("bullets data saved\n")
        })

        # MARK
        observeEvent(input$mark,{
            k <- as.numeric(input$k)
            shiny.tt$grooves[[as.numeric(input$k)]]$marked <<- TRUE
            cat("Marked Groove:", input$k, "\n")
            if (hasname_scanid) {
                cat("Marked Groove Bullet ID:", shiny.tt$scan_id[k], "\n")
            } else {
                cat("Bullet ID is not available. \n")
            }

        })

        # UNMARK
        observeEvent(input$unmark,{
            shiny.tt$grooves[[as.numeric(input$k)]]$marked <<- FALSE
            cat("Unmarked Groove:", input$k, "\n")
        })

        # EVENT: UPDATE CROSSCUT VALUE
        observeEvent(input$updateCC, {

            k <- as.numeric(input$k)
            tmp.tt <- shiny.tt %>% slice(k)

            tmp.tt$crosscut <- as.numeric(input$cc)

            tmp.tt <- tmp.tt %>%
                mutate(ccdata = purrr::map2(.x = x3p, .y = crosscut, .f = x3p_crosscut)
                ) %>%
                mutate(grooves = ccdata %>% purrr::map(.f = cc_locate_grooves, method = "middle", adjust = 30, return_plot = TRUE))

            shiny.tt[k, c("crosscut", "ccdata", "grooves")] <<- tmp.tt %>% select(crosscut, ccdata, grooves)

            output$groovePlot <- renderPlot({
                k <- as.numeric(input$k)
                p <- shiny.tt$grooves[[k]]$plot
                p
            })
            output$groovelocations <- renderText({
                k <- as.numeric(input$k)
                paste("Left Groove: ",shiny.tt$grooves[[k]]$groove[1],
                      " Right Groove: ",shiny.tt$grooves[[k]]$groove[2])
            })
            cat("updated crosscut value: ", shiny.tt$crosscut[k], "\n")
            if (hasname_scanid) {
                cat("Bullet ID:", shiny.tt$scan_id[k], "\n")
            } else {
                cat("Bullet ID is not available. \n")
            }
            output$ccvalue <- renderText({
                paste("Current Crosscut Value: ", shiny.tt$crosscut[as.numeric(input$k)] ) })

        })

        # EVENT: DRAW SIGNATURE
        observeEvent(input$drawsig, {
            k <- as.numeric(input$k)
            tmp.tt <- shiny.tt %>% slice(k) %>% mutate(sigs = purrr::map2(.x = ccdata, .y = grooves,
                                                             .f = function(x, y) {
                                                                 cc_get_signature(ccdata = x, grooves = y, span1 = 0.75, span2 = 0.03)
                                                             }))
            output$sigPlot <- renderPlot({
                k <- as.numeric(input$k)
                p <- tmp.tt$sigs[[1]] %>% filter(!is.na(sig), !is.na(raw_sig)) %>%
                    ggplot(aes(x = x)) +
                    geom_line(aes(y = raw_sig), colour = "grey70") +
                    geom_line(aes(y = sig), colour = "grey30") +
                    ylab("value") + ylim(c(-7.5, 7.5)) + theme_bw()
                p
            })
        })

        # EVENT: PLOT CLICK
        observeEvent(input$plot_click,{
            k <- as.numeric(input$k)
            xloc <- input$plot_click$x

            gr <- shiny.tt$grooves[[k]]$groove
            if (abs(gr[1]-xloc) < abs(gr[2]-xloc)) {
                shiny.tt$grooves[[k]]$groove[1] <<- xloc
            } else {
                shiny.tt$grooves[[k]]$groove[2] <<- xloc
            }
            output$groovePlot <- renderPlot({
                k <- as.numeric(input$k)
                p <- shiny.tt$grooves[[k]]$plot +
                    geom_vline(xintercept = shiny.tt$grooves[[k]]$groove[1], colour="red") +
                    geom_vline(xintercept = shiny.tt$grooves[[k]]$groove[2], colour="red")

                p
            })
            output$groovelocations <- renderText({
                paste("Left Groove: ",shiny.tt$grooves[[as.numeric(input$k)]]$groove[1],
                      " Right Groove: ",shiny.tt$grooves[[as.numeric(input$k)]]$groove[2])
            })
        })

        # EVENT: DISPLAY X3P
        observeEvent(input$displayx3p, {
            k <- as.numeric(input$k)
            if (hasname_scanid) {
                cat("Displaying Bullet ID:", shiny.tt$scan_id[k], "\n")
            } else {
                cat("Bullet ID is not available. \n")
            }
            output$x3prgl <- renderRglwidget({
                image_x3p(shiny.tt$x3p[k][[1]] %>%
                              x3p_add_hline(yintercept = shiny.tt$crosscut[k],
                                            size = 10))
            })
        })

        # EVENT: SELECT K
        observeEvent(input$k, {
            output$sigPlot <- renderPlot({
                k <- as.numeric(input$k)
                ggplot() + geom_blank()
            })

            if (hasname_scanid){
                updateSelectInput(session, "scanID","Investigate according to the Scan ID :",
                                  selected = all_scan_id[as.numeric(input$k)],
                                  choices = all_scan_id)
            }

        })

        # EVENT: SELECT SCANID
        if(hasname_scanid) {
            observeEvent(input$scanID, {
                scanidx <- shiny.tt %>% tibble::rowid_to_column() %>%
                    filter(scan_id == input$scanID) %>% pull(rowid)

                updateSelectInput(session, "k","Investigate kth land:",
                                  selected = scanidx,
                                  choices=1:n)
            })
        }

        # EVENT: INPUT CROSSCUT VALUE WITH TEXT
        observeEvent(input$cc, {
            updateSliderInput(session, "cc_slide", "change crosscut with a slider",
                              min = 0, max = 1000,
                              value = as.numeric(input$cc), step = 0.1)
        })
        # EVENT: INPUT CROSSCUT VALUE WITH SLIDER
        observeEvent(input$cc_slide, {
            updateNumericInput(session, "cc", "change crosscut to:",
                               as.numeric(input$cc_slide), min = 0, max = 1000)
        })


        # CONDITIONAL PANEL SETUP
        output$hasname_x3p <- reactive({
            hasname_x3p
        })
        outputOptions(output, "hasname_x3p", suspendWhenHidden = FALSE)

        output$hasname_scanid <- reactive({
            hasname_scanid
        })
        outputOptions(output, "hasname_scanid", suspendWhenHidden = FALSE)

    },

    options = list(height = 500)
)

