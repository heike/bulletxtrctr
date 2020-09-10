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

shinyApp(

    ui = fluidPage(
        selectInput("k","Investigate kth plot:", selected = 1,
                    choices=1:length(shiny.tt$grooves)),
        textOutput("groovelocations"),
        plotOutput("groovePlot", click = "plot_click"),
        actionButton("confirm", "Confirm"),
        actionButton("save", "Save"),
        actionButton("mark", "Mark"),
        actionButton("unmark", "Unmark"),
        actionButton("displayx3p", "Display x3p image"),
        textOutput("ccvalue"),
        textInput("cc", "change crosscut to"),
        actionButton("updateCC", "Update Crosscut Value"),
        actionButton("drawsig", "Draw Signature"),
        plotOutput("sigPlot"),
        rglwidgetOutput("x3prgl")
    ),

    server = function(input, output, session) {
        
        output$groovePlot <- renderPlot({
            k <- as.numeric(input$k)
            p <- shiny.tt$grooves[[k]]$plot

            p
        })
        output$groovelocations <- renderText({
            paste("Left Groove: ",shiny.tt$grooves[[as.numeric(input$k)]]$groove[1],
                  " Right Groove: ",shiny.tt$grooves[[as.numeric(input$k)]]$groove[2])
        })
        output$sigPlot <- renderPlot({
            k <- as.numeric(input$k)
            ggplot() + geom_blank()
        })
        
        
        output$ccvalue <- renderText({ 
            paste("Current Crosscut Value: ", 
                                             shiny.tt$crosscut[as.numeric(input$k)] ) })
        
        observeEvent(input$confirm,{
            cat("Confirmed", input$k, "\n")
            updateSelectInput(session, "k","Investigate kth plot:",
                              selected = as.numeric(input$k)+1,
                              choices=1:length(shiny.tt$grooves))
        })
        observeEvent(input$save,{
            saveRDS(shiny.tt$grooves, file="data/bullets_output.rds")
            cat("bullets data saved\n")
        })
        observeEvent(input$mark,{
            shiny.tt$grooves[[as.numeric(input$k)]]$marked <<- TRUE
            cat("Marked Groove:", input$k, "\n")
        })
        
        observeEvent(input$unmark,{
            shiny.tt$grooves[[as.numeric(input$k)]]$marked <<- FALSE
            cat("Unmarked Groove:", input$k, "\n")
        })
        
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
            cat("Bullet ID:", shiny.tt$scan_id[k], "\n")
            output$ccvalue <- renderText({ 
                paste("Current Crosscut Value: ", shiny.tt$crosscut[as.numeric(input$k)] ) })

        })

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
        
        observeEvent(input$displayx3p, {
            k <- as.numeric(input$k)
            output$x3prgl <- renderRglwidget({
                image_x3p(shiny.tt$x3p[k][[1]])
            })
        })
        
        observeEvent(input$k, {
            output$sigPlot <- renderPlot({
                k <- as.numeric(input$k)
                ggplot() + geom_blank()
            })
        })

    },

    options = list(height = 500)
)

