#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

pacman::p_load(tidyverse, 
       shiny, 
       crqa, 
       plotly, 
       SparseM, 
       shinycssloaders, 
       promises, 
       future)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("CRQA_GUI"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            # Input: Select a file ----
            fileInput("file1", "Choose Timeseries 1",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv",
                                 ".txt")
            ),
            
            fileInput("file2", "Choose Timeseries 2",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv",
                                 ".txt")
            ),
            
            textInput("maxDelay", "MAX Delay"),
            
            actionButton("getParms","Get CRQA Parameters"),
            
            textInput("embed", "Embedding dimension"),
            textInput("delay", "Delay"),
            textInput("radius", "Radius"),
            
            actionButton("getCRP","Plot CRP"),
            actionButton("getCRQA","Run CRQA"),
            actionButton("getDiag","Plot DRP")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotlyOutput("tsPlot") %>% withSpinner(color="#0dc5c1"),
            plotlyOutput("tsPlot_normed") %>% withSpinner(color="#0dc5c1"),
            tableOutput("parameters") %>% withSpinner(color="#0dc5c1"),
            tableOutput("CRQA") %>% withSpinner(color="#0dc5c1"),
            plotOutput("CRP") %>% withSpinner(color="#0dc5c1"),
            plotlyOutput("diagP") %>% withSpinner(color="#0dc5c1")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
# Globals

    #parms <- crqa::optimizeParam(ts1,ts2,par,min.rec = 2,max.rec = 5)
    
    
    output$tsPlot <- renderPlotly({
        

                if(is.null(input$file1))     return(NULL) 
                if(is.null(input$file2))     return(NULL)
                
                filename1 <- input$file1$datapath
                filename2 <- input$file2$datapath
                
                ts1 <- readr::read_csv(filename1, col_names = FALSE) 
                ts2 <- readr::read_csv(filename2, col_names = FALSE)
                
                df <- data.frame(ts1,ts2,1:2500)
                names(df) <- c("ts1","ts2","time")
                
                df <- tidyr::gather(df,key = "timeseries",value = "position",1:2) %>% as_tibble()
                
                
                p <- ggplot(df, aes(x = time, y=position)) +
                    geom_line(aes(color=timeseries))
                ggplotly(p)           
        
        
        
    })
    
    output$tsPlot_normed <- renderPlotly({
        

                if(is.null(input$file1))     return(NULL) 
                if(is.null(input$file2))     return(NULL)
                
                filename1 <- input$file1$datapath
                filename2 <- input$file2$datapath
                
                ts1 <- readr::read_csv(filename1, col_names = FALSE) %>% scale()
                ts2 <- readr::read_csv(filename2, col_names = FALSE) %>% scale()
                
                df <- data.frame(ts1,ts2,1:2500)
                names(df) <- c("ts1","ts2","time")
                
                df <- tidyr::gather(df,key = "timeseries",value = "position",1:2) %>% as_tibble()
                

        
        p <- ggplot(df, aes(x = time, y=position)) +
            geom_line(aes(color=timeseries))
        ggplotly(p)
        
        
        
    })
    
    getParameters <- observeEvent(input$getParms,{
        
 

            if(is.null(input$file1))     return(NULL) 
            if(is.null(input$file2))     return(NULL)
            
            filename1 <- input$file1$datapath
            filename2 <- input$file2$datapath
            
            ts1 <- readr::read_csv(filename1, col_names = FALSE) 
            ts2 <- readr::read_csv(filename2, col_names = FALSE)
            
            timeseries1 <- ts1 
            timeseries2 <- ts2 
            
            lgM <- as.numeric(input$maxDelay)
            
            par <- list(lgM = lgM,
                        steps = seq(1, 10, 1),
                        radiusspan = 200, 
                        radiussample = 10,
                        normalize = 2, 
                        rescale = 1, 
                        mindiagline = 2,
                        minvertline = 2, 
                        tw = 0, 
                        whiteline = FALSE,
                        recpt = FALSE, 
                        fnnpercent = 10,
                        typeami="mindip")
            
            getParms <- crqa::optimizeParam(timeseries1,timeseries2,par,min.rec = 2,max.rec = 5) %>% as_tibble()

            output$parameters <- renderTable({
                getParms
            })
            
            output$RP <- renderPlot({
                SparseM::image(getParms$RP)
            })
    })
    
    getCRQA <- observeEvent(input$getCRQA,{
        
        
        filename1 <- input$file1$datapath
        filename2 <- input$file2$datapath
        
        ts1 <- readr::read_csv(filename1, col_names = FALSE) 
        ts2 <- readr::read_csv(filename2, col_names = FALSE)
        
        timeseries1 <- ts1$X1
        timeseries2 <- ts2$X1
        
        normalize = 2
        rescale = 1            
        mindiagline = 2
        minvertline = 2 
        tw = 0 
        whiteline = FALSE
        recpt = FALSE
        
        delay <- as.numeric(input$delay)
        embed <- as.numeric(input$embed) 
        radius <- as.numeric(input$radius) 
        
        crqa_out <- crqa(ts1=timeseries1,
                         ts2=timeseries2,
                         delay = delay,
                         embed = embed,
                         radius = radius, 
                         rescale = rescale,
                         normalize = normalize,
                         mindiagline = mindiagline, 
                         minvertline = minvertline,
                         tw = tw, 
                         whiteline = whiteline, 
                         recpt = recpt
        )
        
        crqa_out_up <- crqa(ts1=timeseries1,
                            ts2=timeseries2,
                            delay = delay,
                            embed = embed,
                            radius = radius, 
                            rescale = rescale,
                            normalize = normalize,
                            mindiagline = mindiagline, 
                            minvertline = minvertline,
                            tw = tw, 
                            whiteline = whiteline, 
                            recpt = recpt,
                            side="upper"
        )
        
        crqa_out_lo <- crqa(ts1=timeseries1,
                            ts2=timeseries2,
                            delay = delay,
                            embed = embed,
                            radius = radius, 
                            rescale = rescale,
                            normalize = normalize,
                            mindiagline = mindiagline, 
                            minvertline = minvertline,
                            tw = tw, 
                            whiteline = whiteline, 
                            recpt = recpt,
                            side="lower"
                            
        )
        
        both <- crqa_out[1:9] %>% as_tibble()        
        upper <- crqa_out_up[1:9] %>% as_tibble()        
        lower <- crqa_out_lo[1:9] %>% as_tibble()
        
        values <- bind_rows(both, upper, lower)
        side <- c("both", "upper", "lower") %>% as_tibble()
        names(side) <- "side"
        
        # diagCRQA
        
        diag_out <- drpdfromts(ts1 = ts1, 
                               ts2 = ts2, 
                               ws = 250,
                               datatype = "continuous",
                               radius = radius,
                               delay = delay,
                               embed = embed,
                               rescale = rescale,
                               normalize = normalize,                     
                               mindiagline = mindiagline, 
                               minvertline = minvertline,
                               tw = tw) 
        maxlag <- diag_out$maxlag %>% as_tibble()
        maxlag[2:3,] <- maxlag[1]
        names(maxlag) <- "maxlag"
        
        getCRQA <- bind_cols(side,values, maxlag)
        
        output$CRQA <- renderTable({
            getCRQA
        })
        
    })
    
    getCRP <- observeEvent(input$getCRP, {
        
        
        filename1 <- input$file1$datapath
        filename2 <- input$file2$datapath
        
        ts1 <- readr::read_csv(filename1, col_names = FALSE) 
        ts2 <- readr::read_csv(filename2, col_names = FALSE)
        
        timeseries1 <- ts1$X1
        timeseries2 <- ts2$X1
        
        normalize = 2
        rescale = 1            
        mindiagline = 2
        minvertline = 2 
        tw = 0 
        whiteline = FALSE
        recpt = FALSE
        
        delay <- as.numeric(input$delay)
        embed <- as.numeric(input$embed) 
        radius <- as.numeric(input$radius) 
        
        crqa_out <- crqa(ts1=timeseries1,
                         ts2=timeseries2,
                         delay = delay,
                         embed = embed,
                         radius = radius, 
                         rescale = rescale,
                         normalize = normalize,
                         mindiagline = mindiagline, 
                         minvertline = minvertline,
                         tw = tw, 
                         whiteline = whiteline, 
                         recpt = recpt
        )
        
        output$CRP <- renderPlot({
            SparseM::image(crqa_out$RP)
        })
        
    })
    
    getDiag <- observeEvent(input$getDiag,{
        
        filename1 <- input$file1$datapath
        filename2 <- input$file2$datapath
        
        ts1 <- readr::read_csv(filename1, col_names = FALSE)$X1
        ts2 <- readr::read_csv(filename2, col_names = FALSE)$X1
        
        timeseries1 <- ts1$X1
        timeseries2 <- ts2$X1
        
        normalize = 2
        rescale = 1            
        mindiagline = 2
        minvertline = 2 
        tw = 0 
        whiteline = FALSE
        recpt = FALSE
        
        delay <- as.numeric(input$delay)
        embed <- as.numeric(input$embed) 
        radius <- as.numeric(input$radius) 
        
        # diagCRQA
        
        diag_out <- drpdfromts(ts1 = ts1, 
                               ts2 = ts2, 
                               ws = 250,
                               datatype = "continuous",
                               radius = radius,
                               delay = delay,
                               embed = embed,
                               rescale = rescale,
                               normalize = normalize,                     
                               mindiagline = mindiagline, 
                               minvertline = minvertline,
                               tw = tw) 
        maxlagProfile <- tibble("RR" = diag_out$profile,
                                "lag" = -250:250)
        
        p <- ggplot(maxlagProfile, aes(x=lag,y=RR)) + geom_line() + theme_bw()
        
        output$diagP <- renderPlotly({
            ggplotly(p)
        })
    })
}    


# Run the application 
shinyApp(ui = ui, server = server)
