


data <- eventReactive (input$go,{
  
  req(input$account)
  
  apps <- applications(input$account) %>% 
      mutate(created=str_sub(created_time,1,10),updated=str_sub(updated_time,1,10))
  
  apps$name <- as.character(apps$name)
  apps$status <- as.character(apps$status)
  
  print(glimpse(apps))
  
  choices <- sort(apps$name)
  
  info=list(apps=apps,choices=choices)
  return(info)
  
})

output$appSelect <- renderUI({
  
  
  inputPanel(
    selectInput("app", label="Select App",selected="premierLeague", data()$choices),
    actionButton("getChart","Get Chart"))
})

output$appsTable <- DT::renderDataTable({
 
  
data()$apps %>%
    select(name,status,created,updated) %>% 
     arrange(status) %>% 
  DT::datatable(class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = TRUE, searching = TRUE,info=FALSE))
})



appData <- eventReactive (input$getChart,{
  
  req(input$app)
  req(input$account)
  appInfo <- showUsage(appDir = getwd(), appName = input$app, account = input$account,
                                server = NULL, usageType = "hours", from = NULL, until = NULL,
                                interval = NULL)
  
  
  
  appInfo$time <- as.POSIXct(appInfo$timestamp, origin="1970-01-01")
  
#  sort(appInfo$time) #146 readings 73x2
  
  ## allow for zero 
  
  
  appInfo <-appInfo %>% 
    arrange(time) %>% 
    filter(hours!=0.00)
  
 
  
  appInfo <- appInfo %>% 
    mutate(date=as.Date(str_sub(time,1,10)),hr=as.integer(str_sub(time,12,13)))
  
  
  
  
  
})

output$appChart <- renderPlotly({
  
  start <- min(appData()$date) 
  
  end <- max(appData()$date)
  allDates <- data.frame(date=seq(start, end, by = "days"))
  
  print(glimpse(appData()))
  
  appData() %>% 
    group_by(date) %>% 
    summarize(totTime=sum(hours)) %>% 
    right_join(allDates) %>% 
    mutate(hrs=ifelse(is.na(totTime),0,totTime)) %>% 
    plot_ly(x=date,y=hrs,markers="lines")%>% 
      layout(hovermode = "closest",
             xaxis=list(title=" "),
             yaxis=list(title="Hours per Day")
      )
  
})