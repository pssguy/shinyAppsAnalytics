


data <- eventReactive (input$go,{
  
  req(input$account)
  
  apps <- applications(input$account) %>% 
      mutate(created=str_sub(created_time,1,10),updated=str_sub(updated_time,1,10))
  
  apps$name <- as.character(apps$name)
  apps$status <- as.character(apps$status)
  
  print(glimpse(apps))
  
  info=list(apps=apps)
  return(info)
  
})

# output$appSelect <- uiOutput({
#   
# })

output$appsTable <- DT::renderDataTable({
 
  
data()$apps %>%
    select(name,status,created,updated) %>% 
     arrange(status) %>% 
  DT::datatable(class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = TRUE, searching = TRUE,info=FALSE))
})