
dashboardPage(
  title = "shinyApps Analysis",
  skin = "yellow",
  dashboardHeader(title = "shinyApps Analysis"),
  
  dashboardSidebar(
   includeCSS("custom.css"),
   includeMarkdown("about.md"),
inputPanel(
    textInput("account", "Enter Account Name"),
    actionButton("go","Get Data")
    
),
    uiOutput("appSelect"),
    
    
    
    
    sidebarMenu(
      id = "sbMenu",
      
            menuItem(
        "Analysis", tabName = "analysis"
        ),
        
        tags$hr(),
        menuItem(
          text = "",href = "https://mytinyshinys.shinyapps.io/dashboard",badgeLabel = "All Dashboards and Trelliscopes (14)"
        ),
        tags$hr(),
        
        tags$body(
          a(
            class = "addpad",href = "https://twitter.com/pssGuy", target = "_blank",img(src =
                                                                                          "images/twitterImage25pc.jpg")
          ),
          a(
            class = "addpad2",href = "mailto:agcur@rogers.com", img(src = "images/email25pc.jpg")
          ),
          a(
            class = "addpad2",href = "https://github.com/pssguy",target = "_blank",img(src =
                                                                                         "images/GitHub-Mark30px.png")
          ),
          a(
            href = "https://rpubs.com/pssguy",target = "_blank",img(src = "images/RPubs25px.png")
          )
        )
      )
    ),
        
      
  
    
    dashboardBody(tabItems(
      
      

            
tabItem("analysis",
        DT::dataTableOutput("appsTable"),
        plotlyOutput("appChart"))
        
#tabItem("info",includeMarkdown("about.md"))     
            
           
            
            
            
            
            
            
            
    ) # tabItems
    ) # body
  ) # page
  