## app.R ##
if (!require("pacman")) install.packages("pacman")

p_load(shiny)
p_load(shinydashboard)
p_load(tidyverse)
p_load(stringr)
p_load(dygraphs)
p_load(readxl)
p_load(tidyverse)
p_load(plotly)
p_load(xts)
p_load(networkD3)
p_load(tibble)
p_load(forcats)
p_load(googlesheets)
p_load(timevis)
p_load(lubridate)
p_load(visNetwork)

source("1_load_data.R", local = T)
source("2_plot.R", local = T)


# Shiny session -----------------------------------------------------------


header <- dashboardHeader(title = "Fantasy Basketball Dashboard",
                          disable = F)

sidebar <- dashboardSidebar(disable = T)

body <- dashboardBody(
  
  fluidPage(
    tabsetPanel(
      tabPanel(
        title = "Performance", 
        
        fluidRow(
          
          selectInput("homeTeam", 
                      label = h3("Choose the team:"),
                      choices = list("House of Guards" = "House of Guards",
                                     "Flash" = "Flash",
                                     "WizKids" = "WizKids",
                                     "Shaqtin' A Fool" = "Shaqtin' A Fool",
                                     "The Butler Did It" = "The Butler Did It",
                                     "Heinsohn and Cooz" = "Heinsohn and Cooz",
                                     "Lauren's Legit Team" = "Lauren's Legit Team",
                                     "TRUST THE PROCESS" = "TRUST THE PROCESS",
                                     "YouCantStopThaD" = "YouCantStopThaD",
                                     "Damion's Team" = "Damion's Team",
                                     "Derrick's Team" = "Derrick's Team",
                                     "Pippen Ain't Easy" = "Pippen Ain't Easy"),
                      selected = "Heinsohn and Cooz",
                      multiple = F)
          
        ), # end fluidRow
        
        fluidRow(
          valueBoxOutput("vbox_std_rank", width = 3),
          
          valueBoxOutput("vbox_wk_rank", width = 3), #,
          
          valueBoxOutput("vbox_win_matchup", width = 3)

        ), # end fluidRow
        
        fluidPage(
          tabsetPanel(
            tabPanel(
              title = "Overall",
              
              fluidRow(
                box(
                  title = "Weekly Standing",
                  status = "primary",
                  plotlyOutput("plot_standing"),
                  solidHeader = T,
                  width = 8, height = 700
                ),
                
                box(
                  title = "Performance by winning categories",
                  status = "primary",
                  plotOutput("plot_win_loss"),
                  solidHeader = T,
                  width = 4, height = 700
                )
              )
            ), # end of tabPanel
            
            tabPanel(
              title = "By Category",
              
              fluidRow(
                box(
                  title = "Peer Comparison",
                  status = "primary",
                  solidHeader = T,
                  width = 12,
                  column(6,
                         selectInput("comprTeam", 
                                     label = h3("Step 1: Select the teams to compare:"),
                                     choices = list("House of Guards" = "House of Guards",
                                                    "Flash" = "Flash",
                                                    "WizKids" = "WizKids",
                                                    "Shaqtin' A Fool" = "Shaqtin' A Fool",
                                                    "The Butler Did It" = "The Butler Did It",
                                                    "Heinsohn and Cooz" = "Heinsohn and Cooz",
                                                    "Lauren's Legit Team" = "Lauren's Legit Team",
                                                    "TRUST THE PROCESS" = "TRUST THE PROCESS",
                                                    "YouCantStopThaD" = "YouCantStopThaD",
                                                    "Damion's Team" = "Damion's Team",
                                                    "Derrick's Team" = "Derrick's Team",
                                                    "Pippen Ain't Easy" = "Pippen Ain't Easy"),
                                     selected = "WizKids",
                                     multiple = T)),
                  
                  column(6,
                         h3("Step 2: Click Go to Show Charts:"), 
                         actionButton("go", h4("Go"), width = 120, height = 55)) 
                  #                             class = "btn btn-primary", style = style))
                )
                
              ), # end of fluidRow
              
              fluidRow(                
                box(
                  title = "Field Goal Percent",
                  status = "primary",
                  plotlyOutput("plot_fg"),
                  solidHeader = T,
                  width = 6),
                
                box(
                  title = "Free Throw Percent",
                  plotlyOutput("plot_ft"),
                  status = "primary",
                  solidHeader = T,
                  width = 6)
                ),
              
              fluidRow(
                box(
                  title = "Three Points Made",
                  status = "primary",
                  plotlyOutput("plot_3pt"),
                  solidHeader = T,
                  width = 6),
                
                box(
                  title = "Points",
                  status = "primary",
                  plotlyOutput("plot_pts"),
                  solidHeader = T,
                  width = 6)
              ),
              
              fluidRow(                
                box(
                  title = "Rebounds",
                  status = "primary",
                  plotlyOutput("plot_reb"),
                  solidHeader = T,
                  width = 6),
                
                box(
                  title = "Assists",
                  status = "primary",
                  plotlyOutput("plot_ast"),
                  solidHeader = T,
                  width = 6)
              ),
              
              fluidRow(                
                box(
                  title = "Steals",
                  status = "primary",
                  plotlyOutput("plot_stl"),
                  solidHeader = T,
                  width = 6),
                
                box(
                  title = "Blocks",
                  status = "primary",
                  plotlyOutput("plot_blk"),
                  solidHeader = T,
                  width = 6)
              ),
              
              fluidPage(
                box(
                  title = "Turnovers",
                  status = "primary",
                  plotlyOutput("plot_to"),
                  solidHeader = T,
                  width = 6)
              )
              
            ) # end of tabPanel
          ) # end of tabsetPanel
        ) # end of fluidPage
        
        ), # end of tabPanel
      
      # tabPanel(
      #   title = "Weekly Report", 
      #   box(
      #     includeMarkdown("3_wk13_report.Rmd"),
      #     width = 12
      #   )
      # ), # end of tabPanel
      
      tabPanel(
        title = "Trade Network",

        box(
          title = "Who trade with whom?",
          status = "primary",
          solidHeader = T,
          visNetworkOutput("tradenetwork", height = "1200px", width = "1200px"),
          width = 12,
          height = 1300)
      ) # end of tabPanel
    )
    
    ) # end of fludPage


) # end of dashboardBody


# build everything into the dashboard page --------------------------------

ui <- dashboardPage(skin = "black", 
                    header, 
                    sidebar, 
                    body)


server <- function(input, output) {
  output$plot_standing <- renderPlotly(
    ggplotly(rank_plot, tooltip = c("group", "text"), height = 600)
  )

  output$plot_win_loss <- renderPlot(
    viz_result, height = 600
  )
  
  standing_rank <- reactive({
    dat_rank %>%
      filter(team == input$homeTeam) %>%
      tail(n=1) %>% .[["rank"]]
  })
  
  output$vbox_std_rank <- renderValueBox(
    valueBox(
      value = standing_rank(),
      subtitle = "Rank in standings",
      color = case_when(
        standing_rank() %in% 1:4 ~ "green",
        standing_rank() %in% 5:6 ~ "blue",
        standing_rank() %in% 7:10 ~ "yellow",
        standing_rank() %in% 11:14 ~ "red"
      ),
      width = 3
    )
  )
  
  wk_rank <- reactive({
    rank_this_week %>% 
      filter(team == input$homeTeam) %>%
      .[["wk_rank"]]
  })
  
  output$vbox_wk_rank <- renderValueBox(
    valueBox(
      value = wk_rank(),
      subtitle = "Rank in this week",
      color = case_when(
        wk_rank() >= 1 & wk_rank() <= 4 ~ "green",
        wk_rank() > 4 & wk_rank() <= 6 ~ "blue",
        wk_rank() > 6 & wk_rank() <= 10 ~ "yellow",
        wk_rank() > 10 & wk_rank() <= 14 ~ "red"
      ),
      width = 3
    )
  )
  
  output$vbox_win_matchup <- renderValueBox(
    valueBox(
      value = scoreCompr(input$homeTeam),
      subtitle = "Matchups won over other teams",
      color = case_when(
        scoreCompr(input$homeTeam) > 10 & scoreCompr(input$homeTeam) <= 13 ~ "green",
        scoreCompr(input$homeTeam) > 6 & scoreCompr(input$homeTeam) <= 10 ~ "blue",
        scoreCompr(input$homeTeam) > 3 & scoreCompr(input$homeTeam) <= 6 ~ "yellow",
        scoreCompr(input$homeTeam) >= 0 & scoreCompr(input$homeTeam) <= 3 ~ "red"
      ),
      width = 3
    )
  )
  
  team_list <- eventReactive(input$go, {
    input$comprTeam
  })
  
  # FG pct plot
  output$plot_fg <- renderPlotly(
    ggplotly(cat_compr_plot(var = "fg_pct", team_list = team_list(), var_name = "FG%"), 
             tooltip = c("group", "text"))
  )
  
  # FT pct plot
  output$plot_ft <- renderPlotly(
    ggplotly(cat_compr_plot(var = "ft_pct", team_list = team_list(), var_name = "FT%"), 
             tooltip = c("group", "text"))
  )
  
  # 3ptm plot
  output$plot_3pt <- renderPlotly(
    ggplotly(cat_compr_plot(var = "`3ptm`", team_list = team_list(), var_name = "3PTS"), 
             tooltip = c("group", "text"))
  )
  
  # points plot
  output$plot_pts <- renderPlotly(
    ggplotly(cat_compr_plot(var = "pts", team_list = team_list(), var_name = "PTS"), 
             tooltip = c("group", "text"))
  )
  
  # reb plot
  output$plot_reb <- renderPlotly(
    ggplotly(cat_compr_plot(var = "reb", team_list = team_list(), var_name = "Rebounds"), 
             tooltip = c("group", "text"))
  )
  
  # ast plot
  output$plot_ast <- renderPlotly(
    ggplotly(cat_compr_plot(var = "ast", team_list = team_list(), var_name = "Assist"), 
             tooltip = c("group", "text"))
  )
  
  # stl plot
  output$plot_stl <- renderPlotly(
    ggplotly(cat_compr_plot(var = "stl", team_list = team_list(), var_name = "Steal"), 
             tooltip = c("group", "text"))
  )
  
  # blk plot
  output$plot_blk <- renderPlotly(
    ggplotly(cat_compr_plot(var = "blk", team_list = team_list(), var_name = "Block"), 
             tooltip = c("group", "text"))
  )
  
  # to plot
  output$plot_to <- renderPlotly(
    ggplotly(cat_compr_plot(var = "to", team_list = team_list(), var_name = "Turnover"), 
             tooltip = c("group", "text"))
  )
  
  # trade network
  
  output$tradenetwork <- renderVisNetwork(
    visNetwork(nodes, edges, 
               width = "1300px", height = "100%") %>% # to change in canvas size, do it in the visNetworkOutput session
      visNodes(shapeProperties = list(useBorderWithImage = TRUE),
               borderWidth = 3, borderWidthSelected = 5) %>%
      visOptions(highlightNearest = TRUE)
  )

}  # end of server



shinyApp(ui, server)