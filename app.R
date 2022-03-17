# ==============================================================================
#                           Import Packages
# ==============================================================================

library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(plotly)
library(dplyr)
library(ggplot2)
library(tidyr)

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)


# ==============================================================================
#                            Load the data and define constants
# ==============================================================================

xbox <- read.csv("data/processed/xbox.csv") %>% drop_na()
ps4 <- read.csv("data/processed/ps4.csv") %>% drop_na()
summary <- read.csv("data/processed/summary.csv")
videoGame <- read.csv("data/processed/videoGame.csv")
xbox_NA <- xbox  %>%
  select(Year, North.America)  %>%
  group_by(Year)  %>%
  summarise(NA_sales = sum(North.America))  %>%
  slice(1:6)

ps4_NA <- ps4  %>%
  select(Year, North.America)  %>%
  group_by(Year)  %>%
  summarise(NA_sales = sum(North.America))  %>%
  slice(1:6)

xbox_global <- xbox  %>%
  select(Year, Global)  %>%
  group_by(Year)  %>%
  summarise(global_sales = sum(Global))  %>%
  slice(1:6)

ps4_global <- ps4  %>%
  select(Year, Global)  %>%
  group_by(Year)  %>%
  summarise(global_sales= sum(Global))  %>%
  slice(1:6)

year_range <- seq(min(ps4_NA$Year), max(ps4_NA$Year))
year_range_label <- setNames(as.list(as.character(year_range)), as.integer(year_range))


# ==============================================================================
#                           Styles
# ==============================================================================
# the style arguments for the sidebar. 
SIDEBAR_STYLE = list(
    "position"="fixed",
    "top"=0,
    "left"=0,
    "bottom"=0,
    "width"="14rem",
    "padding"="2rem 1rem",
    "background-color"="#ADD8E6",
    "overflow"="scroll"
)

# the styles for the main content position it to the right of the sidebar and
# add some padding.
CONTENT_STYLE = list(
    "margin-left"="2rem",
    "margin-right"="2rem",
    "padding"="2rem 1rem",
    "width"="1500px",
    "height"="1500px"
)

# ==============================================================================
#                            sidebar
# ==============================================================================

sidebar <- dbcCol(
  list(
    htmlBr(),
    htmlH4("Video Game Sales Analytics", className = "Heading-3", style=list("text-align"='center')),
    htmlHr(),
    htmlP("A dashboard to analyze sales of major players in the video game industry", style=list("text-align"='center')),
    htmlHr(),
    htmlH6("Sales Trend Companies", className = "Heading-5", style=list("text-align"='center')),
    dbcCol(
      list(
        dccDropdown(
          id = "company-select",
          options = list(
            list(label='PlayStation4', value='ps4'),
            list(label='Xbox', value='xbox'),
            list(label="Both", value='xbox+ps4')
          ),
          value = c("ps4"),
        )
      ),
      width = 15,
      style = list("padding" = "10px 10px 10px 0px"),
    ),
    htmlHr(),
    htmlH6("Market Share Year", className = "Heading-5", style=list("text-align"='center')),
    dbcCol(
      list(
        dccDropdown(
          id = "year_of_market_share",
          options = c(2013:2018) %>%
            purrr::map(function(col) list(label = col, value = col)),
          value = 2016
        )
      )
    ),
    htmlBr(),
    htmlHr(),
    htmlH6("Top Genres Year", className = "Heading-5", style=list("text-align"='center')),
    dbcCol(
      list(
        dccDropdown(
          id = "year_of_genre",
          options = c(2013:2018) %>%
            purrr::map(function(col) list(label = col, value = col)),
          value = 2016
        )
      )
    ),
    htmlBr(),
    htmlHr(),
    htmlH6("Top Publisher Year", className = "Heading-5", style=list("text-align"='center')),
    dbcCol(
      list(
        dccDropdown(
          id = "year_of_publisher",
          options = c(2013:2018) %>%
            purrr::map(function(col) list(label = col, value = col)),
          value = 2016
        )
      )
    ),
    htmlBr(),
    htmlHr(),
    htmlH6("Critic Score Year", className = "Heading-5", style=list("text-align"='center')),
    dbcCol(
      list(
        dccDropdown(
          id = "year_of_critic_score",
          options = c(2013:2018) %>%
            purrr::map(function(col) list(label = col, value = col)),
          value = 2016
        )
      )
    ),        
    htmlBr(),
    htmlHr(),
    htmlH6("User Score Year", className = "Heading-5", style=list("text-align"='center')),
    dbcCol(
      list(
        dccDropdown(
          id = "year_of_user_score",
          options = c(2013:2018) %>%
            purrr::map(function(col) list(label = col, value = col)),
          value = 2016
        )
      )
    ), 
    htmlBr(),
    htmlHr(),
    htmlP("
        This dashboard was made by Amelia Tang, 
        Alex Yinan Guo, Yike Shi, and Mahmoodur Rahman.  
        Last updated 2022-03-18")
  ),
  style = SIDEBAR_STYLE,
  md=2)

# ==============================================================================
#                            content
# ==============================================================================

content <- dbcCol(list(
    dccTabs(id="tabs", children=list(
        dccTab(label='North America', children=list(
            dbcRow(list(
                dbcCol(
                    list(
                        htmlBr(),
                        dccGraph(id='sales_plot'),
                        dccSlider(
                            id="slider-year",
                            min=min(xbox_NA$Year),
                            max=max(xbox_NA$Year),
                            step=1,
                            value=2017,
                            marks=year_range_label,
                            tooltip=list(
                                always_visible=TRUE,
                                placement="top")))),
                dbcCol(list(
                    htmlBr(),
                    dccGraph(id='na_market_share_plot'))))),
            htmlBr(),
            htmlHr(),
            htmlBr(),
            dbcRow(list(
                dbcCol(list(dbcRow(list(
                    dccGraph(id='genre_plot_na'),
                    htmlH6("Number of Top Genres to Display"),
                    dccDropdown(
                        id='num_of_bar_genre_na',
                        options = c(3, 4, 5, 6, 7, 8) %>%
                            purrr::map(function(col) list(label = col, value = col)),
                        value = 5)
                )),
                htmlBr(),
                htmlBr(),
                htmlBr(),
                dbcRow(list(
                    dccGraph(id='publisher_plot_na'),
                    htmlH6("Number of Top Publishers to Display"),
                    dccDropdown(
                        id='num_of_bar_publisher_na',
                        options = c(3, 4, 5, 6, 7, 8) %>%
                            purrr::map(function(col) list(label = col, value = col)),
                        value = 5))))),
                dbcCol(list(dbcRow(list(
                    dccGraph(id='na_critic_score_plot'))),
                    htmlBr(),
                    htmlBr(),
                    htmlBr(),
                    htmlBr(),
                    htmlBr(),
                    dbcRow(list(
                        dccGraph(id='na_user_score_plot'))))))))),
        
        dccTab(label='Global', children=list(
            dbcRow(list(
                dbcCol(
                    list(
                        htmlBr(),
                        dccGraph(id='sales_plot_global'),
                        dccSlider(
                            id="slider-year2",
                            min=min(xbox_NA$Year),
                            max=max(xbox_NA$Year),
                            step=1,
                            value=2017,
                            marks=year_range_label,
                            tooltip=list(
                                always_visible=TRUE,
                                placement="top")))),
                dbcCol(list(
                    htmlBr(),
                    dccGraph(id='global_market_share_plot'))))),
            htmlBr(),
            htmlHr(),
            htmlBr(),
            dbcRow(list(
                dbcCol(list(dbcRow(list(
                    dccGraph(id='genre_plot_global'),
                    htmlH6("Number of Top Genres to Display"),
                    dccDropdown(
                        id='num_of_bar_genre_global',
                        options = c(3, 4, 5, 6, 7, 8) %>%
                            purrr::map(function(col) list(label = col, value = col)),
                        value = 5)
                )),
                htmlBr(),
                htmlBr(),
                htmlBr(),
                dbcRow(list(
                    dccGraph(id='publisher_plot_global'),
                    htmlH6("Number of Top Publishers to Display"),
                    dccDropdown(
                        id='num_of_bar_publisher_global',
                        options = c(3, 4, 5, 6, 7, 8) %>%
                            purrr::map(function(col) list(label = col, value = col)),
                        value = 5))))),
                dbcCol(list(dbcRow(list(
                    dccGraph(id='global_critic_score_plot'))),
                    htmlBr(),
                    htmlBr(),
                    htmlBr(),
                    htmlBr(),
                    htmlBr(),
                    dbcRow(list(
                        dccGraph(id='global_user_score_plot')))))))))))), 
    style=CONTENT_STYLE)

# ==============================================================================
#                            layout
# ==============================================================================


app$layout(
  dbcContainer(
    list(dbcRow(
      list(sidebar, content)
     # className = "h-100"
    ))))
# ==============================================================================
#                            Callbacks and backend
# ==============================================================================
app$callback(
  output('sales_plot', 'figure'),
  list(input('company-select', 'value'),
       input("slider-year", "value")),
  function(company, year) {
    if (company == 'ps4') {
      ps4_NA_yr <- ps4_NA  %>%
        filter(Year <= year)  %>%
        rename(Sales = NA_sales)
      p <- plot_ly(ps4_NA_yr, x = ~Year, y = ~Sales, type = 'scatter', mode = 'lines')  %>%
        layout(title =('North America Sales Trend'))
    } else if (company == 'xbox'){
      xbox_NA_yr <- xbox_NA  %>%
        filter(Year <= year)  %>%
        rename(Sales = NA_sales)
      p <- plot_ly(xbox_NA_yr, x = ~Year, y = ~Sales, type = 'scatter', mode = 'lines')  %>%
        layout(title =('North America Sales Trend'))
    } else {
      NA_sales_plot <- merge(ps4_NA, xbox_NA, by = 'Year') %>%
        rename(Sales = NA_sales.x)  %>%
        filter(Year <= year)
      p <- plot_ly(NA_sales_plot, x = ~Year, y = ~Sales, name = "PlayStation4", type = 'scatter', mode = 'lines')
      p <- p  %>% add_trace(y = ~NA_sales.y, name = 'XBox', mode = 'lines')  %>%
        layout(title =('North America Sales Trend'))
    }
    ggplotly(p)
  }
)

app$callback(
  output('sales_plot_global', 'figure'),
  list(input('company-select', 'value'),
       input("slider-year2", "value")),
  function(company, year) {
    if (company == 'ps4') {
      ps4_global_yr <- ps4_global  %>%
        filter(Year <= year)  %>%
        rename(Sales = global_sales)
      p <- plot_ly(ps4_global_yr, x = ~Year, y = ~Sales, type = 'scatter', mode = 'lines')  %>%
        layout(title =('Global Sales Trend'))
    } else if (company == 'xbox'){
      xbox_global_yr <- xbox_global  %>%
        filter(Year <= year)  %>%
        rename(Sales = global_sales)
      p <- plot_ly(xbox_global_yr, x = ~Year, y = ~Sales, type = 'scatter', mode = 'lines')  %>%
        layout(title =('Global Sales Trend'))
    } else {
      global_sales_plot <- merge(ps4_global, xbox_global, by = 'Year') %>%
        rename(Sales = global_sales.x)  %>%
        filter(Year <= year)
      p <- plot_ly(global_sales_plot, x = ~Year, y = ~Sales, name = "PlayStation4", type = 'scatter', mode = 'lines')
      p <- p  %>% add_trace(y = ~global_sales.y, name = 'XBox', mode = 'lines')  %>%
        layout(title =('Global Sales Trend'))
    }
    ggplotly(p)
  }
)


app$callback(
  output('genre_plot_na', 'figure'),
  list(input('year_of_genre', 'value'),
       input('num_of_bar_genre_na', 'value')),
  function(year, num) {
    summary_na <- summary %>%
      filter(North.America !=0) %>%
      filter(Year == year) %>%
      add_count(Genre)  %>%
      distinct(Genre, n) %>%
      arrange(desc(n)) %>%
      slice(1:num)
    p <- ggplot(data = summary_na, aes(y = reorder(Genre, n), x = n)) +
      geom_bar(stat = "identity", fill = "#4E78A5") +
      labs(x = 'Counts', y = 'Genre')  +
      ggtitle('North America Top Genres')+
      theme(plot.title = element_text(hjust = 0.5))
    ggplotly(p)
  }
)

app$callback(
  output('genre_plot_global', 'figure'),
  list(input('year_of_genre', 'value'),
       input('num_of_bar_genre_global', 'value')        
  ),
  function(year, num) {
    summary_global <- summary %>%
      filter(Year == year) %>%
      add_count(Genre)  %>%
      distinct(Genre, n) %>%
      arrange(desc(n)) %>%
      slice(1:num)
    p <- ggplot(data = summary_global, aes(y = reorder(Genre, n), x = n)) +
      geom_bar(stat = "identity", fill = "#4E78A5") +
      labs(x = 'Counts', y = 'Genre')  +
      ggtitle('Global Top Genres')+
      theme(plot.title = element_text(hjust = 0.5))
    ggplotly(p)
  }
)

app$callback(
  output('publisher_plot_na', 'figure'),
  list(input('year_of_publisher', 'value'),
       input('num_of_bar_publisher_na', 'value')        
  ),
  function(year, num) {
    summary_na <- summary %>%
      filter(North.America !=0) %>%
      filter(Year == year) %>%
      add_count(Publisher)  %>%
      distinct(Publisher, n) %>%
      arrange(desc(n)) %>%
      slice(1:num)
    p <- ggplot(data = summary_na, aes(y = reorder(Publisher, n), x = n)) +
      geom_bar(stat = "identity", fill = "#4E78A5") +
      labs(x = 'Counts', y = 'Publisher')  +
      ggtitle('North America Top Publishers')+
      theme(plot.title = element_text(hjust = 0.5))
    ggplotly(p)
  }
)

app$callback(
  output('publisher_plot_global', 'figure'),
  list(input('year_of_publisher', 'value'),
       input('num_of_bar_publisher_global', 'value')        
  ),
  function(year, num) {
    summary_na <- summary %>%
      filter(Year == year) %>%
      add_count(Publisher)  %>%
      distinct(Publisher, n) %>%
      arrange(desc(n)) %>%
      slice(1:num)
    p <- ggplot(data = summary_na, aes(y = reorder(Publisher, n), x = n)) +
      geom_bar(stat = "identity", fill = "#4E78A5") +
      labs(x = 'Counts', y = 'Publisher') +
      ggtitle('Global Top Publishers')+
      theme(plot.title = element_text(hjust = 0.5))
    ggplotly(p)
  }
)


app$callback(
  output('na_market_share_plot', 'figure'),
  list(input('year_of_market_share', 'value')),
  function(year, num) {
    p <- summary %>%
      group_by(Year, tag) %>%
      summarize(North.America = sum(North.America)) %>%
      filter(Year == 2016) %>%
      plot_ly(labels=~tag, values=~North.America, type='pie') %>% 
      layout(title = 'North America Market Share',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) 
    ggplotly(p)
  }
)


app$callback(
  output('global_market_share_plot', 'figure'),
  list(input('year_of_market_share', 'value')),
  function(year, num) {
    p <- summary %>%
      group_by(Year, tag) %>%
      summarize(Global = sum(Global)) %>%
      filter(Year == 2016) %>%
      plot_ly(labels=~tag, values=~Global, type='pie') %>% 
      layout(title = 'Global Market Share',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) 
    ggplotly(p)
  }
)

app$callback(
  output('na_critic_score_plot', 'figure'),
  list(input('year_of_critic_score', 'value')),
  function(year) {
    p <- videoGame %>%
      filter(Year == year) %>%
      group_by(Platform) %>%
      summarize(Critic_Score = mean(Critic_Score, na.rm=TRUE)) %>%
      drop_na() %>%
      ggplot(aes(x=reorder(Platform, -Critic_Score), y=Critic_Score)) +
      geom_bar(stat="identity", fill = "#4E78A5") +
      labs(x = 'Platform', y = 'Critic Score') +
      ggtitle('North America Critic Scores') +
      theme(plot.title = element_text(hjust = 0.5))
    
    ggplotly(p)
  }
)

app$callback(
  output('global_critic_score_plot', 'figure'),
  list(input('year_of_critic_score', 'value')),
  function(year) {
    p <- videoGame %>%
      filter(Year == year) %>%
      group_by(Platform) %>%
      summarize(Critic_Score = mean(Critic_Score, na.rm=TRUE)) %>%
      drop_na() %>%
      ggplot(aes(x=reorder(Platform, -Critic_Score), y=Critic_Score)) +
      geom_bar(stat="identity", fill = "#4E78A5") +
      labs(x = 'Platform', y = 'Critic Score') +
      ggtitle('Global Critic Scores') +
      theme(plot.title = element_text(hjust = 0.5))
    
    ggplotly(p)
  }
)

app$callback(
  output('na_user_score_plot', 'figure'),
  list(input('year_of_user_score', 'value')),
  function(year) {
    p <- videoGame %>%
      filter(Year == year) %>%
      group_by(Platform) %>%
      summarize(User_Score = mean(User_Score, na.rm=TRUE)) %>%
      drop_na() %>%
      ggplot(aes(x=reorder(Platform, -User_Score), y=User_Score)) +
      geom_bar(stat="identity", fill = "#4E78A5") +
      labs(x = 'Platform', y = 'User score') +
      ggtitle('North America User Scores') +
      theme(plot.title = element_text(hjust = 0.5))
    
    ggplotly(p)
  }
)

app$callback(
  output('global_user_score_plot', 'figure'),
  list(input('year_of_user_score', 'value')),
  function(year) {
    p <- videoGame %>%
      filter(Year == year) %>%
      group_by(Platform) %>%
      summarize(User_Score = mean(User_Score, na.rm=TRUE)) %>%
      drop_na() %>%
      ggplot(aes(x=reorder(Platform, -User_Score), y=User_Score)) +
      geom_bar(stat="identity", fill = "#4E78A5") +
      labs(x = 'Platform', y = 'User score') +
      ggtitle('Global User Scores') +
      theme(plot.title = element_text(hjust = 0.5))
    
    ggplotly(p)
  }
)

app$run_server(host = '0.0.0.0')

