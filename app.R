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
#                            sidebar
# ==============================================================================

sidebar <- dbcCol(
    list(
        htmlBr(),
        htmlH4("Video Game Sales Analytics App", className = "Heading-3", style=list("text-align"='center')),
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
                        list(label='xbox', value='xbox'),
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
        htmlBr(),
        htmlHr(),
        htmlH6("Top Genres Year", className = "Heading-5", style=list("text-align"='center')),
        dbcCol(
            list(
                dccDropdown(
                    id = "year_of_genre",
                    options = c(2000:2020) %>%
                        purrr::map(function(col) list(label = col, value = col)),
                    value = 2010
                )
            )
        ),
        htmlBr(),
        htmlHr(),
        htmlH6("Top Publisher Year", className = "Heading-5", style=list("text-align"='center')),
        htmlBr(),
        htmlHr(),
        htmlH6("Critic Score Year", className = "Heading-5", style=list("text-align"='center')),
        htmlBr(),
        htmlHr(),
        htmlP("
        This dashboard was made by Amelia Tang, 
        Alex Yinan Guo, Yike Shi, and Mahmoodur Rahman.  
        Last updated 2022-03-14")
    ),
    style = list("background-color" = "#ADD8E6"),
    md=2)

# ==============================================================================
#                            content
# ==============================================================================

content <- dbcCol(list(
    dccTabs(id="tabs", children=list(
        dccTab(label='North America', children=list(dbcRow(list(dbcCol(
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
                htmlH4("Alex NA Market Share Placeholder"))))),
            dbcRow(list(
                dbcCol(list(
                    dccGraph(id='genre_plot_na'),
                    dccDropdown(
                        id='num_of_bar_genre_na',
                        options = c(3, 4, 5, 6, 7, 8) %>%
                        purrr::map(function(col) list(label = col, value = col)),
                        value = 3)
                    ),
                dbcCol(list(
                    htmlH4("Maeve NA Top Publisher Placeholder"))),
                dbcCol(list(
                    htmlH4("Alex NA Critic Score Placeholder"))))))),
        dccTab(label='Global', children=list(dbcRow(list(dbcCol(
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
                htmlH4("Alex GB Market Share Placeholder"))))),
            dbcRow(list(
                dbcCol(list(
                    htmlH4("Maeve GB Top Grene Placeholder"))),
                dbcCol(list(
                    htmlH4("Maeve GB Top Publisher Placeholder"))),
                dbcCol(list(
                    htmlH4("Alex GB Critic Score Placeholder"))))))))))))

# ==============================================================================
#                            layout
# ==============================================================================

   
app$layout(
    dbcContainer(
        list(dbcRow(
            list(sidebar, content),
            className = "h-100"
        )),
        fluid = TRUE,
        style = list("height" = "100vh", "width"="80%")
    )
)
            
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
         layout(title =('Sales Trend'))
      } else if (company == 'xbox'){
        xbox_NA_yr <- xbox_NA  %>%
          filter(Year <= year)  %>%
            rename(Sales = NA_sales)
        p <- plot_ly(xbox_NA_yr, x = ~Year, y = ~Sales, type = 'scatter', mode = 'lines')  %>%
            layout(title =('Sales Trend'))
      } else {
         NA_sales_plot <- merge(ps4_NA, xbox_NA, by = 'Year') %>%
             rename(Sales = NA_sales.x)  %>%
             filter(Year <= year)
         p <- plot_ly(NA_sales_plot, x = ~Year, y = ~Sales, name = "PlayStation4", type = 'scatter', mode = 'lines')
         p <- p  %>% add_trace(y = ~NA_sales.y, name = 'XBox', mode = 'lines')  %>%
             layout(title =('Sales Trend'))
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
                layout(title =('Sales Trend'))
        } else if (company == 'xbox'){
            xbox_global_yr <- xbox_global  %>%
                filter(Year <= year)  %>%
                rename(Sales = global_sales)
            p <- plot_ly(xbox_global_yr, x = ~Year, y = ~Sales, type = 'scatter', mode = 'lines')  %>%
                layout(title =('Sales Trend'))
        } else {
            global_sales_plot <- merge(ps4_global, xbox_global, by = 'Year') %>%
                rename(Sales = global_sales.x)  %>%
                filter(Year <= year)
            p <- plot_ly(global_sales_plot, x = ~Year, y = ~Sales, name = "PlayStation4", type = 'scatter', mode = 'lines')
            p <- p  %>% add_trace(y = ~global_sales.y, name = 'XBox', mode = 'lines')  %>%
                layout(title =('Sales Trend'))
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
                geom_bar(stat = "identity") +
                labs(x = 'Counts', y = 'Genre') +
                ggtitle('Top North America Genre') 
        ggplotly(p)
    }
)



app$run_server(host = '0.0.0.0')
