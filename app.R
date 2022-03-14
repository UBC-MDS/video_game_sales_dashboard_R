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
        htmlP("Select Companies", className = "Heading-5", style=list("text-align"='center')),
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
            width = 10,
            style = list("padding" = "10px 10px 10px 0px"),
        )
    ),
    style = list("background-color" = "#ADD8E6"),
    md=2
)

# ==============================================================================
#                            content
# ==============================================================================

content <- dbcCol(
    list(
        htmlBr(),
        dccGraph(id='sales_plot'),
        dccSlider(
            id="slider-year",
            min=min(xbox_NA$Year),
            max=max(xbox_NA$Year),
            step=1,
            value=max(xbox_NA$Year),
            marks=year_range_label,
            tooltip=list(
                always_visible=TRUE,
                placement="top"
            ))))


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



app$run_server(host = '0.0.0.0')
