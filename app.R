library(shiny)
library(shinythemes)
library(tidyverse)
library(fec16)
library(tidyverse)
library(kableExtra)
library(patchwork)
library(dotwhisker)
library(dplyr)
library(gganimate)
library(gifski)
library(av)

# Load & clean data
# Read csv files
tech_pulse <- read_csv("data/sf_tech_pulse.csv")
real_gdp <- read_csv("data/real_gdp.csv")
cpi <- read_csv("data/cpi.csv")
interest_rates <- read_csv("data/interest_rates.csv")
wage_rates <- read_csv("data/wage_rates.csv")
outcome_choices <- c("Real GDP", "CPI", "Interest Rate", "Wage Rate")
all_choices <- c("SF Tech Pulse", "Real GDP", "CPI", "Interest Rate", "Wage Rate")

# Merge into data object
data <- tech_pulse %>%
  merge(real_gdp) %>%
  merge(cpi) %>%
  merge(interest_rates) %>%
  merge(wage_rates) %>%
  rename(date = DATE,
         sftp = SFTPAGRM158SFRBSF,
         gdp = GDPC1,
         cpi = CPIAUCSL,
         interest = INTDSRUSM193N,
         wage = LES1252881600Q) %>%
  mutate(year = substr(date, 1, 4),
         month = substr(date, 6, 7)) %>%
  relocate(year, .after = date) %>%
  relocate(month, .after = year)

# Reformat into date and numerics
data$date <- as.Date(data$date)
data$year <- as.numeric(data$year)
data$month <- as.numeric(data$month)

# Calculate percentage change in indicators
data <- data %>% 
  mutate(gdp_change = (gdp - lag(gdp))/lag(gdp)) %>%
  mutate(cpi_change = (cpi - lag(cpi))/lag(cpi)) %>%
  mutate(interest_change = (interest - lag(interest))/lag(interest)) %>%
  mutate(wage_change = (wage - lag(wage))/lag(wage))

# Summary statistics
response <- data %>% select(sftp, gdp_change, cpi_change, interest_change, wage_change)
sum_min <- sapply(response, min, na.rm=TRUE)
sum_median <- sapply(response, median, na.rm=TRUE)
sum_max <- sapply(response, max, na.rm=TRUE)
sum_mean <- sapply(response, mean, na.rm=TRUE)
sum_sd <- sapply(response, sd, na.rm=TRUE)

# Linear regressions
gdp_lm <- lm(gdp_change ~ sftp, data = data)
gdp_coef <- coef(summary(gdp_lm))
gdp_cor <- cor(data$sftp, data$gdp_change, use = "complete.obs")
gdp_reg <- c(gdp_coef["(Intercept)",][1], 
             gdp_coef["sftp",],
             gdp_cor,
             gdp_cor^2)

cpi_lm <- lm(cpi_change ~ sftp, data = data)
cpi_coef <- coef(summary(cpi_lm))
cpi_cor <- cor(data$sftp, data$cpi_change, use = "complete.obs")
cpi_reg <- c(cpi_coef["(Intercept)",][1], 
             cpi_coef["sftp",],
             cpi_cor,
             cpi_cor^2)

interest_lm <- lm(interest_change ~ sftp, data = data)
interest_coef <- coef(summary(interest_lm))
interest_cor <- cor(data$sftp, data$interest_change, use = "complete.obs")
interest_reg <- c(interest_coef["(Intercept)",][1], 
                  interest_coef["sftp",],
                  interest_cor,
                  interest_cor^2)

wage_lm <- lm(wage_change ~ sftp, data = data)
wage_coef <- coef(summary(wage_lm))
wage_cor <- cor(data$sftp, data$wage_change, use = "complete.obs")
wage_reg <- c(wage_coef["(Intercept)",][1], 
              wage_coef["sftp",],
              wage_cor,
              wage_cor^2)

# Define UI ---

ui <- fluidPage(
  
  theme = shinytheme("sandstone"),
  
  titlePanel(
    h1("Relationship Between Tech Sector and Economic Growth", align = "center"),
    windowTitle = "Relationship Between Tech Sector and Economic Growth"),
  
  navlistPanel(
    
    tabPanel(
      "Introduction",
      h3("Rationale"),
      p("I want to study the relationship between the health of the technology 
        sector and the growth of the US economy. Technology plays a large role 
        in today's society, so I want to learn how it can impact the economic 
        wellbeing of an entire nation. I can measure associations and possibly 
        correlations between technological indicators such as the San Francisco 
        Tech Pulse and economic indicators such as real GDP growth rate, CPI, 
        interest rates, and wage rates. This can be done by creating linear 
        regressions, if appropriate, or overlaying time trends on line graphs 
        to visualize any consistent patterns. "),
      HTML('<center><img src="techphone.jpg" width="400"></center>')),
    
    tabPanel(
      "Trends Over Time",
      h3("Trends Over Time"),
      p("Select a tech or economic indicator to view that measure as time 
        progresses. Note that the economic indicators were transformed into 
        percentage change trends in order to visualize a more clear and direct 
        relationship between indexed measures. These measures were measured
        quarterly from 1979 to 2020."),
      sidebarPanel(
        selectInput(
          inputId = "select_var_time",
          label = "Choose a statistic:",
          choices = all_choices,
          selected = "SF Tech Pulse"
        )
      ),
      br(), br(), br(), br(), br(), br(), br(),
      uiOutput("var_time_text"),
      br(), br(),
      uiOutput("var_time_plot")
    ),
    
    tabPanel(
      "Data Regressions",
      h3("Data Regressions"),
      p("Choose an economic indicator outcome variable to view a plot of that
        variable plotted against SF Tech Pulse values. Each plot includes the
        regression least-squares line that best describes the linear 
        relationship between the two variables."),
      sidebarPanel(
        selectInput(
          inputId = "select_var_reg",
          label = "Choose an outcome variable:",
          choices = outcome_choices,
          selected = "SF Tech Pulse"
        )
      ),
      plotOutput("var_reg_plot"),
      br(), br(), br(), br(), br(), br(), br(), br(), br(),
      tableOutput("var_reg_table")
    ),
    
    tabPanel(
      "Summary Statistics",
      h3("Summary Statistics"),
      p("The following table shows the summary statistics of each technological 
        and economic indicator, measured quarterly from 1979 to 2020. Out of all 
        the variables we're looking at, they each seem to be centered at a 
        little above 0. The discount rate percentage change has the largest 
        range of 1.4, and the GDP percentage change has the smallest range of 
        0.045. With regard to spread, the SF Tech Pulse has the largest standard 
        deviation of 0.167, and the GDP percentage change has the smallest 
        standard deviation of 0.007."),
      tableOutput("sum_stat"),
      br(), br(),
      p("This table summarizes the regression statistics from running linear 
        regressions of each economic indicator against the SF Tech Pulse. As 
        expected, all indicators but Wage Rate have a positive slope estimate. 
        The absolute values of t statistics of each model are all relatively 
        high, and thus all p-values are near 0. The correlations (absolute value) 
        range from 0.2 to 0.4, suggesting moderate linear relationships. The 
        coefficients of determination are relatively low - the GDP vs. SFTP 
        model captures 14.8% of the variation in GDP, while the wage rate vs. 
        SFTP model captures 5.4% of the variation in wage rate."),
      tableOutput("sum_reg"),
      br(), br(),
      p("The following graph is a coefficient plot that visualizes the confidence 
        intervals for each linear model. None of the intervals capture the value 
        of 0, and all p-values are near 0, so each model is statistically 
        significant."),
      plotOutput("coef_plot")
    ),
    
    tabPanel(
      "About Me",
      h3("About Me"),
      p("Hi there, thanks for viewing my final project! My name is Melissa, and I 
        am a senior at Thomas Jefferson High School for Science and Technology 
        who is interested in pursuing computer science and economics.
        Outside of academics, I love playing the piano, listening to K-pop music,
        and drinking copious amounts of bubble tea. You can access my GitHub ", 
        a("here", href ="https://github.com/mzwu"), '.'),
      HTML('<center><img src="headshot.jpg" width="400"></center>')
    )
  )
)

server <- function(input, output, session) {
  
  output$var_time_text <- renderUI({
    if(input$select_var_time == "SF Tech Pulse") {
      tagList("The ", a("San Francisco Tech Pulse", href="https://fred.stlouisfed.org/series/SFTPAGRM158SFRBSF"), 
      " measures the health of activity in the US information technology sector. 
      The index is determined by indicators including investment in IT goods, 
      consumption of personal computers, employment in the IT sector, industrial 
      production, and more. The SF Tech Pulse was measured monthly by the 
      Federal Reserve Bank of San Francisco, but was discontinued in March of 
      2020. The units used are Percent Change at Annual Rate, Seasonally Adjusted.")
    } else {
    if(input$select_var_time == "Real GDP") {
        tagList(a("Real Gross Domestic Product", href="https://fred.stlouisfed.org/series/GDPC1"),
        " measures the total value of the goods and services produced in the US, 
        adjusted for inflation. GDP includes consumption, investment, government 
        expenditures, and net exports. The measure is computed quarterly by the 
        US Bureau of Economic Analysis, and is measured in units of Billions of 
        Chained 2012 Dollars, Seasonally Adjusted Annual Rate.")
      } else {
    if(input$select_var_time == "CPI") {
        tagList("The ", a("Consumer Price Index", href="https://fred.stlouisfed.org/series/CPIAUCSL"),
        " measures the average change in prices of goods and services, including 
        food, clothing, shelter, fuel, service fees, and sales taxes. Prices are 
        weighted based on their spending importance. The CPI can be used to 
        determine changes in the overall price level, with increases suggesting 
        inflation and decreases suggesting deflation. This index accounts for 
        about 88% of the US population, including wage-earners, self-employed, 
        unemployed, retirees, and those not in the labor force. The CPI is 
        measured monthly by the US Bureau of Labor Statistics.")
      } else {
    if(input$select_var_time == "Interest Rate") {
        tagList("Interest rates affect incentives for consumers to spend and 
        save, with higher interest rates promoting higher saving, and lower 
        interest rates promoting more spending. The ", 
        a("discount rate", href="https://fred.stlouisfed.org/series/INTDSRUSM193N"),
        " is a type of interest rate at which the Federal Reserve makes loans to 
        banks. The interest rate is measured by the International Monetary Fund 
        each month, and is measured in Percent per Annum, Not Seasonally Adjusted.")
      } else {
    if(input$select_var_time == "Wage Rate") {
        tagList("The median ", a("wage rate", href="https://fred.stlouisfed.org/series/LES1252881600Q"),
        " measures the usual weekly real earnings of workers aged 16 and above. 
        This data covers workers in the public and private sectors, but excludes 
        those who are self-employed. Usual weekly earnings are measured prior to 
        taxes and deductions and include overtime pay and tips. This data is 
        measured quarterly by the US Bureau of Labor Statistics, and is measured 
        in 1982-84 CPI Adjusted Dollars, Seasonally Adjusted.")
      }
    }
    }
    }
    }
  })
  
  results <- reactive({
    data
  })
  
    output$var_time_plot <- renderUI({
      # HTML('<center><img src="sftp.gif" width="400"></center>')
      if(input$select_var_time == "SF Tech Pulse") {
        tagList(img(src = "sftp.gif"))
      } else {
        if(input$select_var_time == "Real GDP") {
          tagList(img(src = "gdp.gif"))
        } else {
          if (input$select_var_time == "CPI") {
            tagList(img(src = "cpi.gif"))
          } else {
            if (input$select_var_time == "Interest Rate") {
              tagList(img(src = "interest.gif"))
            } else {
              if (input$select_var_time == "Wage Rate") {
                tagList(img(src = "wage.gif"))
              }
            }
          }
        }
      }
      
      # outfile <- tempfile(fileext = '.gif')
      # list(src = "sftp.gif",
      #      contentType = 'image/gif',
      #      width = 400,
      #      height = 300,
      #      alt = "text")
    })
  
  # output$var_time_plot <- renderPlot({
  #   if(input$select_var_time == "SF Tech Pulse") {
  #     results() %>%
  #       ggplot(aes(x = date, y = sftp)) + 
  #         geom_line() +
  #         geom_point() +
  #         labs(title = "San Francisco Tech Pulse Over Time",
  #              x = "Date", y = "SF Tech Pulse",
  #              subtitle = "1979-2000",
  #              caption = "Federal Reserve Bank of San Francisco") +
  #         theme_bw()
  #   } else {
  #   if(input$select_var_time == "Real GDP") {
  #     results() %>%
  #       ggplot(aes(x = date, y = gdp_change)) + 
  #       geom_line() + 
  #       geom_point() +
  #       labs(title = "Real Gross Domestic Product Percentage Change Over Time",
  #            x = "Date", y = "Real GDP % Change",
  #            subtitle = "1979-2000",
  #            caption = "US Bureau of Economic Analysis") +
  #       theme_bw()
  #   } else {
  #   if (input$select_var_time == "CPI") {
  #     results() %>%
  #       ggplot(aes(x = date, y = cpi_change)) + 
  #       geom_line() + 
  #       geom_point() +
  #       labs(title = "Consumer Price Index Percentage Change Over Time",
  #            x = "Date", y = "CPI % Change",
  #            subtitle = "1979-2000",
  #            caption = "US Bureau of Labor Statistics") +
  #       theme_bw()
  #   } else {
  #   if (input$select_var_time == "Interest Rate") {
  #     results() %>%
  #       ggplot(aes(x = date, y = interest_change)) + 
  #       geom_line() + 
  #       geom_point() +
  #       labs(title = "Discount Rate Percentage Change Over Time",
  #            x = "Date", y = "Discount Rate % Change",
  #            subtitle = "1979-2000",
  #            caption = "International Monetary Fund") +
  #       theme_bw()
  #   } else {
  #   if (input$select_var_time == "Wage Rate") {
  #     results() %>%
  #       ggplot(aes(x = date, y = wage_change)) + 
  #       geom_line() + 
  #       geom_point() +
  #       labs(title = "Median Wage Rate Percentage Change Over Time",
  #            x = "Date", y = "Median Wage Rate % Change",
  #            subtitle = "1979-2000",
  #            caption = "US Bureau of Labor Statistics") +
  #       theme_bw()
  #   }
  #   }
  #   }
  #   }
  #   }
  #   
  # })
  
  output$var_reg_plot <- renderPlot({
    if(input$select_var_reg == "Real GDP") {
      results() %>%
        ggplot(aes(x = sftp, y = gdp_change)) +
        geom_point() +
        geom_smooth(method = lm) +
        labs(title = "Real GDP % Change vs. SF Tech Pulse",
             x = "SF Tech Pulse", y = "Real GDP % Change") +
        theme_bw()
    } else {
      if(input$select_var_reg == "CPI") {
        results() %>%
          ggplot(aes(x = sftp, y = cpi_change)) +
          geom_point() +
          geom_smooth(method = lm) +
          labs(title = "CPI % Change vs. SF Tech Pulse",
               x = "SF Tech Pulse", y = "CPI % Change") +
          theme_bw()
      } else {
        if (input$select_var_reg == "Interest Rate") {
          results() %>%
            ggplot(aes(x = sftp, y = interest_change)) +
            geom_point() +
            geom_smooth(method = lm) +
            labs(title = "Discount Rate % Change vs. SF Tech Pulse",
                 x = "SF Tech Pulse", y = "Discount Rate % Change") +
            theme_bw()
        } else {
          if (input$select_var_reg == "Wage Rate") {
            results() %>%
              ggplot(aes(x = sftp, y = wage_change)) +
              geom_point() +
              geom_smooth(method = lm) +
              labs(title = "Wage Rate % Change vs. SF Tech Pulse",
                   x = "SF Tech Pulse", y = "Wage Rate % Change") +
              theme_bw()
          }
        }
      }
    }
    
  })
  
  output$var_reg_table <- function() {
    if(input$select_var_reg == "Real GDP") {
      reg_table <- rbind(gdp_reg)
      rownames(reg_table) <- c("GDP % Change vs. SF Tech Pulse")
      colnames(reg_table) <- c("Y-Intercept", "Slope Estimate", "Std. Error", 
                         "t value", "Pr(>|t|)", "Correlation", expression("$R^2$"))
      reg_table %>%
        round(digits = 3) %>%
        kbl() %>%
        kable_styling(bootstrap_options = c("hover", "responsive"))
    } else {
      if(input$select_var_reg == "CPI") {
        reg_table <- rbind(cpi_reg)
        rownames(reg_table) <- c("CPI % Change vs. SF Tech Pulse")
        colnames(reg_table) <- c("Y-Intercept", "Slope Estimate", "Std. Error", 
                                 "t value", "Pr(>|t|)", "Correlation", expression("$R^2$"))
        reg_table %>%
          round(digits = 3) %>%
          kbl() %>%
          kable_styling(bootstrap_options = c("hover", "responsive"))
      } else {
        if (input$select_var_reg == "Interest Rate") {
          reg_table <- rbind(interest_reg)
          rownames(reg_table) <- c("Interest Rate % Change vs. SF Tech Pulse")
          colnames(reg_table) <- c("Y-Intercept", "Slope Estimate", "Std. Error", 
                                   "t value", "Pr(>|t|)", "Correlation", expression("$R^2$"))
          reg_table %>%
            round(digits = 3) %>%
            kbl() %>%
            kable_styling(bootstrap_options = c("hover", "responsive"))
        } else {
          if (input$select_var_reg == "Wage Rate") {
            reg_table <- rbind(wage_reg)
            rownames(reg_table) <- c("Wage Rate % Change vs. SF Tech Pulse")
            colnames(reg_table) <- c("Y-Intercept", "Slope Estimate", "Std. Error", 
                                     "t value", "Pr(>|t|)", "Correlation", expression("$R^2$"))
            reg_table %>%
              round(digits = 3) %>%
              kbl() %>%
              kable_styling(bootstrap_options = c("hover", "responsive"))
          }
        }
      }
    }
  }
  
  output$sum_stat <- function(){
    stats <- cbind(sum_min, sum_median, sum_max, sum_mean, sum_sd)
    rownames(stats) <- c("SF Tech Pulse", "GDP % Change", "CPI % Change", 
                         "Interest Rate % Change", "Wage Rate % Change")
    colnames(stats) <- c("Min", "Median", "Max", "Mean", "SD")
    
    stats %>%
      round(digits = 3) %>%
      kbl() %>%
      kable_styling(bootstrap_options = c("hover", "responsive"))
  }
  
  output$sum_reg <- function() {
    reg <- rbind(gdp_reg, cpi_reg, interest_reg, wage_reg)
    rownames(reg) <- c("GDP % Change", "CPI % Change", 
                       "Interest Rate % Change", "Wage Rate % Change")
    colnames(reg) <- c("Y-Intercept", "Slope Estimate", "Std. Error", 
                       "t value", "Pr(>|t|)", "Correlation", expression("$R^2$"))
    
    reg %>%
      round(digits = 3) %>%
      kbl() %>%
      kable_styling(bootstrap_options = c("hover", "responsive"))
  }
  
  output$coef_plot <- renderPlot({
    dwplot(list(gdp_lm, cpi_lm, interest_lm, wage_lm),
           model_order = c("Model 1", "Model 2", "Model 3", "Model 4")) %>%
      relabel_predictors(c(sftp = "SF Tech Pulse")) +
      theme_bw() + 
      xlab("Coefficient Estimate") + ylab("") +
      geom_vline(xintercept = 0,
                 colour = "grey60",
                 linetype = 2) +
      ggtitle("Relationship Between Tech Sector and Economic Growth") +
      scale_color_discrete(
        name = "Economic Indicator",
        labels = c("Real GDP", "CPI", "Discount Rate", "Wage Rate"))
  })
  
}

shinyApp(ui, server)