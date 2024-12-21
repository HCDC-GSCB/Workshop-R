# ====== R Scipt for take home exercise solution === 

covid_cases <- readRDS("~/GitHub/Workshop-R/day1/data/covid_cases.rds")

library(skimr)

skim(covid_cases)

first_report_date <- min(covid_cases$date)

latest_report_date <- max(covid_cases$date)

covid_cases$case_global <- rowSums(covid_cases[,-1])

covid_cases$percent_chn <- covid_cases$cases_chn/covid_cases$case_global 

percent_cases <- function(data,x) {
  column_name <- paste0("cases_", x)
  percent_column <- paste0("percent_", x)
  data[,percent_column] <- data[,column_name]/data$case_global
  return(data)
  }

covid_cases <- percent_cases(covid_cases,"kor")

# Gọi hàm và cập nhật data gốc

percent_cases(covid_cases, "vnm")

f <- function(x) {
  data <- covid_cases
  column_name <- paste0("cases_", x)
  percent_column <- paste0("percent_", x)
  data[,percent_column] <- data[,column_name]/data$case_global
  return(data[,percent_column])
}
 f("vnm")

#câu 3.5
 
 # Uncomment and run the following if you have not installed tidyverse or ggplot2
 # install.packages("tidyverse")
 library(ggplot2)
 plot_col <- "cases_chn"
 country <- "China"
 
 ggplot() +
   geom_col( # layer for bar chart
     aes( # define columns for x, y axis
       x = covid_cases$date, # this is equivalent to covid_cases[["date"]]
       y = covid_cases[[plot_col]] 
     ), 
     fill = "cornflowerblue" # choose color for bar chart
   ) +
   geom_line( # layer for line chart
     aes( # define columns for x, y axis
       x = covid_cases$date, 
       y = covid_cases[[plot_col]] 
     ), 
     color = "red" # choose color for line chart
   ) +
   labs(
     y = "Cases",
     x = "Date",
     title = paste0("Reported Covid cases for ", country) # define title for the plot
   )
 
 plot_cases <- function(x, country, color1, color2, mindate, maxdate) {
   plot_col <- paste0("cases_", x)
   ggplot() +
   geom_col( # layer for bar chart
     aes( # define columns for x, y axis
       x = covid_cases$date, # this is equivalent to covid_cases[["date"]]
       y = covid_cases[[plot_col]] 
     ), 
     fill = color1 # choose color for bar chart
   ) +
   geom_line( # layer for line chart
     aes( # define columns for x, y axis
       x = covid_cases$date, 
       y = covid_cases[[plot_col]] 
     ), 
     color = color2 # choose color for line chart
   ) +
     scale_x_date(limits = c(as.Date(mindate), as.Date(maxdate)))+
    labs(
     y = "Cases",
     x = "Date",
     title = paste0("Reported Covid cases for ", country) # define title for the plot
   )}
 
 plot_cases("kor", "Korea", "pink", "black", mindate = as.Date("2020-02-14"), maxdate = as.Date ("2020-04-12")) 
plot_cases("vnm", "VietNam", "red", "yellow","2020-02-14", "2020-04-12" ) 
 