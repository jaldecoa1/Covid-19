library(tidyverse)
library(lubridate)
library(plotly)

theme_set(theme_minimal())
#Load dataset
covid_data <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/04c9ce883a9f29704d463f00f5d3a82f019b01e4/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

#Convert to long dataset
covid_data <- covid_data %>%
        pivot_longer(-c(`Province/State`, `Country/Region`, Lat, Long),
                     names_to = "date", values_to = "confirmed_cases_n") %>%
        rename(
                province_state = `Province/State`,
                country_region = `Country/Region`,
                latitude = Lat,
                longitude = Long
        )

#Designate date column
covid_data <- covid_data %>% 
        mutate(date = mdy(date))

#Create column that sums all cases by region and date
covid_data <- covid_data %>%
        group_by(country_region, date) %>%
        summarize(confirmed_cases_n = sum(confirmed_cases_n)) %>%
        ungroup()

#Plot counts of US COVID-19 cases in 2020
US <- covid_data %>%
        filter(country_region == "US") %>%
        ggplot(aes(x = date, y = confirmed_cases_n)) +
        geom_line(color = "blue") +
        scale_x_date(date_breaks = "1 week", date_labels = "%d %b") +
        scale_y_continuous(labels = scales::comma) +
        labs(x = "Date", y = "Confirmed cases (n)", title = "Number of confirmed COVID-19 cases in the World (Top) and US (Bottom) in 2020")

#Plot counts of Global COVID-19 cases in 2020
World <- covid_world_data %>%
        ggplot(aes(x = date, y = confirmed_cases_tot)) +
        geom_line(color = "red") +
        scale_x_date(date_breaks = "1 week", date_labels = "%d %b") +
        scale_y_continuous(labels = scales::comma) +
        labs(x = "Date", y = "Confirmed cases (n)", title = "Number of confirmed COVID-19 cases in the World in 2020")

#Generate interactive graph
subplot(World, US, nrows = 2)


