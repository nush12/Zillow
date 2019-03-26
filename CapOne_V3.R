library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(scales)
library(plotly)    
library(ggplot2)

# Read Files --------------------------------------------------------------

listings_original <- read_csv("data/listings.csv")
zillow <- read_csv("data/Zip_Zhvi_2bedroom.csv")


listings_2bed <- filter(listings_original, bedrooms == 2) %>% 
                 select(id, name, listing_url,host_id, host_is_superhost,
                         neighbourhood_cleansed, neighbourhood_group_cleansed,
                         zipcode, latitude, longitude,property_type,accommodates, 
                         square_feet, price,number_of_reviews, review_scores_rating,
                         review_scores_cleanliness, review_scores_location, 
                         review_scores_value )

# Removing $ and , from monetary columns ----------------------------------
listings_2bed$price <- as.numeric(gsub("[\\$,]", "", listings_2bed$price))
listings_2bed$security_deposit <- as.numeric(gsub("[\\$,]" , "", listings_2bed$security_deposit))
listings_2bed$cleaning_fee     <- as.numeric(gsub("[\\$,]", "", listings_2bed$cleaning_fee))

# Cleaning Zipcodes -------------------------------------------------------
listings_2bed[which(str_length(listings_2bed$zipcode)<5),]$zipcode
listings_2bed[which(str_length(listings_2bed$zipcode)>5),]$zipcode
listings_2bed[530,"zipcode"]   <- "11426"
listings_2bed[1644, "zipcode"] <- "10003"

# Converting to date ------------------------------------------------------
listings_2bed$host_since <- mdy(listings_2bed$host_since)

# Converting to Ordered Factors -------------------------------------------
listings_2bed$review_scores_location    <- ordered(listings_2bed$review_scores_location,
                                               levels = 1:10)
listings_2bed$review_scores_value       <- ordered(listings_2bed$review_scores_value,
                                               levels = 1:10)


# Forecasting Property value for Next 5 years -----------------------------

 zillow_forecast <- select(zillow, City, State, RegionName, names(zillow)[142],
                           names(zillow)[154], names(zillow)[166], names(zillow)[178],
                           names(zillow)[190], names(zillow)[202], names(zillow)[214],
                           names(zillow)[226], names(zillow)[238], names(zillow)[250],
                           names(zillow)[262])%>%
                   filter( City == "New York") %>%
                   mutate(YoY1 = `2013-06`/`2012-06`) %>%
                   mutate(YoY2 = `2014-06`/`2013-06`) %>%
                   mutate(YoY3 = `2015-06`/`2014-06`) %>%
                   mutate(YoY4 = `2016-06`/`2015-06`) %>%
                   mutate(YoY5 = `2017-06`/`2016-06`) %>%
                   mutate(avg_growth = (YoY1+YoY2+YoY3+YoY4+YoY5)/5) %>%
                   mutate(`2018-06` = avg_growth * `2017-06`) %>%
                   mutate(`2019-06` = avg_growth * `2018-06`) %>%
                   mutate(`2020-06` = avg_growth * `2019-06`) %>%
                   mutate(`2021-06` = avg_growth * `2020-06`) %>%
                   mutate(`2022-06` = avg_growth * `2021-06`)
#                   gather(key = "year", value = "prop_value", -c(City, State,
#                          RegionName)) %>%
#                   arrange(RegionName)
names(zillow_forecast)[14] <- "investment"                   
names(zillow_forecast)[25] <- "year_5_forecast"

merged_ijoin <- inner_join(listings_2bed, zillow_forecast, by = c("zipcode" = "RegionName"))
merged_sheets <- left_join(listings_2bed, zillow_forecast, by = c("zipcode" = "RegionName"))
nrow(merged_sheets)
nrow(merged_ijoin)

# Median Rent -------------------------------------------------------------

merged_ijoin <- merged_ijoin %>% 
  group_by(zipcode) %>%
  mutate(median_rent_zip = median(price)) %>%
  mutate(rent_range_zip  = ifelse(median_rent_zip < 150, "65-150",
                           ifelse(median_rent_zip > 150 & median_rent_zip < 200, 
                                  "150-250",
                           ifelse(median_rent_zip > 250 & median_rent_zip < 450, 
                                  "250-400", ">400"))))

merged_ijoin<- merged_ijoin %>% 
  group_by(neighbourhood_group_cleansed) %>%
  mutate(median_rent_nhood = median(price)) %>%
  mutate(rent_range_nhood  = ifelse(median_rent_nhood < 150, "65-150",
                             ifelse(median_rent_nhood > 150 & median_rent_nhood < 200, 
                                    "150-250",
                             ifelse(median_rent_nhood > 250 & median_rent_nhood < 450,
                                    "250-400", ">400"))))


# annual payback and payback period for each zipcode----------------------------------------------------------

merged_ijoin <- merged_ijoin %>% group_by(zipcode) %>%
  mutate(annual_payback = 0.75 * 365 * median_rent_zip) %>%
  mutate(payback_period = investment/annual_payback)

# Tableau Sheet ------------------------------------------------------- 
tableau_sheet <- as.data.frame(merged_sheets)
save(tableau_sheet, file = "data/CapOne.RData")
# Plots 1 -------------------------------------------------------------------

# Preparing data for the plot ---------------------------------------------
property_df <- listings_2bed %>%
                 filter(property_type %in% c("Apartment","Condominium", "House",
                                             "Loft")) %>% 
                 group_by(neighbourhood_group_cleansed) %>%
                 mutate(total_property = n()) %>%
                 group_by(neighbourhood_group_cleansed, property_type) %>%
                 mutate(borough_prop_count = n())
property_df2 <-unique(select(property_df, neighbourhood_group_cleansed,
                             property_type, total_property, borough_prop_count)) %>%
                 mutate(ratio = borough_prop_count/total_property)

property_df2 <- bind_rows(property_df2, list(neighbourhood_group_cleansed = "Staten Island",property_type = "Condominium", total_property = 0, borough_prop_count = 0, ratio = 0))
property_df2 <- bind_rows(property_df2, list(neighbourhood_group_cleansed = "Staten Island",property_type = "Loft", total_property = 0, borough_prop_count = 0, ratio = 0))


ggplot(property_df2, aes(x = neighbourhood_group_cleansed, y = ratio,
                         fill = property_type)) +
  geom_bar(position = "dodge",stat="identity") + 
  xlab("Borough") + ylab("Count") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual("Property Type",values=c("#357b8a","turquoise3", "#ff5a5f",
                   "darkseagreen")) +
  ggtitle("Types of Properties Rented in NYC") +
  theme_minimal()+
  theme(text = element_text(family = "Georgia", size = 10, face = "bold"),
        plot.title = element_text(size = 16,color = "#ff5a5f", margin = margin(b = 7)),
        plot.subtitle = element_text(size = 10, color = "darkslategrey", margin = margin(b = 7)))


# Plot3 -------------------------------------------------------------------
listings_2bed %>% group_by(zipcode) %>%
                  summarise(num = n()) %>%
                  arrange() %>%
                  top_n(20) %>%
                  ggplot(aes(x = reorder(zipcode,num), y = num))+
                  geom_bar(stat = "identity", fill = "#ff5a5f") +
                  coord_flip() +
                  labs(title = "Most popular zipcodes", 
                  subtitle = "zipcodes with most number of Airbnb hosts",
                  colour   = " Borough") +  
                  xlab("Zipcode") +
                  ylab("Forecasted growth") +
                  theme_minimal()+
                  theme(text = element_text(size = 10),
                  plot.title = element_text(size = 16,color = "#ff5a5f", 
                                            face = "bold",margin = margin(b = 7)),
                  plot.subtitle = element_text(size = 10, color = "darkslategrey", 
                                               margin = margin(b = 7)))

# plot 4 ------------------------------------------------------------------
merged_ijoin %>%
  group_by(zipcode) %>%
  mutate(growth = year_5_forecast/investment) %>% 
  ggplot(aes(x= reorder(zipcode,growth), y= growth, 
                        color = neighbourhood_group_cleansed)) +
  geom_point( size=3) +
  geom_segment(aes(x=zipcode, xend=zipcode, y=min(growth),yend=max(growth)), 
               linetype="dashed", size=0.1, color = "grey") +
  scale_color_manual(values = c("#ff5a5f","#66b2b2","#FFA992",
                                 "#6979DA")) +
  labs(title = "Property Value Growth in 5 Years", 
        subtitle = "calculated for 22 available zipcodes",
        colour   = " Borough") +  
  xlab("Zipcode") +
  ylab("Forecasted growth") +
  scale_y_continuous(labels = scales::percent)+
  coord_flip() +
  theme_minimal() +
  theme(text = element_text(size = 10),
          plot.title = element_text(size = 16, color = "#ff5a5f", face = "bold",margin = margin(b = 7)),
          plot.subtitle = element_text(size = 10, color = "darkslategrey", margin = margin(b = 7)))
  

# Plot 3 ------------------------------------------------------------------

merged_ijoin %>%
  group_by(zipcode) %>%
  ggplot(aes(x= reorder(zipcode, desc(payback_period)), y= payback_period)) +
  geom_point( color="#ff5a5f", size=3) +
  geom_segment(aes(x=zipcode, xend=zipcode, y=min(payback_period),yend=max(payback_period)), 
               linetype="dashed", size=0.1, color = "grey") +   # Draw dashed lines
  labs(title="Payback Period for Initial Investment", 
       subtitle="calculated for 22 available zipcodes") +  
  xlab("Zipcode") +
  ylab("Payback Period in Years") +
  coord_flip() +
    theme_minimal()+
    theme(text = element_text(size = 10),
          plot.title = element_text(size = 16,color = "#ff5a5f", face = "bold",
                                    margin = margin(b = 7)),
          plot.subtitle = element_text(size = 10, color = "darkslategrey", margin = margin(b = 7)))


# Plot 4 ------------------------------------------------------------------
listings_2bed  %>% filter(price < 2000) %>%
    ggplot(aes(x = price)) +
    geom_histogram(binwidth = 35,fill = "#ff5a5f") +
    labs(title = "Distribution of Airbnb Rental Price",
         subtitle = "Includes all 5 boroughs") +
    xlab("Price Per Night") +
  scale_x_continuous(labels = scales::dollar, breaks = seq(10, 1800, by = 200)) +
    theme_minimal()+
    theme(text = element_text(size = 10),
          plot.title = element_text(size = 16,color = "#ff5a5f", face = "bold",
                                    margin = margin(b = 7)),
          plot.subtitle = element_text(size = 10, color = "darkslategrey", margin = margin(b = 7)))

# Data for the Shiny c --------------------------------------------------

    app_data <- as.data.frame(merged_ijoin)
    save(app_data, file = "data/app_data.RData")  

# App Plot ----------------------------------------------------------------
year_num <- 3
    merged_ijoin %>% 
      select(zipcode,median_rent_zip, investment, annual_payback) %>%
      mutate(payback = (annual_payback * year_num) - investment ) %>%
      mutate(recovery = ifelse(payback >= 0, "Complete", "Incomplete")) %>%
      ggplot(aes(x = reorder(zipcode, payback), y = payback,
                 fill = as.factor(recovery))) +
      geom_bar(stat = "identity", width = 0.6) +
      labs(title = "Distribution of Airbnb Rental Price",
           subtitle = "Includes all 5 boroughs") +
      xlab("zipcode") +
      ylab("Payback amount") +
      scale_y_continuous(labels = comma)+
    theme_minimal()+
      theme(text = element_text(size = 10),
            plot.title = element_text(size = 16,color = "#ff5a5f", face = "bold",
                                      margin = margin(b = 7)),
            plot.subtitle = element_text(size = 10, color = "darkslategrey", 
                                         margin = margin(b = 7)))
     
    