---
title: "Airbnb Rentals"
author: "Anusha Chintakunta Manjunatha"
date: "March 22, 2019"
output: 
  html_document:
    keep_md: true
---








## Analysing Short-term Property Renting in NYC




### Introduction   

#####**Problem Statement**
You are consulting for a real estate company that has a niche in purchasing properties to rent out short-term as part of their business model specifically within New York City.  The real estate company has already concluded that two bedroom properties are the most profitable; however, they do not know which zip codes are the best to invest in.  
The real estate company has engaged your firm to build out a data product and provide your conclusions to help them understand which zip codes would generate the most profit on short term rentals within New York City.
You will be looking at publicly available data from Zillow and AirBnB:

* Cost data: Zillow provides us an estimate of value for two-bedroom properties
* Revenue data: AirBnB is the medium through which the investor plans to lease out their investment property. Fortunately for you, we are able to see how much properties in certain neighborhoods rent out for in New York City
* You can assume an occupancy rate of 75% or you can come up with your own model to calculate occupancy; just let us know how you came to that calculation      
After meeting with the strategy team, you've got an idea of where to start, key concerns, and how you can help this real estate company with the market data while keeping the following assumptions in mind:

* The investor will pay for the property in cash (i.e. no mortgage/interest rate will need to be accounted for).
* The time value of money discount rate is 0% (i.e. $1 today is worth the same 100 years from now).
* All properties and all square feet within each locale can be assumed to be homogeneous (i.e. a 1000 square foot property in a locale such as Bronx or Manhattan generates twice the revenue and costs twice as much as any other 500 square foot property within that same locale.).




### Packages Required 

##### **Packages**

**R** has been used to perform all data wrangling and analytical tasks. I have also used **Tableau Public** and **R Shiny** to show interactive maps and dashboards.

* `readr`     - To read .csv files
* `dplyr`     - For efficient data wrangling activities
* `ggplot2`   - For beautiful visiualizations
* `DT`        - To display Tables in HTML format
* `tidyverse` - The mother of all Data wrangling packages.
* `scales`    - For scaling axis in ggplot2
* `lubridate` - To manipulate date fields
* `stringr`   - For string manipulation
* `shiny`     - To build interactive apps
* `Tableau`   - For beautiful visiualizations


### Data Preperation and Analysis 


##### **About the data**

**************************************************************
The data provided to us are two csv files. **listings.csv** and **Zip_ZHVI_2bedroom.csv**.
The listings file has details about Airbnb listings in the 5 boroughs of New York.
The Zillow Home Value Index(ZHVI) file has the property value of 2 bedroom house from the 90s till 2017. We can clearly see from the available data that there are many missing values and many columns  that are redundant. In this analysis only few variables are considered. There are no imputations done for any missing values. A deeper analysis is required to perform all these activities and would also require scraping data from various sources.  

+ Important Variables Considered:
              
     Variable   | Description
     -------------  | -------------
     zipcode        | Zipcode
     price          | property renatlal price per night 
     bedrooms       | No. of bedrooms in the property 
    neighborhood_group_cleansed | 5 Boroughs of NYC
    1998-06 - 2017-06 | NYC 2 bedroom Property Prices over the years
    review_location_rating | ratings given for the airbnb rental locations


```r
listings_original <- read_csv("data/listings.csv")
zillow <- read_csv("data/Zip_Zhvi_2bedroom.csv")
```

As you can see the code, few important variables have been considered for this analysis. The most important variables are zipcodes, price, neighborhoods, review_location_rating etc.
I have removed $ characters from prices and converted them to numbers. Also, rating columns are converted into ordered vectors of range 1 to 10.  Zipcode values which had a range have been assigned the value of the first instance of the range.

The unwanted columns like variables for State and city were removed as we know that airbnb data contains only data for New York city and hence on merging them together it wi lltake care of the required observations for new york city and so have filtered out variables like City, State etc. Of the two datasets, Zillow was relatively cleaner and did not require much work. 



```r
#Selecting only Important variables which we can use for this analysis
listings_2bed <- filter(listings_original, bedrooms == 2) %>% 
                 select(id, name, listing_url,host_id,host_since, host_is_superhost,
                         neighbourhood_cleansed, neighbourhood_group_cleansed,
                         zipcode, latitude, longitude,property_type,accommodates, 
                         price,number_of_reviews, review_scores_rating,
                         review_scores_cleanliness, review_scores_location, 
                         review_scores_value )

# Removing $ and , from monetary columns ----------------------------------
listings_2bed$price <- as.numeric(gsub("[\\$,]", "", listings_2bed$price))

# Cleaning Zipcodes -------------------------------------------------------
#listings_2bed[which(str_length(listings_2bed$zipcode)<5),]$zipcode
#listings_2bed[which(str_length(listings_2bed$zipcode)>5),]$zipcode
listings_2bed[530,"zipcode"]   <- "11426"
listings_2bed[1644, "zipcode"] <- "10003"

# Converting to date ------------------------------------------------------
listings_2bed$host_since <- mdy(listings_2bed$host_since)

# Converting to Ordered Factors -------------------------------------------
listings_2bed$review_scores_location    <- ordered(listings_2bed$review_scores_location,
                                               levels = 1:10)
listings_2bed$review_scores_value       <- ordered(listings_2bed$review_scores_value,
                                               levels = 1:10)
```

#####**Forecasting Future Property Prices**   

The Zillow table has past prices of the two bed room houses. In this section we have tried to forecast the next 5 years growth in the property prices based on the historic 5 years data.
This is acheived by calculating the average year on year growth over last 5 years and treating the average constant next 5 years. This will form basis for our analysis on calculating Payback Periods for different rental properties.


```r
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
names(zillow_forecast)[14] <- "investment"                   
names(zillow_forecast)[25] <- "year_5_forecast"
merged_ijoin <- inner_join(listings_2bed, zillow_forecast, by = c("zipcode" = "RegionName"))
merged_sheets <- left_join(listings_2bed, zillow_forecast, by = c("zipcode" = "RegionName"))
```

*******************************************************************************************
#####**Calculating Median Rental Prices**  
*******************************************************************************************
We see from the distribution of the price variable that is is skewed. Therefore I have used a median price per zipcode to estimate the price per night per property in that particular zipcode.  


```r
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
```

![](Capone_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
merged_ijoin <- merged_ijoin %>% 
  group_by(zipcode) %>%
  mutate(median_rent_zip = median(price)) %>%
  mutate(rent_range_zip  = ifelse(median_rent_zip < 150, "65-150",
                           ifelse(median_rent_zip > 150 & median_rent_zip < 200, 
                                  "150-250",
                           ifelse(median_rent_zip > 250 & median_rent_zip < 450, 
                                  "250-400", ">400"))))
```

*******************************************************************************************
#####**Crafting a Visual Data Narrative**   
******************************************************************************************* 

In the below interactive map we can see what areas of the city are being booked most prominently. It is very apparent that Manhattan is the most sorted after areas in the city. Brooklyn also looks crowded  with airbnb rentals. Feel free to explore more by interacting with the tableau public map I have created for this exercise. Please visit the tableau page if the view does not appear proper.

https://public.tableau.com/profile/nush#!/vizhome/AirbnbNewYork_15531795122020/Dashboard1


<iframe src="https://public.tableau.com/views/AirbnbNewYork_15531795122020/Dashboard1?:showVizHome=no&:embed=true"
 width="900" height="650"></iframe>  
 
*******************************************************************************************

#####**What kind of properties being rented**
*******************************************************************************************

From the below chart we can clearly see that Apartments are being rented more. Given the size of the city and the population this is an expected result. We can also see the distribution across the 5 Boroughs of New York.

```r
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
```

![](Capone_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
  
    
*******************************************************************************************
#####**Most Popular Zipcodes Based on Number of Hosts**  
*******************************************************************************************
 Here we have claculated Popularity of zipcodes based on the number of rentals. Clearly zipcode 11211 is the winner with close to 300 entries.
 

```r
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
                  ylab("Number of Airbnb Rentals") +
                  theme_minimal()+
                  theme(text = element_text(size = 10),
                  plot.title = element_text(size = 16,color = "#ff5a5f", 
                                            face = "bold",margin = margin(b = 7)),
                  plot.subtitle = element_text(size = 10, color = "darkslategrey", 
                                               margin = margin(b = 7)))
```

![](Capone_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
  
  
*****************************************************************************************************************
#####**Most Popular Properties Based on Location Review**
********************************************************************************************************  
Here most popular zipcodes based on location review scores are plotted. We can see that Zipcode 10024 is the most popular of the lot.  

```r
 merged_sheets %>%
   select(zipcode, review_scores_location) %>%
   group_by(zipcode) %>%
   na.omit() %>%
   mutate(mean_locrev = mean(as.numeric(as.character(review_scores_location)))) %>%
   mutate(n_locrev = n()) %>%
   filter(n_locrev > 40) %>%
   arrange(desc(mean_locrev)) %>%
   slice(1)%>%
  head(20) %>%
   ggplot(aes ( x = reorder(zipcode,mean_locrev), y = mean_locrev) )+
   geom_point( color="turquoise3", size=4) +
   geom_segment(aes(x=zipcode, xend=zipcode, y=min(mean_locrev),yend=max(mean_locrev)), 
                linetype="dashed", size=0.1, color = "grey") +   # Draw dashed lines
   labs(title="Most Popular Zipcodes Based on Location Review", 
        subtitle="Calculated for all available zipcodes in Airbnb data") +  
   xlab("Zipcode") +
   ylab("Mean Rating") +
   scale_y_continuous(breaks = seq(8,10 , by = 0.5))+
   coord_flip() +
   theme_minimal()+
   theme(text = element_text(size = 10),
         plot.title = element_text(size = 16,color = "#ff5a5f", face = "bold",
                                   margin = margin(b = 7)),
         plot.subtitle = element_text(size = 10, color = "darkslategrey", margin = margin(b = 7)))
```

![](Capone_files/figure-html/unnamed-chunk-7-1.png)<!-- -->
    
******************************************************************************************* 
#####**Where are the Property Values Growing?**  
*******************************************************************************************
Here we have calculated Growth of the property value in 5 years using the forecast value calculated earlier. This is an important factor to be considered for the long run. 


```r
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
```

![](Capone_files/figure-html/unnamed-chunk-8-1.png)<!-- -->


*******************************************************************************************
#####**Which zipcode gives quicker ROI?**
*******************************************************************************************

Here we are calculating the Payback Period by considering 75% occupancy and Median Rental price per zipcode calculated earlier. Using this we are estimating the years in which the rental properties will be able to payback the initial investment amount.

Please refer to the Shiny app for an interactive visualization of the same.

```r
merged_ijoin <- merged_ijoin %>% group_by(zipcode) %>%
  mutate(annual_payback = 0.75 * 365 * median_rent_zip) %>%
  mutate(payback_period = investment/annual_payback)

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
```

![](Capone_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

*******************************************************************************************
#####**Summary**
*******************************************************************************************
* The top zip codes in terms of quicker ROI are:10305,10312, 10304
* The most popular zipcodes  11217,11238 and 10002
* Most important locations in terms of Property Value growth are 11217, 11231, 11215, 11201
   

*******************************************************************************************
#####**Next Steps**
*******************************************************************************************
* Modeling or imputation to predict the prices of the missing zipcodes in zillow dataset
* Including review data,host verification and other property related information  of host and property for better prediction of which zip codes to invest in
 
