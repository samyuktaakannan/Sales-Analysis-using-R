
{
library(tidyverse)
library(janitor)
library(tidytext) 
library(textclean)
library(ggplot2)
library(stringi)
library(stringr)
library(scales)
options(scipen = 999)


future_50 <- read.csv("C:\\Users\\91994\\Documents\\Future50.csv")

independence100 <- read.csv("C:\\Users\\91994\\Documents\\Independence100.csv")

top250 <- read.csv("C:\\Users\\91994\\Documents\\Top250.csv")




fct_count(independence100$State)

independence100_cleaned <- 
  independence100 %>%
  mutate(state_cleaned = gsub( "\\.", "", str_squish(str_to_lower(State)) ),
         state_cleaned = case_when(state_cleaned == "calif" ~ "CA",
                                   state_cleaned == "colo"  ~ "CO",
                                   state_cleaned == "dc"    ~ "DC",
                                   state_cleaned == "fla"   ~ "FL",
                                   state_cleaned == "ga"    ~ "GA",
                                   state_cleaned == "ill"   ~ "IL",
                                   state_cleaned == "ind"   ~ "IN",
                                   state_cleaned == "mass"  ~ "MA",
                                   state_cleaned == "mich"  ~ "MI",
                                   state_cleaned == "nc"    ~ "NC",
                                   state_cleaned == "nj"    ~ "NJ",
                                   state_cleaned == "ny"    ~ "NY",
                                   state_cleaned == "nev"   ~ "NV",
                                   state_cleaned == "ore"   ~ "OR",
                                   state_cleaned == "pa"    ~ "PA",
                                   state_cleaned == "tenn"  ~ "TN",
                                   state_cleaned == "texas" ~ "TX",
                                   state_cleaned == "va"    ~ "VA")
  )

library(maps)
long_lat <- map_data("state") 
long_lat$region <- str_to_title(long_lat$region) 

independence100_duped <- 
  independence100_cleaned %>%
  group_by(Rank, Restaurant) %>%
  right_join(data.frame(state_abb = state.abb, 
                       state_name = state.name), by = c("state_cleaned" = "state_abb")) %>%
  right_join(long_lat, by = c("state_name" = "region")) %>%
  mutate(rn = row_number()) %>%
  mutate(rn_max = max(rn)) %>%
  mutate(normalized_sales = Sales / rn_max) %>%
  select(-c(rn, rn_max)) %>%
  ungroup() 

independence100_map_ready <- 
  independence100_duped %>%
  group_by(state_cleaned) %>%
  summarise(Sales = sum(normalized_sales)) %>%
  right_join(data.frame(state_abb = state.abb, 
                        state_name = state.name), by = c("state_cleaned" = "state_abb")) %>%
  right_join(long_lat, by = c("state_name" = "region"))

independence100_map_ready$state_cleaned <- factor(independence100_map_ready$state_cleaned)
independence100_map_ready <- independence100_map_ready[order(independence100_map_ready$order), ] # sorting by order (for drawing states)







options(repr.plot.width=15, repr.plot.height=8)

p <- ggplot(independence100_map_ready, aes(long, lat, group = group, fill = Sales)) +
  geom_polygon(color = "black") 
  

centroids <- data.frame(region = tolower(state.name), long = state.center$x, lat = state.center$y)
centroids$abb <- state.abb[match(centroids$region,tolower(state.name))]

independence100_map_ready <- 
  independence100_map_ready %>%
  left_join(centroids %>%
              mutate(state_name = str_to_title(region)),
            by = c("state_name" = "state_name"))

map_with_state_labels <-
  p +
  with(centroids, 
       annotate(geom = "text", x = long, y = lat, label = abb, 
                size = 4, color = "white", family = "Times")
  )
map_with_state_labels +
  scale_fill_continuous(labels = comma) + 
  scale_fill_continuous(labels = dollar) + 
  ggtitle("Independence Result of Top 100 Restaurant in 2020 (by Sales figures)") +
  theme(plot.title = element_text(hjust = 0.5))







sapply(top250, class)
top250_cleaned <- 
  top250 %>%
  mutate(yoy_sales = as.numeric(str_replace(YOY_Sales, "%", "")),
         yoy_units = as.numeric(str_replace(YOY_Units, "%", "")))

top250_cluster_ready <- 
  top250_cleaned %>%
  keep(is.numeric) %>%
  select(-1) 

top250_cluster_ready <- scale(top250_cluster_ready)

set.seed(123) 
 
library(cluster)
library(factoextra)

top250_cleaned$cluster <- clust_20_4_clusters$cluster

ggplot(top250_cleaned, aes(x = log(Units), y = log(Sales), label = Restaurant)) + 
  geom_point(aes(col = as.character(cluster)), size = 2) +
  geom_text(size = 2) + 
  labs(col = "Cluster",
       x = "Log of Units",
       y = "Log of Sales") + 
  ggtitle("Sales Against Units by Cluster") +
  theme(plot.title = element_text(hjust = .5))
}