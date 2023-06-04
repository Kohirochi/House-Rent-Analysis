# CHAI LI QI
# TP061156

# Data Import
# Import data from csv file
data = read.csv( "D:\\APU\\Degree\\Year 2\\Sem 1\\PFDA\\Assignment\\House_Rent_Dataset.csv", header=TRUE)
View(data)

# Save a copy of data
backup_data = data
write.csv(backup_data,"D:\\APU\\Degree\\Year 2\\Sem 1\\PFDA\\Assignment\\House_Rent_Dataset_Backup.csv", row.names = FALSE)

# Data exploration
# Libraries used
# install.packages("tidyverse")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("DescTools")
# install.packages("RColorBrewer")
# install.packages("stringi")
# install.packages("lubridate")
library(tidyverse)
library(dplyr)
library(ggplot2) # Generate chart
library(DescTools)
library(RColorBrewer) # Generate colors
library(stringi)
library(lubridate)

# Display number of rows and columns
dim(data)
# Display number of row
nrow(data)
# Display number of column
ncol(data)
# Display the structure of the data
str(data)
glimpse(data)
# View table in Rstudio
view(data)
# First and last 10 row of the table
head(data)
tail(data)
# Display column name
names(data)
# Display summary of data
summary(data)


# Data cleaning
# Change column name
col_name = c("Posted_Date", "BHK", "Rental_Price", "Size", "Floor", "Type_of_Area", "Locality", "City", "Furnishing_Status", "Preferred_Tenant", "Number_of_Bathroom", "Point_of_Contact")
names(data) = col_name
names(data)

# Check for duplicate data
duplicate_data = data[duplicated(data),]
View(duplicate_data)

# Check for missing values
sum(is.na(data))
# Check for missing values in each column
apply(data, 2, function(col)sum(is.na(col)))

# Ensure total number of floor is exist in every values 
data %>%
  # Check row that don't have out of
  filter(grepl("out of", Floor, fixed=TRUE) == FALSE)

# Change the value to meet consistency
data = data %>%
  mutate(Floor = if_else(grepl("out of", Floor, fixed=TRUE) == TRUE,
                               Floor,
                               if_else(Floor=="Ground", 
                                       "Ground out of 1", 
                                       paste(Floor, "out of", Floor)
                                       )
                               )
         )

# Check again
data %>%
  filter(grepl("out of", Floor, fixed=TRUE) == FALSE)

# Check if current floor is higher than total number of floors
modified_data = data %>%
  mutate(Floor=stri_replace_all_regex(Floor,
                                      pattern=c('out of', 'Ground', 'Upper Basement', 'Lower Basement'),
                                      replacement=c('/', '0', '-1', '-2'),
                                      vectorize=FALSE)) %>%
  # Create floor unit column
  mutate(Floor_Unit=(as.integer(sapply(strsplit(Floor, ' \\/ '), function(x) x[[1]])))) %>%
  # Create building level column
  mutate(Building_level=(as.integer(sapply(strsplit(Floor, ' \\/ '), function(x) x[[2]]))))

# Get the row number of incorrect floor
incorrect_floor_row = which(modified_data$Floor_Unit > modified_data$Building_level)
# View the row
data[incorrect_floor_row,]

# Change the floor value
data[incorrect_floor_row,] = modified_data %>%
  filter(Floor_Unit > Building_level) %>%
  mutate(Floor= paste0(Floor_Unit, " out of ", Floor_Unit)) %>%
  select(-one_of(c("Floor_Unit", "Building_level")))

# Check again if the value is correct 
data[incorrect_floor_row,]


# Check the difference
max(data$Rental_Price) - median(data$Rental_Price)
median(data$Rental_Price) - min(data$Rental_Price)
# Get the row number with maximum rental price
max_price_row = which.max(data$Rental_Price)
# View the row with maximum rental price
View(data[max_price_row,])
# Calculate the median with conditions
replace_value = median(data$Rental_Price[data$City == "Bangalore" & data$BHK == 3 & data$Type_of_Area == "Carpet Area" & data$Furnishing_Status == "Semi-Furnished" & data$Size > 2000])
replace_value
# Replace the value with median
data$Rental_Price[max_price_row] = replace_value
# View the row again
View(data[max_price_row,])


# Data Manipulation
data = data %>%
  # Simplify the floor
  mutate(Floor=stri_replace_all_regex(Floor,
                                            pattern=c('out of', 'Ground', 'Upper Basement', 'Lower Basement'),
                                            replacement=c('/', '0', '-1', '-2'),
                                            vectorize=FALSE)) %>%
  # Create building level column
  mutate(Building_Level=
           ifelse((as.integer(sapply(strsplit(Floor, ' \\/ '), function(x) x[[2]])) > 7), 
                  "High-rise",
                  ifelse(as.integer(sapply(strsplit(Floor, ' \\/ '), function(x) x[[2]])) < 4,
                         "Low-rise",
                         "Mid-rise"
                  )
           )
  ) %>%
  # Create catogorized floor unit
  mutate(Floor_Unit=
           ifelse((as.integer(sapply(strsplit(Floor, ' \\/ '), function(x) x[[1]])) >= 9),
                  "High",
                  ifelse((as.integer(sapply(strsplit(Floor, ' \\/ '), function(x) x[[1]])) <= 4),
                         "Low",
                         "Middle"
                  )
           )
  )

data = data %>%
  # Remove "Contact" word
  mutate(Point_of_Contact=gsub("Contact ", "", Point_of_Contact)) %>%
  # Convert to date data type
  mutate(Posted_Date = as.Date(data$Posted_Date,"%m/%d/%y"))  %>%
  # Reorder dataframe
  select(Posted_Date, Rental_Price, Furnishing_Status, BHK, Number_of_Bathroom, Floor, Floor_Unit, Building_Level, everything()) %>%
  # sort by date
  arrange(Posted_Date)

head(data)

# Save data
cleaned_data = data
write.csv(cleaned_data,"D:\\APU\\Degree\\Year 2\\Sem 1\\PFDA\\Assignment\\House_Rent_Dataset_Ready.csv", row.names = FALSE)


# Question 1: What is the distribution of houses across every column in the dataset?

# Analysis 1-1: Find the number of houses/apartments/flats that are available by city

# Get the unique city list with frequency
table_city= table(data$City, dnn = c("City"))
table_city
# Convert it to data frame
df_city = as.data.frame(table_city, responseName = "Count")
df_city
# Plot the bar chart
ggplot(data=df_city, aes(x=City, y=Count)) +
  ggtitle("The number of houses/apartments/flats available for rent in each city") +
  geom_bar(stat="identity", color="black", fill="#30B3F3") +
  geom_text(aes(label=df_city[,2]), vjust=1.5, color="white", size=3.5) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Sort by count
df_city = df_city %>% 
  arrange(Count)
S
# Create percentage labels
pie_labels = paste0(df_city$City, " - ", round(100 * df_city$Count/sum(df_city$Count), 2), "%")
# Plot pie chart
pie(df_city$Count, labels=pie_labels, radius=1, main="The number of houses/apartments/flats available for rent in each city", col=brewer.pal(6, "Set2"), border="white", clockwise = TRUE)


# Analysis 1-2: Find the number of houses/apartments/flats available for rent by BHK

# Get the bedroom list with frequency
table_BHK = table(data$BHK, dnn = c("BHK"))
table_BHK
# Convert it to data frame
df_BHK = as.data.frame(table_BHK, responseName = "Count")
df_BHK
# Plot the bar chart
ggplot(data=df_BHK, aes(x=BHK, y=Count)) +
  ggtitle("The number of houses/apartments/flats available for rent by BHK") +
  geom_bar(stat="identity", color="black", fill="#30B3F3") +
  geom_text(aes(label=df_BHK[,2]), vjust=-0.5, color="black", size=3.5) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Compute the position of labels
df_BHK = df_BHK %>% 
  arrange(desc(BHK)) %>%
  mutate(prop = Count / sum(df_BHK$Count) * 100) %>%
  mutate(percentage = paste0(round(Count / sum(df_BHK$Count) * 100, 2), "%")) %>%
  mutate(ypos = cumsum(prop) - 0.5*prop )
df_BHK

# Plot pie chart
ggplot(data=df_BHK, aes(x="", y=prop, fill=BHK)) +
  ggtitle("The number of houses/apartments/flats available for rent by BHK") +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + 
  theme_void() +
  geom_text(aes(x = c(1.65, 1.55, 1.4, 1, 1, 1), y = ypos, label = percentage), color="black", size=4) +
  theme(plot.title = element_text(hjust=0.5)) +
  scale_fill_brewer(palette="Set2")


# Analysis 1-3: Find the number of houses that are available by number of bathroom

# Get the bathroom list with frequency
table_bathroom = table(data$Number_of_Bathroom, dnn = c("Number_of_Bathroom"))
table_bathroom
# Convert it to data frame
df_bathroom = as.data.frame(table_bathroom, responseName = "Count")
df_bathroom
# Plot the bar chart
ggplot(data=df_bathroom, aes(x=Number_of_Bathroom, y=Count)) +
  ggtitle("The number of houses/apartments/flats available for rent by the number of bathrooms") +
  geom_bar(stat="identity", color="black", fill="#30B3F3") +
  geom_text(aes(label=df_bathroom[,2]), vjust=-0.5, color="black", size=3.5) +
  xlab("Number of Bathroom") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Compute the position of labels
df_bathroom = df_bathroom %>% 
  arrange(desc(Number_of_Bathroom)) %>%
  mutate(prop = Count / sum(df_bathroom$Count) * 100) %>%
  mutate(percentage = paste0(round(Count / sum(df_bathroom$Count) * 100, 2), "%")) %>%
  mutate(ypos = cumsum(prop) - 0.5*prop )
df_bathroom

# Plot pie chart
ggplot(data=df_bathroom, aes(x="", y=prop, fill=Number_of_Bathroom)) +
  ggtitle("The number of houses/apartments/flats available for rent by the number of bathrooms") +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + 
  theme_void() +
  labs(fill="Number of Bathroom") +
  geom_text(aes(x=c(1.61, 1.55, 1.4, 1.3, 1.2, 1, 1, 1), y=ypos, label=percentage), color="black", size=3) +
  theme(plot.title = element_text(hjust=0.5)) +
  scale_fill_brewer(palette="Set2")


# Analysis 1-4: Find the number of houses that are available by type of area

# Get the type of area list with frequency
table_area = table(data$Type_of_Area, dnn = c("Type_of_Area"))
table_area
# Convert it to data frame
df_area = as.data.frame(table_area, responseName = "Count")
df_area
# Plot the bar chart
ggplot(data=df_area, aes(x=Type_of_Area, y=Count)) +
  ggtitle("The number of houses/apartments/flats available for rent by the type of area") +
  geom_bar(stat="identity", color="black", fill="#30B3F3") +
  geom_text(aes(label=df_area[,2]), vjust=-0.5, color="black", size=3.5) +
  theme_bw() +
  xlab("Type of Area") +
  theme(plot.title = element_text(hjust = 0.5))

# Compute the position of labels
df_area = df_area %>% 
  arrange(desc(Type_of_Area)) %>%
  mutate(prop = Count / sum(df_area$Count) * 100) %>%
  mutate(percentage = paste0(round(Count / sum(df_area$Count) * 100, 2), "%")) %>%
  mutate(ypos = cumsum(prop) - 0.5*prop )
df_area

# Plot pie chart
ggplot(data=df_area, aes(x="", y=prop, fill=Type_of_Area)) +
  ggtitle("The number of houses/apartments/flats available for rent by the type of area") +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + 
  theme_void() +
  labs(fill="Type of Area") +
  geom_text(aes(x=c(1, 1, 1.55), y=ypos, label=percentage), color="black", size=4) +
  theme(plot.title = element_text(hjust=0.5)) +
  scale_fill_brewer(palette="Set1")


# Analysis 1-5: Find the number of houses that are available by furnishing status

# Get the furnishing status list with frequency
table_furnishing = table(data$Furnishing_Status, dnn = c("Furnishing_Status"))
table_furnishing
# Convert it to data frame
df_furnishing = as.data.frame(table_furnishing, responseName = "Count")
df_furnishing
# Plot the bar chart
ggplot(data=df_furnishing, aes(x=Furnishing_Status, y=Count)) +
  ggtitle("The number of houses/apartments/flats available for rent by furnishing status") +
  geom_bar(stat="identity", color="black", fill="#30B3F3") +
  geom_text(aes(label=df_furnishing[,2]), vjust=1.5, color="white", size=3.5) +
  theme_bw() +
  xlab("Furnishing Status") +
  theme(plot.title = element_text(hjust = 0.5))

df_furnishing = df_furnishing %>% 
  # Sort by Count ascending
  arrange(Count) %>%
  # Normalize the values
  mutate(fraction = Count / sum(df_furnishing$Count)) %>%
  # Compute ending point
  mutate(ymax = cumsum(fraction)) %>%
  # Compute starting point
  mutate(ymin = c(0, head(ymax, n=-1)) ) %>%
  # Compute the percentage of the value
  mutate(percentage = paste0(round(Count / sum(df_furnishing$Count) * 100, 2), "%")) %>%
  # Compute the position of labels
  mutate(label_pos = (ymax + ymin) / 2 ) %>%
  # Create new labels
  mutate(labels = paste0(Furnishing_Status, "\n Percentage: ", percentage))
df_furnishing

# Plot the donut chart
ggplot(df_furnishing, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Furnishing_Status)) +
  ggtitle("The number of houses/apartments/flats available for rent by furnishing status") +
  geom_rect() +
  geom_text( x=1.5, aes(y=label_pos, label=labels, color=Furnishing_Status), size=4) +
  coord_polar(theta="y", direction=1) + # Make the chart round
  xlim(c(-1, 4)) + # Indicate the thickness of the chart
  theme_void() +
  labs(fill="Furnishing Status", color="Furnishing Status") +
  theme(plot.title = element_text(hjust=0.5))


# Analysis 1-6: Find the number of houses/apartments/flats available for rent by preferred tenant

# Get the preferred tenant list with frequency
table_tenant = table(data$Preferred_Tenant, dnn = c("Preferred_Tenant"))
table_tenant
# Convert it to data frame
df_tenant = as.data.frame(table_tenant, responseName = "Count")
df_tenant
# Plot the lolipop chart
ggplot(df_tenant, aes(x=Preferred_Tenant, y=Count)) +
  ggtitle("The number of houses/apartments/flats available for rent by preferred tenant") +
  geom_segment( aes(x=Preferred_Tenant, xend=Preferred_Tenant, y=0, yend=Count)) +
  geom_point( size=5, color="black", fill=alpha("purple", 0.3), alpha=0.7, shape=21, stroke=1) +
  geom_text(aes(label=df_tenant[,2]), vjust=-1, color="black", size=3.5) +
  theme_bw() +
  xlab("Preferred Tenant") +
  theme(plot.title = element_text(hjust = 0.5))


df_tenant = df_tenant %>% 
  # Sort by Count ascending
  arrange(Count) %>%
  # Normalize the values
  mutate(fraction = Count / sum(df_tenant$Count)) %>%
  # Compute ending point
  mutate(ymax = cumsum(fraction)) %>%
  # Compute starting point
  mutate(ymin = c(0, head(ymax, n=-1)) ) %>%
  # Compute the percentage of the value
  mutate(percentage = paste0(round(Count / sum(df_tenant$Count) * 100, 2), "%")) %>%
  # Compute the position of labels
  mutate(label_pos = (ymax + ymin) / 2 ) %>%
  # Create new labels
  mutate(labels = paste0(Preferred_Tenant, "\n Percentage: ", percentage))
df_tenant

# Plot the donut chart
ggplot(df_tenant, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Preferred_Tenant)) +
  ggtitle("The number of houses/apartments/flats available for rent by preferred tenant") +
  geom_rect() +
  geom_text( x=1.5, aes(y=label_pos, label=labels, color=Preferred_Tenant), size=4) +
  coord_polar(theta="y", direction=1) + # Make the chart round
  xlim(c(-1, 4)) + # Indicate the thickness of the chart
  theme_void() +
  labs(fill="Preferred Tenant", color="Preferred Tenant") +
  theme(plot.title = element_text(hjust=0.5))


# Analysis 1-7: Find the number of houses/apartments/flats available for rent by point of contact  

# Get the point of contact list with frequency
table_contact = table(data$Point_of_Contact, dnn = c("Point_of_Contact"))
table_contact
# Convert it to data frame
df_contact = as.data.frame(table_contact, responseName = "Count")
df_contact
# Plot the bar chart
ggplot(data=df_contact, aes(x=Point_of_Contact, y=Count)) +
  ggtitle("The number of houses/apartments/flats available for rent by point of contact") +
  geom_bar(stat="identity", color="black", fill="#30B3F3") +
  geom_text(aes(label=df_contact[,2]), vjust=-0.5, color="black", size=3.5) +
  theme_bw() +
  xlab("Point of Contact") +
  theme(plot.title = element_text(hjust = 0.5))

df_contact = df_contact %>% 
  # Sort by Count ascending
  arrange(Count) %>%
  # Normalize the values
  mutate(fraction = Count / sum(df_contact$Count)) %>%
  # Compute ending point
  mutate(ymax = cumsum(fraction)) %>%
  # Compute starting point
  mutate(ymin = c(0, head(ymax, n=-1)) ) %>%
  # Compute the percentage of the value
  mutate(percentage = paste0(round(Count / sum(df_contact$Count) * 100, 2), "%")) %>%
  # Compute the position of labels
  mutate(label_pos = (ymax + ymin) / 2 ) %>%
  # Create new labels
  mutate(labels = paste0(Point_of_Contact, "\n Percentage: ", percentage))
df_contact

# Plot the donut chart
ggplot(df_contact, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Point_of_Contact)) +
  ggtitle("The number of houses/apartments/flats available for rent by point of contact") +
  geom_rect() +
  geom_text( x=1.5, aes(y=label_pos, label=labels, color=Point_of_Contact), size=4) +
  coord_polar(theta="y", direction=1) + # Make the chart round
  xlim(c(-1, 4)) + # Indicate the thickness of the chart
  theme_void() +
  labs(fill="Point of Contact", color="Point of Contact") +
  theme(plot.title = element_text(hjust=0.5))


# Analysis 1-8: Find the number of houses/apartments/flats available for rent by month in 2020

# Get the month list with frequency
df_month = as.data.frame(data %>% 
              mutate(Posted_Date = ymd(Posted_Date)) %>%
              mutate(Month = format(Posted_Date, "%m/%Y")) %>%
              group_by(Month) %>% 
              summarise(Count=n()))
df_month

# Plot the bar chart
ggplot(data=df_month, aes(x=Month, y=Count)) +
  ggtitle("The number of houses/apartments/flats available for rent by month") +
  geom_bar(stat="identity", color="black", fill="#30B3F3") +
  geom_text(aes(label=df_month[,2]), vjust=1.5, color="white", size=3.5) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

df_month = df_month %>% 
  # Sort by Count ascending
  arrange(Count) %>%
  # Normalize the values
  mutate(fraction = Count / sum(df_month$Count)) %>%
  # Compute ending point
  mutate(ymax = cumsum(fraction)) %>%
  # Compute starting point
  mutate(ymin = c(0, head(ymax, n=-1)) ) %>%
  # Compute the percentage of the value
  mutate(percentage = paste0(round(Count / sum(df_month$Count) * 100, 2), "%")) %>%
  # Compute the position of labels
  mutate(label_pos = (ymax + ymin) / 2 ) %>%
  # Create new labels
  mutate(labels = paste0(Month, "\n Percentage: ", percentage))
df_month

# Plot the donut chart
ggplot(df_month, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Month)) +
  ggtitle("The number of houses/apartments/flats available for rent by month") +
  geom_rect() +
  geom_text( x=1.5, aes(y=label_pos, label=labels, color=Month), size=4) +
  coord_polar(theta="y", direction=1) + # Make the chart round
  xlim(c(-1, 4)) + # Indicate the thickness of the chart
  theme_void() +
  theme(plot.title = element_text(hjust=0.5)) +
  scale_fill_manual(values=c("#005096","#0070d2","#0f8fff","#87c7ff")) + # Change chart colour
  scale_colour_manual(values=c("#005096","#0070d2","#0f8fff","#87c7ff")) # Change labels colour


# Analysis 1-9: Find the number of houses/apartments/flats available for rent by building level  

# Get the bedroom list with frequency
table_building = table(ordered(data$Building_Level, c("Low-rise", "Mid-rise", "High-rise")), dnn = c("Building_Level"))
table_building
# Convert it to data frame
df_building = as.data.frame(table_building, responseName = "Count")
df_building
# Plot the bar chart
ggplot(data=df_building, aes(x=Building_Level, y=Count)) +
  ggtitle("The number of houses/apartments/flats available for rent by building level") +
  geom_bar(stat="identity", color="black", fill="#30B3F3", width=0.3) +
  geom_text(aes(label=Count), vjust=1.5, color="white", size=3.5) +
  theme_bw() +
  xlab("Building Level") +
  theme(plot.title = element_text(hjust = 0.5))

df_building = df_building %>% 
  # Sort by Count ascending
  arrange(Count) %>%
  # Normalize the values
  mutate(fraction = Count / sum(df_building$Count)) %>%
  # Compute ending point
  mutate(ymax = cumsum(fraction)) %>%
  # Compute starting point
  mutate(ymin = c(0, head(ymax, n=-1)) ) %>%
  # Compute the percentage of the value
  mutate(percentage = paste0(round(Count / sum(df_building$Count) * 100, 2), "%")) %>%
  # Compute the position of labels
  mutate(label_pos = (ymax + ymin) / 2 ) %>%
  # Create new labels
  mutate(labels = paste0(Building_Level, "\n Percentage: ", percentage))
df_building

# Plot the donut chart
ggplot(df_building, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Building_Level)) +
  ggtitle("The number of houses/apartments/flats available for rent by building level") +
  geom_rect() +
  geom_text( x=1.5, aes(y=label_pos, label=labels, color=Building_Level), size=4) +
  coord_polar(theta="y", direction=1) + # Make the chart round
  xlim(c(-1, 4)) + # Indicate the thickness of the chart
  theme_void() +
  labs(fill="Building Level", color="Building Level") +
  theme(plot.title = element_text(hjust=0.5))


# Analysis 1-10: Find the distribution of houses/apartments/flats available for rent by rental prices
round_any = function(x, digit_places, f=round){
  f(x / digit_places) * digit_places
}

# Calculate optimal bin width
bin_width = round_any((max(data$Rental_Price) - min(data$Rental_Price)) / round(sqrt(nrow(data))), 10000)
bin_width

# Plot the histogram
ggplot(data, aes(x=Rental_Price)) + 
  ggtitle("The distribution of houses/apartments/flats available for rent by rental price") +
  geom_histogram(binwidth=bin_width, color="black", aes(fill=..count..)) + 
  ylab("Count") +
  xlab("Rental Price (Rupee)") +
  scale_fill_gradient("Count", low="green", high="red") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),  axis.text.x=element_text(angle=45,hjust=1,vjust=1)) + 
  scale_x_continuous(breaks = seq(0, max(data$Rental_Price), by=30000))


# Analysis 1-11: Find the distribution of houses/apartments/flats available for rent by size
# Calculate optimal bin width
bin_width = round_any((max(data$Size) - min(data$Size)) / round(sqrt(nrow(data))), 10)
bin_width
# Plot the histogram
ggplot(data, aes(x=Size)) + 
  ggtitle("The distribution of houses/apartments/flats available for rent by size") +
  geom_histogram(binwidth=bin_width, color="black", aes(fill=..count..)) + 
  ylab("Count") +
  xlab("Floor Size (square feet)") +
  scale_fill_gradient("Count", low="green", high="red") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous(breaks = seq(0, max(data$Size), by=500))


# Question 2: What kind of house available in each city?

# Analysis 2-1: Find the distribution of houses/apartments/flats available for rent by furnishing status in each city

city_table = table(data$Furnishing_Status, data$City, dnn = c("Furnishing_Status", "City"))
city_table
df = as.data.frame(city_table, responseName = "Count")
df
# Plot the stacked bar chart
ggplot(data=df, aes(x=City, y=Count, fill=Furnishing_Status)) +
  ggtitle("The distribution of houses/apartments/flats available for rent by furnishing status in each city") +
  geom_bar(stat="identity") +
  geom_text(aes(label=Count), position=position_stack(vjust=0.5), color="white", size=3.5) + 
  theme_bw() +
  labs(fill="Furnishing Status") +
  theme(plot.title = element_text(hjust = 0.5))


# Analysis 2-2: Find the distribution of houses/apartments/flats available for rent by area type in each city
city_table = table(data$Type_of_Area, data$City, dnn = c("Type_of_Area", "City"))
city_table
df = as.data.frame(city_table, responseName = "Count")
df
# Plot the grouped bar chart
ggplot(data=df, aes(x=City, y=Count, fill=Type_of_Area)) +
  ggtitle("The distribution of houses/apartments/flats available for rent by type of area in each city") +
  geom_bar(stat="identity", color="black", size=0.1, position=position_dodge()) +
  geom_text(aes(label=Count), vjust=-0.5, color="black", position=position_dodge(0.9), size=3.5) + 
  theme_bw() +
  labs(fill="Type of Area") +
  theme(plot.title = element_text(hjust = 0.5))


# Analysis 2-3: Find the distribution of houses/apartments/flats available for rent by point of contact in each city
city_table = table(data$Point_of_Contact, data$City, dnn = c("Point_of_Contact", "City"))
city_table
df = as.data.frame(city_table, responseName = "Count")
df
# Plot the grouped bar chart
ggplot(data=df, aes(x=City, y=Count, fill=Point_of_Contact)) +
  ggtitle("The distribution of houses/apartments/flats available for rent by point of contact in each city") +
  geom_bar(stat="identity", color="black", size=0.1, position=position_dodge()) +
  geom_text(aes(label=Count), vjust=-0.5, color="black", position=position_dodge(0.9), size=3.5) + 
  theme_bw() +
  labs(fill="Point of Contact") +
  theme(plot.title = element_text(hjust = 0.5))


# Analysis 2-4: Find the distribution of houses/apartments/flats available for rent by preferred tenant in each city
city_table = table(data$Preferred_Tenant, data$City, dnn = c("Preferred_Tenant", "City"))
city_table
df = as.data.frame(city_table, responseName = "Count")
df
ggplot(data=df, aes(x=City, y=Count, fill=Preferred_Tenant)) +
  ggtitle("The distribution of houses/apartments/flats available for rent by preferred tenant in each city") +
  geom_bar(stat="identity", color="black", size=0.1, position=position_dodge()) +
  geom_text(aes(label=Count), vjust=-0.5, color="black", position=position_dodge(0.9), size=3.5)+ 
  theme_bw() +
  labs(fill="Preferred Tenant") +
  theme(plot.title = element_text(hjust = 0.5))


# Analysis 2-5: Find the distribution of houses/apartments/flats available for rent by building level in each city
city_table = table(factor(data$Building_Level, levels = c("Low-rise", "Mid-rise", "High-rise")), data$City, dnn = c("Building_Level", "City"))
city_table
df = as.data.frame(city_table, responseName = "Count")
df
ggplot(data=df, aes(x=City, y=Count, fill=Building_Level)) +
  ggtitle("The distribution of houses/apartments/flats available for rent by the building level in each city") +
  geom_bar(stat="identity", color="black", size=0.1, position=position_dodge()) +
  geom_text(aes(label=Count), vjust=-0.5, color="black", position=position_dodge(0.9), size=3.5)+ 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(fill="Building Level") +
  scale_fill_manual(values=c("#00BA38","#619CFF","#F8766D")) 


# Analysis 2-6: Find the distribution of houses/apartments/flats available for rent by BHK in each city
city_table = table(data$BHK, data$City, dnn = c("BHK", "City"))
city_table
df = as.data.frame(city_table, responseName = "Count")
ggplot(data=df, aes(x=factor(BHK), y=Count, fill=BHK)) +
  ggtitle("The distribution of houses/apartments/flats available for rent by BHK in each city") +
  geom_bar(stat="identity", color="black", size=0.1, position=position_dodge()) +
  geom_text(aes(label=Count), vjust=-0.5, color="black", position=position_dodge(0.9), size=3.5)+ 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~City, ncol=length(unique(df$City))) +
  xlab("BHK") +
  scale_fill_brewer(palette = "Blues") # Change chart colour


# Analysis 2-7: Find the distribution of houses/apartments/flats available for rent by number of bathroom in each city
city_table = table(data$Number_of_Bathroom, data$City, dnn = c("Number_of_Bathroom", "City"))
city_table
df = as.data.frame(city_table, responseName = "Count")
ggplot(data=df, aes(x=factor(Number_of_Bathroom), y=Count, fill=Number_of_Bathroom)) +
  ggtitle("The distribution of houses/apartments/flats available for rent by the number of bathroom in each city") +
  geom_bar(stat="identity", color="black", size=0.1, position=position_dodge()) +
  geom_text(aes(label=Count), vjust=-0.5, color="black", position=position_dodge(0.9), size=3.5)+ 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~City, ncol=length(unique(df$City))) +
  xlab("Number of Bathroom") +
  labs(fill="Number of Bathroom") +
  scale_fill_brewer(palette = "Blues") # Change chart colour


# Analysis 2-8: Find the average rental price in each city
# Calculate mean by city
df = aggregate(list(Mean=data$Rental_Price), list(City=data$City), FUN=mean)
df
# Plot the bar chart
ggplot(data=df, aes(x=City, y=Mean, fill=City)) +
  ggtitle("The average rental price in each city") +
  geom_bar(stat="identity", color="black", size=0.1) +
  geom_text(aes(label=round(Mean,2)), vjust=-0.5, color="black", size=3.5)+ 
  theme_bw() +
  ylab("Average Rent Price (Rupee)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette = "Set2") 


# Analysis 2-9: Find the average size in each city
# Calculate mean by city
df = aggregate(list(Mean=data$Size), list(City=data$City), FUN=mean)
df
# Plot the bar chart
ggplot(data=df, aes(x=City, y=Mean, fill=City)) +
  ggtitle("The average size in each city") +
  geom_bar(stat="identity", color="black", size=0.1) +
  geom_text(aes(label=round(Mean,2)), vjust=-0.5, color="black", size=3.5)+ 
  theme_bw() +
  ylab("Average Size (sqft.)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette = "Set2")

# Analysis 2-10: Find the distribution of houses/apartments/flats available for rent by month in each city
# Get the month list with frequency in each city
df_month = as.data.frame(data %>% 
                           mutate(Posted_Date = ymd(Posted_Date)) %>% # you may not need this line
                           mutate(Month = format(Posted_Date, "%m/%Y")) %>%
                           group_by(City, Month) %>% 
                           summarise(Count=n()))
df_month
# Plot the bar graph
ggplot(data=df_month, aes(x=factor(Month), y=Count, fill=Month)) +
  ggtitle("The distribution of houses/apartments/flats available for rent by month in each city") +
  geom_bar(stat="identity", color="black", size=0.1, position=position_dodge()) +
  geom_text(aes(label=Count), vjust=-0.5, color="black", position=position_dodge(0.9), size=3.5)+ 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x=element_text(angle=45,hjust=1,vjust=1)) +
  facet_wrap(~City, ncol=length(unique(df_month$City))) +
  xlab("Month") +
  scale_fill_brewer(palette = "Blues") 

# Question 3: What type of houses are preferred by tenants?

# Analysis 3-1: Find the distribution of houses/apartments/flats preferred by tenants by furnishing status
tenant_table = table(data$Furnishing_Status, data$Preferred_Tenant, dnn = c("Furnishing_Status", "Tenants"))
tenant_table
df = as.data.frame(tenant_table, responseName = "Count")
df
# Add Bachelors/Family's count to both bachelors and family
df$Count = df$Count + df$Count[df$Tenants == "Bachelors/Family"]
# Remove Bachelors/Family's row
df = df[df$Tenants != "Bachelors/Family",]
df

# Plot the grouped bar chart
ggplot(data=df, aes(x=Furnishing_Status, y=Count, fill=Tenants)) +
  ggtitle("The distribution of houses/apartments/flats preferred by tenants by furnishing status") +
  geom_bar(stat="identity", color="black", size=0.1, position=position_dodge()) +
  geom_text(aes(label=Count), vjust=1.5, color="white", position=position_dodge(0.9), size=3.5) +
  xlab("Furnishing Status") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


# Analysis 3-2: Find the distribution of houses/apartments/flats preferred by tenants by type of area
tenant_table = table(data$Type_of_Area, data$Preferred_Tenant, dnn = c("Type_of_Area", "Tenants"))
tenant_table
df = as.data.frame(tenant_table, responseName = "Count")
df
# Add Bachelors/Family's count to both bachelors and family
df$Count = df$Count + df$Count[df$Tenants == "Bachelors/Family"]
# Remove Bachelors/Family's row
df = df[df$Tenants != "Bachelors/Family",]
df
# Plot the grouped bar chart
ggplot(data=df, aes(x=Type_of_Area, y=Count, fill=Tenants)) +
  ggtitle("The distribution of houses/apartments/flats preferred by tenants by type of area") +
  geom_bar(stat="identity", color="black", size=0.1, position=position_dodge()) +
  geom_text(aes(label=Count), vjust=-0.5, color="black", position=position_dodge(0.9), size=3.5) + 
  xlab("Type of Area") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


# Analysis 3-3: Find the distribution of houses/apartments/flats preferred by tenants by point of contact
tenant_table = table(data$Point_of_Contact, data$Preferred_Tenant, dnn = c("Point_of_Contact", "Tenants"))
tenant_table
df = as.data.frame(tenant_table, responseName = "Count")
df
# Add Bachelors/Family's count to both bachelors and family
df$Count = df$Count + df$Count[df$Tenants == "Bachelors/Family"]
# Remove Bachelors/Family's row
df = df[df$Tenants != "Bachelors/Family",]
df
# Plot the grouped bar chart
ggplot(data=df, aes(x=Point_of_Contact, y=Count, fill=Tenants)) +
  ggtitle("The distribution of houses/apartments/flats preferred by tenants by point of contact") +
  geom_bar(stat="identity", color="black", size=0.1, position=position_dodge()) +
  geom_text(aes(label=Count), vjust=-0.5, color="black", position=position_dodge(0.9), size=3.5) + 
  xlab("Point of Contact") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


# Analysis 3-4: Find the distribution of houses/apartments/flats preferred by tenants by city 
tenant_table = table(data$City, data$Preferred_Tenant, dnn = c("City", "Tenants"))
tenant_table
df = as.data.frame(tenant_table, responseName = "Count")
df
# Add Bachelors/Family's count to both bachelors and family
df$Count = df$Count + df$Count[df$Tenants == "Bachelors/Family"]
# Remove Bachelors/Family's row
df = df[df$Tenants != "Bachelors/Family",]
df
# Plot the grouped bar chart
ggplot(data=df, aes(x=City, y=Count, fill=Tenants)) +
  ggtitle("The distribution of houses/apartments/flats preferred by tenants by city") +
  geom_bar(stat="identity", color="black", size=0.1, position=position_dodge()) +
  geom_text(aes(label=Count), vjust=-0.5, color="black", position=position_dodge(0.9), size=3.5) + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


# Analysis 3-5: Find the distribution of houses/apartments/flats preferred by tenants by building level
tenant_table = table(factor(data$Building_Level, levels = c("Low-rise", "Mid-rise", "High-rise")), data$Preferred_Tenant, dnn = c("Building_Level", "Tenants"))
tenant_table
df = as.data.frame(tenant_table, responseName = "Count")
df
# Add Bachelors/Family's count to both bachelors and family
df$Count = df$Count + df$Count[df$Tenants == "Bachelors/Family"]
# Remove Bachelors/Family's row
df = df[df$Tenants != "Bachelors/Family",]
df
# Plot the grouped bar chart
ggplot(data=df, aes(x=Building_Level, y=Count, fill=Tenants)) +
  ggtitle("The distribution of houses/apartments/flats preferred by tenants by building level") +
  geom_bar(stat="identity", color="black", size=0.1, position=position_dodge()) +
  geom_text(aes(label=Count), vjust=-0.5, color="black", position=position_dodge(0.9), size=3.5)+ 
  theme_bw() +
  xlab("Building Level") +
  theme(plot.title = element_text(hjust = 0.5)) 


# Analysis 3-6: Find the distribution of houses/apartments/flats preferred by tenants by BHK
tenant_table = table(data$BHK, data$Preferred_Tenant, dnn = c("BHK", "Tenants"))
tenant_table
df = as.data.frame(tenant_table, responseName = "Count")
df
# Add Bachelors/Family's count to both bachelors and family
df$Count = df$Count + df$Count[df$Tenants == "Bachelors/Family"]
# Remove Bachelors/Family's row
df = df[df$Tenants != "Bachelors/Family",]
df
# Plot the grouped bar chart
ggplot(data=df, aes(x=factor(BHK), y=Count, fill=BHK)) +
  ggtitle("The distribution of houses/apartments/flats preferred by tenants by BHK") +
  geom_bar(stat="identity", color="black", size=0.1, position=position_dodge()) +
  geom_text(aes(label=Count), vjust=-0.5, color="black", position=position_dodge(0.9), size=3.5)+ 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~Tenants, ncol=length(unique(df$Tenants))) +
  xlab("BHK") +
  scale_fill_brewer(palette = "Blues") # Change chart colour


# Analysis 3-7: Find the distribution of houses/apartments/flats preferred by tenants by number of bathroom
tenant_table = table(data$Number_of_Bathroom, data$Preferred_Tenant, dnn = c("Number_of_Bathroom", "Tenants"))
tenant_table
df = as.data.frame(tenant_table, responseName = "Count")
df
# Add Bachelors/Family's count to both bachelors and family
df$Count = df$Count + df$Count[df$Tenants == "Bachelors/Family"]
# Remove Bachelors/Family's row
df = df[df$Tenants != "Bachelors/Family",]
df
# Plot the grouped bar chart
ggplot(data=df, aes(x=factor(Number_of_Bathroom), y=Count, fill=Number_of_Bathroom)) +
  ggtitle("The distribution of houses/apartments/flats preferred by tenants by number of bathroom") +
  geom_bar(stat="identity", color="black", size=0.1, position=position_dodge()) +
  geom_text(aes(label=Count), vjust=-0.5, color="black", position=position_dodge(0.9), size=3.5)+ 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~Tenants, ncol=length(unique(df$Tenants))) +
  xlab("Number of Bathroom") +
  labs(fill="Number of Bathroom") +
  scale_fill_brewer(palette = "Blues") # Change chart colour


# Analysis 3-8: Find the average rental price preferred by tenants
# Calculate mean by tenants
bachelors_mean = mean(data$Rental_Price[grepl( "Bachelors", data$Preferred_Tenant, fixed = TRUE)])
family_mean = mean(data$Rental_Price[grepl( "Family", data$Preferred_Tenant, fixed = TRUE)])
df = data.frame(Tenants  = c("Bachelors", "Family"), Mean = c(bachelors_mean, family_mean))
df

# Plot the bar chart
ggplot(data=df, aes(x=Tenants, y=Mean, fill=Tenants)) +
  ggtitle("The average rental price preferred by tenants") +
  geom_bar(stat="identity", color="black", size=0.1) +
  geom_text(aes(label=round(Mean,2)), vjust=-0.5, color="black", size=3.5)+ 
  theme_bw() +
  ylab("Average Rent Price (Rupee)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette = "Set2")


# Analysis 3-9: Find the average size preferred by tenants
# Calculate mean by tenants
bachelors_mean = mean(data$Size[grepl( "Bachelors", data$Preferred_Tenant, fixed = TRUE)])
family_mean = mean(data$Size[grepl( "Family", data$Preferred_Tenant, fixed = TRUE)])
df = data.frame (Tenants  = c("Bachelors", "Family"), Mean = c(bachelors_mean, family_mean))
df

# Plot the bar chart
ggplot(data=df, aes(x=Tenants, y=Mean, fill=Tenants)) +
  ggtitle("The average size preferred by tenants") +
  geom_bar(stat="identity", color="black", size=0.1) +
  geom_text(aes(label=round(Mean,2)), vjust=-0.5, color="black", size=3.5)+ 
  theme_bw() +
  ylab("Average Size (sq.)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette = "Set2")


# Analysis 3-10: Find the distribution of houses/apartments/flats preferred by tenants by month
# Get the month list with frequency by tenants
df_month = as.data.frame(data %>% 
                           # mutate(Posted_Date = mdy(Posted_Date)) %>% # you may not need this line
                           mutate(Month = format(Posted_Date, "%m/%Y")) %>%
                           group_by(Preferred_Tenant, Month) %>% 
                           summarise(Count=n()))
df_month
# Add Bachelors/Family's count to both bachelors and family
df_month$Count = df_month$Count + df_month$Count[df_month$Preferred_Tenant == "Bachelors/Family"]
# Remove Bachelors/Family's row
df_month = df_month[df_month$Preferred_Tenant != "Bachelors/Family",]
df_month

# Plot the bar graph
ggplot(data=df_month, aes(x=Month, y=Count, fill=Preferred_Tenant)) +
  ggtitle("The distribution of houses/apartments/flats preferred by tenants by month") +
  geom_bar(stat="identity", color="black", size=0.1, position=position_dodge()) +
  geom_text(aes(label=Count), vjust=-0.5, color="black", position=position_dodge(0.9), size=3.5)+ 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Month") +
  scale_fill_brewer(palette = "Blues") 


# Question 4: What influences the range of houses’ rental price in the dataset?

# Analysis 4-1: Find the range of rental price by furnishing status
# Plot boxplot
ggplot(data, aes(x=Furnishing_Status, y=Rental_Price, fill=Furnishing_Status)) + 
  ggtitle("The range of rental price by furnishing status") +
  geom_boxplot(alpha=0.4) + 
  coord_flip() +
  xlab("Furnishing Status") +
  ylab("Rental Price (Rupee)") +
  labs(fill="Furnishing Status") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x=element_text(angle=45,hjust=1,vjust=1)) +
  scale_y_continuous(breaks = seq(0, max(data$Rental_Price), by=30000)) +
  scale_color_brewer(palette="Set2")


# Analysis 4-2: Find the range of rental price by building level
# Plot boxplot
ggplot(data, aes(x=ordered(Building_Level, c("Low-rise", "Mid-rise", "High-rise")), y=Rental_Price, fill=Building_Level)) + 
  ggtitle("The range of rental price by building level") +
  geom_boxplot(alpha=0.4) + 
  coord_flip() +
  xlab("Building Level") +
  ylab("Rental Price (Rupee)") +
  labs(fill="Building Level") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x=element_text(angle=45,hjust=1,vjust=1)) +
  scale_y_continuous(breaks = seq(0, max(data$Rental_Price), by=30000)) +
  scale_color_brewer(palette="Set2")


# Analysis 4-3: Find the range of rental price by type of area
# Plot boxplot
ggplot(data, aes(x=Type_of_Area, y=Rental_Price, fill=Type_of_Area)) + 
  ggtitle("The range of rental price by tht type of area") +
  geom_boxplot(alpha=0.4) + 
  coord_flip() +
  xlab("Type of Area") +
  ylab("Rental Price (Rupee)") +
  labs(fill="Type of Area") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x=element_text(angle=45,hjust=1,vjust=1)) +
  scale_y_continuous(breaks = seq(0, max(data$Rental_Price), by=30000)) +
  scale_color_brewer(palette="Set2")


# Analysis 4-4: Find the range of rental price by BHK
# Plot boxplot
ggplot(data, aes(x=as.factor(BHK), y=Rental_Price, fill=as.factor(BHK))) + 
  ggtitle("The distribution of rental price by BHK") +
  geom_boxplot(alpha=0.4) + 
  coord_flip() +
  xlab("BHK") +
  ylab("Rental Price (Rupee)") +
  labs(fill="BHK") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x=element_text(angle=45,hjust=1,vjust=1)) +
  scale_y_continuous(breaks = seq(0, max(data$Rental_Price), by=30000)) +
  scale_color_brewer(palette="Set2")


# Analysis 4-5: Find the range of rental price by number of bathrooms
# Plot boxplot
ggplot(data, aes(x=as.factor(Number_of_Bathroom), y=Rental_Price, fill=as.factor(Number_of_Bathroom))) +
  ggtitle("The range of rental price by number of bathrooms") +
  geom_boxplot(alpha=0.4) + 
  coord_flip() +
  xlab("Number of Bathroom") +
  ylab("Rental Price (Rupee)") +
  labs(fill="Number of Bathroom") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x=element_text(angle=45,hjust=1,vjust=1)) +
  scale_y_continuous(breaks = seq(0, max(data$Rental_Price), by=30000)) +
  scale_color_brewer(palette="Set2")


# Analysis 4-6: Find the range of rental price by city
# Plot boxplot
ggplot(data, aes(x=City, y=Rental_Price, fill=City)) +
  ggtitle("The range of rental price by city") +
  geom_boxplot(alpha=0.4) + 
  coord_flip() +
  xlab("City") +
  ylab("Rental Price (Rupee)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x=element_text(angle=45,hjust=1,vjust=1)) +
  scale_y_continuous(breaks = seq(0, max(data$Rental_Price), by=30000)) +
  scale_color_brewer(palette="Set2")


# Analysis 4-7: Find the range of rental price by point of contact
# Plot boxplot
ggplot(data, aes(x=Point_of_Contact, y=Rental_Price, fill=Point_of_Contact)) +
  ggtitle("The range of rental price by point_of_contact") +
  geom_boxplot(alpha=0.4) + 
  coord_flip() +
  xlab("Point of Contact") +
  ylab("Rental Price (Rupee)") +
  labs(fill="Point of Contact") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x=element_text(angle=45,hjust=1,vjust=1)) +
  scale_y_continuous(breaks = seq(0, max(data$Rental_Price), by=30000)) +
  scale_color_brewer(palette="Set2")


# Analysis 4-8: Find the range of rental price by month
# Plot boxplot
df_month = data %>% 
  mutate(Month = format(Posted_Date, "%m/%Y"))
# Plot boxplot
ggplot(df_month, aes(x=Month, y=Rental_Price, fill=Month)) +
  ggtitle("The range of rental price by month") +
  geom_boxplot(alpha=0.4) + 
  coord_flip() +
  xlab("Month") +
  ylab("Rental Price (Rupee)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x=element_text(angle=45,hjust=1,vjust=1)) +
  scale_y_continuous(breaks = seq(0, max(data$Rental_Price), by=30000)) +
  scale_color_brewer(palette="Set2")


# Analysis 4-9: Find the range of rental price by preferred tenant
# Plot boxplot
ggplot(data, aes(x=Preferred_Tenant, y=Rental_Price, fill=Preferred_Tenant)) +
  ggtitle("The range of rental price by preferred tenant") +
  geom_boxplot(alpha=0.4) + 
  coord_flip() +
  xlab("Preferred Tenant") +
  ylab("Rental Price (Rupee)") +
  labs(fill="Preferred Tenant") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x=element_text(angle=45,hjust=1,vjust=1)) +
  scale_y_continuous(breaks = seq(0, max(data$Rental_Price), by=30000)) +
  scale_color_brewer(palette="Set2")



# Question 5: What are the factors that affecting the rental price of houses

# Analysis 5-1: Find the distribution of rental price by furnishing status
# Plot violin chart
ggplot(data, aes(x=Furnishing_Status, y=Rental_Price, fill=Furnishing_Status)) + 
  ggtitle("The distribution of rental price by furnishing status") +
  geom_violin(adjust=2) +
  coord_flip() +
  xlab("Furnishing Status") +
  ylab("Rental Price (Rupee)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x=element_text(angle=45,hjust=1,vjust=1)) +
  scale_y_continuous(breaks = seq(0, max(data$Rental_Price), by=30000))


# Analysis 5-2: Find the distribution of rental price by building level
# Plot density chart
ggplot(data=data, aes(x=Rental_Price, group=Building_Level, fill=Building_Level)) +
  ggtitle("The distribution of rental price by building level") +
  geom_density(adjust=10, alpha=.4) +
  ylab("Density") +
  xlab("Rental Price (Rupee)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x=element_text(angle=45,hjust=1,vjust=1)) +
  scale_x_continuous(breaks = seq(0, max(data$Rental_Price), by=30000))


# Analysis 5-3: Find the relationship between size and rental price
ggplot(data, aes(x=Size, y=Rental_Price)) +
  ggtitle("The relationship between size and rental price") +
  geom_point(aes(color=Rental_Price)) +
  geom_smooth(method=lm , color="red", fill="grey", se=TRUE, formula=y~x-1) +
  xlab("Size (sq. ft)") +
  ylab("Rental Price (Rupee)") +
  theme_bw() +
  labs(color="Rental Price (Rupee)") +
  theme(plot.title = element_text(hjust = 0.5))


# Analysis 5-4: Find the relationship between size and rental price by city
ggplot(data, aes(x=Size, y=Rental_Price)) +
  ggtitle("The relationship between size and rental price by city") +
  geom_point(aes(color=City)) +
  geom_smooth(method=lm , color="black", fill="grey", se=TRUE, formula=y~x-1) +
  xlab("Size (sq. ft)") +
  ylab("Rental Price (Rupee)") +
  facet_wrap(~City)+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


# Analysis 5-5: Find the average rental price by furnishing status 
# Calculate mean by furnishing status
df = aggregate(list(Mean=data$Rental_Price), list(Furnishing_Status=data$Furnishing_Status), FUN=mean)
df
# Plot the bar chart
ggplot(data=df, aes(x=Furnishing_Status, y=Mean, fill=Furnishing_Status)) +
  ggtitle("The average rental price by furnishing status") +
  geom_bar(stat="identity", color="black", size=0.1) +
  geom_text(aes(label=round(Mean,2)), vjust=-0.5, color="black", size=3.5)+ 
  theme_bw() +
  ylab("Average Rental Price (Rupee)") +
  labs(fill="Furnishing Status", x="Furnishing Status") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette = "Set2") 


# Analysis 5-6: Find the average rental price by area type  
# Calculate mean by area type
df = aggregate(list(Mean=data$Rental_Price), list(Type_of_Area=data$Type_of_Area), FUN=mean)
df
# Plot the bar chart
ggplot(data=df, aes(x=Type_of_Area, y=Mean, fill=Type_of_Area)) +
  ggtitle("The average rental price by area type") +
  geom_bar(stat="identity", color="black", size=0.1) +
  geom_text(aes(label=round(Mean,2)), vjust=-0.5, color="black", size=3.5)+ 
  theme_bw() +
  ylab("Average Rental Price (Rupee)") +
  xlab("Type of Area") +
  labs(fill="Type of Area") + # Change legend title
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette = "Set2")


# Analysis 5-7: Find the average rental price by point of contact  
# Calculate mean by point of contact 
df = aggregate(list(Mean=data$Rental_Price), list(Point_of_Contact=data$Point_of_Contact), FUN=mean)
df
# Plot the bar chart
ggplot(data=df, aes(x=Point_of_Contact, y=Mean, fill=Point_of_Contact)) +
  ggtitle("The average rental price by point of contact") +
  geom_bar(stat="identity", color="black", size=0.1) +
  geom_text(aes(label=round(Mean,2)), vjust=-0.5, color="black", size=3.5)+ 
  theme_bw() +
  ylab("Average Rental Price (Rupee)") +
  xlab("Point of Contact") +
  labs(fill="Point of Contact") + # Change legend title
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette = "Set2")


# Analysis 5-8: Find the average rental price by preferred tenant  
# Calculate mean by preferred tenant
df = aggregate(list(Mean=data$Rental_Price), list(Preferred_Tenant=data$Preferred_Tenant), FUN=mean)
df
# Plot the bar chart
ggplot(data=df, aes(x=Preferred_Tenant, y=Mean, fill=Preferred_Tenant)) +
  ggtitle("The average rental price by preferred tenant") +
  geom_bar(stat="identity", color="black", size=0.1) +
  geom_text(aes(label=round(Mean,2)), vjust=-0.5, color="black", size=3.5)+ 
  theme_bw() +
  ylab("Average Rental Price (Rupee)") +
  xlab("Preferred Tenant") +
  labs(fill="Preferred Tenant") + # Change legend title
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette = "Set2") 


# Analysis 5-9: Find the average rental price by building level 
# Calculate mean by building level 
df = aggregate(list(Mean=data$Rental_Price), list(Building_Level=data$Building_Level), FUN=mean)
df
# Plot the bar chart
ggplot(data=df, aes(x=Building_Level, y=Mean, fill=Building_Level)) +
  ggtitle("The average rental price by building level") +
  geom_bar(stat="identity", color="black", size=0.1) +
  geom_text(aes(label=round(Mean,2)), vjust=-0.5, color="black", size=3.5)+ 
  theme_bw() +
  ylab("Average Rental Price (Rupee)") +
  xlab("Building Level") +
  labs(fill="Building Level") + # Change legend title
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette = "Set2")


# Analysis 5-10: Find the average rental price by BHK 
# Calculate mean by BHK 
df = aggregate(list(Mean=data$Rental_Price), list(BHK=data$BHK), FUN=mean)
df
# Plot the bar chart
ggplot(data=df, aes(x=BHK, y=Mean, fill=as.factor(BHK))) +
  ggtitle("The average rental price by BHK") +
  geom_bar(stat="identity", color="black", size=0.1) +
  geom_text(aes(label=round(Mean,2)), vjust=-0.5, color="black", size=3.5)+ 
  theme_bw() +
  ylab("Average Rental Price (Rupee)") +
  xlab("BHK") +
  labs(fill="BHK") + # Change legend title
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette = "Blues")


# Analysis 5-11: Find the average rental price by number of bathrooms 
# Calculate mean by number of bathroom  
df = aggregate(list(Mean=data$Rental_Price), list(Number_of_Bathroom=data$Number_of_Bathroom), FUN=mean)
df
# Plot the bar chart
ggplot(data=df, aes(x=Number_of_Bathroom, y=Mean, fill=as.factor(Number_of_Bathroom))) +
  ggtitle("The average rental price by number of bathrooms") +
  geom_bar(stat="identity", color="black", size=0.1) +
  geom_text(aes(label=round(Mean,2)), vjust=-0.5, color="black", size=3.5)+ 
  theme_bw() +
  ylab("Average Rental Price (Rupee)") +
  xlab("Number of Bathroom") +
  labs(fill="Number of Bathroom") + # Change legend title
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette = "Blues")


# Analysis 5-12: Find the average rental price by month 
# Calculate mean by month
df_month = as.data.frame(data %>% mutate(Month = format(Posted_Date, "%m/%Y")))
df = aggregate(list(Mean=df_month$Rental_Price), list(Month=df_month$Month), FUN=mean)
df
# Plot the bar chart
ggplot(data=df, aes(x=Month, y=Mean, group=1)) +
  ggtitle("The average rental price by month") +
  geom_line(aes(color="red")) +
  geom_point(aes(color="red")) +
  geom_text(aes(label=round(Mean,2)), hjust=1, vjust=-1, color="black", size=3.5)+ 
  theme_bw() +
  ylab("Average Rent Price (Rupee)") +
  theme(plot.title = element_text(hjust = 0.5))


# Question 6: What are the aspects that affecting the trend of the average rental price per square feet over time?
df = data %>%
  # Calculate average rental price per square feet
  mutate(PPSF = Rental_Price / Size, Month = format(Posted_Date, "%m/%Y"))


# Analysis 6-1: Find the average rental price per square feet over time
# Calculate average rental price per square feet by time
df_mean = aggregate(list(Mean=df$PPSF), list(Month=df$Month), FUN=mean)
df_mean
# Plot the line point graph
ggplot(data=df_mean, aes(x=Month, y=Mean, group=1)) +
  ggtitle("The average rental price per square feet over time") +
  geom_line(aes(color="red")) +
  geom_point(aes(color="red")) +
  geom_text(aes(label=round(Mean,2)), color="black", hjust=1, vjust=-0.9, size=3) +
  xlab("Month") +
  ylab("Average rental price per square feet(ppsf)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


# Analysis 6-2: Find the relationship between average rental price per square feet and BHK over time
# Calculate average rental price per square feet grouped by BHK and time
df_mean = aggregate(list(Mean=df$PPSF), list(BHK=df$BHK, Month=df$Month), FUN=mean)
df_mean
# Plot the line point graph
ggplot(data=df_mean, aes(x=Month, y=Mean, group=BHK)) +
  ggtitle("The average rental price per square feet by BHK over time") +
  geom_line(aes(color=as.factor(BHK))) +
  geom_point(aes(color=as.factor(BHK))) +
  geom_text(aes(label=round(Mean,2)), color="black", hjust=0, vjust=-0.9, size=2) +
  xlab("Month") +
  ylab("Average rental price per square feet(ppsf)") +
  facet_wrap(~BHK) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(color="BHK") # Change legend title


# Analysis 6-3: Find the relationship between average rental price per square feet and city over time
# Calculate average rental price per square feet grouped by city and time
df_mean = aggregate(list(Mean=df$PPSF), list(City=df$City, Month=df$Month), FUN=mean)
df_mean
# Plot the line point graph
ggplot(data=df_mean, aes(x=Month, y=Mean, group=City)) +
  ggtitle("The average rental price per square feet by city over time") +
  geom_line(aes(color=City)) +
  geom_point(aes(color=City)) +
  geom_text(aes(label=round(Mean,2)), color="black", hjust=1, vjust=-0.9, size=2) +
  xlab("Month") +
  ylab("Average rental price per square feet(ppsf)") +
  facet_wrap(~City) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


# Analysis 6-4: Find the relationship between average rental price per square feet and furnishing status over time
# Calculate average rental price per square feet grouped by furnishing status and time
df_mean = aggregate(list(Mean=df$PPSF), list(Furnishing_Status=df$Furnishing_Status, Month=df$Month), FUN=mean)
df_mean
# Plot the line point graph
ggplot(data=df_mean, aes(x=Month, y=Mean, group=Furnishing_Status)) +
  ggtitle("The average rental price per square feet by furnishing status over time") +
  geom_line(aes(color=Furnishing_Status)) +
  geom_point(aes(color=Furnishing_Status)) +
  geom_text(aes(label=round(Mean,2)), color="black", hjust=0.5, vjust=-1, size=3) +
  xlab("Month") +
  ylab("Average rental price per square feet(ppsf)") +
  facet_wrap(~Furnishing_Status) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(color="Furnishing Status") # Change legend title


# Analysis 6-5: Find the relationship between average rental price per square feet and type of area over time
# Calculate average rental price per square feet grouped by type of area and time
df_mean = aggregate(list(Mean=df$PPSF), list(Type_of_Area=df$Type_of_Area, Month=df$Month), FUN=mean)
df_mean
# Remove built area row
df_mean = df_mean[df_mean$Type_of_Area != "Built Area", ]
df_mean
# Plot the line point graph
ggplot(data=df_mean, aes(x=Month, y=Mean, group=Type_of_Area)) +
  ggtitle("The average rental price per square feet by type of area over time") +
  geom_line(aes(color=Type_of_Area)) +
  geom_point(aes(color=Type_of_Area)) +
  geom_text(aes(label=round(Mean,2)), color="black", hjust=0.5, vjust=-1, size=3) +
  xlab("Month") +
  ylab("Average rental price per square feet(ppsf)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(color="Type of Area") # Change legend title


# Analysis 6-6: Find the relationship between average rental price per square feet and building level over time
# Calculate average rental price per square feet grouped by preferred tenants and time
df_mean = aggregate(list(Mean=df$PPSF), list(Building_Level=df$Building_Level, Month=df$Month), FUN=mean)
df_mean
# Plot the line point graph
ggplot(data=df_mean, aes(x=Month, y=Mean, group=Building_Level)) +
  ggtitle("The average rental price per square feet by building level over time") +
  geom_line(aes(color=Building_Level)) +
  geom_point(aes(color=Building_Level)) +
  geom_text(aes(label=round(Mean,2)), color="black", hjust=0.5, vjust=-1.5, size=3) +
  xlab("Month") +
  ylab("Average rental price per square feet(ppsf)") +
  facet_wrap(~Building_Level) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(color="Building Level") # Change legend title

# Analysis 6-7: Find the relationship between average rental price per square feet and preferred tenants over time
# Calculate average rental price per square feet grouped by preferred tenants and time
df_mean = aggregate(list(Mean=df$PPSF), list(Preferred_Tenant=df$Preferred_Tenant, Month=df$Month), FUN=mean)
df_mean
# Plot the line point graph
ggplot(data=df_mean, aes(x=Month, y=Mean, group=Preferred_Tenant)) +
  ggtitle("The average rental price per square feet by preferred tenant over time") +
  geom_line(aes(color=Preferred_Tenant)) +
  geom_point(aes(color=Preferred_Tenant)) +
  geom_text(aes(label=round(Mean,2)), color="black", hjust=0.5, vjust=-1, size=3) +
  xlab("Month") +
  ylab("Average rental price per square feet(ppsf)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(color="Preferred Tenant") # Change legend title
