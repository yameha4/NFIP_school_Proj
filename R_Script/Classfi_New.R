# required libraries
library(stringr)
library(ggplot2)
library(plyr)
library(ROCR)

# required functions
source("analyze_policies.R") # function to preprocess policies data
source("analyze_claims.R") # function to preprocess claims data

# policies data
policies <- Fn_Analyze_Policies()
# claims data
claims <- Fn_Analyze_Claims()

# combine county data on policies and claims
all_data <- merge(policies, claims, by = c("state", "county"), all = TRUE)
# convert state and county names to be consistent with ggplot2
all_data$state <- tolower(all_data$state)
all_data$county <- tolower(all_data$county)
# remove " county" and " parish" from county names
all_data$county <- gsub(" county", "", all_data$county)
all_data$county <- gsub(" parish", "", all_data$county)

# geo referencing info on counties and states
geo_county <- map_data("county")
names(geo_county) <- c("long", "lat", "group", "order", "state", "county")
geo_state <- map_data("state")


# data for graphics
gfx_data <- merge(geo_county, all_data, by = c("state", "county"))
gfx_data <- gfx_data[order(gfx_data$order), ]
# discretise variables of interest
gfx_data$policies_gfx <- cut(gfx_data$policies,
                             breaks = c(1, 10000, 400000),
                             labels = c("1 - 10k", "10k - 400k"))
gfx_data$payments_gfx <- cut(gfx_data$total_pay/10^6,
                             breaks = c(0, 0.01, 7300),
                             labels = c("0 - 10k", "10k - 7.3B"))
------------------------

  # barplot of job type by income group
  # get the counts by industry and income group
count <- table(gfx_data[gfx_data$state == 'state',]$policies_gfx)["<=10K"]
count <- table(gfx_data[gfx_data$state == 'state',]$policies_gfx)[">=10K"]

---------------------------
  #Export file and mnipulated in excel
gfx_data
write.table(gfx_data, "gfx_data.csv")


 #Import the new excel file
setwd("C:/Users/AEshete/Documents/NFIP")
dir()
library(readxl)
NFIP_Data <- read_excel("~/NFIP/Final_NFIP_Data.xlsx")
View(NFIP_Data)
str(NFIP_Data)

#Data fame converstion
tmp<-NFIP_Data
----as.numeric(Policies_gfx_Class.factor)
tmp$Policies_gfx_Class <- as.factor(tmp$Policies_gfx_Class)
str(tmp)
tmp$Payment_gfx_Class <- as.factor(tmp$Payment_gfx_Class)
str(tmp)

#how many Plices and Pyaments are high and low
tmp <- table(policies_gfx_Class)
(tmp[[3]]/tmp[[1]])*100





# histogram of Premium and calim group
ggplot(gfx_data) + aes(x=as.numeric(policies_gfx), group=geo_state, fill=geo_state) +
  geom_histogram(binwidth=1, color='black')

-------------------------

# plot policies
plot_map <- ggplot(data = gfx_data) +
  geom_polygon(aes(long, lat, group = group, fill = policies_gfx)) +
  geom_path(data = geo_state,
            aes(x = long, y = lat, group = group),
            fill = NA,
            na.rm = TRUE) +
  labs(list(title = "NFIP Policies Per County", x = NULL, y = NULL)) +
  guides(fill = guide_legend(title = "Policies Per County")) +
  scale_fill_brewer(palette = "Accent")
print(plot_map)

# plot payments
plot_map <- ggplot(data = gfx_data) +
  geom_polygon(aes(long, lat, group = group, fill = payments_gfx)) +
  geom_path(data = geo_state,
            aes(x = long, y = lat, group = group),
            fill = NA,
            na.rm = TRUE) +
  labs(list(title = "NFIP Payments Per County (USD)", x = NULL, y = NULL)) +
  guides(fill = guide_legend(title = "Payments Per County (USD)")) +
  scale_fill_brewer(palette = "Accent")
print(plot_map)









New_all_data<-all_data
colnames(New_all_data)<-c( 'state', 'county', 'policies',
                           'insurance', 'premium', 'total_loss',
                           'closed_loss', 'open_loss', 'cwop_loss', 'total_pay')

ggplot(New_all_data) + aes(x=as.numeric(premium), group=, fill=income) +
  geom_histogram(binwidth=1, color='black')






