setwd("C:/Users/AEshete/Documents/NFIP")
dir()
library(readxl)
FEMA_1 <- read_excel("~/NFIP/FEMA_1.xlsx")
View(FEMA_1)
str(FEMA_1)
FEMA_1 <- read_excel("~/NFIP/FEMA_1.xlsx")
write.table(Fn_Analyze_Claims, "C:/Users/AEshete/Documents/NFIP/formatted_claims_county.txt", sep="\t")
write.table(Fn_Analyze_Claims, file, append = FALSE, sep = " ", dec = ".",
            row.names = TRUE, col.names = TRUE)
summary(FEMA_1)
dim(FEMA_2)
range(FEMA_1$total_loss)
range(FEMA_2$`premium P-I-F`)

library(stringr)
library(ggplot2)
policies <- Fn_Analyze_Policies()
reading policies by county file dated -  2017-10-04 20:02:38
claims <- Fn_Analyze_Claims()
reading claims by county file dated -  2017-10-04 20:02:38

all_data <- merge(policies, claims, by = c("state", "county"), all = TRUE)
write.table(all_data, "all_data.csv")

all_data$state <- tolower(all_data$state)
all_data$county <- tolower(all_data$county)
all_data$county <- gsub(" county", "", all_data$county)
all_data$county <- gsub(" parish", "", all_data$county)




New_all_data<-all_data
colnames(New_all_data)<-c( 'state', 'county', 'policies',
                           'insurance', 'premium', 'total_loss',
                           'closed_loss', 'open_loss', 'cwop_loss', 'total_pay')

ggplot(New_all_data) + aes(x=as.numeric(premium), group=, fill=income) +
  geom_histogram(binwidth=1, color='black')




library(plyr)
library(ROCR)




