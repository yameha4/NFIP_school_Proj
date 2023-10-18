#Import the new excel file
setwd("C:/Users/AEshete/Documents/NFIP")
dir()
library(readxl)
NFIP_Data <- read_excel("~/NFIP/Final_NFIP_Data.xlsx")
View(NFIP_Data)
str(NFIP_Data)
summary(NFIP_Data)
#Data fame converstion
tmp<-NFIP_Data
tmp$Policies_gfx_Class <- as.factor(tmp$Policies_gfx_Class)
tmp$Payment_gfx_Class <- as.factor(tmp$Payment_gfx_Class)
str(tmp)
summary(tmp)
#If any remove all the NA from the table
vect<- c(1, 2, 3, NA)
is.na(vect)
tmp1 <- tmp[complete.cases(tmp),]
NROW(tmp1)
summary(tmp1)
str(tmp1)

#How many Polices and Pyaments are high and low
#Remove column
tmp_pol<- tmp
#tmp_pol$Payment_gfx_Class<- NULL
tmp_pol$long<- NULL
tmp_pol$lat<- NULL
tmp_pol$group<- NULL
tmp_pol$order<- NULL
#tmp_pol$policies_gfx<- NULL
#tmp_pol$Payment_gfx<- NULL

str(tmp_pol)
View(tmp_pol)
summary(tmp_pol)

#create graphs
counts <- table(tmp_pol$Payment_gfx_Class)
barplot(counts, main="Total Loss Distribution of Total Payment ", 
        xlab="Number of Total Payment <10k & >=10k", col=c("blue","red"))


# Basic Scatterplot Matrix
counts1 <- table(tmp_pol$Payment_gfx_Class)
counts2 <- table(tmp_pol$Policies_gfx_Class)
pairs(~total_loss+premium+insurance,data=tmp_pol, 
      main="Simple Scatterplot Matrix")





attach(tmp_pol)
plot(Policies_gfx_Class, Payment_gfx_Class)
abline(lm(Policies_gfx_Class~Payment_gfx_Class))
title("regression of policy on total payment")










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











