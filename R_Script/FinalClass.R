setwd("C:/Users/AEshete/Documents/NFIP")
dir()
library(readxl)
FemaCombine <- read_excel("~/NFIP/FemaCombine.xlsx")
View(FemaCombine)
str(FemaCombine)


all_data1<-FemaCombine
head(all_data1)

all_data1$bins <- cut(all_data1$policies, breaks=c(1, 10000, 400000), labels=c("1 - 10k", "10k - 400k"))


# create a dataframe
all_data1 <- rep(levels(all_data1$policies))
P_group <- rep(c('<=10000K', '>10000K'), 3)
df <- data.frame(Low, High, NA)
df
View(all_data1)


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

P_trial <- all_data1[, "P_group"]
head(P_group, 2)


head(New_all_data)
New_all_data$Classf=exp
Classf=exp
cbind(New_all_data, Classf)
View(all_data)
all_data["ClassF] <- NA



New_all_data<-c( 'state', 'county', 'policies',
                           'insurance', 'premium', 'total_loss',
         'closed_loss', 'open_loss', 'cwop_loss', 'total_pay')


New_all_data1$ClassF <- New_all_data
