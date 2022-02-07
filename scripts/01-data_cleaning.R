#### Preamble ####
# Purpose: Clean the survey data downloaded from open data Toronto
# Author: Fan Yang
# Data: 3rd February 2021
# Contact: fanmartin.yang@mail.utoronto.ca
# License: MIT




#Get data from opendatatoronto
library(opendatatoronto)
library(knitr)
library(ggplot2)
library(astsa)
library(stats)
library(opendatatoronto)
library(dplyr)

# get package
package <- show_package("e28bc818-43d5-43f7-b5d9-bdfb4eda5feb")
package

# get all resources for this package
resources <- list_package_resources("e28bc818-43d5-43f7-b5d9-bdfb4eda5feb")

# identify datastore resources; by default, Toronto Open Data sets datastore resource format to CSV for non-geospatial and GeoJSON for geospatial resources
datastore_resources <- filter(resources, tolower(format) %in% c('csv', 'geojson'))

# load the first datastore resource as a sample
data <- filter(datastore_resources, row_number()==1) %>% get_resource()



#the times series plot of the marriage licences at four civic centers
ggplot(data=data,mapping=aes(x=TIME_PERIOD,y=MARRIAGE_LICENSES,group=CIVIC_CENTRE,color=CIVIC_CENTRE))+geom_line()+
  theme(axis.text.x = element_text(angle = 90))


# Find the basic information of the marriage licences at four centers sepeartely
ET=data[which(data$CIVIC_CENTRE=="ET"),]
NY=data[which(data$CIVIC_CENTRE=="NY"),]
TO=data[which(data$CIVIC_CENTRE=="TO"),]
SC=data[which(data$CIVIC_CENTRE=="SC"),]
ET1=sum(ET$MARRIAGE_LICENSES)
summary(ET$MARRIAGE_LICENSES)
NY1=sum(NY$MARRIAGE_LICENSES)
summary(NY$MARRIAGE_LICENSES)
TO1=sum(TO$MARRIAGE_LICENSES)
summary(TO$MARRIAGE_LICENSES)
SC1=sum(SC$MARRIAGE_LICENSES)
summary(SC$MARRIAGE_LICENSES)
ET=c(1, 175.2, 365,19624)
NY=c(122,282.6,997,35604)
TO=c(3,631,1773,76349)
SC=c(78,220,782,30512)
table1=rbind(ET,NY,TO,SC)
colnames(table1)=c("Min","Mean","Max","Total")
knitr::kable(table1,caption="Table1. Basic Information of Four Civic Centers")

#apply seasonal transformation to the marriage licences at SC center
SC=data[which(data$CIVIC_CENTRE=="SC"),]
d=ts(SC$MARRIAGE_LICENSES)
d1=diff(d,lag=12)
plot.ts(d1,type="l",xlab="Case Counts")



#check the acf and pacf of the plot
a1=acf2(d1)


#run the sarima model
x=sarima(d,2,0,1,1,1,2,12)
mo1= capture.output(sarima(d,2,0,1,1,1,2,12)) 

#forcast the next 10 months
Cases=d
p=sarima.for(Cases,10,2,0,1,1,1,2,12,main="Fig5. Predicted Issued Marriage Licences in Next 10 Months")

#transform the output of the sarima model to table2
knitr::kable(x$ttable,caption="Table2. Summary of the SARIMA Model")


