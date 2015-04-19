## Notes for Analytics Edge
## AN ANALYTICAL DETECTIVE
WHO<-read.csv("C:/Users/Joel/Desktop/Data/WHO.csv")

dURL<-"https://courses.edx.org/c4x/MITx/15.071x_2/asset/mvtWeek1.csv"
download.file(dURL, destfile="C:/Users/Joel/Desktop/Data/mvtWeek1.csv")
mvt<-read.csv("C:/Users/Joel/Desktop/Data/mvtWeek1.csv")

#Row Of data
nrow(mvtWeek1)
max(mvtWeek1$ID)
min(mvtWeek1$Beat)
str(mvt)


nrow(subset(mvtWeek1, LocationDescription=="ALLEY",))
DateConvert = as.Date(strptime(mvtWeek1$Date, "%m/%d/%y %H:%M"))
summary(DateConvert)
mvtWeek1$Month = months(DateConvert)
mvtWeek1$Weekday = weekdays(DateConvert)
mvtWeek1$Date = DateConvert


nrow(filter(mvt, Arrest=="TRUE"))
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
mvt$Date = DateConvert
?as.factor


##In which month did the fewest motor vehicle thefts occur?
#library(doBy)
#aggregate(mvt, by=LocationDescription, count, na.rm=TRUE)
which.min(with(mvt, table(Month)))
which.max(with(mvt, table(Weekday)))

##Which month has the largest number of motor vehicle thefts for which an arrest was made?
#i.e. where arrest is True
mvttrial<-filter(mvt, Arrest=="TRUE")
which.max(with(mvttrial, table(Month)))
hist(mvt$Date, breaks=100)

mvttrial<- with(mvttrial, table(Year))
str(mvttrial)
boxplot(Year, data=mvttrial)
##Analysis of behaviour of thefts

##For what proportion of motor vehicle thefts in 2001 was an arrest made?
mvttrial<-with(mvt, table(Year, Arrest))

##PROBLEM 4.1 - POPULAR LOCATIONS
sort(table(mvt$LocationDescription))

DF <- as.data.frame(as.matrix(tail(sort(table(mvt$LocationDescription)),6)))
DF$V2 <- rownames(DF); rownames(DF) <- NULL
Top5<-DF[-c(4), ] 
rm(DF); colnames(Top5)<-c("TheftNumbers", "LocationDescription")
sum(Top5$TheftNumbers)
Top5$LocationDescription = factor(Top5$LocationDescription)

Top5 = subset(mvt, LocationDescription=="STREET" | LocationDescription=="PARKING LOT/GARAGE(NON.RESID.)" | LocationDescription=="ALLEY" | LocationDescription=="GAS STATION" | LocationDescription=="DRIVEWAY - RESIDENTIAL")
Top5$LocationDescription = factor(Top5$LocationDescription)




print(aggdata)

library(swirl)
install_from_swirl("Getting and Cleaning Data")
swirl()

install.packages(c("JGR","Deducer","DeducerExtras"))
library(JGR)
JGR()














######################
## PHARMAC Asthma test
######################
require(scales)
library(ggplot2)
library(reshape2)
library(grid)
library(gridExtra)

######
# SI Function to convert large numbers to text
######

format_si <- function(...) {
  # Format a vector of numeric values according
  # to the International System of Units.
  # http://en.wikipedia.org/wiki/SI_prefix
  #
  # Based on code by Ben Tupper
  # https://stat.ethz.ch/pipermail/r-help/2012-January/299804.html
  # Args:
  #   ...: Args passed to format()
  #
  # Returns:
  #   A function to format a vector of strings using
  #   SI prefix notation
  #
  
  function(x) {
    limits <- c(1e-24, 1e-21, 1e-18, 1e-15, 1e-12,
                1e-9,  1e-6,  1e-3,  1e0,   1e3,
                1e6,   1e9,   1e12,  1e15,  1e18,
                1e21,  1e24)
    prefix <- c("y",   "z",   "a",   "f",   "p",
                "n",   "µ",   "m",   " ",   "k",
                "M",   "G",   "T",   "P",   "E",
                "Z",   "Y")
    
    # Vector with array indices according to position in intervals
    i <- findInterval(abs(x), limits)
    
    # Set prefix to " " for very small values < 1e-24
    i <- ifelse(i==0, which(limits == 1e0), i)
    
    paste(format(round(x/limits[i], 1),
                 trim=TRUE, scientific=FALSE, ...),
          prefix[i])
  }
}

Pharmac<-read.csv("C:/Users/Joel/Downloads/DHB_Example_Data.txt", sep="\t")
Pharmac$financial_year <- as.factor(Pharmac$financial_year)
Pharmac$Cost<-as.numeric(gsub(",","", Pharmac$Cost..ex.GST.))
Pharmac$Prescriptions<-as.numeric(gsub(",","", Pharmac$Prescriptions))
summary(Pharmac)
str(Pharmac)
head(Pharmac)

#Group By Demography, Financial Year ~ Cost
library(doBy)
byYrDHBCost <- summaryBy(Cost ~ DHB, data=Pharmac, FUN=sum)
str(byYrDHBCost)
byYrDHBCost$DHB <- factor(byYrDHBCost$DHB, levels = byYrDHBCost$DHB[order(byYrDHBCost$Cost.sum)])

# Geography to sort by
horiz.bar.totalcost<-
ggplot(byYrDHBCost, aes(x=DHB, y=Cost.sum)) + 
  geom_point(stat="identity", position=position_dodge()) +
  geom_segment(aes(xend=DHB, y=0, yend=Cost.sum), color="black", alpha=1/4) +
  coord_flip() +
  scale_y_continuous(labels=format_si()) +
  geom_point(size=3, fill="white") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.text.x = element_text(angle = 30, hjust = 1), legend.position="none") +
  ylab('Cost') +
  xlab('DHB') +
  labs(title="Cost of Treatments")

# Create Two Subsets
Pharmac.Cost1.High<-subset(Pharmac, Pharmac$DHB=="Waitemata" | Pharmac$DHB=="Canterbury" | Pharmac$DHB=="Counties Manukau" |
                             Pharmac$DHB=="Auckland" | Pharmac$DHB=="Waikato" | Pharmac$DHB=="Southern" |
                             Pharmac$DHB=="Capital Coast" | Pharmac$DHB=="Bay of Plenty" | Pharmac$DHB=="MidCentral" |
                             Pharmac$DHB=="Hawkes Bay",)

Pharmac.Cost1.Low<-subset(Pharmac, Pharmac$DHB=="Hutt Valley" | Pharmac$DHB=="Northland" |
                            Pharmac$DHB=="Taranaki" | Pharmac$DHB=="Nelson Marlborough" | Pharmac$DHB=="Lakes" |
                            Pharmac$DHB=="Whanganui" | Pharmac$DHB=="South Canterbury" | Pharmac$DHB=="Wairarapa" |
                            Pharmac$DHB=="Tairawhiti",)                  

# First Subset Graph
Pharmac.Cost1.High.Line<-
ggplot(Pharmac.Cost1.High, aes(x=financial_year, y=Cost, group=Group, color=Group)) + 
  geom_line() +
  facet_grid(~DHB) +
  scale_y_continuous(labels=format_si(), breaks=seq(0,5000000,by=1000000)) +
  geom_point(size=0, fill="white") +
  theme(panel.grid.major = element_blank() , panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank(),
        strip.text.x = element_text(size=8)) + 
  ylab('Total Cost') +
  labs(title="Cost of Asthma Treatments By Year (2000 - 2015) and DHB")

# Second Subset Graph
Pharmac.Cost1.Low.Line<-
  ggplot(Pharmac.Cost1.Low, aes(x=financial_year, y=Cost, group=Group, color=Group)) + 
  geom_line() +
  facet_grid(~DHB) +
  scale_y_continuous(labels=format_si(), breaks=seq(0,5000000,by=1000000)) +
  scale_x_discrete(breaks=seq(2000,2014,by=3)) +
  geom_point(size=0, fill="white") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.text.x = element_text(angle = 90, vjust = 1), strip.text.x = element_text(size=8)) + 
  ylab('Total Cost') +
  xlab('Year')

grid.newpage() # Open a new page on grid device
pushViewport(viewport(layout = grid.layout(2, 4)))
print(horiz.bar.totalcost, vp = viewport(layout.pos.row = 1:2, layout.pos.col = 1))
print(Pharmac.Cost1.High.Line, vp = viewport(layout.pos.row = 1:1, layout.pos.col = 2:4)) 
print(Pharmac.Cost1.Low.Line, vp = viewport(layout.pos.row = 2:2, layout.pos.col = 2:4))


#What was the cause of the changes in costs???
# Third Subset Graph
Pharmac.Scripts.High.Line<-
  ggplot(Pharmac.Cost1.High, aes(x=financial_year, y=Prescriptions, group=Group, color=Group)) + 
  geom_line() +
  facet_grid(~DHB) +
  scale_y_continuous(labels=format_si()) +
  geom_point(size=0, fill="white") +
  theme(panel.grid.major = element_blank() , panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank(),
        strip.text.x = element_text(size=8)) + 
  ylab('Prescription Count') +
  labs(title="Asthma Prescriptions By Year (2000 - 2015) and DHB")

# Fourth Subset Graph
Pharmac.CostPerScripts.High.Line<-
  ggplot(Pharmac.Cost1.High, aes(x=financial_year, y=Cost/Prescriptions, group=Group, color=Group)) + 
  geom_line() +
  facet_grid(~DHB) +
  scale_y_continuous(labels=format_si()) +
  scale_x_discrete(breaks=seq(2000,2014,by=3)) +
  geom_point(size=0, fill="white") +
  theme(panel.grid.major = element_blank() , panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.text.x = element_text(angle = 90, vjust = 1), strip.text.x = element_text(size=8)) + 
  ylab('Cost ($) per Prescription') +
  labs(title="Cost per Prescription By Year (2000 - 2015) and DHB") +
  xlab('Year')

grid.newpage() # Open a new page on grid device
pushViewport(viewport(layout = grid.layout(2, 1)))
print(Pharmac.Scripts.High.Line, vp = viewport(layout.pos.row = 1:1, layout.pos.col = 1)) 
print(Pharmac.CostPerScripts.High.Line, vp = viewport(layout.pos.row = 2:2, layout.pos.col = 1))

#Think that costs either increased or remained the same due to decrease in costs of treatments but an increase uptake in prescriptions.
#Perform a half and half experiment. 2000 - 2007 vs 2007 - 2014
head(Pharmac)
PharmacSplitStudy<-Pharmac
PharmacSplitStudy$financial_year<-as.numeric(as.character(as.factor(PharmacSplitStudy$financial_year)))
PharmacSplitStudy.1<-subset(PharmacSplitStudy,financial_year<=2007, select=c(financial_year, DHB, Prescriptions, Cost))
PharmacSplitStudy.2<-subset(PharmacSplitStudy,financial_year>=2007, select=c(financial_year, DHB, Prescriptions, Cost))
rm(PharmacSplitStudy)

PharmacSplitStudy.1$study<-c("2000 - 2007"); PharmacSplitStudy.2$study<-c("2007 - 2014")
PharmacSplitStudy<-rbind(PharmacSplitStudy.1, PharmacSplitStudy.2); rm(PharmacSplitStudy.1); rm(PharmacSplitStudy.2)

byCost.SplitStudy <- summaryBy(Cost ~ study+DHB, data=PharmacSplitStudy, FUN=sum)
byScripts.SplitStudy <- summaryBy(Prescriptions ~ study+DHB, data=PharmacSplitStudy, FUN=sum)

PharmacSplitStudy$CostPerScript<-PharmacSplitStudy$Cost/PharmacSplitStudy$Prescriptions
byCostPerScripts.SplitStudy<-summaryBy( CostPerScript ~ study+DHB, data=PharmacSplitStudy, FUN=mean)

#Fifth Subset Graph
ggpbyCost.SplitStudy<-
  ggplot(byCost.SplitStudy, aes(x=study, y=Cost.sum, color=study, fill=study)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  facet_grid(~DHB) +
  scale_y_continuous(labels=format_si()) +
  geom_point(size=0, fill="white") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.text.x = element_text(angle = 90, vjust = 1), strip.text.x = element_text(size=8)) + 
  ylab('Cost ($)') +
  xlab('Period of Study') +
  labs(title="Comparing Total Costs By Periods For All DHB's")

#Sixth Subset Graph
ggpbyScripts.SplitStudy<-
ggplot(byScripts.SplitStudy, aes(x=study, y=Prescriptions.sum, color=study, fill=study)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  facet_grid(~DHB) +
  scale_y_continuous(labels=format_si()) +
  geom_point(size=0, fill="white") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.text.x = element_text(angle = 90, vjust = 1), strip.text.x = element_text(size=8)) + 
  ylab('Prescription Count') +
  xlab('Period of Study') +
  labs(title="Comparing Prescription Numbers By Periods For All DHB's")

#Seventh Subset Graph
ggpbyCostPerScripts.SplitStudy<-
ggplot(byCostPerScripts.SplitStudy, aes(x=study, y=CostPerScript.mean, color=study, fill=study)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  facet_grid(~DHB) +
  scale_y_continuous(labels=format_si()) +
  geom_point(size=0, fill="white") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.text.x = element_text(angle = 90, vjust = 1), strip.text.x = element_text(size=8)) + 
  ylab('Cost per Prescription ($)') +
  xlab('Period of Study') +
  labs(title="Comparing Cost per Prescription By Periods For All DHB's")

grid.newpage() # Open a new page on grid device
pushViewport(viewport(layout = grid.layout(3, 1)))
print(ggpbyCost.SplitStudy, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(ggpbyScripts.SplitStudy, vp = viewport(layout.pos.row = 2, layout.pos.col = 1)) 
print(ggpbyCostPerScripts.SplitStudy, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))












meltedPharmac<-melt(data=Pharmac, id.vars=c("Group", "financial_year", "DHB"), measure.vars=c("Prescriptions", "Cost"), na.rm = FALSE)
castPharmac<-dcast(data=meltedPharmac, financial_year + Group + DHB ~ variable, fun.aggregate=sum)

# First
ggplot(Pharmac, aes(x=financial_year, y=Cost, group=Group, color=Group)) + 
  geom_line() +
  facet_grid(~DHB) +
  scale_y_continuous(labels = dollar) +
  geom_point(size=3, fill="white") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))











?dcast





# Costs fall then climb (2000 - 2008; 2008 - 2014)
TotalCosts<-
  ggplot(data=Pharmac, aes(x=financial_year, y=Cost)) + 
  geom_line(stat="identity", position=position_dodge())
  scale_y_continuous(labels = dollar) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


  geom_bar(stat="identity", position=position_dodge()) +


# Prescriptions increase slowly (2000 - 2005; up 2005 - 2012; down 2012 - 2014) 
TotalPrescriptions<-
  ggplot(data=Pharmac, aes(x=financial_year, y=Prescriptions)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


# So increasing costs could be the cause and not increasing demand for asthma pharma
# Costs per perscription increased (2002-2006) then steadied and maintained just below $150 2006->
TotalCostsPerPrescription<-
  ggplot(data=Pharmac, aes(x=financial_year, y=Cost/Prescriptions, label=as.integer(Cost/Prescriptions))) + 
  geom_bar(stat="identity", position=position_dodge()) +
  scale_y_continuous(labels = dollar) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


grid.arrange(TotalCosts, TotalPrescriptions, TotalCostsPerPrescription, nrow = 3,
             main = "Total Costs & Prescriptions")



Pharmac<-cbind(Pharmac, byYrDHBCost$DHB)

ggplot(byYrDHBCost, aes(x=DHB, y=Cost.sum)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  scale_y_continuous(labels = dollar) +
  geom_point(size=3, fill="white")

str(Pharmac)









ggplot(aes(x=financial_year, y=value, color=variable), data=meltedPharmac) +
  geom_line() +
  scale_y_continuous(labels = comma)



ggplot(data=mdf, aes(x=Year, y=value, group = Company, colour = Company)) +
  geom_line() +
  geom_point( size=4, shape=21, fill="white")




ByDemograph




ggplot(data=Pharmac, aes(x=financial_year, y=Cost/Prescriptions, label=as.integer(Cost/Prescriptions))) + 
  geom_bar(stat="identity", position=position_dodge())
  geom_point(colour="white", fill="red", shape=21) + 
  scale_size_area(max_size = 50)+
  geom_text(size=4)+
  theme_bw()+
  facet_grid(DHB~Group)








#Bad
ggplot(data=Pharmac, aes(x=financial_year, y=Prescriptions, label=as.integer(Prescriptions))) + 
  geom_bar(stat="identity", position=position_dodge()) +
  facet_grid(DHB~.)





#Total

ggplot(data=Pharmac, aes(x=financial_year, y=Prescriptions, size=Prescriptions, label=as.integer(Prescriptions))) + 
  geom_point(colour="white", fill="red", shape=21) + 
  scale_size_area(max_size = 15) +
  facet_grid(DHB~Group) +
  geom_text(size=4) +
  theme_bw()
  


subset(Pharmac,DHB=="Auckland" & financial_year==2014)


size_var <- runif(15, 1,10)
ggplot(data=Pharmac, aes(x=financial_year, y=Cost/Prescriptions, label=Cost/Prescriptions)) +
  geom_point(aes(size=size_var), shape=21) +
  scale_size_continuous(range=c(2,15)) +
  theme(legend.position = "none")



geom_bar(stat="identity", position=position_dodge()) +
  geom_point(colour="red") +
  scale_size_area(to=c(1,20)) +
  geom_text(size=3)




unique(Pharmac$DHB)
unique(Pharmac$financial_year)





+ 
  facet_wrap(~Pharmac$DHB)



ggplot(data=Pharmac, aes(x=financial_year, y=Prescriptions)) +
  geom_bar(stat="identity", position=position_dodge()) +   
  facet_grid(Pharmac$DHB ~ .)










ggplot() + 
  geom_point(data=Pharmac, aes(x=financial_year, y=Cost)) + 
  geom_smooth(data=Pharmac, aes(x=financial_year, y=Cost), fill="blue", colour="darkblue", size=1) +
  scale_y_continuous(labels = dollar) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))



