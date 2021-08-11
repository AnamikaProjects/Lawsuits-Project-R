#Load xlsx
library(xlsx)
# Load the readxl package
library(readxl)
#Getting the right
getwd()
#To oberve the frequency great visualisation tool
library(funModeling) 
library(tidyverse) 
library(Hmisc)
# Read the sheets, one by one
LS <- read_excel('Lawsuits.xlsx', sheet = 'Lawsuits')
LS

#No missing value found in the entire dataset
sum(is.na(LS))
library(moments)
library(ggplot2)
library(dplyr)
glimpse(LS)
dim(LS)
str(LS)
#Gender 
Sex<-LS$Gender
Sex
Sex<- table(Sex)
Sex
barplot(Sex,main='Gender Distribution',xlab='Gender',ylab='Frequency',col=c('beige','bisque4'))
#Pie chart
pielabels <- sprintf("%s = %3.1f%s", Sex,
                     100*Sex/sum(Sex), "%")
pie(Sex, labels=pielabels,
    clockwise=TRUE,
    radius=1,
    border="red",
    cex=0.8,
    main="Gender distribution")

str(LS)
#Specialty frequency barplot
Sp<-ggplot(data = LS) +geom_bar(mapping = aes(x = Specialty))
Sp + theme(axis.text.x = element_text(angle = 45, hjust = 1))


LS %>%
  count(Specialty)
#71  are married
LS %>%
  count(`Marital Status`)

#Maritial Status
Ms<-ggplot(data = LS) +geom_bar(mapping = aes(x =`Marital Status` )) +scale_colour_brewer(palette = "Set2")
MS<-Ms + theme(axis.text.x = element_text(angle = 45, hjust = 5))
Ms<-Ms+theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(), axis.text = element_text(size = 14), 
         legend.text = element_text(size = 13), axis.title = element_text(size = 14), axis.line = element_line(size = 0.4, colour = "grey10"),
         plot.background = element_rect(fill = "#C8EDAF"), legend.background = element_rect(fill = "#C8EDAF"))
Ms

#Private Attory frequency
PA<-ggplot(data = LS) +geom_bar(mapping = aes(x =`Private Attorney` ))
PA + theme(axis.text.x = element_text(angle = 45, hjust = 1))

PrivateAt<-LS$`Private Attorney`
PrivateAt
PrivateAt<- table(PrivateAt)
PrivateAt


#barplot(PrivateAt,main='Private Attorney Distribution',xlab='Private Attorney',ylab='Frequency',col=c('beige','bisque4'))

#Insurance

Ins<-ggplot(data = LS) +geom_bar(mapping = aes(x = Insurance))
Ins + theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Count of different insurance 30% is unkown
LS%>%
  count(Insurance)
#pielabels <- sprintf("%s = %3.1f%s", Ins,
#                     100*Ins/sum(Ins), "%")
#pie(Ins, labels=pielabels,
#    clockwise=TRUE,
#    radius=1,
#    border="red",
#    cex=0.8,
#    main="Insurance distribution")

library(dplyr)

LS%>%
  filter(Severity>=6)
#Group by gender
Gender<-LS%>%
  group_by(Gender)%>%
  summarise(total=n(),
            pay=sum(Payment),
            Med=median(Payment),
            Mean=mean(Payment),
            SD=sd(Payment),
            VAR=var(Payment))
Gender

#below we combine unkown and Unkown together because there is a typo of capital letters
levels(LS$Insurance) <- c(levels(LS$Insurance),'unkown')
LS$Insurance[LS$Insurance=='Unknown'] <- 'unknown'
LS
#Insurance boxplot after correcting unkown 
Ins<-ggplot(data = LS) +geom_bar(mapping = aes(x = Insurance))
Ins + theme(axis.text.x = element_text(angle = 45, hjust = 1))+theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(), axis.text = element_text(size = 11), 
                                                                     legend.text = element_text(size = 12), axis.title = element_text(size = 14), axis.line = element_line(size = 0.4, colour = "grey10"),
                                                                     plot.background = element_rect(fill = "#B0AFED"), legend.background = element_rect(fill = "#C8EDAF"))
#Graph of Insurance
Insu <- LS%>%
  group_by(Insurance) %>%
  summarise(total=n(),
            pay=sum(Payment),
            Med=median(Payment),
            Mean=mean(Payment),
            SD=sd(Payment),
            VAR=var(Payment))
Insu        
#Need to make a boxplot of the Insurance

Private <- LS%>%
  group_by('Private Attorney') %>%
  summarise(total=n(),
            pay=sum(Payment),
            Med=median(Payment),
            Mean=mean(Payment)
  )
Private
#Checking for private attorney 
#Private is way more in terms of mean and median for payment
PRi <-LS%>%
  group_by(`Private Attorney`)%>%
  summarise(total=n(),
            pay=sum(Payment),
            Med=median(Payment),
            Mean=mean(Payment),
            SD=sd(Payment),
            VAR=var(Payment)
            )
PRi
#Group by Specialty
SPEC <-LS%>%
  group_by(Specialty)%>%
  summarise(total=n(),
            pay=sum(Payment),
            Med=median(Payment),
            Mean=mean(Payment),
            SD=sd(Payment),
            VAR=var(Payment)
  )
#group by severity
SEVEE <-LS%>%
  group_by(Severity)%>%
  summarise(total=n(),
            pay=sum(Payment),
            Med=median(Payment),
            Mean=mean(Payment),
            SD=sd(Payment),
            VAR=var(Payment),
            KUR=kurtosis(Payment),
            SKEW=skewness(Payment),
            
  )

SEVEE


kurtosis(LS$Payment)
skewness(LS$Payment)
###########have to see#########
freq(LS$Severity)
PAA<-table(LS$`Private Attorney`)
pielab <- sprintf("%s = %3.1f%s", PAA,
                     100*PAA/sum(PAA), "%")
#Pie chart of Private Attorney
pie(PAA, labels=pielab,
    clockwise=TRUE,
    radius=1,
    border="red",
    cex=0.8,
    main="Private Attorney",
    col=c("Green","Pink")
    )
legend(1.3, .1, c("Non-Private","Private"), cex = 0.9, fill = PAA)


MARII <-LS%>%
  group_by(`Marital Status`)%>%
  summarise(total=n(),
            pay=sum(Payment),
            Med=median(Payment),
            Mean=mean(Payment),
            SD=sd(Payment),
            VAR=var(Payment)
  )
MARII
LS%>%
  count(`Private Attorney`)
#Checking which are the highest payment
HigestTOLowest<- LS%>%
  arrange(desc(Payment))
HigestTOLowest

#Gender i have to create a box plot
ggplot(LS, aes(Gender, Payment)) +
  geom_point() +
  geom_smooth()
# box plot of the gender to see outliers and others
g<- ggplot(LS, aes(Gender, Payment)) +
  geom_boxplot() +
  geom_smooth()
g

#boxplot for insurance 
ggplotInsurance <- ggplot(LS, aes(Insurance, Payment)) +
  geom_boxplot() +
  geom_smooth() + theme(axis.text.x = element_text(angle = 45, hjust = 1))+theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(), axis.text = element_text(size = 11), 
                                                                                 legend.text = element_text(size = 12), axis.title = element_text(size = 14), axis.line = element_line(size = 0.4, colour = "grey10"),
                                                                                 plot.background = element_rect(fill = "#EDAFEC"), legend.background = element_rect(fill = "#EDAFEC"))

ggplotInsurance

#Bad  graph because of many categories .redraw it
# Group by age and Payment
by_Age<- LS %>%
  group_by(Age,Specialty) 
by_Age
#We have to correct the typos for obGyN to OBGYN
levels(LS$Specialty) <- c(levels(LS$Specialty),'ObGyN')
LS$Specialty[LS$Specialty=='ObGyn'] <- 'OBGYN'
LS
# Vector of Specialty to examine
specialty1 <- c('Pediatrics', 'Plastic Surgeon', 'Internal Medicine',
               'Urological Surgery', 'General Surgery', 'OBGYN',
               'Orthopedic Surgery', 'Ophthamology', 'Emergency Medicine',
               'ObGyn', 'Anesthesiology', 'Neurology/Neurosurgery',
               'Family Practice', 'Dermatology', 'Physical Medicine',
               'Cardiology', 'Resident', 'Pathology', 'Radiology',
               'Thoracic Surgery', 'Occupational Medicine')

# Filter 
filteredSpecialty <- by_Age %>%
  filter(Specialty %in% specialty1)

# Line plot
ggplot(filteredSpecialty, aes(Age, Payment, color = Specialty)) +
  geom_line()

#Gender
by_G<- LS %>%
  group_by(Age,Gender) 

# Vector of four countries to examine
countries <- c('Male','Female')

# Filter 
filtered_4_countries <- by_G %>%
  filter(Gender %in% countries)

# Line plot
ggplot(filtered_4_countries, aes(Age, Payment, color = Gender)) +
  geom_line()

#F
by_Se<- LS %>%
  group_by(Age,Gender)

countries <- c('Pediatrics', 'Plastic Surgeon', 'Internal Medicine',
               'Urological Surgery', 'General Surgery', 'OBGYN',
               'Orthopedic Surgery', 'Ophthamology', 'Emergency Medicine',
               'ObGyn', 'Anesthesiology', 'Neurology/Neurosurgery',
               'Family Practice', 'Dermatology', 'Physical Medicine',
               'Cardiology', 'Resident', 'Pathology', 'Radiology',
               'Thoracic Surgery', 'Occupational Medicine')


f <- by_Se %>%
  filter(Specialty %in% countries)


ggplot(f, aes(Age,Payment)) +
  geom_line() +
  facet_wrap(~ Specialty,scales = "free_y")


library(moments)
skewness(LS$Payment)
kurtosis(LS$Payment)
jarque.test(LS$Payment)

#count private attorney 
LS %>% 
  count(`Private Attorney`)

#observe the Payment but we can't treat them as outliers
ggplot(data = LS) +
  geom_histogram(mapping = aes(x = Payment), binwidth = 500)


#The 3 numbers in the last are outliers if we do z score and >3
LS %>% 
  count(cut_width(Payment, 500))



# not getting executed but will try to
#AGE<-function(age){
 # if (age<=18){
  #  return (small)
  #}else if(age>18 & age <=35){
   # retun (young)
  #}else if(age>35 & age <=60){
   # return (old)
  #}else {
  #  return(seniorCitzen)
  #}
#}

#fun<-lapply(LS$Age,AGE)

library(psych)
skew(LS$Payment)
quantile(LS$Payment)
#quantile(LS$Age)

#Total payment in the dataset 
Total=sum(LS$Payment)
Total

#Famiy Practice
FamilyP<-LS %>%
  filter(Specialty=='Family Practice')
FamilyP
sum(FamilyP$Payment)
MedFP<- median(FamilyP$Payment)
MedFP
MeanFP<-mean(FamilyP$Payment)
MeanFP

#print(paste('Average Family Practice:',MedFP/17))

#General Surgery
GeneralS<-LS %>%
  filter(Specialty=='General Surgery')
GeneralS

MedGS<- median(GeneralS$Payment)
MedGS
MeanGS<-mean(GeneralS$Payment)
MeanGS
sum(GeneralS$Payment)
#print(paste('Average General Surgery:',MedGS/14))

#Anesthesiology
Anes<-LS %>%
  filter(Specialty=='Anesthesiology')
Anes

MedAN<- median(Anes$Payment)
MedAN
MeanAN<-mean(Anes$Payment)
MeanAN
sum(Anes$Payment)

#Orthopedic Surgery
ORT<-LS %>%
  filter(Specialty=='Orthopedic Surgery')
ORT

MedOR<- median(ORT$Payment)
MedOR
MeanOR<-mean(ORT$Payment)
MeanOR
sum(ORT$Payment)

#OBGYN
OBGYN<-LS %>%
  filter(Specialty=='OBGYN')
OBGYN

MedOBGYN<- median(OBGYN$Payment)
MedOBGYN
MeanOBGYN<-mean(OBGYN$Payment)
MeanOBGYN
sum(OBGYN$Payment)

#Top 5  speciality consisits of 67.23 %  Payment  but they are more in number
ProportionofTop5<-(sum(FamilyP$Payment)+sum(GeneralS$Payment)+sum(OBGYN$Payment)+sum(ORT$Payment)+sum(OBGYN$Payment))/Total
ProportionofTop5


#Function to  check median,mean,total_sum
SpecialtyFunction <- function(a){
  Med<-median(a$Payment)
  Mean<-mean(a$Payment)
  SUM<-sum(a$Payment)
  return (list(Med,Mean,SUM))
  }
#Checking statistics for Internal Medicine
IM<-LS%>%
  filter(Specialty=='Internal Medicine')
IM
I<-SpecialtyFunction(IM)
I
#checking statistics for Neurology/Neurosurgery
Neuro <- LS%>%
  filter(Specialty=='Neurology/Neurosurgery')
Neuro

Neurolo<-SpecialtyFunction(Neuro)
Neurolo

#checking statistics for Emergency Medicine
Emergency <- LS%>%
  filter(Specialty=='Emergency Medicine')
Emergency

EMERGEN<- SpecialtyFunction(Emergency)
EMERGEN
#combination of Private attorney and Private insurance
CombinationAllPrivate<-LS%>%
  group_by(Insurance,Specialty,`Private Attorney`) %>%
  filter(`Private Attorney`==1,Insurance=='Private')

glimpse(CombinationAllPrivate)
#Median and mean is higher than normal
SpecialtyFunction(CombinationAllPrivate)

#Comparing Private to all the values including Private
#hist for All
hist(LS$Payment,
     col = "#00009950", freq = FALSE, xlab = "Payment",
     main = "Payment of all values vs Payment for Private ")
#hist for combinationAllPrivate
hist(CombinationAllPrivate$Payment, add = TRUE,
     col = "#99000050", freq = FALSE)     

#vertical line for All,we may take median
abline(v = median(LS$Payment),
       col = "#00009950", lwd = 2)
#vertical line for CombinationAll for mean ,we may take median
abline(v = median(CombinationAllPrivate$Payment),
       col = "#99000050", lwd = 2)
#have to remove the y axis as density

#Private attorney
PrivateAttorney1<-LS%>%
  filter(`Private Attorney`==1)
PrivateAttorney1
NonPrivateAttorney<-LS%>%
  filter(`Private Attorney`==0)
NonPrivateAttorney

#Make PrivateAttorney vs Non-Private
dat <- PrivateAttorney1$Payment
extra_dat <- NonPrivateAttorney$Payment
#Plot
plot(density(dat),col="blue")
lines(density(extra_dat),col="red")

#Histogram of Private vs Non-Private
hist(PrivateAttorney1$Payment,
     col = "#00009950", freq = FALSE, xlab = "Payment",
     main = "Payment of Private Attorney vs Payment for Non-Private Attorney")
#hist for combinationAllPrivate
hist(NonPrivateAttorney$Payment, add = TRUE,
     col = "#99000050", freq = FALSE)     

#vertical line for All,we may take median
abline(v = median(PrivateAttorney1$Payment),
       col = "#00009950", lwd = 2)
#vertical line for CombinationAll for mean ,we may take median
abline(v = median(NonPrivateAttorney$Payment),
       col = "#99000050", lwd = 2)


#To oberve the frequency great visualisation tool
library(funModeling) 
library(tidyverse) 
library(Hmisc)
freq(LS)

#basic eda done in one function
basic_eda <- function(data)
{
  glimpse(data)
  df_status(data)
  freq(data) 
  profiling_num(data)
  plot_num(data)
  describe(data)
}

basic_eda(LS)

#checking metrices
df_status(LS)

#Not usefull now but is used for numerical variable
#data_prof=profiling_num(LS)
#data_prof
#contingency table we can create to see for any two variable


#describe(LS)

#Checking for specialty that is Surgery
Surgery <- LS%>%
  filter(Specialty=='General Surgery' | Specialty =='Orthopedic Surgery' | Specialty =='Neurology/Neurosurgery' |Specialty=='Urological Surgery' | Specialty=='Plastic Surgeon' | Specialty=='Thoracic Surgery')
Surgery

hist(Surgery$Payment)

Surgerysats<-SpecialtyFunction(Surgery)
Surgerysats


#checking for specialty that is medicine
Medicine <- LS%>%
  filter(Specialty=='Internal Medicine' | Specialty =='Emergency Medicine' | Specialty=='Physical Medicine' | Specialty=='Occupational Medicine')
hist(Medicine$Payment)

Medicine
Medicinestats <- SpecialtyFunction(Medicine)
Medicinestats



#Histogram of Surgery vs Medicine
hist(Surgery$Payment,
     col = "#00009950", freq = FALSE, xlab = "Payment",
     main = "Surgery vs Medicine for Payment")
#hist for Medicine
hist(Medicine$Payment, add = TRUE,
     col = "#99000050", freq = FALSE)     

#vertical line for surgery,we may take median
abline(v = median(Surgery$Payment),
       col = "#00009950", lwd = 2)
#vertical line for Medicine for median ,we may take median
abline(v = median(Medicine$Payment),
       col = "#99000050", lwd = 2)



# Histogram Grey Color
hist(Medicine, col=rgb(0.1,0.1,0.1,0.5),xlim=c(0,10), ylim=c(0,200), main="Overlapping Histogram")
hist(Surgery, col=rgb(0.8,0.8,0.8,0.5), add=T)


# Histogram Colored (blue and red)
hist(Medicine, col=rgb(1,0,0,0.5),xlim=c(0,10), ylim=c(0,200), main="Overlapping Histogram", xlab="Variable")
hist(Surgery, col=rgb(0,0,1,0.5), add=T)

#Have to make a two histogram together

#Surgery and Private
SurgeryPrivate <- LS%>%
  filter(`Private Attorney`==1,Specialty=='General Surgery' | Specialty =='Orthopedic Surgery' | Specialty =='Neurology/Neurosurgery' |Specialty=='Urological Surgery' | Specialty=='Plastic Surgeon' | Specialty=='Thoracic Surgery')
SurgeryPrivate
SpecialtyFunction(SurgeryPrivate)

#Medicine and Private not needed
MedicinePrivate <- LS%>%
  filter(Insurance=='Private',`Private Attorney`==1,Specialty=='Internal Medicine' | Specialty =='Emergency Medicine' | Specialty=='Physical Medicine' | Specialty=='Occupational Medicine')
MedicinePrivate
SpecialtyFunction(MedicinePrivate)

#whole prive Speciality
SpecialityPrivateALL<- LS%>%
  filter(`Private Attorney`==1,Insurance=='Private')
SpecialityPrivateALL
SpecialtyFunction(SpecialityPrivateALL)



table(LS$Specialty,LS$Insurance)

#We should merge the two unkown columns into one column unknown

#Severity

SEVE <- LS %>%
  filter(Severity==9 | Severity==8 | Severity==7 | Severity==6)
SEVE



#Proportion of top 4 severity consist of 63.9 % of the payment..High severity means high payment
sum(SEVE$Payment)/sum(LS$Payment)

SEVELESS <-LS%>%
  filter(Severity==1 | Severity==2 | Severity==3 | Severity==4 | Severity==5)

SEVELESS
#Majority of high severity is done by private attorney
SEVE1 <- LS %>%
  filter(Severity==9 | Severity==8 | Severity==7 | Severity==6,`Private Attorney`==1)
SEVE1

HighSeverityPrivategraph<-table(SEVE$`Private Attorney`)
barplot(HighSeverityPrivategraph,main='Private Attorney Distribution in High Severity',xlab='Private Attorney',ylab='Frequency',col=c('beige','bisque4'))
#SS<- data.frame("HighSeverityAll":HighSeverityAll,"HighSeverityPrivate":HighSeverityPrivate)
#SS

#############Boxplot of less severity and high severity with respect to payment
boxplot(SEVE$Payment,SEVELESS$Payment , xlab="High Severity vs Low Severity",
        main="boxplot of High Severity  VS Less Severity ",ylab='Payment'
)
plot(density(SEVELESS$Payment))
plot(density(SEVE$Payment))

#See the graph to see relation between two variabe (work to be done)
library("DataExplorer")
plot_correlation(LS)


#
library(vcd)
#mosaic(LS, shade=TRUE, legend=TRUE)
#ssoc(LS, shade=TRUE)


#We are grouping ages
AGE1<-LS%>%
  filter(Age<18)
AGE1

AGE2 <- LS%>%
  filter(Age>=18 & Age<40)
AGE2

AGE3<-LS%>%
  filter(Age>=40 & Age<60)
AGE3

AGE4 <- LS%>%
  filter(Age>=60)
AGE4


#Here average we are taking median is more in 60 and above
SpecialtyFunction(AGE1)
SpecialtyFunction(AGE2)
SpecialtyFunction(AGE3)
SpecialtyFunction(AGE4)

#More severity is more in 60 and above followed by 35+
table(AGE1$Severity)
table(AGE2$Severity)
table(AGE3$Severity)
table(AGE4$Severity)

#AGE and specialty
table(AGE1$Specialty)
table(AGE2$Specialty)
table(AGE3$Specialty)
table(AGE4$Specialty)

freq(AGE1$Specialty)
freq(AGE2$Specialty)
freq(AGE3$Specialty)
freq(AGE4$Specialty)

ggplot(data = AGE1) +
  geom_histogram(mapping = aes(x = Payment), binwidth = 500)
ggplot(data = AGE2) +
  geom_histogram(mapping = aes(x = Payment), binwidth = 500)
ggplot(data = AGE3) +
  geom_histogram(mapping = aes(x = Payment), binwidth = 500)
ggplot(data = AGE4) +
  geom_histogram(mapping = aes(x = Payment), binwidth = 500)


AAAG1<-ggplot(data = AGE1) +
  geom_histogram(mapping = aes(x = Payment), binwidth = 500) +scale_colour_brewer(palette = "Set2")
AAAG1<-AAAG1 + theme(axis.text.x = element_text(angle = 45, hjust = 5))
AAAG1<-AAAG1+theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(), axis.text = element_text(size = 10), 
             legend.text = element_text(size = 13), axis.title = element_text(size = 10), axis.line = element_line(size = 0.4, colour = "grey10"),
             plot.background = element_rect(fill = "#afbaed"), legend.background = element_rect(fill = "#afbaed"))
AAAG1


AAAG2<-ggplot(data = AGE2) +
  geom_histogram(mapping = aes(x = Payment), binwidth = 500) +scale_colour_brewer(palette = "Set2")
AAAG2<-AAAG2 + theme(axis.text.x = element_text(angle = 45, hjust = 5))
AAAG2<-AAAG2+theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(), axis.text = element_text(size = 10), 
                   legend.text = element_text(size = 10), axis.title = element_text(size = 10), axis.line = element_line(size = 0.4, colour = "grey10"),
                   plot.background = element_rect(fill = "#afbaed"), legend.background = element_rect(fill = "#afbaed"))
AAAG2

AAAG3<-ggplot(data = AGE3) +
  geom_histogram(mapping = aes(x = Payment), binwidth = 500) +scale_colour_brewer(palette = "Set2")
AAAG3<-AAAG3 + theme(axis.text.x = element_text(angle = 45, hjust = 5))
AAAG3<-AAAG3+theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(), axis.text = element_text(size = 10), 
                   legend.text = element_text(size = 10), axis.title = element_text(size = 10), axis.line = element_line(size = 0.2, colour = "grey10"),
                   plot.background = element_rect(fill = "#edafaf"), legend.background = element_rect(fill = "#edafaf"))
AAAG3

AAAG4<-ggplot(data = AGE4) +
  geom_histogram(mapping = aes(x = Payment), binwidth = 500) +scale_colour_brewer(palette = "Set2")
AAAG4<-AAAG4 + theme(axis.text.x = element_text(angle = 45, hjust = 5))
AAAG4<-AAAG4+theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(), axis.text = element_text(size = 10), 
                   legend.text = element_text(size = 16), axis.title = element_text(size = 14), axis.line = element_line(size = 0.6, colour = "grey10"),
                   plot.background = element_rect(fill = "#eaafed"), legend.background = element_rect(fill = "#eaafed"))
AAAG4


AAGE1 <-AGE1%>%
  group_by(Specialty)%>%
  summarise(total=n(),
            pay=sum(Payment),
            Med=median(Payment),
            Mean=mean(Payment),
            SD=sd(Payment),
            VAR=var(Payment),
            KUR=kurtosis(Payment),
            SKEW=kurtosis(Payment)
  )
AAGE1

AAGE2 <-AGE2%>%
  group_by(Specialty)%>%
  summarise(total=n(),
            pay=sum(Payment),
            Med=median(Payment),
            Mean=mean(Payment),
            SD=sd(Payment),
            VAR=var(Payment),
            KUR=kurtosis(Payment),
            SKEW=kurtosis(Payment)
  )
AAGE2

AAGE3 <-AGE3%>%
  group_by(Specialty)%>%
  summarise(total=n(),
            pay=sum(Payment),
            Med=median(Payment),
            Mean=mean(Payment),
            SD=sd(Payment),
            VAR=var(Payment),
            KUR=kurtosis(Payment),
            SKEW=kurtosis(Payment)
  )
AAGE3

AAGE4 <-AGE4%>%
  group_by(Specialty)%>%
  summarise(total=n(),
            pay=sum(Payment),
            Med=median(Payment),
            Mean=mean(Payment),
            SD=sd(Payment),
            VAR=var(Payment),
            KUR=kurtosis(Payment),
            SKEW=kurtosis(Payment)
  )
AAGE4


AAGE11 <-AGE1%>%
  summarise(total=n(),
            pay=sum(Payment),
            Med=median(Payment),
            Mean=mean(Payment),
            SD=sd(Payment),
            VAR=var(Payment),
            KUR=kurtosis(Payment),
            SKEW=skewness(Payment)
  )
AAGE11

AAGE12 <-AGE2%>%
  summarise(total=n(),
            pay=sum(Payment),
            Med=median(Payment),
            Mean=mean(Payment),
            SD=sd(Payment),
            VAR=var(Payment),
            KUR=kurtosis(Payment),
            SKEW=skewness(Payment)
  )
AAGE12


AAGE13 <-AGE3%>%
  summarise(total=n(),
            pay=sum(Payment),
            Med=median(Payment),
            Mean=mean(Payment),
            SD=sd(Payment),
            VAR=var(Payment),
            KUR=kurtosis(Payment),
            SKEW=skewness(Payment)
  )
AAGE13

AAGE14 <-AGE4%>%
  summarise(total=n(),
            pay=sum(Payment),
            Med=median(Payment),
            Mean=mean(Payment),
            SD=sd(Payment),
            VAR=var(Payment),
            KUR=kurtosis(Payment),
            SKEW=skewness(Payment)
  )
AAGE14
#The insurance which is Private and Gender is Female
PF <- LS%>%
  filter(Insurance=='Private',Gender=='Female')
PF 
dim(PF)
#47.8 percent is private and female
print(34/71)

# Mix both unkown together
UNKOINSU<-LS%>%
  filter(Insurance=='Unknown'| Insurance=='unknown')
UNKOINSU
count(UNKOINSU)
#36 values are unkown for Insurance ,out of 118
36/118
#30% are unkown


#ggplot(LS,aes(Gender,Payment))+theme(plot.background = element_rect(fill = "#C8EDAF"), legend.background = element_rect(fill = "#C8EDAF"))+geom_point()
ggplot(LS,aes(Gender,Payment))+theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(), axis.text = element_text(size = 14), 
                                     legend.text = element_text(size = 13), axis.title = element_text(size = 14), axis.line = element_line(size = 0.4, colour = "grey10"),plot.background = element_rect(fill = "#C8EDAF"), legend.background = element_rect(fill = "#C8EDAF"))+geom_point()

#We have to see mainly which are the columns we have to focus

#hist(LS$Payment,
     #col = "#00009950", freq = FALSE, xlab = "Payment",
     #main = "Payment vs count")
#abline(v = mean(LS$Payment),
       #col = "#00009950", lwd = 2)
#just 
#male
Mapay <- LS%>%
  filter(Gender=='Male')
#female
Fepay <-LS%>%
  filter(Gender=='Female')

#hist for male
hist(Mapay$Payment,
     col = "#00009950", freq = FALSE, xlab = "Payment",
     main = "Payment vs count")
#hist for female
hist(Fepay$Payment, add = TRUE,
     col = "#99000050", freq = FALSE)     

#vertical line for male,we may take median
abline(v = mean(Mapay$Payment),
       col = "#00009950", lwd = 2)
#vertical line for female for mean ,we may take median
abline(v = mean(Fepay$Payment),
       col = "#99000050", lwd = 2)


##Severity and Gender

LS$Severity<-as.factor(LS$Severity)
LS$Gender<-as.factor(LS$Gender)
LS$Severity
spineplot(LS$Severity,LS$Gender,
          xlab = "severity",ylab = "Gender",col=c("blue","green"))

#Insurance and Gender
#There are many unknowns in Male gender In total 30 % are unknown
LS$Insurance<-as.factor(LS$Insurance)
LS$Insurance
spineplot(LS$Gender,LS$Insurance,ylab = "Insurance",
          xlab = "Gender",
          col =c("red","green","yellow","purple","orange","blue"))



