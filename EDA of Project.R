getwd()                                        #getting current directory
setwd("F://Statistics for BA//Home-Work 2")    #changing current directory


df_lawsuits1 = readxl::read_xlsx("Lawsuits.xlsx")      #reading Lawsuits data and storing it to a object
df_lawsuits2 = distinct(df_lawsuits1)                  #To remove any duplicate rows, from this we can get distinct rows and columns.
df_lawsuits = df_lawsuits2[df_lawsuits2$Age != 0,]     #Removing rows where age = 0 as it is not productive data.


#Relation between payments from various type of insurance for private / non private attorney

library(ggplot2)                                      #including library for ggplot
ggplot(df_lawsuits, aes(fill=sort(`Private Attorney`, decreasing = TRUE), y=Payment, x=stringr::str_to_title(Insurance)),   #sort ,sorting so that all values of private and non private stays together, stringr::str_to_title for formatting data to first letter upper case rest lower case
) +  xlab("Insurance")+ ylab("Total Payment")+
    geom_bar(position='stack', stat='identity') + labs(fill = "Private/ Non private Attorney")


#Checking for any NA data or NULL data
install.packages("psych")   # installing packages psych for using describe function
library(psych)              #including library psych after installation
describe( readxl::read_xlsx("Lawsuits.xlsx"))      #using describe function


#Histogram of various numeric columns

hist(df_lawsuits$Payment,    #Histogram of payment column
     xlim = c(0,7000),
     ylim = c(0, 100),
     main = "Payment",
     xlab = "Payment",
     col= c(4:9))


hist(df_lawsuits$Age,        #Histogram of Age column
     xlim = c(0,100),
     ylim = c(0, 35),
     main = "Age",
     xlab = "Age",
     col= c(4:9))




hist(df_lawsuits$Severity,      #Histogram of Severity column
     xlim = c(1,10),
     ylim = c(0,55),
     main = "Severity",
     xlab = "Severity",
     col= c(4:9))

hist(df_lawsuits$`Marital Status`,     #Histogram of Marital status column
     xlim = c(0,4), 
     ylim = c(0,80),
     main = "Marital status",
     xlab = "Marital Status",
     col= c(4:9))


#Insurance wise Payments

library(ggplot2)
df_paysum_Insurancesum =  aggregate(x= df_lawsuits$Payment,                                  #getting sum of all payments, grouping insurance wise
                                    by= list(stringr::str_to_title(df_lawsuits$Insurance)),  
                                    FUN=sum)

ggplot(data=df_paysum_Insurancesum, aes(x=x, y=Group.1)) + geom_bar(stat="identity") + title("Insurance wise payments") + 
    xlab("Total Payments(in thousands)") + ylab("Type of Insurance") 


#Severity wise Payment

sum1 = aggregate(x= df_lawsuits$Payment,              #getting sum of all payments, grouping Severity wise
                 by= list(df_lawsuits$Severity), 
                 FUN=sum)

ggplot(data=sum1, aes(y=x, x=Group.1)) + geom_bar(stat="identity") + title("Severity wise payments") + 
    ylab("Total Payments(in thousands)") + xlab("Type of Severity")  + xlim(1,10)



#Bar graph for Specialty 

df_speciality = aggregate(x= df_lawsuits$Payment,                    #getting sum of all payments, grouping Specialty wise
                          by= list(stringr::str_to_title(df_lawsuits$Specialty)),  
                          FUN=sum)

ggplot(data=df_speciality, aes(x=x, y=Group.1)) + geom_bar(stat="identity") + title("Speciality wise payments") + 
    xlab("Total Payments(in thousands)") + ylab("Speciality") 



#Marital Status wise payment

df_paySum_Maritalwise = aggregate(x= df_lawsuits$Payment,                  #getting sum of all payments, grouping Marital status wise
                                  by= list(df_lawsuits$`Marital Status`),  
                                  FUN=sum)

Marital_status=c("Divorced", "Single", "Married", "Widowed","Unknown")     #for assigning names of bar columns

ggplot(data=df_paySum_Maritalwise, aes(x=x, y=Marital_status)) + geom_bar(stat="identity") + title("Marital status wise payments") + 
    xlab("Total Payments(in thousands)") + ylab("Marital Status") 


#Correlations between numeric columns:

df_forScatterplot = data.frame(df_lawsuits$Payment,df_lawsuits$Severity,df_lawsuits$Age, df_lawsuits$`Private Attorney`, df_lawsuits$`Marital Status`)     #creating a new data set of all numeric column

plot(df_forScatterplot)    # scatter plots between all numeric column.

cor(df_forScatterplot$df_lawsuits..Private.Attorney.,df_forScatterplot$df_lawsuits.Payment)   # correlation between Attorney type and payments
cor(df_forScatterplot$df_lawsuits..Private.Attorney.,df_forScatterplot$df_lawsuits.Severity)  # correlation between Attorney type and Severity
cor(df_forScatterplot$df_lawsuits..Private.Attorney.,df_forScatterplot$df_lawsuits.Age)       # correlation between Attorney type and Age
cor(df_forScatterplot$df_lawsuits..Private.Attorney.,df_forScatterplot$df_lawsuits..Marital.Status.)   # correlation between Attorney type and Marital status


#Comparing Private/ Non private attorney Payments

df_paySum = aggregate(x= df_lawsuits$Payment,                      #getting sum of all payments, grouping Attorney type wise
                      by= list(df_lawsuits$`Private Attorney`),
                      FUN=sum)

ggplot(data=df_paySum, aes(y=x, x=Group.1)) + geom_bar(stat="identity") + title("Attorney wise payments") + 
    xlab("Non Private(0) - Private(1) Attorney") + ylab("Mean payments (in thousands of dollars) ") 


#Surgical Payments VS Non-Surgical payments

install.packages("dplyr")   
library(dplyr)             #Including dplyr for using filter function

df_Surge=df_lawsuits %>% filter(grepl('Surgeon|Surgery|Neurosurgery', Specialty))      # getting all rows where the terms Surgeon|Surgery|Neurosurgery are there in specialty column denotes they are all surgeries 
df_notSurge = df_lawsuits %>% filter(!grepl('Surgeon|Surgery|Neurosurgery', Specialty))  # getting all rows where the terms Surgeon|Surgery|Neurosurgery are not there in specialty column denotes they are just medical payments / non surgeries

df_paySurge =  aggregate(x= df_Surge$Payment, by=list(df_Surge$Specialty) ,FUN=sum)         # getting sum of all payments in surgical data
df_paynotSurge = aggregate(x= df_notSurge$Payment, by=list(df_notSurge$Specialty) ,FUN=sum)  # getting sum of all payments in non surgical data

barplot(as.matrix(rbind(sum(df_Surge$Payment), sum(df_notSurge$Payment))), beside=TRUE,
        ylim= c(0,60000),
        ylab = "Total Sum of Payments (in thousands of dollars)",
        xlab = "Surgical Or Non Surgical",
        names.arg = c("Surgical", "Non Surgical"),
        col=c(3,4),
        main = "Surgical Vs Non Surgical Payments")

sum(df_paySurge$x)       
sum(df_paynotSurge$x)


#Age wise Payments

plot(df_lawsuits$Age,df_lawsuits$Payment,    #Scatter plot for age wise payments 
     xlab = "Age",
     ylab = "Payment",
     col = "Red" ,
     main= "Age wise payments")

#Relation between payments from various type of insurance according to Marital Status of patients.

# Arranging data according to bins of size 25
hist(df_lawsuits$Age)
age_freq = c()
age_freq = c(age_freq, sum(df_lawsuits$Age < 25 ))
age_freq = c(age_freq, sum(df_lawsuits$Age > 25 &  df_lawsuits$Age < 50))
age_freq = c(age_freq, sum(df_lawsuits$Age > 50 &  df_lawsuits$Age < 75))
age_freq = c(age_freq,sum(df_lawsuits$Age > 75 ))

y = c("0-25", "25-50", "50-75" ,"75-100")

barplot(age_freq,
        xlab = "Age",
        ylab = "Number of payments",
        names.arg = y,
        col = c(3:6),
        ylim = c(0,60),
        main = "Number of insurance age wise",
       )

#Relation between payments from various type of insurance according to Marital Status of patients.

library(ggplot2)
ggplot(df_lawsuits, aes(fill=sort(df_lawsuits$`Marital Status`/4, decreasing = TRUE), y=Payment, x=stringr::str_to_title(Insurance)),
) +  xlab("Insurance")+ ylab("Total Payment")+
    geom_bar(position='stack', stat='identity') + labs(fill = "Marital Status")



#Relation between payments above $800,000 and Private/Non-Private attorney

df_PA= df_lawsuits[df_lawsuits$Payment >= 800,]    # Getting all rows whose payments is above $800 from lawsuit data and storing to df_PA
x = nrow(df_PA[df_PA$`Private Attorney` == 1,])    # From df_PA getting and storing number of rows where private attorney is = 1
y = nrow(df_PA[df_PA$`Private Attorney` != 1,])    # From df_PA getting and storing number of rows where private attorney is not = 1
z=c(x,y)                                           # Storing number of rows of private and non private attorney into a list 
names= c("Private attorney above $800,000", "Non-Private attorney below $800,000")  #for Giving names to 2 bar columns
barplot(z,
        xlab = "Attorney type ",
        ylab = "Count",
        names.arg = names,
        col = c(4,5),
        main = "Count of payments above $800,000 for Private/Non Private attorney"
        )




