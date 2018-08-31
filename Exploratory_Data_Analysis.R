#' ---
#' title: 'Labor Market Analysis'
#' author: 'From: Mansi Agarwal, 2nd TeamMember, 3rd TeamMember'
#' date: '`r format(Sys.time(), "%B %d, %Y")`'
#' output:
#'   md_document:
#'     variant: markdown_github 
#'     toc: true
#'     toc_depth: '6'
#' ---
#' ***
#' ## Code File Basics
#' 
#' ### Code Header
## ------------------------------------------------------------------------
# Course: ECON 5300
# Title: Labour Market Analysis 
# Purpose: 
# Research Question: 

#' 
#' ### Clear Environment and Load packages 
#' 
## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------
knitr::opts_chunk$set(echo = TRUE, tidy = TRUE)

# Clear working directory (remove all objects)
rm(list=ls(all=TRUE)) 

# Load packages
# Load packages
library(tidyverse)
library(gridExtra)
library(GGally)
library(knitr)
library(grid)
library(reshape)
library(reshape2)
library(psych)
library(corrplot)
library(lmtest)
library(sandwich)


#' 
#' ## Load Data to begin EDA
#' 
## ------------------------------------------------------------------------
# Import LMA data
MLD.Data <- read.csv("MLD Data File-1.csv", header = TRUE) 

MLD.Data$LOANPRC <- MLD.Data$LOANPRC * 100

MLD.Data <-  
  MLD.Data %>%
  mutate(WHITE = as.factor(ifelse(BLACK == 0 & HISPAN == 0, 1,0))) %>%
  mutate(RACE = as.factor(ifelse(BLACK == 1 , "Black",
                                 ifelse(HISPAN == 1, "Hispanic",
                                        ifelse(WHITE == 1 , "White",0))))) %>%
  filter(MALE %in% c(0,1)) %>%
  filter(GDLIN %in% c(0,1)) %>%
  filter(MARRIED %in% c(0,1)) 

MLD.Data.Full <- MLD.Data
            


#' 
#' ### Analyse the Structure & Summary of data
## ---- message=FALSE, warning=FALSE---------------------------------------

#Structure of the dataset
str(MLD.Data)

#Summary of the data set ( i.e. Descriptive Statistics of the data)
summary(MLD.Data)

# Check the details of different variables

W<-
MLD.Data %>%
  filter(RACE == "White")

H<-
MLD.Data %>%
  filter(RACE == "Hispanic")

B<-
MLD.Data %>%
  filter(RACE == "Black")

summary(W)
describe(W)
1101/(1101 + 565)


summary(H)
describe(H)

77/(77+31)

summary(B)
describe(B)
120/(120+75)


#' Observations :  
#' 
#' 1. Majority of the individuals applied for loan in our sample data are Male.
#' 2. 87% of the total number of loans in our sample data were approved
#' 3. Majority of the individuals in our sample data are Non-Hispanic White.
#' 4. Maximum value of LOANPRC = 2.57, i.e.someone applied for loan amount that is 250% of the purchase price
#' 
#' 
#' ### Analyse the Visual Summary of data
#' 
## ---- fig.width=13, fig.height=6-----------------------------------------
# Visualize numerical variables

par(mfrow=c(2,2))
  hist(MLD.Data$OBRAT, main = "Histogram of Other Obligations as a percent of total income")
  hist(MLD.Data$LOANPRC, main = "Histogram of Loan Amount")
  hist(log(MLD.Data$OBRAT),   main = "Histogram of Log(OBRAT)")
  hist(log(MLD.Data$LOANPRC), main = "Histogram of log(LOANPRC)")
  

#' 
#' Observations :
#' 
#' 1. LoanPRC and other obligation(i.e. OBRAT)  have left skewed distribution. 
#' 
#' ### Data cleaning and Sample selection
#' 
## ------------------------------------------------------------------------
MLD.Data <-
MLD.Data %>%
  filter(LOANPRC <= 120) 
  #filter(OBRAT < 50)

W<-
MLD.Data %>%
  filter(RACE == "White")

H<-
MLD.Data %>%
  filter(RACE == "Hispanic")

B<-
MLD.Data %>%
  filter(RACE == "Black")

summary(W)
describe(W)

1077/(1077+551)

summary(H)
describe(H)
74/(29+74)

summary(B)
describe(B)
116/(116+69)



#' 
#' ### How different variables are Correlated?
#' 
## ---- message=FALSE, warning=FALSE---------------------------------------
MLD.Data %>% ggpairs()

#' 
#' ### Loan Approval Status by Gender
#' 
## ---- message=FALSE, warning=FALSE, fig.width=13, fig.height=5-----------
L1<-
MLD.Data %>%
  filter(APPROVE == 1) %>%
  group_by(RACE,MALE) %>%
  summarise(count = n()) %>%
    ggplot(aes(x=reorder(RACE, count), y=count, fill=as.factor(MALE))) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = count) , size = 5, vjust = -0.1, position = position_dodge(0.9)) +
    theme_classic() +
    ggtitle("Loan Approved by Gender") +
    xlab("Race") +
    ylab("Count of Individuals") +
    scale_fill_discrete(name="Gender", breaks=c("0", "1"), labels=c("Female", "Male"))
    theme(axis.text.x = element_text(angle = 20, hjust = 1, vjust = 1)) +
    theme(plot.title = element_text(size = 13, face = "bold", hjust= 0.5)) + 
    theme(axis.title = element_text(size = 12), axis.text = element_text(size = 10))
 
L2<-       
MLD.Data %>%
  filter(APPROVE == 0) %>%
  group_by(RACE,MALE) %>%
  summarise(count = n()) %>%
    ggplot(aes(x=reorder(RACE, count), y=count, fill=as.factor(MALE))) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = count) , size = 5, vjust = -0.1, position = position_dodge(0.9)) +
    theme_classic() +
    ggtitle("Loan Not Approved by Gender") +
    xlab("Race") +
    ylab("Count of Individuals") +
    scale_fill_discrete(name="Gender", breaks=c("0", "1"), labels=c("Female", "Male"))
    theme(axis.text.x = element_text(angle = 20, hjust = 1, vjust = 1)) +
    theme(plot.title = element_text(size = 13, face = "bold", hjust= 0.5)) + 
    theme(axis.title = element_text(size = 12), axis.text = element_text(size = 10))
  
grid.arrange(L1,L2, nrow=1) 


#' 
#' ### Percentage of Approvals by Race
#' 
## ---- message=FALSE, warning=FALSE, fig.width=13, fig.height=5-----------
 

L3<-
MLD.Data %>%
  group_by(RACE,APPROVE) %>%
  summarise(count = n()) %>%
  group_by(RACE)%>%
  mutate( Proportion = round(count/sum(count),3) ) %>%
    ggplot(aes(x= RACE, y=Proportion, fill=as.factor(APPROVE))) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = scales::percent(Proportion)) , size = 5, vjust = -0.1, position = position_dodge(0.9)) +
    theme_classic() +
    ggtitle("%age of Loans Approved across each race") +
    xlab("Race") +
    ylab("%age of Individuals") +
    scale_fill_discrete(name="Approval Status", breaks=c("0", "1"), labels=c("Not Approved", "Approved")) +
    theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1)) +
    theme(plot.title = element_text(size = 13, face = "bold", hjust= 0.5)) + 
    theme(axis.title = element_text(size = 12), axis.text = element_text(size = 10))

L4<- 
MLD.Data %>%
  group_by(RACE,APPROVE) %>%
  summarise(count = n()) %>%
    ggplot(aes(x= RACE, y=count, fill=as.factor(APPROVE))) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = count) , size = 5, vjust = -0.1, position = position_dodge(0.9)) +
    theme_classic() +
    ggtitle("Number of Loans Approved across each race") +
    xlab("Race") +
    ylab("Count of Individuals") +
    scale_fill_discrete(name="Approval Status", breaks=c("0", "1"), labels=c("Not Approved", "Approved")) +
    theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1)) +
    theme(plot.title = element_text(size = 13, face = "bold", hjust= 0.5)) + 
    theme(axis.title = element_text(size = 12), axis.text = element_text(size = 10))

grid.arrange(L3,L4, nrow=1)


#' 
#' Observations:
#' 
#' 1. 92.6% of the loans applied by White individuals were approved. However, only 70.5% of the loans applied by Black individuals were approved.
#' 
#' ### Percentage of Approved by Race and Married
#' 
## ----fig.width=13, fig.height=5------------------------------------------

MLD_race_married <- MLD.Data %>% group_by(RACE,MARRIED) %>% 
                      summarise(Total_Number_Married = n())

MLD_race <- MLD.Data %>% group_by(RACE) %>% 
              summarise(Total_Number = n(),
                        Percentage   = paste( (round(((Total_Number/nrow(MLD.Data)) * 100 ), 2)), "%"))


MLD_race_married <- MLD_race_married %>% 
                      inner_join( (MLD_race %>% select(Total_Number,RACE)), by="RACE") %>%
                      mutate(Approve_Percentage = round(((Total_Number_Married/Total_Number)*100), 1)) 

MLD_race_app_mar <- MLD.Data %>% group_by(RACE,MARRIED, APPROVE) %>% 
                      summarise(Total_Number_App_Mar = n())

MLD_race_app_mar <- MLD_race_app_mar %>% 
                      inner_join( (MLD_race_married %>% select(Total_Number_Married, RACE, MARRIED)), by=c("RACE", "MARRIED")) %>%
                      mutate(Approve_Percentage = round(((Total_Number_App_Mar/Total_Number_Married)*100), 1),
                             Key = paste(MARRIED,"_",APPROVE, sep = "")) 


# Plot MLD Approve Percentage by Race
MLD_race_app_mar %>%
  ggplot(aes(x = as.factor(Key), y = Approve_Percentage, fill = RACE)) + 
  geom_bar(stat = "identity", position = "dodge") +
  ylab("Percentage") +
  ggtitle("Married Percentage by Race") + theme_classic() +
  geom_text(aes(label = paste(round(Approve_Percentage, 1), "%", sep="")), vjust=-0.3, colour="black", position = position_dodge(width = .9),  size = 5) +
  theme(plot.title = element_text(size = 15, face = "bold", hjust= 0.5)) + 
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12)) + 
  theme(axis.title.x = element_blank()) + 
  scale_x_discrete(labels = c("0_0" = "Single_Not Approved","0_1" = "Single_Approved", "1_0" = "Married_Not Approved", "1_1" = "Married_Approved"))
  

#' 
#' Observations:
#' 
#' 1. Across all races, more number of loans are approved for married individuals in comparision to un-married individuals.
#' 
#' 
## ---- message=FALSE, warning=FALSE,fig.width=15, fig.height=5------------
L5<-
MLD.Data %>%
  group_by(RACE,GDLIN,APPROVE) %>%
  summarise(count = n()) %>%
  mutate(Status = as.factor(ifelse(GDLIN == 0 & APPROVE == 0, "No_CRworthy_Loan_NotA", 
                            ifelse(GDLIN == 0 & APPROVE == 1, "No_CRworthy_Loan_A",
                            ifelse(GDLIN == 1 & APPROVE == 0, "CRworthy_Loan_NotA", 
                            ifelse(GDLIN == 1 & APPROVE == 1, "CRworthy_Loan_A",0)))))) %>%
    ggplot(aes(x= RACE, y=count, fill=Status)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = count) , size = 5, vjust = -0.1, position = position_dodge(0.9)) +
    theme_classic() +
    ggtitle("Loan Status: Number of Individuals, based on GDLIN & APPROVE") +
    xlab("Race") +
    ylab("Count of Individuals") +
    #scale_fill_discrete(name="Approval Status", breaks=c("0", "1"), labels=c("Not Approved", "Approved")) +
    theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1)) +
    theme(plot.title = element_text(size = 13, face = "bold", hjust= 0.5)) + 
    theme(axis.title = element_text(size = 12), axis.text = element_text(size = 10))

  
L6<-
MLD.Data %>%
  group_by(RACE,GDLIN,APPROVE) %>%
  mutate(Status = as.factor(ifelse(GDLIN == 0 & APPROVE == 0, "No_CRworthy_Loan_NotA", 
                            ifelse(GDLIN == 0 & APPROVE == 1, "No_CRworthy_Loan_A",
                            ifelse(GDLIN == 1 & APPROVE == 0, "CRworthy_Loan_NotA", 
                            ifelse(GDLIN == 1 & APPROVE == 1, "CRworthy_Loan_A",0)))))) %>%
  group_by(RACE,Status) %>%
  summarise(count = n()) %>%
  group_by(RACE) %>%
  mutate( Proportion = round(count/sum(count),3) ) %>%
    ggplot(aes(x= RACE, y=Proportion, fill=Status)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = scales::percent(Proportion)) , size = 5, vjust = -0.1, position = position_dodge(0.9)) +
    theme_classic() +
    ggtitle("Loan Status: %age of Individuals, based on GDLIN & APPROVE") +
    xlab("Race") +
    ylab("%age of Individuals") +
    #scale_fill_discrete(name="Approval Status", breaks=c("0", "1"), labels=c("Not Approved", "Approved")) +
    theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1)) +
    theme(plot.title = element_text(size = 13, face = "bold", hjust= 0.5)) + 
    theme(axis.title = element_text(size = 12), axis.text = element_text(size = 10)) + guides(fill=FALSE)

grid.arrange(L5,L6, nrow=1)

#' 
#' Observations:
#' 
#' 1. Loans are highly likely to be approved if individuals are credit worthy.
#' 
#' 2. For Black individuals, 1.9% of the loans are approved even though they are not credit worthy. However, for white individuals only 1.4% of the loans are approved if individuals are not credit worthy.
#' 
## ---- message=FALSE, warning=FALSE,fig.width=15, fig.height=5------------
L7<-
MLD.Data %>%
  group_by(APPROVE,MALE) %>%
  summarise(count = n()) %>%
    ggplot(aes(x= MALE, y=count, fill=as.factor(APPROVE))) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = count) , size = 5, vjust = -0.1, position = position_dodge(0.9)) +
    theme_classic() +
    ggtitle("Number of loans approved based on Gender & Race") +
    xlab("Gender") +
    ylab("Count of Individuals") +
    scale_fill_discrete(name="Approval Status", breaks=c("0", "1"), labels=c("Not Approved", "Approved")) +
    theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1)) +
    theme(plot.title = element_text(size = 13, face = "bold", hjust= 0.5)) + 
    theme(axis.title = element_text(size = 12), axis.text = element_text(size = 10))

  
L8<-
MLD.Data %>%
  group_by(MALE,APPROVE,RACE) %>%
  mutate(Status = as.factor(ifelse(MALE == 0 & APPROVE == 0, "Female_Loan_NotA", 
                            ifelse(MALE == 0 & APPROVE == 1, "Female_Loan_A",
                            ifelse(MALE == 1 & APPROVE == 0, "Male_Loan_NotA", 
                            ifelse(MALE == 1 & APPROVE == 1, "Male_Loan_A",0)))))) %>%
  group_by(RACE,Status) %>%
  summarise(count = n()) %>%
  group_by(RACE) %>%
  mutate( Proportion = round(count/sum(count),3) ) %>%
    ggplot(aes(x= RACE, y=Proportion, fill=Status)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = scales::percent(Proportion)) , size = 5, vjust = -0.1, position = position_dodge(0.9)) +
    theme_classic() +
    ggtitle("%age of loans approved, based on Gender & Race") +
    xlab("Race") +
    ylab("%age of Individuals") +
    #scale_fill_discrete(name="Approval Status", breaks=c("0", "1"), labels=c("Not Approved", "Approved")) +
    theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1)) +
    theme(plot.title = element_text(size = 13, face = "bold", hjust= 0.5)) + 
    theme(axis.title = element_text(size = 12), axis.text = element_text(size = 10)) 



grid.arrange(L7,L8, nrow=1)

#' 
#' Observations:
#' 
#' 1. Among Whites, 75.8% of the loans that were approved belonged to Males.
#' 
#' ### Approval status wise loan applications 
## ----echo = FALSE, include = TRUE,fig.width=12, fig.height=12------------


# convert the continuous variable OBRAT to categorical variable with 6 different categories
MLD <- MLD.Data %>% mutate(Other.obligation=                 
               cut(MLD.Data$OBRAT,   
                  breaks=c(-100, 0,20,40,60,80,100),    
                  labels=c("No other obligation","upto 20","between 20 and 40","between 40 and 60","between 60 and 80","above 80")))

# convert the continuous variable LOANPRC to categorical variable with 6 different categories
MLD<- MLD %>% mutate(loan_to_price=                 
               cut(MLD$LOANPRC,   
                  breaks=c( 0,.20,.40,.60,.80,1),    
                  labels=c("upto 20","between 20 and 40","between 40 and 60","between 60 and 80","above 80")))
grid.arrange(

MLD %>%
    group_by(loan_to_price,APPROVE) %>%
  summarise(count = n(),proportion=count/nrow(MLD),percentage=round((proportion*100),2))%>%
    ggplot(aes(x=(loan_to_price), y=percentage,fill=factor(APPROVE))) +
    geom_bar(stat = "identity", position = "dodge") +
    guides(fill=FALSE) +
    geom_text(aes(label = percentage) , size = 5, vjust = -0.1, position = position_dodge(0.9)) +
    theme_classic() +
    ggtitle("Unapproved/Approved loan applications based on the loan amount to price ratio") +
    xlab("Loan to price ratio") +
    ylab("Percent of total Loan Applications") +
    theme(axis.text.x = element_text(angle = 20, hjust = 1, vjust = 1)) +
    theme(plot.title = element_text(size = 13, face = "bold", hjust= 0.5)) + 
    theme(axis.title = element_text(size = 17), axis.text = element_text(size = 15)),

MLD %>%
    group_by(Other.obligation,APPROVE) %>%
  summarise(count = n(),proportion=count/nrow(MLD),percentage=round((proportion*100),2))%>%
    ggplot(aes(x=(Other.obligation), y=percentage,fill=factor(APPROVE))) +
    geom_bar(stat = "identity", position = "dodge") +
    guides(fill=FALSE) +
    geom_text(aes(label = percentage) , size = 5, vjust = -0.05, position = position_dodge(0.9)) +
    theme_classic() +
    ggtitle("Unapproved/Approved loan applications based on the other obligation as % of earnings") +
    xlab("other obligations as % of earnings") +
    ylab("Percent of total Loan Applications") +
    theme(axis.text.x = element_text(angle = 20, hjust = 1, vjust = 1)) +
    theme(plot.title = element_text(size = 13, face = "bold", hjust= 0.5)) + 
    theme(axis.title = element_text(size = 17), axis.text = element_text(size = 15)),



nrow=2
)

#' 
#' 
#' ### Percentage wise distribution of Total Unapproved but qualified mortgages 
## ----echo = FALSE, include = TRUE,fig.width=13,fig.height=7--------------

# Looking at the data where the applicant's are meeting the basic guidelines
MLD.meets.basic.criteria<-MLD %>% filter(MLD$GDLIN==1)

# checking for applicants who are meeting the guidelines but still have unapproved loans
Nonapproved.meets<-MLD.meets.basic.criteria %>% filter(MLD.meets.basic.criteria$APPROVE==0)

grid.arrange(
Nonapproved.meets %>%
  group_by(RACE,MALE) %>%
  summarise(count = n(),proportion=count/nrow(Nonapproved.meets),percetage=round(proportion*100,2))%>%
    ggplot(aes(x=reorder(RACE,percetage), y=count,fill=MALE)) +
    geom_bar(stat = "identity", position = "dodge") +
    #guides(fill=FALSE) +
    geom_text(aes(label = percetage) , size = 5, vjust = -0.1, position = position_dodge(0.9)) +
    theme_classic() +
    ggtitle("% of Unapproved but qualified \n mortgage applications by Race") +
    xlab("Race") +
    ylab("Percentage of unapproved applications") +
    theme(axis.text.x = element_text(angle = 20, hjust = 1, vjust = 1)) +
    theme(plot.title = element_text(size = 13, face = "bold", hjust= 0.5)) + 
    theme(axis.title = element_text(size = 12), axis.text = element_text(size = 10)),

Nonapproved.meets %>%
  group_by(RACE,MARRIED) %>%
  summarise(count = n(),proportion=count/nrow(Nonapproved.meets),percetage=round(proportion*100,2))%>%
    ggplot(aes(x=reorder(RACE,percetage), y=count,fill=MARRIED)) +
    geom_bar(stat = "identity", position = "dodge") +
    #guides(fill=FALSE) +
    geom_text(aes(label = percetage) , size = 5, vjust = -0.1, position = position_dodge(0.9)) +
    theme_classic() +
    ggtitle("% of Unapproved but qualified \n mortgage applications by Race") +
    xlab("Race") +
    ylab("Percentage of unapproved applications") +
    theme(axis.text.x = element_text(angle = 20, hjust = 1, vjust = 1)) +
    theme(plot.title = element_text(size = 13, face = "bold", hjust= 0.5)) + 
    theme(axis.title = element_text(size = 12), axis.text = element_text(size = 10)),

nrow=1
)

#' 
#' ### Distribution of unapproved applicants meeting guidelines
#' 
## ----echo = FALSE, include = TRUE----------------------------------------
grid.arrange(
Nonapproved.meets %>%
  group_by(Other.obligation) %>%
  summarise(count = n())%>%
    ggplot(aes(x=(Other.obligation), y=count))+#,fill=factor(MARRIED))) +
    geom_bar(stat = "identity", position = "dodge") +
    #guides(fill=FALSE) +
    geom_text(aes(label = count) , size = 5, vjust = -0.1, position = position_dodge(0.9)) +
    theme_classic() +
    ggtitle("Unapproved but qualified \n mortgages based on the other obligations") +
    xlab("Other Obligations") +
    ylab("Count of applications") +
    theme(axis.text.x = element_text(angle = 20, hjust = 1, vjust = 1)) +
    theme(plot.title = element_text(size = 13, face = "bold", hjust= 0.5)) + 
    theme(axis.title = element_text(size = 12), axis.text = element_text(size = 10)),

Nonapproved.meets %>%
  group_by(loan_to_price) %>%
  summarise(count = n())%>%
    ggplot(aes(x=(loan_to_price), y=count))+#,fill=factor(MARRIED))) +
    geom_bar(stat = "identity", position = "dodge") +
    #guides(fill=FALSE) +
    geom_text(aes(label = count) , size = 5, vjust = -0.1, position = position_dodge(0.9)) +
    theme_classic() +
    ggtitle("Qualified but unapproved \n mortgages based on the loan to price ratio") +
    xlab("Loan to Price ratio") +
    ylab("Count of applications") +
    theme(axis.text.x = element_text(angle = 20, hjust = 1, vjust = 1)) +
    theme(plot.title = element_text(size = 13, face = "bold", hjust= 0.5)) + 
    theme(axis.title = element_text(size = 12), axis.text = element_text(size = 10)),

nrow=1
)

#' 
#' As expected,most of the unapproved loan applications have very high loan to price ratio and that might be the reason for rejection of the loans. Also the number of unapproved loans with low loan to price ratio are very less in number.
#' 
#' Strangely though, large number of unapproved loans belong to the category where other obligations are between 20 to 40 % of the total earnings. There are 21 applicants who have other obligations that will consume between 40 to 60% of the applicantws earning and it makes sense to reject those applications.
#' 
#' 
## ---- message=FALSE, warning=FALSE,fig.width=15, fig.height=5------------
LG<-
MLD.Data %>%
  filter(APPROVE == 1) %>%
  mutate(Loan_Group = cut(LOANPRC, 4)) %>%
  count(Loan_Group, RACE) %>%
  group_by(RACE) %>%
  mutate( Proportion = round(n/sum(n),3) ) %>%
   ggplot(aes(x = RACE, y = Proportion, fill = Loan_Group)) +
   geom_bar(stat="identity", position = "dodge") +
   geom_text(aes(label = scales::percent(Proportion)) , size = 5, vjust = -0.1, position = position_dodge(0.9)) +
   theme_classic() +
   ggtitle("Loan Approved: Among LOANPRC groups") +
   xlab("Race") +
   ylab("%age of Individuals")

OBG<-
MLD.Data %>%
  filter(APPROVE == 1) %>%
  mutate(Other_Obligation_Group = cut(OBRAT, 4)) %>%
  count(Other_Obligation_Group, RACE) %>%
  group_by(RACE) %>%
  mutate( Proportion = round(n/sum(n),3) ) %>%
   ggplot(aes(x = RACE, y = Proportion, fill = Other_Obligation_Group)) +
   geom_bar(stat="identity", position = "dodge") +
   geom_text(aes(label = scales::percent(Proportion)) , size = 5, vjust = -0.1, position = position_dodge(0.9)) +
   theme_classic() +
   ggtitle("Loan Approved:Among Other Obligations groups ") +
   xlab("Race") +
   ylab("%age of Individuals")
  
grid.arrange(LG,OBG, nrow=1)

# LogitModelw = glm(APPROVE ~ OBRAT +  LOANPRC + MARRIED + GDLIN, data=subset(MLD.Data, RACE == "White"), 
#                  family = "binomial")
# summary(LogitModelw)
# 
# LogitModelB = glm(APPROVE ~ OBRAT +  LOANPRC + MARRIED + GDLIN , data=subset(MLD.Data, RACE == "Black"), 
#                  family = "binomial")
# summary(LogitModelB)
# 
# LogitModelH = glm(APPROVE ~ OBRAT +  LOANPRC + MARRIED + GDLIN, data=subset(MLD.Data, RACE == "Hispanic"), 
#                  family = "binomial")
# summary(LogitModelH)



#' 
#' 
## ------------------------------------------------------------------------
library(aod)
library(ggplot2)
library(Rcpp)

LogitModel = glm(MALE ~ OBRAT +  LOANPRC + MARRIED + GDLIN + RACE, data=MLD.Data, 
                 family = "binomial")

LogitModel = glm(APPROVE ~ OBRAT +  LOANPRC + MARRIED + GDLIN + BLACK + HISPAN, data=MLD.Data, 
                 family = "binomial")
summary(LogitModel)

#Generate Log-Likelihood
logLik(LogitModel)

#Generate Odds Ratios
cbind(exp(coef(LogitModel)),1)

#Define prototypical loan applicants (you will need more than 3)
prototype1 <- data.frame(OBRAT=mean(MLD.Data.Full$OBRAT), MARRIED = "1", LOANPRC=mean(MLD.Data.Full$LOANPRC), GDLIN = 1, BLACK = 0, HISPAN = 0 )
prototype2 <- data.frame(OBRAT=mean(MLD.Data.Full$OBRAT), MARRIED = "1", LOANPRC=mean(MLD.Data.Full$LOANPRC), GDLIN = 1, BLACK = 0, HISPAN = 1 )
prototype3 <- data.frame(OBRAT=mean(MLD.Data.Full$OBRAT), MARRIED = "1", LOANPRC=mean(MLD.Data.Full$LOANPRC), GDLIN = 1, BLACK = 1, HISPAN = 0 )

prototype4 <- data.frame(OBRAT=mean(MLD.Data.Full$OBRAT), MARRIED = "1", LOANPRC=mean(MLD.Data.Full$LOANPRC), GDLIN = 0, BLACK = 0, HISPAN = 0 )
prototype5 <- data.frame(OBRAT=mean(MLD.Data.Full$OBRAT), MARRIED = "1", LOANPRC=mean(MLD.Data.Full$LOANPRC), GDLIN = 0, BLACK = 0, HISPAN = 1 )
prototype6 <- data.frame(OBRAT=mean(MLD.Data.Full$OBRAT), MARRIED = "1", LOANPRC=mean(MLD.Data.Full$LOANPRC), GDLIN = 0, BLACK = 1, HISPAN = 0 )

prototype7 <- data.frame(OBRAT=mean(MLD.Data.Full$OBRAT), MARRIED = "0", LOANPRC=mean(MLD.Data.Full$LOANPRC), GDLIN = 1, BLACK = 0, HISPAN = 0 )
prototype8 <- data.frame(OBRAT=mean(MLD.Data.Full$OBRAT), MARRIED = "0", LOANPRC=mean(MLD.Data.Full$LOANPRC), GDLIN = 1, BLACK = 0, HISPAN = 1 )
prototype9 <- data.frame(OBRAT=mean(MLD.Data.Full$OBRAT), MARRIED = "0", LOANPRC=mean(MLD.Data.Full$LOANPRC), GDLIN = 1, BLACK = 1, HISPAN = 0 )

prototype10 <- data.frame(OBRAT=mean(MLD.Data.Full$OBRAT), MARRIED = "0", LOANPRC=mean(MLD.Data.Full$LOANPRC), GDLIN = 0, BLACK = 0, HISPAN = 0 )
prototype11 <- data.frame(OBRAT=mean(MLD.Data.Full$OBRAT), MARRIED = "0", LOANPRC=mean(MLD.Data.Full$LOANPRC), GDLIN = 0, BLACK = 0, HISPAN = 1 )
prototype12 <- data.frame(OBRAT=mean(MLD.Data.Full$OBRAT), MARRIED = "0", LOANPRC=mean(MLD.Data.Full$LOANPRC), GDLIN = 0, BLACK = 1, HISPAN = 0 )

#Predict probabilities for prototypical individuals
prototype1$predictedprob <- predict (LogitModel, newdata = prototype1, type ="response")
prototype2$predictedprob <- predict (LogitModel, newdata = prototype2, type ="response")
prototype3$predictedprob <- predict (LogitModel, newdata = prototype3, type ="response")

prototype4$predictedprob <- predict (LogitModel, newdata = prototype4, type ="response")
prototype5$predictedprob <- predict (LogitModel, newdata = prototype5, type ="response")
prototype6$predictedprob <- predict (LogitModel, newdata = prototype6, type ="response")

prototype7$predictedprob <- predict (LogitModel, newdata = prototype7, type ="response")
prototype8$predictedprob <- predict (LogitModel, newdata = prototype8, type ="response")
prototype9$predictedprob <- predict (LogitModel, newdata = prototype9, type ="response")

prototype10$predictedprob <- predict (LogitModel, newdata = prototype10, type ="response")
prototype11$predictedprob <- predict (LogitModel, newdata = prototype11, type ="response")
prototype12$predictedprob <- predict (LogitModel, newdata = prototype12, type ="response")


rbind.data.frame(
prototype1,
prototype2,
prototype3,
prototype4,
prototype5,
prototype6,
prototype7,
prototype8,
prototype9,
prototype10,
prototype11,
prototype12)

#Estimate Probit Model

ProbitModel = glm(APPROVE ~ OBRAT +  LOANPRC + MARRIED + GDLIN + BLACK + HISPAN, data=MLD.Data, 
                 family = "binomial" (link = "probit"))
summary(ProbitModel)

#Generate Log-Likelihood
logLik(ProbitModel)

#Predict probabilities for prototypical individuals
prototype1$predictedprob <- predict (ProbitModel, newdata = prototype1, type ="response")
prototype2$predictedprob <- predict (ProbitModel, newdata = prototype2, type ="response")
prototype3$predictedprob <- predict (ProbitModel, newdata = prototype3, type ="response")

prototype4$predictedprob <- predict (ProbitModel, newdata = prototype4, type ="response")
prototype5$predictedprob <- predict (ProbitModel, newdata = prototype5, type ="response")
prototype6$predictedprob <- predict (ProbitModel, newdata = prototype6, type ="response")

prototype7$predictedprob <- predict (ProbitModel, newdata = prototype7, type ="response")
prototype8$predictedprob <- predict (ProbitModel, newdata = prototype8, type ="response")
prototype9$predictedprob <- predict (ProbitModel, newdata = prototype9, type ="response")

prototype10$predictedprob <- predict (ProbitModel, newdata = prototype10, type ="response")
prototype11$predictedprob <- predict (ProbitModel, newdata = prototype11, type ="response")
prototype12$predictedprob <- predict (ProbitModel, newdata = prototype12, type ="response")

rbind.data.frame(
prototype1,
prototype2,
prototype3,
prototype4,
prototype5,
prototype6,
prototype7,
prototype8,
prototype9,
prototype10,
prototype11,
prototype12)

#Predited Probability (Logit MOdel)
1 / (1 + exp(-(0.980786 - 0.027265 * (mean(MLD.Data.Full$OBRAT))  - 0.016264 * (mean(MLD.Data.Full$LOANPRC)) + 0.487474 + 3.854172 )))
1 / (1 + exp(-(0.980786 - 0.027265 * (mean(MLD.Data.Full$OBRAT))  - 0.016264 * (mean(MLD.Data.Full$LOANPRC)) + 0.487474 + 3.854172 - 0.901495)))
1 / (1 + exp(-(0.980786 - 0.027265 * (mean(MLD.Data.Full$OBRAT))  - 0.016264 * (mean(MLD.Data.Full$LOANPRC)) + 0.487474 + 3.854172 - 0.720995 )))

#Predited Probability (Probit MOdel)
(0.300495 - 0.012563 * (mean(MLD.Data.Full$OBRAT))  - 0.007673 * (mean(MLD.Data.Full$LOANPRC)) + 0.239988 + 2.220218 ) #1.762697  0.961
(0.300495 - 0.012563 * (mean(MLD.Data.Full$OBRAT))  - 0.007673 * (mean(MLD.Data.Full$LOANPRC)) + 0.239988 + 2.220218 - 0.464631) #1.298066 0.903
(0.300495 - 0.012563 * (mean(MLD.Data.Full$OBRAT))  - 0.007673 * (mean(MLD.Data.Full$LOANPRC)) + 0.239988 + 2.220218 - 0.377086 ) #1.385611   0.917


1 - 0.00604 -  10 * (((0.6^4)/4)  + ((3 * (0.6^8))/4) - ((2 * (0.6^6))/3) - ((2 * (0.6^10))/5) + ((0.6^12)/12))


#' 
