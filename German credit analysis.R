#German Credit Analysis

#view number of rows and columns
dim(german_credit)

#view number of variables
names(german_credit)

#Structure of data
str(german_credit)
summary(german_credit)

#Data Handling/Data Cleaning

#viewing credibility variable
table(german_credit$Creditability)
sum(is.na(german_credit$Creditability))

#viewing Account balance
table(german_credit$`Account Balance`)
sum(is.na(german_credit$`Account Balance`))
boxplot(german_credit$`Account Balance`)

#viewing duration of credit month
summary(german_credit$`Duration of Credit (month)`)
quantile(german_credit$`Duration of Credit (month)`)
boxplot(german_credit$`Duration of Credit (month)`)

#outlier treatment
  
 IQR<-12
 german_credit1<-german_credit
 Bench<-18+3*IQR(german_credit1$`Duration of Credit (month)`)
 Bench

 mean<- 18
 summary(german_credit$`Duration of Credit (month)`)
 german_credit1$`Duration of Credit (month)`<-ifelse((german_credit1$`Duration of Credit (month)`)>Bench,mean,german_credit1$`Duration of Credit (month)`)
 
 
 summary(german_credit1$`Duration of Credit (month)`)
 boxplot(german_credit1$`Duration of Credit (month)`)
 
#viewing Payment status of previous credit
table(german_credit$`Payment Status of Previous Credit`)
sum(is.na(german_credit$`Payment Status of Previous Credit`))

#viewing of Purpose
table(german_credit$Purpose)
sum(is.na(german_credit$Purpose))

#viewing of Credit amount
summary(german_credit$`Credit Amount`)
quantile(german_credit$`Credit Amount`)
boxplot(german_credit$`Credit Amount`)

#outlier treatment
IQR<-3972-1366
bench<- 2320+3*IQR(german_credit$`Credit Amount`)
bench

mean<-3271
german_credit1$`Credit Amount`<- ifelse((german_credit1$`Credit Amount`)>bench,mean,german_credit1$`Credit Amount`)
german_credit1$`Credit Amount`
summary(german_credit1$`Credit Amount`)
boxplot(german_credit1$`Credit Amount`)


#viewing of value saving stock
table(german_credit$`Value Savings/Stocks`)
sum(is.na(german_credit$`Value Savings/Stocks`))

#viewing length of current employment
table(german_credit$`Length of current employment`)
sum(is.na(german_credit$`Length of current employment`))
boxplot(german_credit$`Length of current employment`)

#viewing Installment per cent
table(german_credit$`Instalment per cent`)
sum(is.na(german_credit$`Instalment per cent`))
boxplot(german_credit$`Instalment per cent`)

#viewing maritial status
table(german_credit$`Sex & Marital Status`)
sum(is.na(german_credit$`Sex & Marital Status`))

#viewing gurantors
table(german_credit$Guarantors)
sum(is.na(german_credit$Guarantors))

#viewing duration in current address
table(german_credit$`Duration in Current address`)
sum(is.na(german_credit$`Duration in Current address`))

#viewing most valuable asset available
table(german_credit$`Most valuable available asset`)
sum(is.na(german_credit$`Most valuable available asset`))

#viewing Age
summary(german_credit$`Age (years)`)
quantile(german_credit$`Age (years)`)
boxplot(german_credit$`Age (years)`)
sum(is.na(german_credit$`Age (years)`))

#outlier treatment
IQR<-42-27
IQR
bench<-35.54+2*IQR(german_credit$`Age (years)`)
bench

mean<-35.54
german_credit1$`Age (years)`<-ifelse((german_credit1$`Age (years)`)>bench,mean,german_credit1$`Age (years)`)
summary(german_credit1$`Age (years)`)
boxplot(german_credit1$`Age (years)`)

#viewing concurrent credits
table(german_credit$`Concurrent Credits`)
sum(is.na(german_credit$`Concurrent Credits`))

#viewing type of apartment
table(german_credit$`Type of apartment`)
sum(is.na(german_credit$`Type of apartment`))

#viewing no of credits at this bank
table(is.na(german_credit$`No of Credits at this Bank`))
sum(is.na(german_credit$`No of Credits at this Bank`))

#viewing occupations
table(german_credit$Occupation)
sum(is.na(german_credit$Occupation))

#viewing no of dependents
table(german_credit$`No of dependents`)
sum(is.na(german_credit$`No of dependents`))

#viewing telephone
table(german_credit$Telephone)
sum(is.na(german_credit$Telephone))

#viewing forign worker
table(german_credit$`Foreign Worker`)
sum(is.na(german_credit$`Foreign Worker`))



Logistic_Model<-glm(german_credit1$Creditability~.,family = binomial, data = german_credit1)
summary(Logistic_Model)


