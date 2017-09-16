#
getwd()
setwd("D:\\achi\\R-DS")
getwd()
FullData <- read.csv("Log-Reg-Case-Study.csv")
FullData
#View(FullData)
str(FullData)

set.seed(123)
?set.seed

train = sample(1:nrow(FullData), nrow(FullData)*.7)
sample(train)
test = -train
sample(test)
ModelData <- FullData[train,]
ValidationData <- FullData[test,]

#####################
#Univeriate Analysis#
#####################

summary(ModelData)

#Outlier Identification
?quantile
quantile(ModelData$Age, 0.995)
quantile(ModelData$Age, .999)

#Outlier Capping
summary(ModelData$Age)
ModelData$Age <- ifelse(ModelData$Age>75, 75, ModelData$Age)
summary(ModelData$Age)

#Missing Value Imputation
summary(ModelData$Housing)
ModelData$Housing[ModelData$Housing == ""] <- "A152"
summary(ModelData$Housing)


###  Step2
### Bivariate Analysis#
#######################

#Variable Reduction
table(ModelData$Num_Dependents, ModelData$Default_On_Payment)
aggregate(ModelData$Num_Dependents, by=list(ModelData$Num_Dependents, ModelData$Default_On_Payment), length)

#Missing Value Imputation
table(ModelData$Job_Status, ModelData$Default_On_Payment)
aggregate(ModelData$Job_Status, by=list(ModelData$Job_Status, ModelData$Default_On_Payment), length)
ModelData$Job_Status[ModelData$Job_Status == "" ] <- "A174"
#Category grouping for dummy variable creation
#Set 1 - Convert categorical variables to dummy variables

#Dummy Variables for job Status
table(ModelData$Job_Status, ModelData$Default_On_Payment)
ModelData$Dummy_Job_status_A171 <- ifelse(ModelData$Job_Status == "A171", 1,0)
ModelData$Dummy_Job_status_A172 <- ifelse(ModelData$Job_Status == "A172", 1,0)
ModelData$Dummy_Job_status_A173 <- ifelse(ModelData$Job_Status == "A173", 1,0)

#Dummy variables for the purpose of credit taken
table(ModelData$Purpose_Credit_Taken, ModelData$Default_On_Payment)
ModelData$Dummy_Purpose_Credit_Taken_Low <- ifelse(ModelData$Purpose_Credit_Taken == "P41" | 
                                                     ModelData$Purpose_Credit_Taken == "P43" |
                                                     ModelData$Purpose_Credit_Taken == "P48", 1,0)

ModelData$Dummy_Purpose_Credit_Taken_High <- ifelse(ModelData$Purpose_Credit_Taken == "P49" | 
                                                      ModelData$Purpose_Credit_Taken == "P40" | 
                                                      ModelData$Purpose_Credit_Taken == "P45" | 
                                                      ModelData$Purpose_Credit_Taken == "P46" | 
                                                      ModelData$Purpose_Credit_Taken == "P50", 1,0)


#Dummy variables for Status_checking_Accnt
table(ModelData$Status_Checking_Accnt, ModelData$Default_On_Payment)
ModelData$Dummy_Status_Checking_Accnt_Low <- ifelse(ModelData$Status_Checking_Accnt == "S11", 1,0)
ModelData$Dummy_status_Checking_Accnt_High <- ifelse(ModelData$Status_Checking_Accnt == "S12", 1,0)

#Dummy Variables for credit history
t = table(ModelData$Credit_History, ModelData$Default_On_Payment)
t
#prop.table(t, 1)
ModelData$Dummy_Credit_History_High <- ifelse(ModelData$Credit_History == "A30" |
                                                ModelData$Credit_History == "A31", 1,0)
ModelData$Dummy_Credit_History_Low <- ifelse(ModelData$Credit_History == "A34",1,0)

#Dummy variables for Years at present employment
table(ModelData$Years_At_Present_Employment, ModelData$Default_On_Payment)
ModelData$Dummy_Years_At_Present_Employment_High <- ifelse( ModelData$Years_At_Present_Employment == "E71" | 
                                                              ModelData$Years_At_Present_Employment == "E72", 1,0)
ModelData$Dummy_Years_At_Present_Employment_Medium <- ifelse(ModelData$Years_At_Present_Employment == "E73",1,0)

#Dummy variables for Marital Status Gender
table(ModelData$Marital_Status_Gender, ModelData$Default_On_Payment)
ModelData$Dummy_Marital_status_Gender <- ifelse(ModelData$Marital_Status_Gender == "A91" |
                                                  ModelData$Marital_Status_Gender == "A92", 1,0)

#Dummy variables for Other Debters Gurantors
table(ModelData$Other_Debtors_Guarantors, ModelData$Default_On_Payment)
ModelData$Dummy_Other_Debtors_Guarantors <- ifelse(ModelData$Other_Debtors_Guarantors == "A103", 1,0)

#Dummy Variabls for Housing
table(ModelData$Housing, ModelData$Default_On_Payment)
ModelData$Dummy_Housing <- ifelse(ModelData$Housing == "A152",1,0)

#Dummy variales for Foreign worker
table(ModelData$Foreign_Worker, ModelData$Default_On_Payment)
ModelData$Dummy_Foreign_Worker <- ifelse(ModelData$Foreign_Worker == "A201", 1,0)

#######################################
#####Set 2############################
#Numeric variables can be passed as it is to the model or as dummy variable
#Check both options and include the one that shows better performance
#######################################
#Dummy variable for Age
table(ModelData$Age, ModelData$Default_On_Payment)
ModelData$Dummy_Age <- ifelse(ModelData$Age < 30, 1,0)

#Dummy VAriable for Credit amount
table(ModelData$Credit_Amount, ModelData$Default_On_Payment)
ModelData$Dummy_Credit_Amount <- ifelse(ModelData$Credit_Amount < 5000, 0, 1)

#Dummy variable for Current Address Yrs
table(ModelData$Current_Address_Yrs, ModelData$Default_On_Payment)
ModelData$Dummy_Current_Address_Yrs <- ifelse(ModelData$Current_Address_Yrs == 1, 0, 1)
#Num_credits and Num_dependents doesn't show significant relationship with the dep var


########### STEP 3 ##############
## Multicollinearity analysis####
#################################
library(car) #R package for multicollinearity analysis

#lm stands for linear model(Linear Regression)

#All the variables that are to be analyzed to be included in the equation, with dataset's name at the end
attach(ModelData)
vif_output <- lm(Default_On_Payment ~ Dummy_Job_status_A171 +
                   Dummy_Job_status_A172 + 
                   Dummy_Job_status_A173 + 
                   Dummy_Purpose_Credit_Taken_High +
                   Dummy_Purpose_Credit_Taken_Low +
                   Dummy_status_Checking_Accnt_High +
                   Dummy_Status_Checking_Accnt_Low +
                   Dummy_Credit_History_High +
                   Dummy_Credit_History_Low +
                   Dummy_Years_At_Present_Employment_High +
                   Dummy_Years_At_Present_Employment_Medium +
                   Dummy_Marital_status_Gender +
                   Dummy_Other_Debtors_Guarantors +
                   Dummy_Housing +
                   Dummy_Foreign_Worker + 
                   Dummy_Age +
                   Dummy_Credit_Amount +
                   Dummy_Current_Address_Yrs +
                   Duration_in_Months,
                 data = ModelData
)

#View Vif_Output
vif_output
summary(vif_output)

#Remove the following statement for perfect multicollinearity
#singular.ok = TRUE
vif(vif_output)


#Dummy_Job_status_A172 & Dummy_Job_status_A173 have greater than 2. 
#Remove one which has high value by comparing both
#Regress both the variables separately and take the one with high R suare value

Mullcoll_out1 <- lm(Default_On_Payment ~ Dummy_Job_status_A172, data = ModelData)
Mullcoll_out2 <- lm(Default_On_Payment ~ Dummy_Job_status_A173, data = ModelData)
summary(Mullcoll_out1)
summary(Mullcoll_out2)

vif_output <- lm(Default_On_Payment ~ Dummy_Job_status_A171 +
                   Dummy_Job_status_A172 + 
                   Dummy_Purpose_Credit_Taken_High +
                   Dummy_Purpose_Credit_Taken_Low +
                   Dummy_status_Checking_Accnt_High +
                   Dummy_Status_Checking_Accnt_Low +
                   Dummy_Credit_History_High +
                   Dummy_Credit_History_Low +
                   Dummy_Years_At_Present_Employment_High +
                   Dummy_Years_At_Present_Employment_Medium +
                   Dummy_Marital_status_Gender +
                   Dummy_Other_Debtors_Guarantors +
                   Dummy_Housing +
                   Dummy_Foreign_Worker + 
                   Dummy_Age +
                   Dummy_Credit_Amount +
                   Dummy_Current_Address_Yrs +
                   Duration_in_Months,
                 data = ModelData
)
summary(vif_output)

vif(vif_output)

#Dummy_Purpose_Credit_Taken_High & Dummy_Purpose_Credit_Taken_Low have vif > 1.5
#Regress both variables sparately and retain the one with higher R Square
Multicoll_out3 <- lm(Default_On_Payment ~ Dummy_Purpose_Credit_Taken_High, data=ModelData)
Multicoll_out4 <- lm(Default_On_Payment ~ Dummy_Purpose_Credit_Taken_Low, data=ModelData)
summary(Multicoll_out3)
summary(Multicoll_out4)

#Remove Dummy_Purpose_Credit_Taken_High and repeat VIF
vif_output <- lm(Default_On_Payment ~ Dummy_Job_status_A171 +
                   Dummy_Job_status_A172 + 
                   Dummy_Purpose_Credit_Taken_Low +
                   Dummy_status_Checking_Accnt_High +
                   Dummy_Status_Checking_Accnt_Low +
                   Dummy_Credit_History_High +
                   Dummy_Credit_History_Low +
                   Dummy_Years_At_Present_Employment_High +
                   Dummy_Years_At_Present_Employment_Medium +
                   Dummy_Marital_status_Gender +
                   Dummy_Other_Debtors_Guarantors +
                   Dummy_Housing +
                   Dummy_Foreign_Worker + 
                   Dummy_Age +
                   Dummy_Credit_Amount +
                   Dummy_Current_Address_Yrs +
                   Duration_in_Months,
                 data = ModelData
)
vif(vif_output)

#all VIF values are < 1.5. hence good to proceed to the next step

######################Step 4 ################
#####- Logistic Regression Model Building####
#############################################
#glm - Generalized Liner Model
#family = binomial spedifies its binomial logistic regression model
#data = dataset's name to be included at the end of the equation
LogReg_Outpt <- glm(Default_On_Payment ~ Dummy_Job_status_A171 +
                      Dummy_Job_status_A172 + 
                      Dummy_Purpose_Credit_Taken_Low +
                      Dummy_status_Checking_Accnt_High +
                      Dummy_Status_Checking_Accnt_Low +
                      Dummy_Credit_History_High +
                      Dummy_Credit_History_Low +
                      Dummy_Years_At_Present_Employment_High +
                      Dummy_Years_At_Present_Employment_Medium +
                      Dummy_Marital_status_Gender +
                      Dummy_Other_Debtors_Guarantors +
                      Dummy_Housing +
                      Dummy_Foreign_Worker + 
                      Dummy_Age +
                      Dummy_Credit_Amount +
                      Dummy_Current_Address_Yrs +
                      Duration_in_Months,
                    family = binomial(logit),
                    data = ModelData
)

#view output
summary(LogReg_Outpt, direction="forward")

#Remove insignificant variables
LogReg_Outpt <- glm(Default_On_Payment ~ Dummy_Purpose_Credit_Taken_Low +
                      Dummy_status_Checking_Accnt_High +
                      Dummy_Status_Checking_Accnt_Low +
                      Dummy_Credit_History_High +
                      Dummy_Credit_History_Low +
                      Dummy_Years_At_Present_Employment_High +
                      Dummy_Marital_status_Gender +
                      Dummy_Other_Debtors_Guarantors +
                      Dummy_Housing +
                      Dummy_Foreign_Worker + 
                      Dummy_Age +
                      Dummy_Credit_Amount +
                      Dummy_Current_Address_Yrs +
                      Duration_in_Months,
                    family = binomial(logit),
                    data = ModelData
)

#Revised model output
summary(LogReg_Outpt, direction="forward")

#Estimate the probability of each record
ModelData$Predected_Probability <- predict(LogReg_Outpt, ModelData, type="response")
#View(ModelData)
write.csv(ModelData, "Model_Output.csv")



#### Step 5################
### Logistic Regression - Model Validation

#Outlier Capping
#Change all Age values above 75 to 75
ValidationData$Age <- ifelse(ValidationData$Age >75, 75, ValidationData$Age)

#Missing Value Imputation
#Ipute the missing value with the highest frequency value for Housing
ValidationData$Housing[ValidationData$Housing == ""] <- "A152"

#Imput the missing value with the highest percentage value for Job status
ValidationData$Job_Status[ValidationData$Job_Status = ""] <- "A174"

#Dummy for Job Status 
ValidationData$Dummy_Job_Status_A171 <- ifelse(ValidationData$Job_Status == "A171", 1,0)
ValidationData$Dummy_Job_Status_A172 <- ifelse(ValidationData$Job_Status == "A172", 1,0)
ValidationData$Dummy_Job_Status_A173 <- ifelse(ValidationData$Job_Status == "A173", 1,0)

#Dummy for Purpose of credit taken
ValidationData$Dummy_Purpose_Credit_Taken_Low <- ifelse(ValidationData$Purpose_Credit_Taken == "P41" | 
                                                          ValidationData$Purpose_Credit_Taken == "P43" |
                                                          ValidationData$Purpose_Credit_Taken == "P48", 1,0)

ValidationData$Dummy_Purpose_Credit_Taken_High <- ifelse(ValidationData$Purpose_Credit_Taken == "P49" |
                                                           ValidationData$Purpose_Credit_Taken == "P40" | 
                                                           ValidationData$Purpose_Credit_Taken == "P45" |
                                                           ValidationData$Purpose_Credit_Taken == "P46" | 
                                                           ValidationData$Purpose_Credit_Taken == "P50", 1,0)

#Dummy variables for Status_checking_Accnt
table(ValidationData$Status_Checking_Accnt, ValidationData$Default_On_Payment)
ValidationData$Dummy_Status_Checking_Accnt_Low <- ifelse(ValidationData$Status_Checking_Accnt == "S11", 1,0)
ValidationData$Dummy_status_Checking_Accnt_High <- ifelse(ValidationData$Status_Checking_Accnt == "S12", 1,0)


#Dummy Variables for credit history
t = table(ValidationData$Credit_History, ValidationData$Default_On_Payment)
t
#prop.table(t, 1)
ValidationData$Dummy_Credit_History_High <- ifelse(ValidationData$Credit_History == "A30" |
                                                ValidationData$Credit_History == "A31", 1,0)
ValidationData$Dummy_Credit_History_Low <- ifelse(ValidationData$Credit_History == "A34",1,0)

#Dummy variables for Years at present employment
table(ValidationData$Years_At_Present_Employment, ValidationData$Default_On_Payment)
ValidationData$Dummy_Years_At_Present_Employment_High <- ifelse( ValidationData$Years_At_Present_Employment == "E71" | 
                                                              ValidationData$Years_At_Present_Employment == "E72", 1,0)
ValidationData$Dummy_Years_At_Present_Employment_Medium <- ifelse(ValidationData$Years_At_Present_Employment == "E73",1,0)

#Dummy variables for Marital Status Gender
table(ValidationData$Marital_Status_Gender, ValidationData$Default_On_Payment)
ValidationData$Dummy_Marital_status_Gender <- ifelse(ValidationData$Marital_Status_Gender == "A91" |
                                                  ValidationData$Marital_Status_Gender == "A92", 1,0)

#Dummy variables for Other Debters Gurantors
table(ValidationData$Other_Debtors_Guarantors, ValidationData$Default_On_Payment)
ValidationData$Dummy_Other_Debtors_Guarantors <- ifelse(ValidationData$Other_Debtors_Guarantors == "A103", 1,0)

#Dummy Variabls for Housing
table(ValidationData$Housing, ValidationData$Default_On_Payment)
ValidationData$Dummy_Housing <- ifelse(ValidationData$Housing == "A152",1,0)

#Dummy variales for Foreign worker
table(ValidationData$Foreign_Worker, ValidationData$Default_On_Payment)
ValidationData$Dummy_Foreign_Worker <- ifelse(ValidationData$Foreign_Worker == "A201", 1,0)


#Dummy variable for Age
ValidationData$Dummy_Age <- ifelse(ValidationData$Age < 30, 1,0)

#Dummy VAriable for Credit amount
ValidationData$Dummy_Credit_Amount <- ifelse(ValidationData$Credit_Amount < 5000, 0, 1)

#Dummy variable for Current Address Yrs
ValidationData$Dummy_Current_Address_Yrs <- ifelse(ValidationData$Current_Address_Yrs == 1, 0, 1)
#Num_credits and Num_dependents doesn't show significant relationship with the dep var

vif_output <- lm(Default_On_Payment ~ Dummy_Purpose_Credit_Taken_Low +
                      Dummy_status_Checking_Accnt_High +
                      Dummy_Status_Checking_Accnt_Low +
                      Dummy_Credit_History_High +
                      Dummy_Credit_History_Low +
                      Dummy_Years_At_Present_Employment_High +
                      Dummy_Marital_status_Gender +
                      Dummy_Other_Debtors_Guarantors +
                      Dummy_Housing +
                      Dummy_Foreign_Worker + 
                      Dummy_Age +
                      Dummy_Credit_Amount +
                      Dummy_Current_Address_Yrs +
                      Duration_in_Months,
                    family = binomial(logit),
                    data = ModelData
)

vif(vif_output)
#Estimate the probability of each record
#Note, here we are using LogReg_Output to estimate the probability of default
ValidationData$Predicted_Probability = predict(LogReg_Outpt, ValidationData, type="response")

view(ValidationData)
#Export the validation output data to csv file
write.csv(ValidationData, "Validationi_Output.csv") 

###########################################
#### Model Performance Assessment##########
###########################################
ModelData$Final_Prediction <- ifelse(ModelData$Predected_Probability > 0.3, 1,0)
ValidationData$Final_Prediction <- ifelse(ValidationData$Predicted_Probability < 0.3, 1,0)

#Generate Classification Matrix(Confusion Matrix)
d = table(ModelData$Default_On_Payment, ModelData$Final_Prediction)
t = table(ValidationData$Default_On_Payment, ValidationData$Final_Prediction)
prop.table(d)
prop.table(t)

#Export the final output
write.csv(ModelData,"Final_Model_Output.csv")
write.csv(ValidationData, "Final_validation_Output.csv")

