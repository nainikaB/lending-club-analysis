# Libraries
library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)
library(ggplot2)
library(magrittr)
library(tidyverse)

accepted <- read.csv("~/Downloads/loans/accepted_2007_to_2018q4.csv/accepted_2007_to_2018Q4.csv")
View(accepted)
deleted_col <- subset(accepted, select = -c(member_id, funded_amnt_inv,pymnt_plan, url,desc, zip_code, addr_state, initial_list_status, mths_since_last_delinq, mths_since_last_record, pub_rec, inq_last_6mths, delinq_2yrs, title, purpose, policy_code, mths_since_last_major_derog, open_act_il, open_il_12m, open_il_24m, open_acc_6m,mths_since_rcnt_il, il_util,open_rv_12m,funded_amnt, all_util,total_bal_il, open_rv_24m,total_rev_hi_lim, inq_fi,total_cu_tl, inq_last_12m,acc_open_past_24mths, acc_now_delinq, inq_fi,total_cu_tl, inq_last_12m, acc_open_past_24mths))
str(deleted_col)
filter_unwanted <- filter(deleted_col, annual_inc < 0)
filter_unwanted
accept_loans <- na.omit(grade )
View(accept_loans)

# approach the data as a whole 

# cleaning and wrangling the data 

#convert the number of issue_d into years

library(zoo)#this is a little more forgiving:
as.yearmon(c(deleted_col$issue_d), "%m-%Y")
# converts it into 2013-3-4 for
dates <- as.Date(as.yearmon(c(deleted_col$issue_d), "%b-%Y"))
#adds it to the column
new_year <- deleted_col %>% mutate( new_date = dates)
new_year


#adds years to col and create a new data set 
date <-  as.Date(new_year$new_date,'%Y-%m-%d')
year <- as.numeric(format(date,'%Y'))
year
new_years <- new_year %>% mutate( yearss = year)
new_years


# solving for outliers for dti 

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}
no_outlier <- remove_outliers(new_years$dti)
no_outlier
new_data <- new_years %>% mutate(dtii = no_outlier)
hist(new_data$dtii)

no_loans <- new_data%>% count(dtii)
no_loans
str(no_outlier)
hist(no_outlier)

#deleting na rows from dataset and assigning it to another dataset for dti 

new_improved <- new_years %>% filter( dtii != "")

#find the number of dti each year
no_loans <- new_improved%>% count(dti)
no_loans


dti_count <- no_loans %>% ggplot(aes(x = yearss ,y = dtii, fill = yearss)) + geom_bar(stat = "identity") + ggtitle("Status of the loans", "duration from 2007 to 2018") + labs(x = "Grade", y = "Number of loans as per status") + scale_y_continuous(limits=c(0,10000))
dti_count

# number of loans each year 

#find the number of loans accepted each year
no_loans <- new_years %>% count(loan_amnt, yearss)
no_loans


# plotting the graph for number of loans

num_of_loans <- ggplot(no_loans,aes(x = yearss ,y = n, fill = n)) + geom_bar(stat = "identity") + geom_smooth(method = "lm")  + ggtitle("number of loans per year", "duration from 2007 to 2018") + labs(x = "Year", y = "Number of loans")
num_of_loans

num_of_loans<-ggplot(no_loans, aes(x=yearss, y=n)) + 
  geom_dotplot()
num_of_loans
#loan status 

# deleting na rows from dataset and assigning it to another dataset 

new_improved <- new_years %>% filter( loan_status != "")

# find the status and number of loans 
no_loans <- new_improved%>% count(loan_status)
no_loans
#scale_y_continuous(limits=c(0,500))
#+ scale_fill_gradient2(low='green', mid='green', high='blue')
status_of_loans <- no_loans %>% ggplot(aes(x = loan_status ,y = n, fill = n)) + geom_bar(stat = "identity") + ggtitle("status of loans", "duration from 2007 to 2018") + labs(x = "loan status", y = "status of loans") + scale_fill_gradient2(low='purple', mid='green', high='blue')
status_of_loans

# loan amount 

# data with years converted from Dec 2015 to 2015 
new_years 


# find the loan amt and number of loans 
no_loans <- new_years %>% count(yearss, loan_amnt)
no_loans
num_of_loanamt <- ggplot(no_loans,aes(x = yearss ,y = loan_amnt, fill = n))+ geom_bar(stat = "identity") + geom_smooth(method = "lm")  + ggtitle("Amount of loan issued per year", "duration from 2007 to 2018") + labs(x = "Year", y = "loan amount") + scale_y_continuous(limits=c(0,38000000))
num_of_loanamt


# grading of loans

# data with years converted from Dec 2015 to 2015 
new_years 
# deleting na rows from dataset and assigning it to another dataset 

new_improved <- new_years %>% filter( grade != "")

# find the grading and number of loans 
no_loans <- new_improved%>% count(grade)
no_loans
num_of_grade_loans <- ggplot(no_loans,aes(x = grade ,y = n, fill = grade)) + geom_bar(stat = "identity") + ggtitle("status of loans", "duration from 2007 to 2018") + labs(x = "loan status", y = "status of loans") 
num_of_grade_loans


# term of loans 

# data with years converted from Dec 2015 to 2015 
new_years 

# deleting na rows from dataset and assigning it to another dataset 

new_improved <- new_years %>% filter( term != "")

# find the grading and number of loans 
no_loans <- new_improved%>% count(term)
no_loans
num_of_term_loans <- ggplot(no_loans,aes(x = term ,y = n, fill = n)) + geom_bar(stat = "identity") + ggtitle("status of loans", "duration from 2007 to 2018") + labs(x = "loan status", y = "status of loans") + scale_fill_gradient2(low='purple', mid='green', high='blue') + scale_y_continuous(limits=c(0,10000))
num_of_term_loans


# boxplot for int rate and grade of loans 

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}
no_outlier <- remove_outliers(new_years$int_rate)
no_outlier
new_data <- new_years %>% mutate(interest = no_outlier)
hist(new_data$interest)
new_improved <- new_data %>% filter( grade != "" && interest != "")
new_improved
int_grade <- ggplot(new_improved, aes(x=grade, y=interest)) +
  geom_boxplot(fill="white") 
int_grade
# creating a regression model to understand the various factors involved in loan acceptance

#creating a linear regression model 
model <- lm(loan_amnt ~ int_rate+annual_inc+dti, data = deleted_col)
summary(model)








