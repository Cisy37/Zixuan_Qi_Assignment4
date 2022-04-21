#=================================================================
# Set Directory and Import Data
#=================================================================
####Install####
library(tidyverse)
library(data.table)
library(stargazer)
library(survival)
library(fastDummies)

rm(list = ls())
wd = "C:/Users/Qi zixuan/Desktop/Econ 613/Assignment 4"
setwd(wd)

dat_A4 <- fread("./Data/dat_A4.csv", header = T)
dat_A4_panel <- fread("./Data/dat_A4_panel.csv", header = T)

#=================================================================
# Exercise 1: Preparing the Data
#=================================================================
#### Question 1 ####
pre_dat_A4 <- dat_A4 %>% mutate(age= 2019-as.numeric(KEY_BDATE_Y_1997))
pre_dat_A4$work_exp <- (apply(pre_dat_A4[,c(18:28)],1,sum,na.rm=T)*7)/365
pre_dat_A4$work_exp <- round(pre_dat_A4$work_exp)
head(pre_dat_A4[,c(31,32)])

#### Question 2 ####
pre_dat_A4 <- pre_dat_A4 %>%
  mutate(CV_HGC_BIO_DAD_1997=ifelse(CV_HGC_BIO_DAD_1997=='95',0,CV_HGC_BIO_DAD_1997),
         CV_HGC_BIO_MOM_1997=ifelse(CV_HGC_BIO_MOM_1997=='95',0,CV_HGC_BIO_MOM_1997),
         CV_HGC_RES_DAD_1997=ifelse(CV_HGC_RES_DAD_1997=='95',0,CV_HGC_RES_DAD_1997),
         CV_HGC_RES_MOM_1997=ifelse(CV_HGC_RES_MOM_1997=='95',0,CV_HGC_RES_MOM_1997),
         YSCH.3113_2019=ifelse(YSCH.3113_2019=='1',0,YSCH.3113_2019),
         YSCH.3113_2019=ifelse(YSCH.3113_2019=='2',4,YSCH.3113_2019),
         YSCH.3113_2019=ifelse(YSCH.3113_2019=='3',12,YSCH.3113_2019),
         YSCH.3113_2019=ifelse(YSCH.3113_2019=='4',14,YSCH.3113_2019),
         YSCH.3113_2019=ifelse(YSCH.3113_2019=='5',16,YSCH.3113_2019),
         YSCH.3113_2019=ifelse(YSCH.3113_2019=='6',18,YSCH.3113_2019),
         YSCH.3113_2019=ifelse(YSCH.3113_2019=='7',23,YSCH.3113_2019),
         YSCH.3113_2019=ifelse(YSCH.3113_2019=='8',22,YSCH.3113_2019))
head(pre_dat_A4[,c(8:11,29)])

#### Question 3 ####
# income of age groups
pre_dat_A4_figure1 <- pre_dat_A4 %>%
  filter(YINC_1700_2019>0) 
png(file = "income_age.png")
p1 <- boxplot(YINC_1700_2019 ~ age,data=pre_dat_A4_figure1,  
        xlab="Age", ylab="Income")
dev.off()

# income of gender groups
png(file = "income_gender.png")
p2 <- boxplot(YINC_1700_2019 ~ KEY_SEX_1997,data=pre_dat_A4_figure1, 
              xlab="Gender", ylab="Income")
dev.off()

# income of number of children
png(file = "income_children.png")
p3 <- boxplot(YINC_1700_2019 ~ CV_BIO_CHILD_HH_U18_2019,data=pre_dat_A4_figure1, 
              xlab="Number of Children", ylab="Income")
dev.off()

# Table the share of 0 in the income data
pre_dat_A4_figure2 <- pre_dat_A4 %>%
  filter(!is.na(YINC_1700_2019)) %>% 
  group_by(age) %>%
  summarise(ratio_age = sum(YINC_1700_2019=='0')/n(),
            ratio_age=round(ratio_age,digits=6)) %>%
  ungroup()
pre_dat_A4_figure2

pre_dat_A4_figure3 <- pre_dat_A4 %>%
  filter(!is.na(YINC_1700_2019)) %>% 
  group_by(KEY_SEX_1997) %>%
  summarise(ratio_sex = sum(YINC_1700_2019=='0')/n(),
            ratio_sex=round(ratio_sex,digits=6)) %>%
  ungroup()
pre_dat_A4_figure3

pre_dat_A4_figure4 <- pre_dat_A4 %>% 
  filter(!is.na(YINC_1700_2019)&!is.na(CV_MARSTAT_COLLAPSED_2019)&!is.na(CV_BIO_CHILD_HH_U18_2019)) %>%
  group_by(CV_MARSTAT_COLLAPSED_2019,CV_BIO_CHILD_HH_U18_2019) %>%
  summarise(ratio_mar_child = sum(YINC_1700_2019=='0')/n(),
            ratio_mar_child=round(ratio_mar_child,digits=6)) %>%
  ungroup()
head(pre_dat_A4_figure4)

pre_dat_A4 %>%
  filter(!is.na(YINC_1700_2019)&!is.na(CV_MARSTAT_COLLAPSED_2019)) %>%
  group_by(CV_MARSTAT_COLLAPSED_2019) %>%
  summarise(ratio_mar = sum(YINC_1700_2019=='0')/n(),
            ratio_mar=round(ratio_mar,digits=6)) %>%
  ungroup()

pre_dat_A4 %>% 
  filter(!is.na(YINC_1700_2019)&!is.na(CV_BIO_CHILD_HH_U18_2019)) %>%
  group_by(CV_BIO_CHILD_HH_U18_2019) %>%
  summarise(ratio_child = sum(YINC_1700_2019=='0')/n(),
            ratio_child=round(ratio_child,digits=6)) %>%
  ungroup()

#=================================================================
# Exercise 2: Heckman Selection Model
#=================================================================
#### Question 1 ####
# I regard age, work_exp, the total years of schooling of individual, 
# and gender as regressors in the OLS regression.
pre_dat_A4$KEY_SEX_1997 <- factor(pre_dat_A4$KEY_SEX_1997,labels=c("Male","Female"))

dat_Ex2 <- pre_dat_A4 %>%
  filter(!is.na(YSCH.3113_2019)&!is.na(age)&!is.na(work_exp)&
         !is.na(CV_HGC_BIO_DAD_1997)&!is.na(CV_HGC_BIO_MOM_1997)&
         !is.na(CV_HGC_RES_DAD_1997)&!is.na(CV_HGC_RES_MOM_1997)&
         !is.na(KEY_SEX_1997)&!is.na(CV_BIO_CHILD_HH_U18_2019))

dat_Ex2$observed_index <- dat_Ex2$YINC_1700_2019>0
dat_Ex2$observed_index[is.na(dat_Ex2$YINC_1700_2019)] <- FALSE
observed_index <- dat_Ex2$YINC_1700_2019>0
observed_index[is.na(dat_Ex2$YINC_1700_2019)] <- FALSE

OLS_obs = lm(YINC_1700_2019 ~ age + work_exp + YSCH.3113_2019 + KEY_SEX_1997, data = dat_Ex2[observed_index, ])

summary(OLS_obs)
#### Question 2 ####
# Please see the answers in the PDF

#### Question 3 ####
# I use the total years of schooling of individual's resident father and mother
# and the total years of schooling of individual's biological father and mother
# as exogenous variables, as well as the number of children, to make probit regression.
heckman_select <- function(Y, X, Z, obs, par) {
  alpha = par[1:10]
  probit = X%*%alpha
  beta = par[11:15]
  ols = Z%*%beta
  sigma = par[16]
  rho = par[17]
  Like1 = 1-pnorm(probit[!obs]) 
  Like1[Like1<0.000001] = 0.000001
  Like_sum1 = sum(log(Like1)) - log(sigma)
  Like2=dnorm(Y, mean=ols, sd=sigma)
  Like2[Like2<0.000001] = 0.000001
  Like3=pnorm((probit[obs]+(rho/sigma)*(Y-ols))/sqrt(1-rho^2))
  Like3[Like3<0.000001] = 0.000001
  Like_sum2 = sum(log(Like2)) + sum(log(Like3))
  Like=Like_sum1+Like_sum2
  return(-Like)
}

probit = glm(observed_index ~ age + work_exp + YSCH.3113_2019 + KEY_SEX_1997 + CV_HGC_BIO_DAD_1997 + 
             CV_HGC_BIO_MOM_1997 + CV_HGC_RES_DAD_1997 + CV_HGC_RES_MOM_1997  + 
             CV_BIO_CHILD_HH_U18_2019, data = dat_Ex2, family = binomial(link = 'probit'))
summary(probit)

pre_probit <- -predict(probit)
mills_ratio <- dnorm(pre_probit)/(1-pnorm(pre_probit))
imr <- mills_ratio[observed_index]

ols_select = lm(YINC_1700_2019 ~ age + work_exp + YSCH.3113_2019 + KEY_SEX_1997 + 
               imr, data = dat_Ex2[observed_index, ])
summary(ols_select)

X <- model.matrix(probit)
Z <- model.matrix(ols_select)
start1 <- c(coef(probit), coef(ols_select)[-6], 1, 0)
heckman_optim = optim(start1, heckman_select, Y=dat_Ex2$YINC_1700_2019[observed_index], X=X, Z=Z[,-6], 
                   obs=observed_index, method = 'BFGS',hessian=T)
cbind(OLS_obs$coefficients, unname(heckman_optim$par[11:15]))

# check the results
#library(sampleSelection)
#check1 = selection(observed_index ~ age + work_exp + YSCH.3113_2019 + + KEY_SEX_1997 + CV_HGC_BIO_DAD_1997 + 
#                     CV_HGC_BIO_MOM_1997 + CV_HGC_RES_DAD_1997 + CV_HGC_RES_MOM_1997 + CV_BIO_CHILD_HH_U18_2019, 
#                   YINC_1700_2019 ~ age + work_exp + YSCH.3113_2019 + KEY_SEX_1997, 
#                   data = dat_Ex2, method = '2step')
#summary(check1)

#=================================================================
# Exercise 3:  Censoring
#=================================================================
#### Question 1 ####
dat_Ex3 <- pre_dat_A4 %>% filter(YINC_1700_2019>0)
p1 <- ggplot(dat_Ex3, aes(x=YINC_1700_2019)) + geom_histogram() 
ggsave(p1,filename ='Ex3_1.png', width = 5, height = 3)

#### Question 2 ####
# Please see the answers in the PDF

#### Question 3 ####
dat_tobit <- pre_dat_A4 %>% filter(YINC_1700_2019>0&!is.na(YSCH.3113_2019))
tobit_reg <- function(Y, X, ul, par) {
  sigma = exp(par[length(par)])
  beta = par[-length(par)]
  limit = ul
  ind = Y < ul
  ols = X %*% beta
  Like = sum(ind*log((1/sigma)*dnorm((Y-ols)/sigma))) +
    sum((1-ind)*log(pnorm((ols-limit)/sigma)))
  return(-Like)
}
 
lm_tobit = lm(YINC_1700_2019 ~ age + work_exp + YSCH.3113_2019 + KEY_SEX_1997, data = dat_tobit)
summary(lm_tobit)
X = model.matrix(lm_tobit)
start2 = c(-3395.4249,523.9406,1118.6705,2619.8532,-16442.4929,10.24)
tobit_optim = optim(start2, tobit_reg, Y = dat_tobit$YINC_1700_2019, X = X, ul = 100000,
                    method = 'BFGS')
cbind(lm_tobit$coefficients, unname(tobit_optim$par[1:5]))

# check the results
#library(AER)
#check2 <- tobit(YINC_1700_2019 ~ age + work_exp + YSCH.3113_2019 + KEY_SEX_1997, data = dat_tobit, left= 0, right = 100000)
#summary(check2)

#### Question 4 ####
# Please see the answers in the PDF

#=================================================================
# Exercise 4:  Panel Data
#=================================================================
#### Question 1 ####
# Please see the answers in the PDF

#### Question 2 ####
dat_Ex4 <- dat_A4_panel %>% 
  select(-V1, -KEY_SEX_1997,-KEY_BDATE_M_1997,-KEY_BDATE_Y_1997,-KEY_RACE_ETHNICITY_1997,
         -CV_SAMPLE_TYPE_1997,-CV_HIGHEST_DEGREE_1011_2010,-CV_HIGHEST_DEGREE_1112_2011,
         -CV_HIGHEST_DEGREE_1314_2013)
years <- c(1998:2011,2013,2015,2017,2019)
edu_newname <- paste0("DEGREE_",years)
edu_oldname <- colnames(dat_Ex4)[grepl("DEGREE",colnames(dat_Ex4))]
dat_Ex4 <- dat_Ex4 %>% 
  rename_at(vars(edu_oldname), ~all_of(edu_newname)) %>%
  pivot_longer(-PUBID_1997,names_pattern = "(.*)\\_(\\d{4})", names_to = c(".value", "YEAR")) %>%
  mutate(WORK_EXP = rowSums(across(contains("WKSWK")),na.rm=T),
         DEGREE=ifelse(DEGREE=='1',4,DEGREE), DEGREE=ifelse(DEGREE=='2',12,DEGREE),
         DEGREE=ifelse(DEGREE=='3',14,DEGREE), DEGREE=ifelse(DEGREE=='4',16,DEGREE),
         DEGREE=ifelse(DEGREE=='5',18,DEGREE), DEGREE=ifelse(DEGREE=='6',23,DEGREE),
         DEGREE=ifelse(DEGREE=='7',22,DEGREE)) %>%
  select(1:4,12,21) %>% na.omit() %>%
  mutate(MARSTAT = factor(CV_MARSTAT_COLLAPSED)) %>%
  dummy_cols(select_columns = "MARSTAT", remove_first_dummy = T)
#write.csv(dat_Ex4,'dat_Ex4.csv')

# Within Estimator
dat_within <- dat_Ex4 %>% 
  group_by(PUBID_1997) %>%
  mutate(across(c(`YINC-1700`,DEGREE,WORK_EXP,contains("MARSTAT_")), ~ .x-mean(.x), .names = "M_{col}"))%>%
  ungroup()
lm_within <- lm(`M_YINC-1700` ~ 0+M_DEGREE+M_WORK_EXP+M_MARSTAT_1+M_MARSTAT_2+
                  M_MARSTAT_3+M_MARSTAT_4, data=dat_within)
summary(lm_within)

# Between Estimator
dat_between <- dat_Ex4 %>% 
  group_by(PUBID_1997) %>%
  mutate(across(c(`YINC-1700`,DEGREE,WORK_EXP,contains("MARSTAT_")), ~mean(.x), .names = "M_{col}"))%>%
  ungroup()
lm_between <- lm(`M_YINC-1700` ~ M_DEGREE+M_WORK_EXP+M_MARSTAT_1+M_MARSTAT_2+
                   M_MARSTAT_3+M_MARSTAT_4, data=dat_between)
summary(lm_between)

# First-Difference Estimator
dat_difference <- dat_Ex4 %>% 
  group_by(PUBID_1997) %>%
  mutate(across(c(`YINC-1700`,DEGREE,WORK_EXP,contains("MARSTAT_")), ~.x-dplyr::lag(.x,), .names = "M_{col}")) %>%
  ungroup()
lm_difference <- lm(`M_YINC-1700` ~ 0+M_DEGREE+M_WORK_EXP+M_MARSTAT_1+M_MARSTAT_2+
                      M_MARSTAT_3+M_MARSTAT_4, data=dat_difference)
summary(lm_difference)

# check the results
#library(plm)
#check3 <- plm(YINC.1700~DEGREE+WORK_EXP+MARSTAT,data=dat_Ex4,index=c('PUBID_1997','YEAR'),model="within")
#summary(check3)
#check4 <- plm(YINC.1700~DEGREE+WORK_EXP+MARSTAT,data=dat_Ex4,index=c('PUBID_1997','YEAR'),model="between")
#summary(check4)
#check5 <- plm(YINC.1700~0+DEGREE+WORK_EXP+MARSTAT_1+MARSTAT_2+
#                MARSTAT_3+MARSTAT_4,data=dat_Ex4,index=c('PUBID_1997','YEAR'),model="fd")
#summary(check5)

# Compare
stargazer(lm_within, lm_between, lm_difference,
          title = "Regression Results",
          dep.var.labels = c("Income"),
          covariate.labels = c("Education", "Experience", "Married", "Separated", "Divorced", " Widowed","Intercept"),
          column.labels=c("Within","Between","First-Difference"),
          omit.stat = c("LL","ser","f"),
          type = "html", out="regression.html")

#### Question 3 ####
# Please see the answers in the PDF


