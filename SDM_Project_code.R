library(rio)
library(moments)
library(car)
library(PerformanceAnalytics)
setwd("C:/SDM Project")
df = import("final3.csv")
getwd()

View(df)
library(stringr)

#Removing $ and , sysmbol from amount column to convert it to numerical
num_data <- str_replace_all(df$`Total Liabilities`, "[^[:alnum:]]", "")

df$`Total Liabilities` = num_data
df$`Net Income` <- str_replace_all(df$`Net Income`, "[^[:alnum:]]", "")
df$`Total Assets` <- str_replace_all(df$`Total Assets`, "[^[:alnum:]]", "")
df$`Land Improvements` <- str_replace_all(df$`Land Improvements`, "[^[:alnum:]]", "")
df$`Total Current Assets` <- str_replace_all(df$`Total Current Assets`, "[^[:alnum:]]", "")
df$`Contract Labor` <- str_replace_all(df$`Contract Labor`, "[^[:alnum:]]", "")
df$`Total Salaries (adjusted)` <- str_replace_all(df$`Total Salaries (adjusted)`, "[^[:alnum:]]", "")
df$`Wage-Related Costs (Core)` <- str_replace_all(df$`Wage-Related Costs (Core)`, "[^[:alnum:]]", "")
df$`Total Costs` <- str_replace_all(df$`Total Costs`, "[^[:alnum:]]", "")
df$`Total Unreimbursed and Uncompensated Care` <- str_replace_all(df$`Total Unreimbursed and Uncompensated Care`, "[^[:alnum:]]", "")
df$`Total Bad Debt Expense` <- str_replace_all(df$`Total Bad Debt Expense`, "[^[:alnum:]]", "")

#Checking for NA values or missing values
lapply(df,function(x) { length(which(is.na(x)))})

#feature engineering
df$`Number of Interns and Residents (FTE)`[is.na(df$`Number of Interns and Residents (FTE)`)] <- 0
df$teaching_hospital = ifelse(df$`Number of Interns and Residents (FTE)`==0,"no","yes")
drop = c("Number of Interns and Residents (FTE)","Total Bed Days Available + Total for all Subproviders")
df = df[,!(names(df) %in% drop)]


#Median imputation
df$`FTE - Employees on Payroll`[is.na(df$`FTE - Employees on Payroll`)] <- median(df$`FTE - Employees on Payroll`,na.rm = TRUE)
df$`Number of Beds`[is.na(df$`Number of Beds`)] = median(df$`Number of Beds`,na.rm = TRUE)
df$`Type of Control`[is.na(df$`Type of Control`)] = median(df$`Type of Control`,na.rm = TRUE) 
df$`Cost To Charge Ratio`[is.na(df$`Cost To Charge Ratio`)] = median(df$`Cost To Charge Ratio`,na.rm = TRUE) 
df$`Total Performance Score`[is.na(df$`Total Performance Score`)] = median(df$`Total Performance Score`,na.rm = TRUE)
df$`Provider Type`[is.na(df$`Provider Type`)] = median(df$`Provider Type`,na.rm = TRUE)


lapply(df,function(x) { length(which(is.na(x)))})
sum(is.na(df))

ceiling(df$`FTE - Employees on Payroll`)
df = df[df$State!="",]

#Descriptive Analysis

#Which Drug has highest expense in any hospital
df_higest_expense_drug = df[which.max(df$Avg_Tot_Pymt_Amt),c("Rndrng_Prvdr_Org_Name","DRG_Desc","Avg_Tot_Pymt_Amt","City_x","State")]

#Which Drug has minimum expense in any hospital
df_lowest_expense_drug = df[which.min(df$Avg_Tot_Pymt_Amt),c("Rndrng_Prvdr_Org_Name","DRG_Desc","Avg_Tot_Pymt_Amt","City_x","State")]

#Calculating the Sum of hospital expense by Drug
df_sum_drug = aggregate(df$Avg_Tot_Pymt_Amt, by=list(df$DRG_Desc),FUN =sum)
df_sum_drug_ordered = df_sum_drug[order(-df_sum_drug$x),]


#Calculating hospital expense by drug and by state
df_sum_drug_state = aggregate(df$Avg_Tot_Pymt_Amt, by=list(df$DRG_Desc,df$State),FUN =sum)

#Which hospital expense is more after combining all the drug
df_sum_hospital = aggregate(df$Avg_Tot_Pymt_Amt, by=list(df$Rndrng_Prvdr_Org_Name),FUN =sum)
df_sum_hospital_ordered = df_sum_hospital[order(-df_sum_hospital$x),]
table(df$State)

length(unique(df$State))

table(df$region)

#Exporting csv file                                                                         

write.csv(df,"C:/SDM Project/FiNAL.csv", row.names = FALSE)

df$region = ifelse(df$State == "WA" | df$State == "CA" | df$State == "OR" | df$State == "NV" | df$State == "ID" | 
                     df$State == "AK" | df$State == "MT" | df$State == "WY" | df$State == "UT" | df$State == "AZ" |
                   df$State == "CO" | df$State == "NM" | df$State == "HI", "West", ifelse(df$State == "ND" |
                                                                                            df$State == "SD" | df$State == "NE" | df$State == "KS" |
                                                                                            df$State == "MN" | df$State == "IA" | df$State == "MO"
                                                                                          | df$State == "WI" | df$State == "IL" | df$State == "IN" |
                                                                                            df$State == "OH" | df$State == "MI", "Mid-West", ifelse(df$State == "PA" | df$State == "NY" | df$State == "NJ" |
                                                                                                                                                   df$State == "RI" | df$State == "CT" | df$State == "MA"
                                                                                                                                                    | df$State == "VT" | df$State == "NH" | df$State == "ME", "North-East", ifelse( df$State == "DE" | df$State == "MD" | df$State == "DC"
                                                                                                                                                                                                                                    | df$State == "VA" | df$State == "WV" | df$State == "KY" | df$State == "TN" | 
                                                                                                                                                                                                                                    df$State == "NC" | df$State == "SC" | df$State == "GA" | df$State == "AL"
                                                                                                                                                                                                                                    | df$State == "LA" | df$State == "FL" | df$State == "MS" | df$State == "AR"
                                                                                                                                                                                                                                    | df$State == "OK" | df$State == "TX", "South", NA))))

df$`Provider Type`[is.na(df$`Provider Type`)] = median(df$`Provider Type`,na.rm = TRUE)
df$`Total Bad Debt Expense`[is.na(df$`Total Bad Debt Expense`)] = median(df$`Total Bad Debt Expense`,na.rm = TRUE)
df$`Contract Labor`[is.na(df$`Contract Labor`)] = median(df$`Contract Labor`,na.rm = TRUE)
df$`Land Improvements`[is.na(df$`Land Improvements`)] = median(df$`Land Improvements`,na.rm = TRUE)
df$`Net Income`[is.na(df$`Net Income`)] = median(df$`Net Income`,na.rm = TRUE)

colnames(df) = c("hospital_name","zipcode","ruca","drug_description",
                 "Total_discharge","submitted_charge","hospital_cost",
                 "medicare_coverage","city","State","domain_score",
                 "engagment_score","safety_score","cost_reduction_score",
                 "tps","rural_vs_urban","provider_type","type_of_control",
                 "no_of_employees","beds","bad_debt_expense",
                 "uncomopnseted_care","total_cost","wage_cost",
                 "total_salaries","contract_labor","current_asset",
                 "land_improvment","investment","total_assets",
                 "liabilities","net_income","cost_charge_ratio",
                 "median_income","out_of_pocket_expense",
                 "teaching_hospital")

#converting to correct data type
df$domain_score = as.numeric(df$domain_score)
df$engagment_score = as.numeric(df$engagment_score)
df$safety_score = as.numeric(df$safety_score)
df$cost_reduction_score = as.numeric(df$cost_reduction_score)
df$median_income = as.numeric(df$median_income)

library(PerformanceAnalytics)
library(corrplot)



attach(dfcorr)
dfcorr = subset(df,select = c(domain_score, engagment_score, safety_score, cost_reduction_score,
                              tps, no_of_employees, beds, uncomopnseted_care, total_cost, wage_cost,
                              total_salaries, contract_labor, current_asset, total_assets, liabilities,
                              net_income, cost_charge_ratio, median_income))
chart.Correlation(dfcorr)

dfcorr = import("final_csv.csv")
 
dfcorr = subset(df,select = c(domain_score, engagment_score, safety_score, cost_reduction_score,
                              tps, no_of_employees, beds, uncomopnseted_care,
                              total_salaries, contract_labor,
                              net_income, cost_charge_ratio, median_income))
dfcorr$uncomopnseted_care = as.numeric(dfcorr$uncomopnseted_care)
dfcorr$total_salaries = as.numeric(dfcorr$total_salaries)

dfcorr$contract_labor = as.numeric(dfcorr$contract_labor)
dfcorr$net_income = as.numeric(dfcorr$net_income)

str(dfcorr)





chart.Correlation(dfcorr)






##New code

library(rio)
install_formats()
library(moments)
install.packages('bit64')


df_final = import("final_csv.csv")
str(df_final)
df_final$hospital_name = as.factor(df_final$hospital_name)
df_final$ruca = as.factor(df_final$ruca)
df_final$drug_description = as.factor(df_final$drug_description)
df_final$city = as.factor(df_final$city)
df_final$State = as.factor(df_final$State)
df_final$rural_vs_urban = as.factor(df_final$rural_vs_urban)
df_final$provider_type = as.factor(df_final$provider_type)
df_final$type_of_control = as.factor(df_final$type_of_control)
df_final$teaching_hospital = as.factor(df_final$teaching_hospital)
df_final$region = as.factor(df_final$region)
df_final$no_of_employees = ceiling(df_final$no_of_employees)
library(lme4)
df_final$roundoff_charge = round(df_final$submitted_charge)
attach(df_final)
hist(roundoff_charge)
bmplot(df_final$medicare_coverage, df_final$State)

library(ggplot2)
# Basic box plot
p <- ggplot(df_final, aes(x=medicare_coverage, y=State)) + 
  geom_boxplot()

##model for hospital cost
##m1 = glmer(roundoff_charge~hospital_name+ruca+drug_description+Total_discharge+hospital_cost+domain_score+
  ##           engagment_score+safety_score+cost_reduction_score+provider_type+type_of_control+no_of_employees+beds+uncomopnseted_care+
    ##         total_salaries+contract_labor+net_income+cost_charge_ratio+median_income+teaching_hospital+(1|State), data = df_final, family = poisson(link = log))
#m2 = lmer(log(submitted_charge)~hospital_name+ruca+drug_description+Total_discharge+hospital_cost+domain_score+
 #           engagment_score+safety_score+cost_reduction_score+provider_type+type_of_control+no_of_employees+beds+uncomopnseted_care+
  #          total_salaries+contract_labor+net_income+cost_charge_ratio+median_income+teaching_hospital+(1|State), data = df_final, REML=FALSE)


set.seed(1214)
dfs = df_final[sample(nrow(df_final), 18000),]
View(dfs)
attach(dfs)

library(PerformanceAnalytics)
library(corrplot)

dfcorr = subset(dfs,select = c(domain_score, engagment_score, safety_score, cost_reduction_score,
                              tps, no_of_employees, beds, uncomopnseted_care,
                              total_salaries, contract_labor,
                              net_income, cost_charge_ratio, median_income))

dfcorr$uncomopnseted_care = as.numeric(dfcorr$uncomopnseted_care)
dfcorr$total_salaries = as.numeric(dfcorr$total_salaries)

dfcorr$contract_labor = as.numeric(dfcorr$contract_labor)
dfcorr$net_income = as.numeric(dfcorr$net_income)

chart.Correlation(dfcorr)

m1 = lmer(log(roundoff_charge)~hospital_name+ruca+drug_description+Total_discharge+hospital_cost+domain_score+
                                 engagment_score+safety_score+cost_reduction_score+provider_type+type_of_control+no_of_employees+beds+uncomopnseted_care+
                                 total_salaries+contract_labor+net_income+cost_charge_ratio+median_income+teaching_hospital+(1|State), data = dfs, family = poisson(link = log))

library(lattice)
bwplot(medicare_coverage~State, data = df_final, xlab = "States", ylab = "Medicare Payout", las=2)


