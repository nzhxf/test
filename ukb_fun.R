library(data.table)

###################### ukb_data_process ####################3
ukb_data_process = function (bx,ethnicity = "all") {
# exclude TDI's NA
bx = bx[!is.na(bx$Townsend.deprivation.index.at.recruitment),]
bx$Townsend.deprivation.index.at.recruitment = as.numeric(scale(bx$Townsend.deprivation.index.at.recruitment))
# age
bx$Interpolated.Age.of.participant.when.non.cancer.illness.first.diagnosed[which(bx$Interpolated.Age.of.participant.when.non.cancer.illness.first.diagnosed <= 0)]=NA
# Ethnic background
if (ethnicity == "white"){
bx = bx[which(bx$Ethnic.background %in% c(1,1001,1002,1003)),]
}

if (ethnicity == "all"){
bx$Ethnic.background[which(bx$Ethnic.background %in% c(1,1001,1002,1003))] = 1
bx$Ethnic.background[which(bx$Ethnic.background %in% c(2,2001,2002,2003,2004))] = 2
bx$Ethnic.background[which(bx$Ethnic.background %in% c(3,3001,3002,3003,3004))] = 3
bx$Ethnic.background[which(bx$Ethnic.background %in% c(4,4001,4002,4003))] = 4
bx$Ethnic.background[which(bx$Ethnic.background %in% c(-3,-1,5,6))] = 5

bx$Ethnic.background = as.factor(bx$Ethnic.background)
bx$Ethnic.background = relevel(bx$Ethnic.background,ref=5)
}

# BMI
bx$BMI[is.na(bx$BMI)] = mean(bx$BMI,na.rm=T)
# Smoking status
bx$Smoking.status[which(bx$Smoking.status == -3)] = 3
bx$Smoking.status[is.na(bx$Smoking.status)] = 3
# Alcohol intake frequency
bx$Alcohol.intake.frequency[which(bx$Alcohol.intake.frequency == -3)] = 7
bx$Alcohol.intake.frequency[is.na(bx$Alcohol.intake.frequency)] = 7

# physical activity
bx$Number.of.days.week.walked.10..minutes[is.na(bx$Number.of.days.week.walked.10..minutes)]=0
bx$Number.of.days.week.walked.10..minutes[which(bx$Number.of.days.week.walked.10..minutes<0)]=0
bx$Number.of.days.week.of.moderate.physical.activity.10..minutes[is.na(bx$Number.of.days.week.of.moderate.physical.activity.10..minutes)]=0
bx$Number.of.days.week.of.moderate.physical.activity.10..minutes[which(bx$Number.of.days.week.of.moderate.physical.activity.10..minutes<0)]=0
bx$Number.of.days.week.of.vigorous.physical.activity.10..minutes[is.na(bx$Number.of.days.week.of.vigorous.physical.activity.10..minutes)]=0
bx$Number.of.days.week.of.vigorous.physical.activity.10..minutes[which(bx$Number.of.days.week.of.vigorous.physical.activity.10..minutes<0)]=0
bx$Duration.of.walks[is.na(bx$Duration.of.walks)]=0
bx$Duration.of.walks[which(bx$Duration.of.walks<0)]=0
bx$Duration.of.moderate.physical.activity[is.na(bx$Duration.of.moderate.physical.activity)]=0
bx$Duration.of.moderate.physical.activity[which(bx$Duration.of.moderate.physical.activity<0)]=0
bx$Duration.of.vigorous.activity[is.na(bx$Duration.of.vigorous.activity)]=0
bx$Duration.of.vigorous.activity[which(bx$Duration.of.vigorous.activity<0)]=0

bx$physical_activity = 3.3*bx$Number.of.days.week.walked.10..minutes*bx$Duration.of.walks+
                       4.0*bx$Number.of.days.week.of.moderate.physical.activity.10..minutes*bx$Duration.of.moderate.physical.activity+
                       8.0*bx$Number.of.days.week.of.vigorous.physical.activity.10..minutes*bx$Duration.of.vigorous.activity
bx$physical_activity[which(bx$physical_activity == 0)] = 0
bx$physical_activity[which(bx$physical_activity < 600)] = 1
bx$physical_activity[which(bx$physical_activity >= 600 & bx$physical_activity <= 3000)] = 2
bx$physical_activity[which(bx$physical_activity > 3000)] = 3

# education years
bx$Age.completed.full.time.education[which(bx$Age.completed.full.time.education < 0)] = NA
bx$Age.completed.full.time.education[is.na(bx$Age.completed.full.time.education)] = mean(bx$Age.completed.full.time.education,na.rm=T)

# income
bx$Average.total.household.income.before.tax[which(bx$Average.total.household.income.before.tax<0)] = 0
bx$Average.total.household.income.before.tax[is.na(bx$Average.total.household.income.before.tax)]=mean(bx$Average.total.household.income.before.tax,na.rm=T)

# diet score 
bx$Cooked.vegetable.intake[which(bx$Cooked.vegetable.intake<0)] = 0
bx$Salad.raw.vegetable.intake[which(bx$Salad.raw.vegetable.intake<0)] = 0
bx$vegetable = bx$Cooked.vegetable.intake+bx$Salad.raw.vegetable.intake
bx$vegetable[which(bx$vegetable<=3)]=0
bx$vegetable[which(bx$vegetable>=4)]=1

bx$Fresh.fruit.intake[which(bx$Fresh.fruit.intake<0)] = 0
bx$Dried.fruit.intake[which(bx$Dried.fruit.intake<0)] = 0
bx$fruit = bx$Fresh.fruit.intake+bx$Dried.fruit.intake
bx$fruit[which(bx$fruit<=2)]=0
bx$fruit[which(bx$fruit>=3)]=1

bx$Oily.fish.intake[which(bx$Oily.fish.intake<=1)]=0
bx$Oily.fish.intake[which(bx$Oily.fish.intake==2)]=1
bx$Oily.fish.intake[which(bx$Oily.fish.intake>=3)]=3
bx$Non.oily.fish.intake[which(bx$Non.oily.fish.intake<=1)]=0
bx$Non.oily.fish.intake[which(bx$Non.oily.fish.intake==2)]=1
bx$Non.oily.fish.intake[which(bx$Non.oily.fish.intake>=3)]=3
bx$fish = bx$Oily.fish.intake+bx$Non.oily.fish.intake
bx$fish[which(bx$fish<=1)]=0
bx$fish[which(bx$fish>=2)]=1

bx$Beef.intake[which(bx$Beef.intake<=1)]=0
bx$Beef.intake[which(bx$Beef.intake==2)]=1
bx$Beef.intake[which(bx$Beef.intake>=3)]=3
bx$Lamb.mutton.intake[which(bx$Lamb.mutton.intake<=1)]=0
bx$Lamb.mutton.intake[which(bx$Lamb.mutton.intake==2)]=1
bx$Lamb.mutton.intake[which(bx$Lamb.mutton.intake>=3)]=3
bx$Pork.intake[which(bx$Pork.intake<=1)]=0
bx$Pork.intake[which(bx$Pork.intake==2)]=1
bx$Pork.intake[which(bx$Pork.intake>=3)]=3
bx$meat = bx$Beef.intake+bx$Lamb.mutton.intake+bx$Pork.intake
bx$meat[which(bx$meat<=1)]=1
bx$meat[which(bx$meat>=2)]=0

bx$Processed.meat.intake[which(bx$Processed.meat.intake<=2)] = 1
bx$Processed.meat.intake[which(bx$Processed.meat.intake>=3)] = 0

bx$diet_score = bx$vegetable+bx$fruit+bx$fish+bx$meat+bx$Processed.meat.intake
# SBP & DBP
bx$Systolic.blood.pressure..automated.reading[which(is.na(bx$Systolic.blood.pressure..automated.reading))] = bx$Systolic.blood.pressure..manual.reading[which(is.na(bx$Systolic.blood.pressure..automated.reading))]
bx$Diastolic.blood.pressure..automated.reading[which(is.na(bx$Diastolic.blood.pressure..automated.reading))] = bx$Diastolic.blood.pressure..manual.reading[which(is.na(bx$Diastolic.blood.pressure..automated.reading))]
bx$Systolic.blood.pressure..automated.reading[is.na(bx$Systolic.blood.pressure..automated.reading)] = mean(bx$Systolic.blood.pressure..automated.reading,na.rm=T)
bx$Diastolic.blood.pressure..automated.reading[is.na(bx$Diastolic.blood.pressure..automated.reading)] = mean(bx$Diastolic.blood.pressure..automated.reading,na.rm=T)

id01 = which(bx$Medication.for.cholesterol..blood.pressure..diabetes..or.take.exogenous.hormones == 2)
id02 = which(bx$Medication.for.cholesterol..blood.pressure.or.diabetes == 2)
id_medication = union(id01,id02)
bx$Systolic.blood.pressure..automated.reading[id_medication] = bx$Systolic.blood.pressure..automated.reading[id_medication]+15
bx$Diastolic.blood.pressure..automated.reading[id_medication] = bx$Diastolic.blood.pressure..automated.reading[id_medication]+10

# os
bx$Interpolated.Age.of.participant.when.non.cancer.illness.first.diagnosed[which(bx$Interpolated.Age.of.participant.when.non.cancer.illness.first.diagnosed<0)]=NA
bx$Interpolated.Year.of.participant.when.non.cancer.illness.first.diagnosed[which(bx$Interpolated.Year.of.participant.when.non.cancer.illness.first.diagnosed<0)]=NA
bx$os_status = 0
bx$os_status[!is.na(bx$Age.at.death)] = 1
bx$os_time = 0
bx$os_time[which(bx$os_status == 1)]=bx$Age.at.death[which(bx$os_status == 1)]-bx$Interpolated.Age.of.participant.when.non.cancer.illness.first.diagnosed[which(bx$os_status == 1)]
bx$os_time[which(bx$os_status == 0)]=2016-bx$Interpolated.Year.of.participant.when.non.cancer.illness.first.diagnosed[which(bx$os_status == 0)]

out0 = data.frame(ID = bx$ID,
                 age_diagnose = bx$Interpolated.Age.of.participant.when.non.cancer.illness.first.diagnosed,
		 age_recruitment = bx$Age.at.recruitment,
                 sex = bx$Sex,
                 ethnicity = bx$Ethnic.background,
                 education = bx$Age.completed.full.time.education,
                 income = bx$Average.total.household.income.before.tax,
                 smoking = bx$Smoking.status,
                 drinking = bx$Alcohol.intake.frequency,
                 physical_activity = bx$physical_activity,
                 diet_score = bx$diet_score,
                 BMI = bx$BMI,
                 SBP = bx$Systolic.blood.pressure..automated.reading, 
                 DBP = bx$Diastolic.blood.pressure..automated.reading,
                 TDI = bx$Townsend.deprivation.index.at.recruitment,
                 Frequency_of_friend_family_visits = bx$Frequency.of.friend.family.visits,
                 Able_to_confide = bx$Able.to.confide,
                 Leisure_social_activities = bx$Leisure.social.activities,
                 os_status = bx$os_status,
                 os_time = bx$os_time
                 )

pc = data.frame(fread("C:\\Users\\Huawei\\Desktop\\UKB\\pc.csv"))[,c(1,8:17)]
out = merge(out0,pc,by.x="ID",by.y="id")

return(out)
}


############### mediation analysis ###################
library(survival)
mediation_analysis_cox = function (med_data_raw,mediator = "Frequency_of_friend_family_visits",hpt = F,ethnicity = "all") {

if (ethnicity == "white") {

if (mediator == "Frequency_of_friend_family_visits") {

med_data = med_data_raw[which(med_data_raw$Frequency_of_friend_family_visits > 0),]
med_data$Frequency_of_friend_family_visits[which(med_data$Frequency_of_friend_family_visits %in% c(1,2,3))] = 1
med_data$Frequency_of_friend_family_visits[which(med_data$Frequency_of_friend_family_visits %in% c(4,5,6,7))] = 0

#med_data$ethnicity = as.factor(med_data$ethnicity)
#med_data$ethnicity = relevel(med_data$ethnicity,ref=5)

med_data$os_time[which(med_data$os_time < 0 )] = NA
med_data = med_data[!is.na(med_data$os_time),]

  # Total
  model_total = coxph(Surv(os_time,os_status)~TDI+age_diagnose+sex+education+income+
                         smoking+drinking+physical_activity+diet_score+BMI+
			 SBP+DBP+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10,
                         data = med_data)
  rx_total = data.frame(summary(model_total)$coef)

  if (hpt == T) {
    model_total = coxph(Surv(os_time,os_status)~TDI+age_diagnose+sex+education+income+
                         smoking+drinking+physical_activity+diet_score+BMI+
			 PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10,
                         data = med_data)
    rx_total = data.frame(summary(model_total)$coef)
  }
  
  # a 
  model_a = glm(Frequency_of_friend_family_visits~TDI+age_diagnose+sex+education+income+
                         smoking+drinking+physical_activity+diet_score+BMI+
			 SBP+DBP+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10,
                         family = binomial(link='logit'),data = med_data)
  rx_a = data.frame(summary(model_a)$coef)

  if (hpt == T) {
    model_a = glm(Frequency_of_friend_family_visits~TDI+age_diagnose+sex+education+income+
                         smoking+drinking+physical_activity+diet_score+BMI+
			 +PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10,
                         family = binomial(link='logit'),data = med_data)
    rx_a = data.frame(summary(model_a)$coef)
  }
  
  # b & direct
  model_b = coxph(Surv(os_time,os_status)~Frequency_of_friend_family_visits+
                         TDI+age_diagnose+sex+education+income+
                         smoking+drinking+physical_activity+diet_score+BMI+
			 SBP+DBP+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10,
                         data = med_data)
  rx_b = data.frame(summary(model_b)$coef)

  if (hpt == T) {
    model_b = coxph(Surv(os_time,os_status)~Frequency_of_friend_family_visits+
                         TDI+age_diagnose+sex+education+income+
                         smoking+drinking+physical_activity+diet_score+BMI+
			 PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10,
                         data = med_data)
    rx_b = data.frame(summary(model_b)$coef)
  }
}

if (mediator == "Able_to_confide") {

med_data = med_data_raw[which(med_data_raw$Able_to_confide >= 0),]
med_data$Able_to_confide[which(med_data$Able_to_confide %in% c(2,1,0))] = 0
med_data$Able_to_confide[which(med_data$Able_to_confide %in% c(5,4,3))] = 1

#med_data$ethnicity = as.factor(med_data$ethnicity)
#med_data$ethnicity = relevel(med_data$ethnicity,ref=5)

med_data$os_time[which(med_data$os_time < 0 )] = NA
med_data = med_data[!is.na(med_data$os_time),]
  
  # Total
  model_total = coxph(Surv(os_time,os_status)~TDI+age_diagnose+sex+education+income+
                         smoking+drinking+physical_activity+diet_score+BMI+
			 SBP+DBP+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10,
                         data = med_data)
  rx_total = data.frame(summary(model_total)$coef)

  if (hpt == T) {
    model_total = coxph(Surv(os_time,os_status)~TDI+age_diagnose+sex+education+income+
                         smoking+drinking+physical_activity+diet_score+BMI+
			 PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10,
                         data = med_data)
    rx_total = data.frame(summary(model_total)$coef)
  }
  
  # a 
  model_a = glm(Able_to_confide~TDI+age_diagnose+sex+education+income+
                         smoking+drinking+physical_activity+diet_score+BMI+
			 SBP+DBP+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10,
                         family = binomial(link='logit'),data = med_data)
  rx_a = data.frame(summary(model_a)$coef)

  if (hpt == T) {
    model_a = glm(Able_to_confide~TDI+age_diagnose+sex+education+income+
                         smoking+drinking+physical_activity+diet_score+BMI+
			 +PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10,
                         family = binomial(link='logit'),data = med_data)
    rx_a = data.frame(summary(model_a)$coef)
  }
  
  # b & direct
  model_b = coxph(Surv(os_time,os_status)~Able_to_confide+
                         TDI+age_diagnose+sex+education+income+
                         smoking+drinking+physical_activity+diet_score+BMI+
			 SBP+DBP+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10,
                         data = med_data)
  rx_b = data.frame(summary(model_b)$coef)

  if (hpt == T) {
    model_b = coxph(Surv(os_time,os_status)~Able_to_confide+
                         TDI+age_diagnose+sex+education+income+
                         smoking+drinking+physical_activity+diet_score+BMI+
			 PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10,
                         data = med_data)
    rx_b = data.frame(summary(model_b)$coef)
  }
}

}

if (ethnicity == "all") {

if (mediator == "Frequency_of_friend_family_visits") {

med_data = med_data_raw[which(med_data_raw$Frequency_of_friend_family_visits > 0),]
med_data$Frequency_of_friend_family_visits[which(med_data$Frequency_of_friend_family_visits %in% c(1,2,3))] = 1
med_data$Frequency_of_friend_family_visits[which(med_data$Frequency_of_friend_family_visits %in% c(4,5,6,7))] = 0

#med_data$ethnicity = as.factor(med_data$ethnicity)
#med_data$ethnicity = relevel(med_data$ethnicity,ref=5)

med_data$os_time[which(med_data$os_time < 0 )] = NA
med_data = med_data[!is.na(med_data$os_time),]

  # Total
  model_total = coxph(Surv(os_time,os_status)~TDI+age_diagnose+sex+ethnicity+education+income+
                         smoking+drinking+physical_activity+diet_score+BMI+
			 SBP+DBP+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10,
                         data = med_data)
  rx_total = data.frame(summary(model_total)$coef)

  if (hpt == T) {
    model_total = coxph(Surv(os_time,os_status)~TDI+age_diagnose+sex+ethnicity+education+income+
                         smoking+drinking+physical_activity+diet_score+BMI+
			 PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10,
                         data = med_data)
    rx_total = data.frame(summary(model_total)$coef)
  }
  
  # a 
  model_a = glm(Frequency_of_friend_family_visits~TDI+age_diagnose+sex+ethnicity+education+income+
                         smoking+drinking+physical_activity+diet_score+BMI+
			 SBP+DBP+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10,
                         family = binomial(link='logit'),data = med_data)
  rx_a = data.frame(summary(model_a)$coef)

  if (hpt == T) {
    model_a = glm(Frequency_of_friend_family_visits~TDI+age_diagnose+sex+ethnicity+education+income+
                         smoking+drinking+physical_activity+diet_score+BMI+
			 +PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10,
                         family = binomial(link='logit'),data = med_data)
    rx_a = data.frame(summary(model_a)$coef)
  }
  
  # b & direct
  model_b = coxph(Surv(os_time,os_status)~Frequency_of_friend_family_visits+
                         TDI+age_diagnose+sex+ethnicity+education+income+
                         smoking+drinking+physical_activity+diet_score+BMI+
			 SBP+DBP+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10,
                         data = med_data)
  rx_b = data.frame(summary(model_b)$coef)

  if (hpt == T) {
    model_b = coxph(Surv(os_time,os_status)~Frequency_of_friend_family_visits+
                         TDI+age_diagnose+sex+ethnicity+education+income+
                         smoking+drinking+physical_activity+diet_score+BMI+
			 PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10,
                         data = med_data)
    rx_b = data.frame(summary(model_b)$coef)
  }
}

if (mediator == "Able_to_confide") {

med_data = med_data_raw[which(med_data_raw$Able_to_confide >= 0),]
med_data$Able_to_confide[which(med_data$Able_to_confide %in% c(2,1,0))] = 0
med_data$Able_to_confide[which(med_data$Able_to_confide %in% c(5,4,3))] = 1

#med_data$ethnicity = as.factor(med_data$ethnicity)
#med_data$ethnicity = relevel(med_data$ethnicity,ref=5)

med_data$os_time[which(med_data$os_time < 0 )] = NA
med_data = med_data[!is.na(med_data$os_time),]
  
  # Total
  model_total = coxph(Surv(os_time,os_status)~TDI+age_diagnose+sex+ethnicity+education+income+
                         smoking+drinking+physical_activity+diet_score+BMI+
			 SBP+DBP+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10,
                         data = med_data)
  rx_total = data.frame(summary(model_total)$coef)

  if (hpt == T) {
    model_total = coxph(Surv(os_time,os_status)~TDI+age_diagnose+sex+ethnicity+education+income+
                         smoking+drinking+physical_activity+diet_score+BMI+
			 PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10,
                         data = med_data)
    rx_total = data.frame(summary(model_total)$coef)
  }
  
  # a 
  model_a = glm(Able_to_confide~TDI+age_diagnose+sex+ethnicity+education+income+
                         smoking+drinking+physical_activity+diet_score+BMI+
			 SBP+DBP+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10,
                         family = binomial(link='logit'),data = med_data)
  rx_a = data.frame(summary(model_a)$coef)

  if (hpt == T) {
    model_a = glm(Able_to_confide~TDI+age_diagnose+sex+ethnicity+education+income+
                         smoking+drinking+physical_activity+diet_score+BMI+
			 +PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10,
                         family = binomial(link='logit'),data = med_data)
    rx_a = data.frame(summary(model_a)$coef)
  }
  
  # b & direct
  model_b = coxph(Surv(os_time,os_status)~Able_to_confide+
                         TDI+age_diagnose+sex+ethnicity+education+income+
                         smoking+drinking+physical_activity+diet_score+BMI+
			 SBP+DBP+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10,
                         data = med_data)
  rx_b = data.frame(summary(model_b)$coef)

  if (hpt == T) {
    model_b = coxph(Surv(os_time,os_status)~Able_to_confide+
                         TDI+age_diagnose+sex+ethnicity+education+income+
                         smoking+drinking+physical_activity+diet_score+BMI+
			 PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10,
                         data = med_data)
    rx_b = data.frame(summary(model_b)$coef)
  }
}

}

out = list()
out$total = rx_total
out$a = rx_a
out$b = rx_b
return(out)

}

mediation_analysis_logi = function (med_data_raw,mediator = "Frequency_of_friend_family_visits",hpt = F, ethnicity = "all") {

if (ethnicity == "white") {
if (mediator == "Frequency_of_friend_family_visits") {

med_data = med_data_raw[which(med_data_raw$Frequency_of_friend_family_visits > 0),]
med_data$Frequency_of_friend_family_visits[which(med_data$Frequency_of_friend_family_visits %in% c(1,2,3))] = 1
med_data$Frequency_of_friend_family_visits[which(med_data$Frequency_of_friend_family_visits %in% c(4,5,6,7))] = 0

#med_data$ethnicity = as.factor(med_data$ethnicity)
#med_data$ethnicity = relevel(med_data$ethnicity,ref=5)

med_data$os_time[which(med_data$os_time < 0 )] = NA
med_data = med_data[!is.na(med_data$os_time),]

  # Total
  model_total = glm(os_status~TDI+age_diagnose+sex+education+income+
                         smoking+drinking+physical_activity+diet_score+BMI+
			 SBP+DBP+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10,
                         family=binomial(link = "logit"),data = med_data)
  rx_total = data.frame(summary(model_total)$coef)

  if (hpt == T) {
    model_total = glm(os_status~TDI+age_diagnose+sex+education+income+
                         smoking+drinking+physical_activity+diet_score+BMI+
			 PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10,
                         family=binomial(link = "logit"),data = med_data)
    rx_total = data.frame(summary(model_total)$coef)
  }
  
  # a 
  model_a = glm(Frequency_of_friend_family_visits~TDI+age_diagnose+sex+education+income+
                         smoking+drinking+physical_activity+diet_score+BMI+
			 SBP+DBP+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10,
                         family = binomial(link = "logit"),data = med_data)
  rx_a = data.frame(summary(model_a)$coef)

  if (hpt == T) {
    model_a = glm(Frequency_of_friend_family_visits~TDI+age_diagnose+sex+education+income+
                         smoking+drinking+physical_activity+diet_score+BMI+
			 +PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10,
                         family = binomial(link='logit'),data = med_data)
    rx_a = data.frame(summary(model_a)$coef)
  }
  
  # b & direct
  model_b = glm(os_status~Frequency_of_friend_family_visits+
                         TDI+age_diagnose+sex+education+income+
                         smoking+drinking+physical_activity+diet_score+BMI+
			 SBP+DBP+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10,
                         family=binomial(link = "logit"),data = med_data)
  rx_b = data.frame(summary(model_b)$coef)

  if (hpt == T) {
    model_b = glm(os_status~Frequency_of_friend_family_visits+
                         TDI+age_diagnose+sex+education+income+
                         smoking+drinking+physical_activity+diet_score+BMI+
			 PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10,
                         family=binomial(link = "logit"),data = med_data)
    rx_b = data.frame(summary(model_b)$coef)
  }
}

if (mediator == "Able_to_confide") {

med_data = med_data_raw[which(med_data_raw$Able_to_confide >= 0),]
med_data$Able_to_confide[which(med_data$Able_to_confide %in% c(2,1,0))] = 0
med_data$Able_to_confide[which(med_data$Able_to_confide %in% c(5,4,3))] = 1

#med_data$ethnicity = as.factor(med_data$ethnicity)
#med_data$ethnicity = relevel(med_data$ethnicity,ref=5)

med_data$os_time[which(med_data$os_time < 0 )] = NA
med_data = med_data[!is.na(med_data$os_time),]
  
  # Total
  model_total = glm(os_status~TDI+age_diagnose+sex+education+income+
                         smoking+drinking+physical_activity+diet_score+BMI+
			 SBP+DBP+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10,
                         family=binomial(link = "logit"),data = med_data)
  rx_total = data.frame(summary(model_total)$coef)

  if (hpt == T) {
    model_total = glm(os_status~TDI+age_diagnose+sex+education+income+
                         smoking+drinking+physical_activity+diet_score+BMI+
			 PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10,
                         family=binomial(link = "logit"),data = med_data)
    rx_total = data.frame(summary(model_total)$coef)
  }
  
  # a 
  model_a = glm(Able_to_confide~TDI+age_diagnose+sex+education+income+
                         smoking+drinking+physical_activity+diet_score+BMI+
			 SBP+DBP+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10,
                         family = binomial(link='logit'),data = med_data)
  rx_a = data.frame(summary(model_a)$coef)

  if (hpt == T) {
    model_a = glm(Able_to_confide~TDI+age_diagnose+sex+education+income+
                         smoking+drinking+physical_activity+diet_score+BMI+
			 +PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10,
                         family = binomial(link='logit'),data = med_data)
    rx_a = data.frame(summary(model_a)$coef)
  }
  
  # b & direct
  model_b = glm(os_status~Able_to_confide+
                         TDI+age_diagnose+sex+education+income+
                         smoking+drinking+physical_activity+diet_score+BMI+
			 SBP+DBP+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10,
                         family=binomial(link = "logit"),data = med_data)
  rx_b = data.frame(summary(model_b)$coef)

  if (hpt == T) {
    model_b = glm(os_status~Able_to_confide+
                         TDI+age_diagnose+sex+education+income+
                         smoking+drinking+physical_activity+diet_score+BMI+
			 PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10,
                         family=binomial(link = "logit"),data = med_data)
    rx_b = data.frame(summary(model_b)$coef)
  }
}
}

if (ethnicity == "all") {
if (mediator == "Frequency_of_friend_family_visits") {

med_data = med_data_raw[which(med_data_raw$Frequency_of_friend_family_visits > 0),]
med_data$Frequency_of_friend_family_visits[which(med_data$Frequency_of_friend_family_visits %in% c(1,2,3))] = 1
med_data$Frequency_of_friend_family_visits[which(med_data$Frequency_of_friend_family_visits %in% c(4,5,6,7))] = 0

#med_data$ethnicity = as.factor(med_data$ethnicity)
#med_data$ethnicity = relevel(med_data$ethnicity,ref=5)

med_data$os_time[which(med_data$os_time < 0 )] = NA
med_data = med_data[!is.na(med_data$os_time),]

  # Total
  model_total = glm(os_status~TDI+age_diagnose+sex+ethnicity+education+income+
                         smoking+drinking+physical_activity+diet_score+BMI+
			 SBP+DBP+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10,
                         family=binomial(link = "logit"),data = med_data)
  rx_total = data.frame(summary(model_total)$coef)

  if (hpt == T) {
    model_total = glm(os_status~TDI+age_diagnose+sex+ethnicity+education+income+
                         smoking+drinking+physical_activity+diet_score+BMI+
			 PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10,
                         family=binomial(link = "logit"),data = med_data)
    rx_total = data.frame(summary(model_total)$coef)
  }
  
  # a 
  model_a = glm(Frequency_of_friend_family_visits~TDI+age_diagnose+sex+ethnicity+education+income+
                         smoking+drinking+physical_activity+diet_score+BMI+
			 SBP+DBP+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10,
                         family = binomial(link = "logit"),data = med_data)
  rx_a = data.frame(summary(model_a)$coef)

  if (hpt == T) {
    model_a = glm(Frequency_of_friend_family_visits~TDI+age_diagnose+sex+ethnicity+education+income+
                         smoking+drinking+physical_activity+diet_score+BMI+
			 +PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10,
                         family = binomial(link='logit'),data = med_data)
    rx_a = data.frame(summary(model_a)$coef)
  }
  
  # b & direct
  model_b = glm(os_status~Frequency_of_friend_family_visits+
                         TDI+age_diagnose+sex+ethnicity+education+income+
                         smoking+drinking+physical_activity+diet_score+BMI+
			 SBP+DBP+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10,
                         family=binomial(link = "logit"),data = med_data)
  rx_b = data.frame(summary(model_b)$coef)

  if (hpt == T) {
    model_b = glm(os_status~Frequency_of_friend_family_visits+
                         TDI+age_diagnose+sex+ethnicity+education+income+
                         smoking+drinking+physical_activity+diet_score+BMI+
			 PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10,
                         family=binomial(link = "logit"),data = med_data)
    rx_b = data.frame(summary(model_b)$coef)
  }
}

if (mediator == "Able_to_confide") {

med_data = med_data_raw[which(med_data_raw$Able_to_confide >= 0),]
med_data$Able_to_confide[which(med_data$Able_to_confide %in% c(2,1,0))] = 0
med_data$Able_to_confide[which(med_data$Able_to_confide %in% c(5,4,3))] = 1

#med_data$ethnicity = as.factor(med_data$ethnicity)
#med_data$ethnicity = relevel(med_data$ethnicity,ref=5)

med_data$os_time[which(med_data$os_time < 0 )] = NA
med_data = med_data[!is.na(med_data$os_time),]
  
  # Total
  model_total = glm(os_status~TDI+age_diagnose+sex+ethnicity+education+income+
                         smoking+drinking+physical_activity+diet_score+BMI+
			 SBP+DBP+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10,
                         family=binomial(link = "logit"),data = med_data)
  rx_total = data.frame(summary(model_total)$coef)

  if (hpt == T) {
    model_total = glm(os_status~TDI+age_diagnose+sex+ethnicity+education+income+
                         smoking+drinking+physical_activity+diet_score+BMI+
			 PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10,
                         family=binomial(link = "logit"),data = med_data)
    rx_total = data.frame(summary(model_total)$coef)
  }
  
  # a 
  model_a = glm(Able_to_confide~TDI+age_diagnose+sex+ethnicity+education+income+
                         smoking+drinking+physical_activity+diet_score+BMI+
			 SBP+DBP+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10,
                         family = binomial(link='logit'),data = med_data)
  rx_a = data.frame(summary(model_a)$coef)

  if (hpt == T) {
    model_a = glm(Able_to_confide~TDI+age_diagnose+sex+ethnicity+education+income+
                         smoking+drinking+physical_activity+diet_score+BMI+
			 +PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10,
                         family = binomial(link='logit'),data = med_data)
    rx_a = data.frame(summary(model_a)$coef)
  }
  
  # b & direct
  model_b = glm(os_status~Able_to_confide+
                         TDI+age_diagnose+sex+ethnicity+education+income+
                         smoking+drinking+physical_activity+diet_score+BMI+
			 SBP+DBP+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10,
                         family=binomial(link = "logit"),data = med_data)
  rx_b = data.frame(summary(model_b)$coef)

  if (hpt == T) {
    model_b = glm(os_status~Able_to_confide+
                         TDI+age_diagnose+sex+ethnicity+education+income+
                         smoking+drinking+physical_activity+diet_score+BMI+
			 PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10,
                         family=binomial(link = "logit"),data = med_data)
    rx_b = data.frame(summary(model_b)$coef)
  }
}
}

out = list()
out$total = rx_total
out$a = rx_a
out$b = rx_b
return(out)

}
