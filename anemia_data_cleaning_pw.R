################################################################
#Pregnant women data
################################################################
#all PW data
data_pw<-data1[data1$beneficiary_group_selected == 2 ,1:459]
View(data_pw)

#PW data of selected blocks
data_pw1<-data1[data1$beneficiary_group_selected == 2 & data1$block == " Anandapur" | data1$beneficiary_group_selected == 2 & data1$block == " Keonjhar Sadar" | data1$beneficiary_group_selected == 2 & data1$block == "Ghasipura" | data1$beneficiary_group_selected == 2 & data1$block == "Harichandanpur",1:459]

  
tab1(data_pw1$a2_remember_dob)
tab1(data_pw1$a2_dob)
class(data_pw1$a2_dob)
data_pw1$a2_dob<-as.Date(data_pw1$a2_dob,format="%m/%d/%Y")

tab1(data_pw1$interview_date)
data_pw1$interview_date<-as.Date(data_pw1$interview_date,format="%m/%d/%Y")
class(data_pw1$interview_date)

data_pw1$a2_age<-floor((data_pw1$interview_date-data_pw1$a2_dob)/365)
tab1(data_pw1$a2_age)

tab1(data_pw1$a2_reported_age)

data_pw1$a2_age_n<-NA
data_pw1$a2_age_n<-ifelse(is.na(data_pw1$a2_age),data_pw1$a2_reported_age,data_pw1$a2_age)
tab1(data_pw1$a2_age_n)
mean(data_pw1$a2_age_n)
median(data_pw1$a2_age_n)

tab1(data_pw1$respondent_marital_status)
data_pw1$respondent_marital_status_n<-ordered(data_pw1$respondent_marital_status,levels=c(1,3),labels=c("Married","Widowed"))
tab1(data_pw1$respondent_marital_status_n)

tab1(data_pw1$a2_age_at_marriage)
# data_pw1$a2_age_at_marriage_n<-cut(data_pw1$a2_age_at_marriage,breaks=)
mean(data_pw1$a2_age_at_marriage)
median(data_pw1$a2_age_at_marriage)
IQR(data_pw1$a2_age_at_marriage)
qqplot(data_pw1$a2_age_at_marriage)
skewness(data_pw1$a2_age_at_marriage)
kurtosis(data_pw1$a2_age_at_marriage)
qqline(data_pw1$a2_age_at_marriage)
hist(data_pw1$a2_age_at_marriage)

tab1(data_pw1$a2_num_living_children)
data_pw1$a2_num_living_children_n<-cut(data_pw1$a2_num_living_children,breaks = c(-1,0,1,2,10),labels = c("Never conceived before","Single child","Two chldren","More than two children"))
tab1(data_pw1$a2_num_living_children_n)

tab1(data_pw1$a2_youngest_child_age_years)
class(data_pw1$a2_youngest_child_age_years)

tab1(data_pw1$a2_youngest_child_age_months)
class(data_pw1$a2_youngest_child_age_months)

data_pw1 |> 
  filter(is.na(a2_youngest_child_age_months) & a2_num_living_children != 0)  |> 
  select(record_id,form_completed_by)

################################################
#cleaning lmp date
################################################
class(data_pw1$a2_lmp_date)
data_pw1$a2_lmp_date_n<- as.Date(data_pw1$a2_lmp_date, format = "%m/%d/%Y")
class(data_pw1$a2_lmp_date_n)
tab1(data_pw1$a2_lmp_date_n)

tab1(data_pw1$interview_date)
class(data_pw1$interview_date)
data_pw1$interview_date_n<-as.Date(data_pw1$interview_date, format = "%m/%d/%Y")
class(data_pw1$interview_date_n)
tab1(data_pw1$interview_date_n)

lmp<-data_pw1 |> 
  filter(interview_date_n <= a2_lmp_date_n) |> 
  select(record_id)
(lmp)

##Getting the trimester
data_pw1$a2_days_pregnancy<-NA
data_pw1$a2_weeks_pregnancy<-round((data_pw1$interview_date_n - data_pw1$a2_lmp_date_n)/7)
data_pw1$a2_weeks_pregnancy_n<-as.numeric(data_pw1$a2_weeks_pregnancy)
tab1(data_pw1$a2_weeks_pregnancy_n)
data_pw1$a2_trimester<-cut(data_pw1$a2_weeks_pregnancy_n,breaks = c(0,13,27,50),labels = c("1","2","3"))

weeks<-data_pw1 |> 
  filter(a2_weeks_pregnancy_n <10) |> 
  select(record_id)
(weeks)

tab1(data_pw1$a2_trimester)
class(data_pw1$a2_trimester)
data_pw1$a2_trimester_n<-as.numeric(data_pw1$a2_trimester)
class(data_pw1$a2_trimester_n)
tab1(data_pw1$a2_trimester_n)
data_pw1$a2_hb_value_cbc<-as.numeric(data_pw1$a2_hb_value_cbc)

tab1(data_pw1$a2_hb_value_documented)
tab1(data_pw1$a2_hb_value_reported)
data_pw1$a2_hb_value_doc_rep<-NA
data_pw1$a2_hb_value_doc_rep<-ifelse(
  is.na(data_pw1$a2_hb_value_documented),data_pw1$a2_hb_value_reported,data_pw1$a2_hb_value_documented)
tab1(data_pw1$a2_hb_value_doc_rep)

########################################################
#Trimester wise HB testing values categorization of PW
########################################################
data_pw1$hb_cat <- NA
data_pw1$hb_cat<-ifelse(c(data_pw1$a2_trimester_n == 1|data_pw1$a2_trimester_n == 2| data_pw1$a2_trimester_n == 3) & data_pw1$a2_hb_value_cbc < 7, "Severe",data_pw1$hb_cat)

data_pw1$hb_cat<-ifelse(c(data_pw1$a2_trimester_n == 1 |data_pw1$a2_trimester_n == 3) & data_pw1$a2_hb_value_cbc >=7  & data_pw1$a2_hb_value_cbc <=9.9 , "Moderate",data_pw1$hb_cat)
data_pw1$hb_cat<-ifelse(c(data_pw1$a2_trimester_n == 1 | data_pw1$a2_trimester_n == 3) & data_pw1$a2_hb_value_cbc >=10 & data_pw1$a2_hb_value_cbc <=10.9, "Mild",data_pw1$hb_cat)
data_pw1$hb_cat<-ifelse(c(data_pw1$a2_trimester_n == 1 | data_pw1$a2_trimester_n == 3) & data_pw1$a2_hb_value_cbc >= 11, "Normal",data_pw1$hb_cat)
data_pw1$hb_cat<-ifelse(data_pw1$a2_trimester_n == 2 & data_pw1$a2_hb_value_cbc >=7 & data_pw1$a2_hb_value_cbc <= 9.4, "Moderate",data_pw1$hb_cat)
data_pw1$hb_cat<-ifelse(data_pw1$a2_trimester_n == 2 & data_pw1$a2_hb_value_cbc >=9.5 & data_pw1$a2_hb_value_cbc <= 10.4, "Mild",data_pw1$hb_cat)
data_pw1$hb_cat<-ifelse(data_pw1$a2_trimester_n == 2 & data_pw1$a2_hb_value_cbc >= 10.5, "Normal",data_pw1$hb_cat)
tab1(data_pw1$hb_cat)

# tab1(data_pw1[data_pw1$a2_trimester_n== 1, ]$hb_cat)
# tab1(data_pw1[data_pw1$a2_trimester_n== 2, ]$hb_cat)
# tab1(data_pw1[data_pw1$a2_trimester_n== 3, ]$hb_cat)
#########################################################################
#ANC visits Done
######################################################################
tab1(data_pw1$a2_anc_visits_done)
data_pw1$a2_anc_visits_done_n<-replace(data_pw1$a2_anc_visits_done,data_pw1$a2_anc_visits_due==5,4)
tab1(data_pw1$a2_anc_visits_done_n)
data_pw1$a2_anc_visits_done_n1<-ordered(data_pw1$a2_anc_visits_done_n,levels=c(1,2,3,4,5),labels=c("One","Two","Three","Four","Not yet"))
tab1(data_pw1$a2_anc_visits_done_n1)

tab1(data_pw1$a2_anc_visits_due)
data_pw1$a2_anc_visits_due_n<-ordered(data_pw1$a2_anc_visits_due,levels=c(1,2,3,4,5),labels=c("One","Two","Three","Four","No dues"))
tab1(data_pw1$a2_anc_visits_due_n)
#######################################################################

tab1(data_pw1$a2_registration_ga)
data_pw1 |>
  filter(a2_registration_ga ==7 | a2_registration_ga ==0)|> 
  select(record_id,form_completed_by,a2_registration_ga,a2_anc_visits_due,a2_anc_place_1)

#ANC Place
tab1(data_pw1$a2_anc_place_1)
data_pw1$a2_anc_place_1_n<-ordered(data_pw1$a2_anc_place_1,levels=c(1,2,3,4,5,6,7,77,88),labels=c("At Home by ANM/ASHA","AWC","HWC/AAM","PHC/CHC","Taluk/ District Hospital","Private clinic","Medical college hospital","Not done yet","Others"))
tab1(data_pw1$a2_anc_place_1_n)

tab1(data_pw1$a2_anc_place_2)
data_pw1$a2_anc_place_2_n<-ordered(data_pw1$a2_anc_place_2,levels=c(1,2,3,4,5,6,7,77,88),labels=c("At Home by ANM/ASHA","AWC","HWC/AAM","PHC/CHC","Taluk/ District Hospital","Private clinic","Medical college hospital","Not done yet","Others"))
tab1(data_pw1$a2_anc_place_2_n)

tab1(data_pw1$a2_anc_place_3)
data_pw1$a2_anc_place_3_n<-ordered(data_pw1$a2_anc_place_3,levels=c(1,2,3,4,5,6,7,77,88),labels=c("At Home by ANM/ASHA","AWC","HWC/AAM","PHC/CHC","Taluk/ District Hospital","Private clinic","Medical college hospital","Not done yet","Others"))
tab1(data_pw1$a2_anc_place_3_n)

tab1(data_pw1$a2_anc_place_4)
data_pw1$a2_anc_place_4_n<-ordered(data_pw1$a2_anc_place_4,levels=c(1,2,3,4,5,6,7,77,88),labels=c("At Home by ANM/ASHA","AWC","HWC/AAM","PHC/CHC","Taluk/ District Hospital","Private clinic","Medical college hospital","Not done yet","Others"))
tab1(data_pw1$a2_anc_place_4_n)

#ANC non avail reason
tab1(data_pw1$a2_anc_non_avail_reason___1)
tab1(data_pw1$a2_anc_non_avail_reason___2)
tab1(data_pw1$a2_anc_non_avail_reason___3)
tab1(data_pw1$a2_anc_non_avail_reason___4)
tab1(data_pw1$a2_anc_non_avail_reason___5)
tab1(data_pw1$a2_anc_non_avail_reason___6)
tab1(data_pw1$a2_anc_non_avail_reason___7)
tab1(data_pw1$a2_anc_non_avail_reason___8)
tab1(data_pw1$a2_anc_non_avail_reason___88)

#ANC councelling
tab1(data_pw1$a2_received_anc_counseling)
data_pw1$a2_received_anc_counseling_n<-ordered(data_pw1$a2_received_anc_counseling,levels=c(0,1,77),labels=c("No","Yes","Not Applicable"))
tab1(data_pw1$a2_received_anc_counseling_n)

tab1(data_pw1$a2_anc_counseling_topics___1)
tab1(data_pw1$a2_anc_counseling_topics___2)
tab1(data_pw1$a2_anc_counseling_topics___3)
tab1(data_pw1$a2_anc_counseling_topics___4)
tab1(data_pw1$a2_anc_counseling_topics___5)
tab1(data_pw1$a2_anc_counseling_topics___6)
tab1(data_pw1$a2_anc_counseling_topics___7)
tab1(data_pw1$a2_anc_counseling_topics___8)
tab1(data_pw1$a2_anc_counseling_topics___9)
tab1(data_pw1$a2_anc_counseling_topics___10)
tab1(data_pw1$a2_anc_counseling_topics___11)
tab1(data_pw1$a2_anc_counseling_topics___88)

#Awareness about Anemia
tab1(data_pw1$a2_aware_anemia)
data_pw1$a2_aware_anemia_n<-ordered(data_pw1$a2_aware_anemia,levels=c(0,1),labels=c("No","Yes"))
tab1(data_pw1$a2_aware_anemia_n)

#Source of anemia information
tab1(data_pw1$a2_anemia_info_source___1)
tab1(data_pw1$a2_anemia_info_source___2)
tab1(data_pw1$a2_anemia_info_source___3)
tab1(data_pw1$a2_anemia_info_source___4)
tab1(data_pw1$a2_anemia_info_source___5)
tab1(data_pw1$a2_anemia_info_source___6)
tab1(data_pw1$a2_anemia_info_source___7)
tab1(data_pw1$a2_anemia_info_source___8)
tab1(data_pw1$a2_anemia_info_source___88)

#Symptoms of anemia
tab1(data_pw1$a2_anemia_symptoms___1)
tab1(data_pw1$a2_anemia_symptoms___2)
tab1(data_pw1$a2_anemia_symptoms___3)
tab1(data_pw1$a2_anemia_symptoms___4)
tab1(data_pw1$a2_anemia_symptoms___5)
tab1(data_pw1$a2_anemia_symptoms___6)
tab1(data_pw1$a2_anemia_symptoms___88)

tab1(data_pw1$a2_anemia_symptoms___99)

#Heard AMB program
tab1(data_pw1$a2_heard_amb_program)




#Section-C
#HB test done?
tab1(data_pw1$a2_hb_test_done)

tab1(data_pw1$a2_hb_test_frequency)
data_pw1$a2_hb_test_frequency_n<-ordered(data_pw1$a2_hb_test_frequency, levels=c(0,1,2,3), labels=c("Never","Only in one visit","More than one visit","In all ANC visits"))
tab1(data_pw1$a2_hb_test_frequency_n)

tab1(data_pw1$remember_last_hb_test)
data_pw1$remember_last_hb_test_n<-replace(data_pw1$remember_last_hb_test,data_pw1$a2_hb_test_done==0,NA)
tab1(data_pw1$remember_last_hb_test_n)

tab1(data_pw1$a2_test_result_known)
data_pw1$a2_test_result_known_n<-replace(data_pw1$a2_test_result_known,data_pw1$a2_hb_test_done==0,NA)
tab1(data_pw1$a2_test_result_known_n)

tab1(data_pw1$a2_test_result_type)
data_pw1$a2_test_result_type_n<-replace(data_pw1$a2_test_result_type,data_pw1$a2_test_result_known_n==0|is.na(data_pw1$a2_test_result_known_n),NA)
tab1(data_pw1$a2_test_result_type_n)
data_pw1$a2_test_result_type_n_ord<-ordered(data_pw1$a2_test_result_type_n,levels=c(1,2,3,4,99),labels=c("HB Level: Documented (if document available)","HB Level: Reported (if document not available)","I was told it was normal, I do not have anemia","I was told that I have anemia","Doesn't remember/don't know"))
tab1(data_pw1$a2_test_result_type_n_ord)

tab1(data_pw1$a2_hb_value_documented)

tab1(data_pw1$a2_hb_value_reported)

#########################################################
#categorization of HB value documented and reported of PW  
#########################################################
tab1(data_pw1$a2_hb_value_doc_rep)

data_pw1$a2_hb_val_doc_rep_cat <- NA #Trimester value is available in the main anemia data cleaning file
data_pw1$a2_hb_val_doc_rep_cat<-ifelse(c(data_pw1$a2_trimester_n == 1|data_pw1$a2_trimester_n == 2| data_pw1$a2_trimester_n == 3) & data_pw1$a2_hb_value_doc_rep < 7, "Severe",data_pw1$a2_hb_val_doc_rep_cat)
tab1(data_pw1$a2_hb_val_doc_rep_cat,graph = F)

data_pw1$a2_hb_val_doc_rep_cat<-ifelse(c(data_pw1$a2_trimester_n == 1 |data_pw1$a2_trimester_n == 3) & data_pw1$a2_hb_value_doc_rep >=7  & data_pw1$a2_hb_value_doc_rep <=9.9 , "Moderate",data_pw1$a2_hb_val_doc_rep_cat)
data_pw1$a2_hb_val_doc_rep_cat<-ifelse(c(data_pw1$a2_trimester_n == 1 | data_pw1$a2_trimester_n == 3) & data_pw1$a2_hb_value_doc_rep >=10 & data_pw1$a2_hb_value_doc_rep <=10.9, "Mild",data_pw1$a2_hb_val_doc_rep_cat)
data_pw1$a2_hb_val_doc_rep_cat<-ifelse(c(data_pw1$a2_trimester_n == 1 | data_pw1$a2_trimester_n == 3) & data_pw1$a2_hb_value_doc_rep >= 11, "Normal",data_pw1$a2_hb_val_doc_rep_cat)

data_pw1$a2_hb_val_doc_rep_cat<-ifelse(data_pw1$a2_trimester_n == 2 & data_pw1$a2_hb_value_doc_rep >=7 & data_pw1$a2_hb_value_doc_rep <= 9.4, "Moderate",data_pw1$a2_hb_val_doc_rep_cat)
data_pw1$a2_hb_val_doc_rep_cat<-ifelse(data_pw1$a2_trimester_n == 2 & data_pw1$a2_hb_value_doc_rep >=9.5 & data_pw1$a2_hb_value_doc_rep <= 10.4, "Mild",data_pw1$a2_hb_val_doc_rep_cat)
data_pw1$a2_hb_val_doc_rep_cat<-ifelse(data_pw1$a2_trimester_n == 2 & data_pw1$a2_hb_value_doc_rep >= 10.5, "Normal",data_pw1$a2_hb_val_doc_rep_cat)
tab1(data_pw1$a2_hb_val_doc_rep_cat)

tab1(data_pw1$a2_test_reason___1)
tab1(data_pw1$a2_test_reason___2)
tab1(data_pw1$a2_test_reason___3)
tab1(data_pw1$a2_test_reason___4)
tab1(data_pw1$a2_test_reason___5)
tab1(data_pw1$a2_test_reason___88)
tab1(data_pw1$a2_test_reason_88)

tab1(data_pw1$a2_test_place___1)
tab1(data_pw1$a2_test_place___2)
tab1(data_pw1$a2_test_place___3)
tab1(data_pw1$a2_test_place___4)
tab1(data_pw1$a2_test_place___5)
tab1(data_pw1$a2_test_place___6)
tab1(data_pw1$a2_test_place___7)
tab1(data_pw1$a2_test_place___88)

tab1(data_pw1$a2_test_method)
data_pw1$a2_test_method_ord<-ordered(data_pw1$a2_test_method,levels=c(1,2,3,4,88),labels=c("Blood collected by finger prick and tested by colormatching apparatus","Blood collected by finger prick and tested by digitalmachine","Blood collected from vein","Blood collected by finger prick and tested usingsome device, don't know the device","Any other"))
tab1(data_pw1$a2_test_method_ord)

tab1(data_pw1$a2_informed_anemia_cause)
data_pw1$a2_informed_anemia_cause_ord<-ordered(data_pw1$a2_informed_anemia_cause,levels=c(0,1,77),labels=c("No","Yes","Not applicable/Not anemic"))
tab1(data_pw1$a2_informed_anemia_cause_ord)

tab1(data_pw1$a2_anemia_causes_known___1)
tab1(data_pw1$a2_anemia_causes_known___2)
tab1(data_pw1$a2_anemia_causes_known___3)
tab1(data_pw1$a2_anemia_causes_known___4)
tab1(data_pw1$a2_anemia_causes_known___5)
tab1(data_pw1$a2_anemia_causes_known___88)

tab1(data_pw1$a2_took_anemia_medicine)

tab1(data_pw1$a2_medicine_doc_available)
data_pw1$a2_medicine_doc_available_n<-replace(data_pw1$a2_medicine_doc_available,data_pw1$a2_took_anemia_medicine==0,NA)
tab1(data_pw1$a2_medicine_doc_available_n)

tab1(data_pw1$a2_medicine_name,graph = F)
tab1(data_pw1$a2_medicine_start_date)
tab1(data_pw1$a2_medicine_start_month)
tab1(data_pw1$a2_medicine_strength,graph=F)
tab1(data_pw1$a2_medicine_days_prescribed)
data_pw1$a2_medicine_days_prescribed<-replace(data_pw1$a2_medicine_days_prescribed,data_pw1$a2_medicine_days_prescribed=='4'|data_pw1$a2_medicine_days_prescribed=='4 weeks',28)
data_pw1$a2_medicine_days_prescribed_n<-replace(data_pw1$a2_medicine_days_prescribed,data_pw1$a2_medicine_doc_available_n==0,NA)
tab1(data_pw1$a2_medicine_days_prescribed_n)
class(data_pw1$a2_medicine_days_prescribed_n)
data_pw1$a2_medicine_days_prescribed_n<-as.integer(data_pw1$a2_medicine_days_prescribed_n)

tab1(data_pw1$a2_medicine_taking_time,graph=F)

tab1(data_pw1$a2_medicine_days_taken,graph=F)

tab1(data_pw1$a2_medicine_dose_per_day)
data_pw1 |> filter(a2_medicine_dose_per_day >4) |> 
  select(record_id,form_completed_by,a2_medicine_dose_per_day)

tab1(data_pw1$a2_medicine_source___1)
tab1(data_pw1$a2_medicine_source___2)
tab1(data_pw1$a2_medicine_source___3)
tab1(data_pw1$a2_medicine_source___4)
tab1(data_pw1$a2_medicine_source___5)
tab1(data_pw1$a2_medicine_source___6)
tab1(data_pw1$a2_medicine_source___7)
tab1(data_pw1$a2_medicine_source___8)
tab1(data_pw1$a2_medicine_source___88)

tab1(data_pw1$a2_medicine_provider___1)
tab1(data_pw1$a2_medicine_provider___2)
tab1(data_pw1$a2_medicine_provider___3)
tab1(data_pw1$a2_medicine_provider___4)
tab1(data_pw1$a2_medicine_provider___5)
tab1(data_pw1$a2_medicine_provider___6)
tab1(data_pw1$a2_medicine_provider___7)
tab1(data_pw1$a2_medicine_provider___88)

tab1(data_pw1$a2_paid_for_medicine)
data_pw1$a2_paid_for_medicine_n<-replace(data_pw1$a2_paid_for_medicine,data_pw1$a2_took_anemia_medicine==0,NA)
tab1(data_pw1$a2_paid_for_medicine_n)

tab1(data_pw1$a2_medicine_cost_rs)

tab1(data_pw1$a2_medicine_buying_problem___1)
tab1(data_pw1$a2_medicine_buying_problem___2)
tab1(data_pw1$a2_medicine_buying_problem___3)
tab1(data_pw1$a2_medicine_buying_problem___4)
tab1(data_pw1$a2_medicine_buying_problem___88)
tab1(data_pw1$a2_medicine_buying_problem_88)

tab1(data_pw1$a2_medicine_side_effects___1)
tab1(data_pw1$a2_medicine_side_effects___2)
tab1(data_pw1$a2_medicine_side_effects___3)
tab1(data_pw1$a2_medicine_side_effects___4)
tab1(data_pw1$a2_medicine_side_effects___5)
tab1(data_pw1$a2_medicine_side_effects___6)
tab1(data_pw1$a2_medicine_side_effects___7)
tab1(data_pw1$a2_medicine_side_effects___8)
tab1(data_pw1$a2_medicine_side_effects___9)
tab1(data_pw1$a2_medicine_side_effects___10)
tab1(data_pw1$a2_medicine_side_effects___88)
tab1(data_pw1$a2_medicine_side_effects_88)

tab1(data_pw1$a2_received_injection)

tab1(data_pw1$a2_blood_transfusion_done)
data_pw1 |> filter(a2_blood_transfusion_done==1) |> 
  select(a2_hb_value_cbc,a2_hb_value_documented,a2_hb_value_reported)

tab1(data_pw1$a2_followup_visit_done)

tab1(data_pw1$a2_followup_activities___1)
tab1(data_pw1$a2_followup_activities___2)
tab1(data_pw1$a2_followup_activities___3)
tab1(data_pw1$a2_followup_activities___4)
tab1(data_pw1$a2_followup_activities___5)
tab1(data_pw1$a2_followup_activities___6)
tab1(data_pw1$a2_followup_activities___7)
tab1(data_pw1$a2_followup_activities___8)
tab1(data_pw1$a2_followup_activities___9)
tab1(data_pw1$a2_followup_activities___88)

tab1(data_pw1$a2_facility_followup_done)

tab1(data_pw1$a2_facility_followup_result___1)
tab1(data_pw1$a2_facility_followup_result___2)
tab1(data_pw1$a2_facility_followup_result___3)
tab1(data_pw1$a2_facility_followup_result___4)
tab1(data_pw1$a2_facility_followup_result___5)
tab1(data_pw1$a2_facility_followup_result___6)
tab1(data_pw1$a2_facility_followup_result___88)

tab1(data_pw1$a2_anemia_recovered)
data_pw1$a2_anemia_recovered_n<-ordered(data_pw1$a2_anemia_recovered,levels=c(0,1,77,99),labels=c("No","Yes","Not applicable","Don't Know"))
tab1(data_pw1$a2_anemia_recovered_n)

tab1(data_pw1$a2_recovery_informed_by___1,graph=F)
tab1(data_pw1$a2_recovery_informed_by___2)
tab1(data_pw1$a2_recovery_informed_by___3)
tab1(data_pw1$a2_recovery_informed_by___4)
tab1(data_pw1$a2_recovery_informed_by___5)
tab1(data_pw1$a2_recovery_informed_by___88)

tab1(data_pw1$a2_recovery_confirmed_by___1)
tab1(data_pw1$a2_recovery_confirmed_by___2)
tab1(data_pw1$a2_recovery_confirmed_by___3)
tab1(data_pw1$a2_recovery_confirmed_by___88)

#Section-D
##########
tab1(data_pw1$a2_received_ifa_prophylaxis)
data_pw1$a2_received_ifa_prophylaxis_n<-replace(data_pw1$a2_received_ifa_prophylaxis,data_pw1$a2_trimester_n == 1 & (data_pw1$a2_received_ifa_prophylaxis == 0 | data_pw1$a2_received_ifa_prophylaxis == 1),77)
data_pw1$a2_received_ifa_prophylaxis_n<-replace(data_pw1$a2_received_ifa_prophylaxis_n,(data_pw1$a2_trimester_n == 2 | data_pw1$a2_trimester_n == 3) & data_pw1$a2_received_ifa_prophylaxis_n == 77,0)
tab1(data_pw1$a2_received_ifa_prophylaxis_n)
data_pw1$a2_received_ifa_prophylaxis_n_ord<-ordered(data_pw1$a2_received_ifa_prophylaxis_n,levels=c(0,1,77),labels=c("No","Yes","Currently in First-trimester /Not applicable"))
tab1(data_pw1$a2_received_ifa_prophylaxis_n_ord)
a<-getDescriptionStatsBy(data_pw1$a2_received_ifa_prophylaxis_n==1,data_pw1$a2_trimester_n,statistics = T,show_all_values = T,header_count = T,html = T,add_total_col = T)
(a)

data_pw1 |> filter(a2_trimester_n==3) |> 
  select(a2_received_ifa_prophylaxis_n1)
tab1(data_pw1$a2_received_ifa_prophylaxis_n1)

tab1(data_pw1$a2_ifa_doc_available)
data_pw1$a2_ifa_doc_available_n<-replace(data_pw1$a2_ifa_doc_available,data_pw1$a2_received_ifa_prophylaxis==0|data_pw1$a2_received_ifa_prophylaxis==77,NA)
tab1(data_pw1$a2_ifa_doc_available_n)

tab1(data_pw1$a2_ifa_tab_name,graph = F)
tab1(data_pw1$a2_ifa_start_date)
tab1(data_pw1$a2_ifa_start_date_reported)
tab1(data_pw1$a2_ifa_strength,graph = F)
tab1(data_pw1$a2_ifa_days_prescribed,graph = F)

tab1(data_pw1$a2_ifa_taking_time)
data_pw1$a2_ifa_taking_time_ord<-ordered(data_pw1$a2_ifa_taking_time,levels=c(1,2,3,77,88),labels=c("Before meal","Immediately after having meal","One hour after having meal","Not consuming/Not applicable","Any other"))
tab1(data_pw1$a2_ifa_taking_time_ord)

tab1(data_pw1$a2_ifa_days_taken)
data_pw1$a2_ifa_days_taken_n<-replace(data_pw1$a2_ifa_days_taken,is.na(data_pw1$a2_ifa_taking_time),NA)
tab1(data_pw1$a2_ifa_days_taken_n)

tab1(data_pw1$a2_ifa_dose_per_day)

# subset_to_join<-data_pw1 |> 
#   select(record_id,a2_ifa_days_taken_n)
# data1<-data1 |> 
#   left_join(subset_to_join,by ="record_id")
# view(data1)

tab1(data_pw1$a2_ifa_source___1)
tab1(data_pw1$a2_ifa_source___2)
tab1(data_pw1$a2_ifa_source___3)
tab1(data_pw1$a2_ifa_source___4)
tab1(data_pw1$a2_ifa_source___5)
tab1(data_pw1$a2_ifa_source___6)
tab1(data_pw1$a2_ifa_source___7)
tab1(data_pw1$a2_ifa_source___88)

tab1(data_pw1$a2_ifa_provider___1)
tab1(data_pw1$a2_ifa_provider___2)
tab1(data_pw1$a2_ifa_provider___3)
tab1(data_pw1$a2_ifa_provider___4)
tab1(data_pw1$a2_ifa_provider___5)
tab1(data_pw1$a2_ifa_provider___6)
tab1(data_pw1$a2_ifa_provider___7)
tab1(data_pw1$a2_ifa_provider___88)

tab1(data_pw1$a2_ifa_noncompliance_reason___1)
tab1(data_pw1$a2_ifa_noncompliance_reason___2)
tab1(data_pw1$a2_ifa_noncompliance_reason___3)
tab1(data_pw1$a2_ifa_noncompliance_reason___4)
tab1(data_pw1$a2_ifa_noncompliance_reason___5)
tab1(data_pw1$a2_ifa_noncompliance_reason___6)
tab1(data_pw1$a2_ifa_noncompliance_reason___7)
tab1(data_pw1$a2_ifa_noncompliance_reason___88)

tab1(data_pw1$a2_dewormed_last_year)
data_pw1$a2_dewormed_last_year_n<-replace(data_pw1$a2_dewormed_last_year,is.na(data_pw1$a2_dewormed_last_year),0)
tab1(data_pw1$a2_dewormed_last_year_n)
a1<-getDescriptionStatsBy(data_pw1$a2_dewormed_last_year_n==1,data_pw1$a2_trimester_n,statistics = T,show_all_values = T,header_count = T,html = T,add_total_col = T)
(a1)

tab1(data_pw1$a2_deworming_tablets_count)

tab1(data_pw1$a2_deworming_tabs_consumed)

tab1(data_pw1$a2_deworming_consume_time___1)
data_pw1$a2_deworming_consume_time___1_n<-replace(data_pw1$a2_deworming_consume_time___1,data_pw1$a2_dewormed_last_year_n==0,NA)
tab1(data_pw1$a2_deworming_consume_time___1_n)

tab1(data_pw1$a2_deworming_consume_time___2)
data_pw1$a2_deworming_consume_time___2_n<-replace(data_pw1$a2_deworming_consume_time___2,data_pw1$a2_dewormed_last_year_n==0,NA)
tab1(data_pw1$a2_deworming_consume_time___2_n)

tab1(data_pw1$a2_deworming_consume_time___3)
data_pw1$a2_deworming_consume_time___3_n<-replace(data_pw1$a2_deworming_consume_time___3,data_pw1$a2_dewormed_last_year_n==0,NA)
tab1(data_pw1$a2_deworming_consume_time___3_n)

tab1(data_pw1$a2_deworming_provider)
data_pw1$a2_deworming_provider_ord<-ordered(data_pw1$a2_deworming_provider,levels=c(1,2,3,4,5,6,88),labels=c("ASHA/ANM/other Govt. health staff","AWW","Self by private chemist","Visited health centre for other health issue and given tablet","Health camp/National deworming day camp","Private Doctor","Any other"))
tab1(data_pw1$a2_deworming_provider_ord,graph = F)

tab1(data_pw1$a2_handwashing_times___1,graph = F)
tab1(data_pw1$a2_handwashing_times___2,graph = F)
tab1(data_pw1$a2_handwashing_times___3,graph = F)
tab1(data_pw1$a2_handwashing_times___4,graph = F)
tab1(data_pw1$a2_handwashing_times___5,graph = F)
tab1(data_pw1$a2_handwashing_times___6,graph = F)
tab1(data_pw1$a2_handwashing_times___7,graph = F)
tab1(data_pw1$a2_handwashing_times___88,graph = F)

tab1(data_pw1$a2_soap_use_frequency,graph = F)

tab1(data_pw1$a2_cereal_yes_no,graph = F)
data_pw1$a2_cereal_yes_no<-replace(data_pw1$a2_cereal_yes_no,is.na(data_pw1$a2_cereal_yes_no),0)
kable(tab1(data_pw1$a2_no_times_cereal,graph = F))

tab1(data_pw1$a2_legumes_yes_no,graph = F)
class(data_pw1$a2_legumes_yes_no)
data_pw1$a2_legumes_yes_no<-replace(data_pw1$a2_legumes_yes_no,is.na(data_pw1$a2_legumes_yes_no),0)
kable(tab1(data_pw1$a2_no_times_legumes,graph=F))

tab1(data_pw1$a2_green_veg_yes_no,graph=F)
data_pw1$a2_green_veg_yes_no<-replace(data_pw1$a2_green_veg_yes_no,is.na(data_pw1$a2_green_veg_yes_no),0)
kable(tab1(data_pw1$a2_no_times_green_veg,graph=F))

tab1(data_pw1$a2_jaggery_yes_no,graph=F)
data_pw1$a2_jaggery_yes_no<-replace(data_pw1$a2_red_meat_yes_no,is.na(data_pw1$a2_red_meat_yes_no),0)
tab1(data_pw1$a2_no_times_jaggery,graph=F)

tab1(data_pw1$a2_millets_yes_no,graph=F)
data_pw1$a2_millets_yes_no<-replace(data_pw1$a2_millets_yes_no,is.na(data_pw1$a2_millets_yes_no),0)
tab1(data_pw1$a2_no_times_millets,graph=F)
data_pw1$a2_no_times_millets<-replace(data_pw1$a2_no_times_millets,data_pw1$a2_millets_yes_no == 0,NA)

tab1(data_pw1$a2_milk_yes_no,graph = F)
data_pw1$a2_milk_yes_no<-replace(data_pw1$a2_milk_yes_no,is.na(data_pw1$a2_milk_yes_no),0)
tab1(data_pw1$a2_no_times_milk,graph=F)

tab1(data_pw1$a2_yogurt_yes_no,graph = F)
data_pw1$a2_yogurt_yes_no<-replace(data_pw1$a2_yogurt_yes_no,is.na(data_pw1$a2_yogurt_yes_no),0)
tab1(data_pw1$a2_no_times_yogurt,graph = F)

tab1(data_pw1$a2_fish_yes_no,graph = F)
data_pw1$a2_fish_yes_no<-replace(data_pw1$a2_fish_yes_no,is.na(data_pw1$a2_fish_yes_no),0)
tab1(data_pw1$a2_no_times_fish,graph = F)

tab1(data_pw1$a2_soy_product_yes_no,graph = F)
data_pw1$a2_soy_product_yes_no<-replace(data_pw1$a2_soy_product_yes_no,is.na(data_pw1$a2_soy_product_yes_no),0)
tab1(data_pw1$a2_no_times_soy_product,graph=F)

tab1(data_pw1$a2_lemon_amla_yes_no,graph = F)
data_pw1$a2_lemon_amla_yes_no<-replace(data_pw1$a2_lemon_amla_yes_no,is.na(data_pw1$a2_lemon_amla_yes_no),0)
tab1(data_pw1$a2_no_times_lemon_amla,graph = F)

tab1(data_pw1$a2_fruits_yes_no,graph = F)
data_pw1$a2_fruits_yes_no<-replace(data_pw1$a2_fruits_yes_no,is.na(data_pw1$a2_fruits_yes_no),0)
tab1(data_pw1$a2_no_times_fruits,graph = F)

tab1(data_pw1$a2_fortified_rice_yes_no,graph=F)
data_pw1$a2_fortified_rice_yes_no<-replace(data_pw1$a2_fortified_rice_yes_no,is.na(data_pw1$a2_fortified_rice_yes_no),0)
tab1(data_pw1$a2_no_times_fortified_rice,graph = F)

tab1(data_pw1$a2_egg_yes_no,graph = F)
data_pw1$a2_egg_yes_no<-replace(data_pw1$a2_egg_yes_no,is.na(data_pw1$a2_egg_yes_no),0)
tab1(data_pw1$a2_no_times_egg,graph = F)

tab1(data_pw1$a2_oth_fortified_food_yes_no,graph = F)
data_pw1$a2_oth_fortified_food_yes_no<-replace(data_pw1$a2_oth_fortified_food_yes_no,is.na(data_pw1$a2_oth_fortified_food_yes_no),0)
tab1(data_pw1$a2_no_times_oth_fortified_food,graph=F)

tab1(data_pw1$a2_specify_oth_food)

tab1(data_pw1$a2_received_thr_hcm)
data_pw1$a2_received_thr_hcm<-replace(data_pw1$a2_received_thr_hcm,is.na(data_pw1$a2_received_thr_hcm),0)

tab1(data_pw1$a2_thr_type)
data_pw1$a2_thr_type_n<-replace(data_pw1$a2_thr_type,data_pw1$a2_received_thr_hcm != 1,NA)
data_pw1$a2_thr_type_n_cat<-ordered(data_pw1$a2_thr_type_n,levels=c(1,2,88),labels=c("THR (take home ration)","Hot cooked meal","Others"))
kable(tab1(data_pw1$a2_thr_type_n_cat),)

tab1(data_pw1$a2_rice_source___1)
tab1(data_pw1$a2_pds_rice_buy_get)
tab1(data_pw1$a2_pds_rice_amount)

tab1(data_pw1$a2_rice_source___2)
tab1(data_pw1$a2_pmp_rice_amount)
tab1(data_pw1$a2_pmp_rice_buy_get)

tab1(data_pw1$a2_rice_source___3)
tab1(data_pw1$a2_private_shop_rice_amount)
tab1(data_pw1$a2_private_shop_rice_buy_get)

tab1(data_pw1$a2_rice_source___4)
tab1(data_pw1$a2_own_produce_rice_amount)
tab1(data_pw1$a2_own_produce_rice_buy_get)

tab1(data_pw1$a2_rice_procure_times_pds)
tab1(data_pw1$a2_rice_procure_times_pmp)
data_pw1$a2_rice_procure_times_pmp_1<-replace(data_pw1$a2_rice_procure_times_pmp,data_pw1$a2_rice_source___2==0,NA)
tab1(data_pw1$a2_rice_procure_times_pmp_1)
tab1(data_pw1$a2_rice_procure_times_pmp)
tab1(data_pw1$a2_rice_procure_times_shop)
tab1(data_pw1$a2_rice_procure_times_own)

tab1(data_pw1$a2_rice_fortified)

tab1(data_pw1$a2_heard_anemia_message)

kable(tab1(data_pw1$a2_message_source___1),)
kable(tab1(data_pw1$a2_message_source___2))
kable(tab1(data_pw1$a2_message_source___3))
kable(tab1(data_pw1$a2_message_source___4))
kable(tab1(data_pw1$a2_message_source___5))
kable(tab1(data_pw1$a2_message_source___6))
kable(tab1(data_pw1$a2_message_source___7))
kable(tab1(data_pw1$a2_message_source___8))
kable(tab1(data_pw1$a2_message_source___9))
kable(tab1(data_pw1$a2_message_source___10))
kable(tab1(data_pw1$a2_message_source___88))

tab1(data_pw1$a2_message_content___1)
tab1(data_pw1$a2_message_content___2)
tab1(data_pw1$a2_message_content___3)
tab1(data_pw1$a2_message_content___4)
tab1(data_pw1$a2_message_content___5)
tab1(data_pw1$a2_message_content___6)
tab1(data_pw1$a2_message_content___7)
tab1(data_pw1$a2_message_content___8)
tab1(data_pw1$a2_message_content___9)
tab1(data_pw1$a2_message_content___10)
tab1(data_pw1$a2_message_content___11)
tab1(data_pw1$a2_message_content___12)
tab1(data_pw1$a2_message_content___88)

###################################################
#Section F
###################################################

kable(tab1(data_pw1$a2_has_anemia_card))
kable(tab1(data_pw1$a2_hb_test_today))
kable(tab1(data_pw1$a2_hb_value_poc))
kable(tab1(data_pw1$a2_hb_result_recorded_card))
kable(tab1(data_pw1$a2_cbc_sample_collected))

clean_data<-write.csv(data_pw1,"c:/Rudra/clean_data.csv")

##Were you tested for anemia  in last 1 year?
tab1(data_pw$a2_hb_test_done)
tab1(data_child6_59$a3_hb_test_last_year)
tab1(data_adol_girls$a5_hb_test_last_yrs)
tab1(data_wra$a7_hb_test_last_yrs)

