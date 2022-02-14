

#Data set 확인
library(readxl)
library(ggplot2)
library(dplyr)
library(PerformanceAnalytics)

df_HR = read.csv('C:/Users/user/Desktop/자료/R/HR_comma_sep.csv')
dim(df_HR)
table(is.na(df_HR))
summary(df_HR)

ggplot(df_HR, aes(x=left))+geom_bar(fill="skyblue")+geom_text(stat = "count", 
                                                 aes(label=..count..),
                                                 position = position_dodge(width=1.8),
                                                 vjust=-0.5)




#salary 값 변환
df_HR = df_HR %>% 
  mutate(in_salary=ifelse(salary == 'high', 5,
                          ifelse(salary == 'medium', 3,1)))
head(df_HR)


#이직에 관한 피쳐 비교

df_left1 = df_HR %>%
  filter(left == 1)

summary(df_left1)

df_left0 = df_HR %>%
  filter(left == 0)

summary(df_left0)



#___________________feature summary__________________________
df_left1_check = df_HR %>%
  filter(left == 1) %>% 
  summarise(class = "Left",
            mean_SL=mean(satisfaction_level),
            mean_AMH=mean(average_montly_hours),
            mean_TSC = mean(time_spend_company),
            mean_P = mean(promotion_last_5years),
            mean_S = mean(in_salary),
            count = n()) %>% 
  mutate(tcount = 14999, per = count/tcount*100)
df_left1_check

df_left0_check = df_HR %>%
  filter(left == 0) %>% 
  summarise(class = "Stay",
            mean_SL=mean(satisfaction_level),
            mean_AMH=mean(average_montly_hours),
            mean_TSC = mean(time_spend_company),
            mean_P = mean(promotion_last_5years),
            mean_S = mean(in_salary),
            count = n())%>% 
  mutate(tcount = 14999, per = count/tcount*100)
df_left0_check

df_all_check = df_HR %>%
  summarise(class = "Total",
            mean_SL=mean(satisfaction_level),
            mean_AMH=mean(average_montly_hours),
            mean_TSC = mean(time_spend_company),
            mean_P = mean(promotion_last_5years),
            mean_S = mean(in_salary),
            count = n())%>% 
  mutate(tcount = 14999, per = count/tcount*100)
df_all_check

df_summary = bind_rows(df_all_check,df_left0_check,df_left1_check)
df_summary

#---------------satisfaction_level만족도---------------

ggplot(data = df_summary, aes(x = reorder(class,mean_SL), y = mean_SL))+
  geom_col(aes(fill=class), alpha = 0.5)+
  ylim(0,1)

df_SL = df_HR %>% 
  group_by(satisfaction_level) %>% 
  summarise(mean_Left = mean(left),
            count = n(),
            count_per=count/358)
df_SL

ggplot(data = df_SL, aes(x = satisfaction_level, y = mean_Left))+
  geom_col(aes(fill=satisfaction_level), alpha = 0.5)

#ggplot(data = df_SL, aes(x = satisfaction_level, y = count))+
#  geom_col(aes(fill=satisfaction_level), alpha = 0.5)

ggplot(data = df_SL, aes(x = satisfaction_level))+
  geom_col(aes(y = mean_Left,fill=satisfaction_level), alpha = 0.5)+
  geom_line(aes(y = count_per),color = 'purple')+
  scale_y_continuous(sec.axis = sec_axis(~.*1, name="count_per"))

#---------------average_montly_hours  근무시간---------------

ggplot(data = df_summary, aes(x = reorder(class,mean_AMH), y = mean_AMH))+
  geom_col(aes(fill=class), alpha = 0.5)+
  ylim(0,300)

df_AMH = df_HR %>% 
  group_by(average_montly_hours) %>% 
  summarise(mean_Left = mean(left),
            count = n(),
            count_per=count/153)
df_AMH

ggplot(data = df_AMH, aes(x = average_montly_hours, y = mean_Left))+
  geom_col(aes(fill=average_montly_hours), alpha = 0.5)

ggplot(data = df_AMH, aes(x = average_montly_hours))+
  geom_col(aes(y = mean_Left,fill=average_montly_hours), alpha = 0.5)+
  geom_line(aes(y = count_per),color = 'purple')+
  scale_y_continuous(sec.axis = sec_axis(~.*1, name="count_per"))
  
#---------------time_spend_company  근속년수---------------

ggplot(data = df_summary, aes(x = reorder(class,mean_TSC), y = mean_TSC))+
  geom_col(aes(fill=class), alpha = 0.5)
  
df_TSC = df_HR %>% 
  group_by(time_spend_company) %>% 
  summarise(mean_Left = mean(left),
            count = n(),
            count_per=count/6443)
df_TSC

ggplot(data = df_TSC, aes(x = time_spend_company, y = mean_Left))+
  geom_col(color = "blue")

ggplot(data = df_TSC, aes(x = time_spend_company))+
  geom_col(aes(y = mean_Left,fill=time_spend_company), alpha = 0.5)+
  geom_line(aes(y = count_per),color = 'purple')+
  scale_y_continuous(sec.axis = sec_axis(~.*1, name="count_per"))

#---------------promotion_last_5years 승진여부---------------

ggplot(data = df_summary, aes(x = reorder(class,mean_P), y = mean_P))+
  geom_col(aes(fill=class), alpha = 0.5)+ylim(0,0.05)

df_P = df_HR %>% 
  group_by(promotion_last_5years) %>% 
  summarise(mean_Left = mean(left),
            count = n())
df_P

ggplot(data = df_P, aes(x = promotion_last_5years, y = mean_Left))+
  geom_col(aes(fill=promotion_last_5years), alpha = 0.5)+ylim(0,0.3)

#---------------salary 월급---------------------

ggplot(data = df_summary, aes(x = reorder(class,mean_S), y = mean_S))+
  geom_col(aes(fill=class), alpha = 0.5)

df_S = df_HR %>% 
  group_by(in_salary) %>% 
  summarise(mean_Left = mean(left),
            count = n())
df_S

ggplot(data = df_S, aes(x = in_salary, y = mean_Left))+
  geom_col(aes(fill=in_salary), alpha = 0.5)


#-----------------------부서별---------------------------
df_left1_sales = df_HR %>%
  filter(left == 1) %>% 
  group_by(sales) %>% 
  summarise(class = "Left",
            mean_SL=mean(satisfaction_level),
            mean_P=mean(promotion_last_5years),
            mean_S=mean(in_salary),
            count=n()) %>% 
  mutate(tcount = 3571, per = count/tcount*100)
df_left1_sales

df_left0_sales = df_HR %>%
  filter(left == 0) %>% 
  group_by(sales) %>% 
  summarise(class = "Stay",
            mean_SL=mean(satisfaction_level),
            mean_P=mean(promotion_last_5years),
            mean_S=mean(in_salary),
            count=n())%>% 
  mutate(tcount = 11428 , per = count/tcount*100)
df_left0_sales

df_all_sales = df_HR %>%
  group_by(sales) %>%
  summarise(class = "Total",
            mean_SL=mean(satisfaction_level),
            mean_P=mean(promotion_last_5years),
            mean_S=mean(in_salary),
            count=n())%>% 
  mutate(tcount = 14999, per = count/tcount*100)
df_all_sales

#--------------------만족도 & ------------------------------
#------feature 확인 : 프로젝트 수, 평균 근무시간, 만족도----

#1. 프로젝트 수 별 월평균 근무시간

df_pj_amh <- df_HR %>%  
  group_by(number_project) %>% 
  summarise(eval_mean=mean(last_evaluation),
            amh_mean=mean(average_montly_hours))
df_pj_amh

ggplot(df_pj_amh, aes(x = number_project, y=amh_mean))+geom_col(aes(fill= number_project))
#프로젝트 개수와 월평균 근무시간은 뚜렷한 비례관계 
#프로젝트를 많이 할수록 근무시간 상승

#2. 프로젝트 수 별 만족도
df_pj_sl <- df_HR %>% 
  group_by(number_project) %>% 
  summarise(mean_sl=mean(satisfaction_level)) 
df_pj_sl

ggplot(df_pj_sl, aes(x =number_project, y = mean_sl))+geom_col(aes(fill= number_project))
#직원들이 가장 만족하는 프로젝트 개수는 중간인 3, 4, 5
#너무 적어도(2) 만족도가 떨어지고 
#너무 많으면(7) 만족도가 확연히 떨어짐


#3. 월평균 근무시간 별 만족도
df_amh_sl <- df_HR %>% 
  group_by(average_montly_hours) %>% 
  summarise(mean_sl=mean(satisfaction_level))

ggplot(df_amh_sl, aes(x = average_montly_hours , y = mean_sl))+geom_col(aes(fill= average_montly_hours))+geom_vline(xintercept=mean(df_HR$average_montly_hours), linetype='dashed')
mean(df_HR$average_montly_hours)
#프로젝트 수 별 만족도와 동일한 형태(그래프 모양 같음)
#중간의 180-240 시간대에 만족도가 가장 높고 280이상이 넘어가는 순간 확연히 낮아짐
df_HR

#----직원들이 가장 적합하다고 느끼는(만족하는) 월평균 근무시간 대 및 프로젝트 개수 확인------
#1. 직원들이 만족하는 최적의 월평균 근무시간 
df_class_amh <- df_HR %>% 
  filter(average_montly_hours>180 & average_montly_hours <240) %>%
  mutate(class_amh= ifelse(average_montly_hours<=190, '180-190',
                           ifelse(average_montly_hours<=200, '190-200',
                                  ifelse(average_montly_hours<=210, '200-210',
                                         ifelse(average_montly_hours<=220, '210-220',
                                                ifelse(average_montly_hours<=230, '220-230',
                                                       ifelse(average_montly_hours<=240, '230-240')))))),
         count=n()) %>% 
  group_by(class_amh) %>% 
  summarise(mean_sl= mean(satisfaction_level)) %>% 
  arrange(desc(mean_sl))
df_class_amh

ggplot(df_class_amh, aes(x = class_amh , y = mean_sl))+
  geom_col(aes(fill= class_amh))+
  ylim(0,1)
#근무시간대 별 만족도 그래프를 통해 직원들이 만족하는 시간대 추출 : 180 ~ 240
#세밀한 분석을 위해 15씩 구간을 나누어 class 설정 : class_amh
#직원들이 가장 만족하는 시간대는 211 ~ 240 
#(참고 : 이는 평균인 201시간보다 높은 수치이지만...가장 만족도가 높다고 한다) 
#summary(df_HR)


#2. 최적의 프로젝트 개수 
df_pj_345 <- df_HR %>% 
  filter(number_project==3 | number_project==4 | number_project==5,
         average_montly_hours>=211 & average_montly_hours <240) %>% 
  group_by(number_project) %>% 
  summarise(mean_sl = mean(satisfaction_level),
            mean_amh= mean(average_montly_hours),
            mean_left = mean(left),
            count = n())
df_pj_345

#ggplot(df_pj_345, aes(x = number_project , y = mean_sl, fill=number_project))+geom_col()+ylim(0,1)
ggplot(data = df_pj_345, aes(x = number_project))+
  geom_col(aes(y = mean_sl,fill=number_project), alpha = 0.3)+
  geom_col(aes(y = mean_left,fill=number_project), alpha = 0.9)+
  scale_y_continuous(sec.axis = sec_axis(~.*1, name="mean_sl"))+
  ylim(0,1)
#만족도 : 3<5<4 순으로 높음

#------그럼 위에서 선정한 조건으로 이직상황을 확인해보자--------
#1.프로젝트 수 별 이직평균
df_left= df_HR %>% 
  group_by(number_project) %>% 
  summarise(mean_left=(mean(left)))
df_left

ggplot(df_left, aes(x = number_project, y=mean_left, fill=number_project))+geom_col()
#앞서 말한 내용과 마찬가지로 가운데 피처들(3,4)의 이직율은 낮지만
#가장 적은 프로젝트 개수인 2개와 가장 많은 7개는 이직평균이 높음
#특히 7개는 이직율 100%를 보여주며, 이는 뒤에서 자세히 확인해보자
#따라서 직원들이 가장 만족하는 월평균 근무시간은 211 ~ 240, 프로젝트 개수는 3~4임을 도출

#2. 프로젝트 개수에 따른 만족도와 이직의 상관관계
df_pj_sl_left <- df_HR %>%  
  group_by(number_project) %>% 
  summarise(mean_sl = mean(satisfaction_level),
            mean_left=(mean(left)))
df_pj_sl_left

ggplot(data = df_pj_sl_left, aes(x = number_project))+
  geom_col(aes(y = mean_left,fill=number_project), alpha = 0.9)+
  geom_line(aes(y = mean_sl,fill=number_project, color="purple",size=1))+
  scale_y_continuous(sec.axis = sec_axis(~.*1, name="mean_sl"))

#만족도를 나타내는 막대형 그래프는 아치형으로 프로젝스 개수가 적거나 많으면 만족도가 하락, 적당하면 만족도 증가
#이직평균을 나타내는 꺾은선 그래프는 U자형으로 프로젝트 개수가 적거나 많으면 만족도 이직율 증가, 적당하면 이직율 하락
#즉, 만족도와 이직은 반비례 관계
#->회사는 직원들의 이직을 막기위해 근무환경을 개선해야함
#->따라서 직원들이 가장 만족한다고 추정한 월평균 근무시간 211 ~ 240, 프로젝트 개수 3~4로 조율하면 이직이 줄어들 것

#------------------이직율 100%의 프로젝트 7---------------------------
#프로젝트 개수 7개를 수행한 직원 100%가 이직함
#만족도와 함께 다른 피쳐들을 확인해보자

#프로젝트 개수 별 직원 만족도 및 평가
pj_left_pj <- df_HR %>%  
  group_by(number_project) %>% 
  summarise(mean_le=mean(last_evaluation*100),
            mean_amh=mean(average_montly_hours),
            mean_sl=mean(satisfaction_level)*100,
            mean_p=mean(promotion_last_5years)*100,
            mean_tsc=mean(time_spend_company))
pj_left_pj

ggplot(data = pj_left_pj, aes(x = number_project))+
  geom_col(aes(y = mean_le,fill=number_project), alpha = 0.5)+ #평가평균 
  geom_col(aes(y = mean_sl,fill=number_project), alpha = 0.9)+ #만족도 평균
  geom_line(aes(y = mean_amh,fill=number_project, color="red"), size=1)+ #월근무 시간 평균
  scale_y_continuous(sec.axis = sec_axis(~.*1, name="mean_amh"))
#프로젝트 7개를 수행한 직원의 평가점수 평균(연한 막대 그래프)과 월평균 근무시간(꺾은선 그래프)이 
#7개보다 덜 한 직원들보다 더 높은 것을 확인할 수 있음
#하지만 만족도(연한 막대그래프)는 훨씬 떨어지는 것을 확인할 수 있음 -> 업무부하
#과도한 업무를 수행하면서 승진을 하였는지 확인해보자

#프로젝트 개수 별 승진 및 근속년수 확인
df_promo1 = df_HR %>% 
  filter(promotion_last_5years == 1) #승진평균 산출을 위해 승진자만 필터링
mean(df_promo1$time_spend_company)

ggplot(data = pj_left_pj, aes(x = number_project))+
  geom_col(aes(y = mean_p,fill=number_project), alpha = 0.9)+ #승진 평균
  geom_line(aes(y = mean_tsc,fill=number_project, color="red"),size=1)+ #근속년수 평균
  scale_y_continuous(sec.axis = sec_axis(~.*1, name="mean_tsc"))+
  geom_hline(yintercept=mean(df_promo1$time_spend_company), linetype='dashed') #평균 승진연도 가로평균선
#프로젝트 7개를 수행한 직원 중 아무도 승진하지 못한 것을 확인 (막대그래프)
#7프로젝트 7개 수행 직원들의 평균 근속년수(꺾은선 그래프)가 4년을 넘는 것을 보면 승진을 기대했었을 가능성 존재
#승진이라는 보상을 받았으면 이직율이 줄었을 것을 추측 가능

#위에서 제시한 내용과 같이 직원들이 만족하는 근무 시간대와 프로젝트 개수로 업부부하를 줄여주고 승진이라는 보상을 주면 이직이 줄어들 것



#--------------------승진하기------------------------------
#--------------------승진 & 사고여부 ------------------------------

df_WA_PR = df_HR  %>% 
  group_by(Work_accident) %>% 
  summarise(count_p = sum(promotion_last_5years),
            mean_P = mean(promotion_last_5years),
            count = n()) %>% 
  mutate(per=count_p/14999*100)
df_WA_PR


ggplot(data = df_WA_PR, aes(x = reorder(Work_accident,mean_P), y = mean_P))+
  geom_col(aes(fill=Work_accident), alpha = 0.5)+
  ylim(0,0.05)

ggplot(data = df_WA_PR, aes(x = reorder(Work_accident,count_p), y = count_p))+
  geom_col(aes(fill=Work_accident), alpha = 0.5)+
  ylim(0,300)

#부서별 사고 평균
df_sales_Wp = df_HR %>%
  group_by(sales) %>%
  summarise(mean_wa=mean(Work_accident),
            count_wa=sum(Work_accident),
            mean_p=mean(promotion_last_5years),
            count=n())%>% 
  mutate(tcount = 2169, per_s = count_wa/count*100, per_t = count_wa/tcount*100)
df_sales_Wp

ggplot(data = df_sales_Wp, aes(x = reorder(sales,per_s)))+
  geom_col(aes(y = per_s,fill=sales), alpha = 0.5)+
  geom_col(aes(y = per_t,fill=sales), alpha = 0.5)+
  scale_y_continuous(sec.axis = sec_axis(~.*1, name="per_t"))+
  coord_flip()

#부서별 승진 평균
df_sales_p = df_HR %>%
  group_by(sales) %>%
  summarise(mean_P=mean(promotion_last_5years),
            count_p=sum(promotion_last_5years),
            count=n())%>% 
  mutate(tcount = 319, per_s = count_p/count*100, per_t = count_p/tcount*100)
df_sales_p
 
ggplot(data = df_sales_p, aes(x = reorder(sales,per_s)))+
  geom_col(aes(y = per_s,fill=sales), alpha = 0.9)+
  geom_col(aes(y = per_t,fill=sales), alpha = 0.3)+
  scale_y_continuous(sec.axis = sec_axis(~.*1, name="per_t"))+
  coord_flip()


#--------------------평가점수 & 승진 ------------------------------

df_LE_PR = df_HR  %>% 
  group_by(last_evaluation) %>% 
  summarise(count_p = sum(promotion_last_5years),
            mean_P = mean(promotion_last_5years),
            count = n()) %>% 
  mutate(per=count_p/14999*100)
df_LE_PR
tail(df_LE_PR)

ggplot(data = df_LE_PR, aes(x = last_evaluation, y = mean_P))+
  geom_col(aes(fill=last_evaluation), alpha = 0.5)

df_sales_le = df_HR %>%
  group_by(sales) %>%
  summarise(mean_le=mean(last_evaluation),
            count=n())
df_sales_le

ggplot(data = df_sales_le, aes(x = reorder(sales,mean_le), y = mean_le))+
  geom_col(aes(fill=sales), alpha = 0.5)+
  ylim(0,1)+
  coord_flip()

#--------------------근속년수 & 승진 ------------------------------

df_TSC_PR = df_HR  %>% 
  group_by(time_spend_company) %>% 
  summarise(count_p = sum(promotion_last_5years),
            mean_P = mean(promotion_last_5years),
            count = n()) %>% 
  mutate(per=count_p/319*100)
df_TSC_PR

ggplot(data = df_TSC_PR, aes(x = time_spend_company, y = mean_P))+
  geom_col(aes(fill=time_spend_company), alpha = 0.5)

df_sales_TSC = df_HR %>%
  group_by(sales) %>%
  summarise(mean_tsc=mean(time_spend_company),
            count=n())
df_sales_TSC

ggplot(data = df_sales_TSC, aes(x = reorder(sales,mean_tsc), y = mean_tsc))+
  geom_col(aes(fill=sales), alpha = 0.5)+
  coord_flip()


#--------------------월급 올리기------------------------------
#-------------------- 프로젝트 당 시간 / 평가점수 / 월급------------------------
#이직율을 줄이고 업무만족도를 높이는데 직원의 노력도 필요하다
#업무 효율을 높여 평가 고득점하면 승진 혹은 월급상승으로 만족도를 높일 수 있을 것
#업무 효율을 판단하는 기준으로 프로젝트 하나를 끝내는데 소요하는 시간이 될 수 있다는 가설을 세우고 검증해 보았다.

#프로젝트 하나 당 소요되는 시간과 평가점수 & 임금 확인
df_per_pj_time = df_HR %>%
  group_by(number_project) %>% 
  mutate(per_amh=(average_montly_hours/number_project)) %>% 
  summarise(mean_amh=mean(per_amh))
df_per_pj_time

ggplot(df_per_pj_time, aes(number_project, mean_amh,fill=number_project))+geom_col()
#프로젝트 하나 당 시간을 계산하기 위해 월평균 근무시간/프로젝트 개수를 계산 
#위를 계산하면 업무 효율성이 나올 것이라 예상했는데 프로젝트 2개에 평균 80시간 소요
#업무 효율성이 떨어져 하나의 프로젝트에 80시간 걸린 것이 아니라 장기프로젝트의 존재 가능성 확인 
#위의 가설이 틀릴 가능성 높음
df_per_pj_time_new = df_HR %>%
  mutate(per_amh=(average_montly_hours/number_project))
df_per_pj_time_new


df_pj_time = df_per_pj_time_new %>% 
  mutate(class_per_amh=ifelse(per_amh<=25,"1. 10-25",
                              ifelse(per_amh<=40,"2. 26-40",
                                     ifelse(per_amh<=55,"3. 41-55",
                                            ifelse(per_amh<=70, "4. 56-70",
                                                   ifelse(per_amh<=71,"5.71-90",
                                                          ifelse(per_amh<91, "6. 91-105",
                                                                 ifelse(per_amh<=106,"7. 106-115",
                                                                        ifelse(per_amh<=130, "8. 116-130",
                                                                               ifelse(per_amh<=155,"9. 131-155")))))))))) %>% 
  group_by(class_per_amh) %>% 
  summarise(mean_TSC=mean(time_spend_company),
            mean_le = mean(last_evaluation),
            mean_salary = mean(in_salary)/5,
            count=n())



df_pj_time  
ggplot(data = df_pj_time, aes(x = class_per_amh))+
  geom_col(aes(y= mean_le, fill=class_per_amh), alpha = 0.5)+
  geom_col(aes(y= mean_salary,fill=class_per_amh), alpha = 0.9)+
  scale_y_continuous(sec.axis = sec_axis(~.*1, name="mean_sal"))
#효율성 가설 검증을 위해 15시간 씩 클래스 구분 : per_amh
#평가 : 클래스 당 규칙성이 없어 효율성과의 관계 알 수 없음
#임금 : 클래스 당 차이가 미비하여 큰 영향을 끼치는지 파악이 어려움
#프로젝트 개수와 걸린 시간이 평가에 큰 비중을 차지하지 않는 것을 보아 무조건 프로젝트를 빨리 끝낸다고고 해서 점수를 잘 받는 것은 아님



#-------------------- 근속년수 / 월급------------------------

df_TSC_SA = df_HR  %>% 
  group_by(time_spend_company) %>% 
  summarise(mean_sa = mean(in_salary),
            count = n()) %>% 
  mutate(per=count/14999*100)
df_TSC_SA

ggplot(data = df_TSC_SA, aes(x = time_spend_company, y = mean_sa))+
  geom_col(aes(fill=time_spend_company), alpha = 0.5)

df_sales_sa = df_HR %>%
  group_by(sales) %>%
  summarise(mean_sa=mean(in_salary),
            count=n())
df_sales_sa

ggplot(data = df_sales_sa, aes(x = reorder(sales,mean_sa), y = mean_sa))+
  geom_col(aes(fill=sales), alpha = 0.5)+
  coord_flip()



#-----------------------------프로젝트 7개와 나머지 실적비교-------------
df_HR

df_p7 = df_HR %>% 
  filter(number_project == 7) #프로젝트 7개 수행 직원 필터링

df_p_2to6 = df_HR %>% 
  filter(number_project != 7) #프로젝트 2~6개 수행 직원 필터링

df_p7  %>% #
  summarise(salary = mean(in_salary),
            evaluation = mean(last_evaluation),
            satisfaction = mean(satisfaction_level),
            hours = mean(average_montly_hours),
            promition = mean(promotion_last_5years),
            years = mean(time_spend_company),
            count = n()) %>% 
  mutate(per=count/14999*100)

df_p_2to6  %>% 
  summarise(salary = mean(in_salary),
            evaluation = mean(last_evaluation),
            satisfaction = mean(satisfaction_level),
            hours = mean(average_montly_hours),
            promition = mean(promotion_last_5years),
            years = mean(time_spend_company),
            count = n()) %>% 
  mutate(per=count/14999*100)

df_HR%>% 
  summarise(salary = mean(in_salary),
            evaluation = mean(last_evaluation),
            satisfaction = mean(satisfaction_level),
            hours = mean(average_montly_hours),
            promition = mean(promotion_last_5years),
            years = mean(time_spend_company),
            count = n()) %>% 
  mutate(per=count/14999*100)

#--------------------결론 Left 비율 낮추기-------------------------------
#-------------------------------------------------------
#-------------------------------------------------------






