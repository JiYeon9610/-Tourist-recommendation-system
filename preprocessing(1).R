data<- read.csv(choose.files(),header=T)

names(data) <- 1:219

###############select & rename variable###########

library(tidyverse)
library(magrittr)
library(ggplot2)
data_sub <- data %>% select(1,16, 21,22,23,24,25,28:32,34,38,
                            106:113, 117,119:121, 122:124,
                            125:127, 128,  139,140,141,142,143,144:150,
                            213,214,216,217,218)

data_price<-data%>% select(153,156:165)

data_renamed <- data_sub %>% rename(revisit = '1', decision_time_code = '16', purpose = '21', motivation1 = '22', motivation2 = '23', motivation3 = '24', path_get_inform1 = '25', 
                          companion_alone = '28', companion_family = '29', companion_friend_couple = '30', 
                          companion_colleague = '31', companion_etc = '32',  num_companion = '34', prefer_activity = '38', 
                          visit_seoul = '106', visit_incheon = '107',visit_gyeonggi = '108',visit_gangwon = '109',
                          visit_chungcheong = '110',visit_gyeongsang = '111',visit_jeolla = '112',visit_jeju = '113',
                          period = '117', prefer_visit_place1 = '119', prefer_visit_place2 = '120', prefer_visit_place3 = '121', 
                          shopping_goods1 = '122', shopping_goods2 = '123',shopping_goods3 = '124',
                          shopping_place1 = '125', shopping_place2 = '126', shopping_place3 = '127', satisfaction = '128', 
                          revisit_intention = '139', 
                          recommend_intention = '140', image_korea_before = '141', image_korea_after = '142', 
                          travel_type = '143', 
                          accomodations_hotel = '144',accomodations_guesthouse = '145',
                          accomodations_condo = '146',accomodations_friend = '147',accomodations_official = '148',
                          accomodations_temple = '149',accomodations_homestay = '150',
                          date_survey = '213', nation = '214',  gender = '216', job = '217', age = '218')


table(data_renamed$revisit) ##1:처음 방문 / 2: 재방문
data_renamed$revisit <- ifelse(data_renamed$revisit==1, 0, 1) ##0:처음 방문 / 1: 재방문


data_renamed$companion_alone <- ifelse(is.na(data_renamed$companion_alone), 0, 1)
data_renamed$companion_family <- ifelse(is.na(data_renamed$companion_family), 0, 1)
data_renamed$companion_friend_couple <- ifelse(is.na(data_renamed$companion_friend_couple), 0, 1)
data_renamed$companion_colleague <- ifelse(is.na(data_renamed$companion_colleague), 0, 1)
data_renamed$companion_etc <- ifelse(is.na(data_renamed$companion_etc), 0, 1)


table(data_renamed$visit_seoul)
data_renamed$visit_seoul <- ifelse(is.na(data_renamed$visit_seoul), 0, 1)
data_renamed$visit_incheon <- ifelse(is.na(data_renamed$visit_incheon), 0, 1)
data_renamed$visit_gyeonggi <- ifelse(is.na(data_renamed$visit_gyeonggi), 0, 1)
data_renamed$visit_gangwon <- ifelse(is.na(data_renamed$visit_gangwon), 0, 1)
data_renamed$visit_chungcheong <- ifelse(is.na(data_renamed$visit_chungcheong), 0, 1)
data_renamed$visit_gyeongsang <- ifelse(is.na(data_renamed$visit_gyeongsang), 0, 1)
data_renamed$visit_jeolla <- ifelse(is.na(data_renamed$visit_jeolla), 0, 1)
data_renamed$visit_jeju <- ifelse(is.na(data_renamed$visit_jeju), 0, 1)

data_renamed$accomodations_hotel <- ifelse(is.na(data_renamed$accomodations_hotel), 0, 1)
data_renamed$accomodations_guesthouse <- ifelse(is.na(data_renamed$accomodations_guesthouse), 0, 1)
data_renamed$accomodations_condo <- ifelse(is.na(data_renamed$accomodations_condo), 0, 1)
data_renamed$accomodations_friend <- ifelse(is.na(data_renamed$accomodations_friend), 0, 1)
data_renamed$accomodations_official <- ifelse(is.na(data_renamed$accomodations_official), 0, 1)
data_renamed$accomodations_temple <- ifelse(is.na(data_renamed$accomodations_temple), 0, 1)
data_renamed$accomodations_homestay <- ifelse(is.na(data_renamed$accomodations_homestay), 0, 1)



data_price_renamed <- data_price %>% rename(total_pay = '153', pay_accomodations = '156', pay_agency_nation = '157', pay_shopping = '158', pay_food = '159',
                                            pay_transportation = '160', pay_entertainment = '161', pay_agency_kor = '162', pay_culture = '163', pay_exercise = '164', pay_etc = '165')


full_data<-cbind(data_renamed, data_price_renamed)

setwd('C:/Users/dhy03/Desktop')

write.csv(full_data, file='final_visitdata_2017.csv',row.names=FALSE)
