###############################################################################
################################ group 2 ######################################
###############################################################################








g <- tour_group2
g <- read.csv(choose.files(), header = TRUE)


##install.packages('igraph')
library(igraph)
library(ggplot2)
library(tidyverse)
library(magrittr)

#################### check NA ################
sum(is.na(g$prefer_visit_place1)) ##0
sum(is.na(g$prefer_visit_place2)) ##전체 obs 3241개 중 158
sum(is.na(g$prefer_visit_place3)) ##전체 obs 3241개 중 513
sum(is.na(g$prefer_visit_place2) | is.na(g$prefer_visit_place3)) #515
sum(is.na(g$prefer_visit_place2) & is.na(g$prefer_visit_place3)) #156

sum(g$prefer_visit_place1 == 998 | g$prefer_visit_place1 == 999)
sum(g$prefer_visit_place2 == 998 | g$prefer_visit_place2 == 999, na.rm = T)
sum(g$prefer_visit_place2 == 998 | g$prefer_visit_place2 == 999, na.rm = T)

## 1만 쓴 사람들은 제외
g <- g[!is.na(g$prefer_visit_place2) | !is.na(g$prefer_visit_place3), ]


#####################################################
################# make link data ####################
#####################################################


# 세개의 조합으로 이루어져 있어 두 조합씩 만들어줌
prefer12 <- select(g, X, prefer_visit_place1, prefer_visit_place2)
prefer13 <- select(g, X, prefer_visit_place1, prefer_visit_place3)
prefer23 <- select(g, X, prefer_visit_place2, prefer_visit_place3)

# 이름 통일 후 합쳐줌
prefer12 <- rename(prefer12, place1=prefer_visit_place1, place2=prefer_visit_place2)
prefer13 <- rename(prefer13, place1=prefer_visit_place1, place2=prefer_visit_place3)
prefer23 <- rename(prefer23, place1=prefer_visit_place2, place2=prefer_visit_place3)
combination <- rbind(prefer12,prefer13,prefer23)


# na 값 모두 지우기
sum(is.na(combination$place1))
sum(is.na(combination$place2))
sum(is.na(combination$place1) | is.na(combination$place2))
combination <- combination[complete.cases(combination$place1) & complete.cases(combination$place2),]
sum(is.na(combination))

rm(prefer12)
rm(prefer13)
rm(prefer23)


# 조합의 순서가 다르므로 큰 숫자를 앞으로 작은 숫자를 뒤로 정렬

comb1 <- combination[combination$place1 > combination$place2,] ##place1의 값이 큰거
comb2 <- combination[combination$place1 < combination$place2,] ##place2의 값이 큰거
comb2 <- rename(comb2, place2=place1, place1=place2)
combination <- rbind(comb1, comb2)
combination <- arrange(combination, X)

rm(comb1)
rm(comb2)



##link_freq
link <- combination %>% group_by(place1,place2) %>% count()
sum(link$n)
link <- rename(link,link_freq=n)

## 각각의 place_frep 계산

#place1_freq
nplace1 <- combination %>% group_by(place1) %>% count()

#place2_freq
nplace2 <- combination %>% group_by(place2) %>% count()
sum(nplace1$n)
sum(nplace2$n)


## place_freq <- place_freq + place2_freq
nplace12 <- full_join(nplace1,nplace2,by=c('place1'='place2'))

nplace12 %<>% replace_na(list(n.x = 0))
nplace12 %<>% replace_na(list(n.y = 0))
nplace12 <- nplace12 %>% mutate(placefreq = n.x + n.y)

nplace12<-select(nplace12,-n.x,-n.y)
nplace12<-rename(nplace12,place=place1)

sum(nplace12$placefreq)

rm(nplace1)
rm(nplace2)

##place1_freq
link_data1 <- left_join(link,nplace12,by=c('place1'='place'))
link_data1 <- rename(link_data1,place1_freq=placefreq)

##place2_freq
link_data <- left_join(link_data1,nplace12,by=c('place2'='place'))
link_data <- rename(link_data , place2_freq=placefreq)

rm(link_data1)

## make probability variable
link_data<-mutate(link_data,prob=link_freq/(place1_freq*place2_freq)*nrow(combination))
link_data<-arrange(link_data,desc(prob))




#####################################################
################# make node data ####################
#####################################################

node <- read.csv("C:\\Users\\dpfk1\\Desktop\\SEIN\\P-sat\\주제분석\\한국여행 컨설팅\\association rule analisis\\names.csv", header = TRUE)
node %<>% rename(place = name, placenum = key)

place<-c(1:135,997)
area<-NULL
nodes<-cbind(place,area)
nodes<-as.data.frame(nodes)
nodes$place=as.character(nodes$place)
nodes[1:20,2]='서울'
nodes[21:27,2]='인천'
nodes[28:44,2]='경기'
nodes[45:53,2]='강원'
nodes[54:58,2]='대전'
nodes[59:62,2]='충북'
nodes[63:69,2]='충남'
nodes[70:71,2]='세종'
nodes[72:78,2]='경북'
nodes[79:87,2]='경남'
nodes[88:92,2]='대구'
nodes[93:95,2]='울산'
nodes[96:104,2]='부산'
nodes[105:107,2]='광주'
nodes[108:113,2]='전북'
nodes[114:123,2]='전남'
nodes[124:135,2]='제주도'
nodes[136,2]='etc'

nodes[1:20,3]=1
nodes[21:27,3]=2
nodes[28:44,3]=3
nodes[45:53,3]=4
nodes[54:58,3]=5
nodes[59:62,3]=6
nodes[63:69,3]=7
nodes[70:71,3]=8
nodes[72:78,3]=9
nodes[79:87,3]=10
nodes[88:92,3]=11
nodes[93:95,3]=12
nodes[96:104,3]=13
nodes[105:107,3]=14
nodes[108:113,3]=15
nodes[114:123,3]=16
nodes[124:135,3]=17
nodes[136,3]=18

node %<>% filter(placenum <= 135 | placenum == 997)

nodes <- cbind(node[,c(2,3)], nodes[,c(2,3)])
nodes %<>% rename(area = "V2", area.code = "V3")

nplace12 %<>% arrange(place)

node_data <- left_join(nplace12, nodes, by = c('place' = "placenum"))

node_data %<>% rename(place_name = place.y)


node_data$area.code <- as.factor(node_data$area.code)

str(link_data)
str(node_data)


##########################################stringr(콤마랑 괄호 없애기)
node_data
place_name <- node_data$place_name
place_name
#install.packages('qdapRegex')
library(qdapRegex)
library(stringr)
place_name <- place_name %>% rm_round()
place_name
place_name <- place_name %>% str_remove("\\)") # 해운ㄷ
place_name <- sub(",.*$","", place_name) 
node_data$place_name <- place_name
node_data





####################### modeling ######################

library(visNetwork)

## 객체 만들기
net <- graph_from_data_frame(d = link_data, vertices = node_data, directed=F)
class(net)
net

## 탐색
E(net) 
V(net)
E(net)$link_freq
V(net)$area



plot(net, edge.arrow.size = 0.5)



######### filter by link_freq & prob filtering #######

# link data
links <- link_data %>% filter(link_freq >= 15)

# node data
index <- links %>% gather(x, place, place1, place2) %>% select(place)
index <- unique(index) %>% arrange(place)
nodes <- left_join(index, node_data, by = c('place' = 'place'))


################### plot #####################

net_group2 <- graph_from_data_frame(d=links, vertices = nodes, directed=F)
plot(net_group2, edge.arrow.size=.5)


############ advances plot ################

library(RColorBrewer)
coul = brewer.pal(9, "Set1")

# Create a vector of color
my_color=coul[as.numeric(as.factor(V(net_group2)$area))]


# Make the plot
#####layout
l <- layout_with_kk(net_group2)*0.5
par(mar = c(12,2,6,3))

###### Make the plot
set.seed(1)
plot(net_group2, main="Area Network",
     edge.width = log(E(net_group2)$link_freq*0.9),
     vertex.color = my_color, 
     vertex.size = log(V(net_group2)$placefreq,)*3,
     vertex.label=V(net_group2)$place_name,
     vertex.label.color="black",
     vertex.label.font = 2,
     vertex.label.dist = 0,
     vertex.label.cex=.7,
     vertex.frame.color = my_color,
     layout=l,
     rescale = F
)


### degree centrality graph
library(RColorBrewer)
degree(net_group2, mode = 'all')


## Blue
coul = brewer.pal(6, "Blues")
V(net_group2)$color <- as.integer(5*(c.d-min(c.d))/diff(range(c.d))+1)
palette=coul[as.integer(as.factor(V(net_group3)$color))]

c.d   <- degree(net_group2, mode = 'all')
set.seed(1)
plot(net_group2,vertex.color=palette,main="Degree Centrality",
   edge.width = log(E(net_group2)$link_freq*0.9),
   vertex.size = log(V(net_group2)$placefreq,)*3,
     vertex.label=V(net_group2)$place_name,
     vertex.label.color='black',
     vertex.label.font = 2,
     vertex.label.dist = 0,
     vertex.label.cex=.7,
   vertex.frame.color = palette,
     layout=l,
   rescale=F)


############## check centrality ##############

### degree centrality

degree(net_group2, mode = 'all')


### closeness centrality

closeness(net_group2, mode = 'all', weights = NA)   # 끊겨져 있는 데이터에서는 적절하지 않음

### betweenness centrality

betweenness(net_group2 , directed = F, weights = NA)  # 마찬가지로 적절하지 않음 



nodes_group2 <- nodes
links_group2 <- links


