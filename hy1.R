rule<-read.csv(file.choose(), header =T) # 위에 있는 항목 이름을 표시할때 header = T 
#rule <- readLines(file.choose()) 
head(rule) 
attach(rule) 
a1 
table(a1) 
table(Gender) # 데이터가 없는 곳은 subset에서 제외한 후 다시 진행 
test <- subset(rule, select=c("a37","a38","a41","a42",   
                              "a43","a44","a45","a46","a47","a48","a49","a50", 
                              "a51","a53","a54","a55")) 
head(test) 
## 요인분석 
fit <- factanal(test, factors=5, rostation="varimax",scores = "regression") 
print(fit, cutoff=0.4, digit=3) # 0.4이상 / 3자리까지만 
e_value<- eigen(cor(test)) 
e_value # 결과값 맨 위에 $values 부분에서 1.0밑으로 내려가기전까지만 묶어서 한다. 
# a40과 같이 2개 이상의 요소에 걸릴 경우 한가지라도 0.6이상이라면 그것을 지키고 
# 둘다 0.6이 안넘는다면 지워야 한다. --> 설명력을 높이기 위해서 
fit$scores
#종속변수
#1 41,42,43,44,45,46,48,49 (동호회 활동)
#2 53,54,55 ( 여행)
#3 35,36 (모험형 레저)
#4 47, 50,51 (컴퓨터 게임)
#5 37,38 (구기운동)













test2 <- subset(rule, select=c("a18","a20","a21","a22",   
                               "a23","a24","a25","a26","a27","a28","a29","a30", 
                               "a31","a33","a34")) 
head(test2) 
## 요인분석 
fit1 <- factanal(test2, factors=2, rostation="varimax") 
print(fit1, cutoff=0.4, digit=3) # 0.4이상 / 3자리까지만 
e_value<- eigen(cor(test2)) 
e_value 




test3 <- subset(rule, select=c("a1", "a3","a6",   
                               "a7","a8","a10","a12","a13","a14")) 
head(test3) 
## 요인분석 
fit2 <- factanal(test3, factors=5, rostation="varimax") 
print(fit2, cutoff=0.4, digit=3) # 0.4이상 / 3자리까지만 
e_value<- eigen(cor(test3)) 
e_value 






test4 <- subset(rule, select=c("a18","a20","a21","a22",   
                               "a23","a24","a25","a26","a27","a28","a29","a30", 
                               "a31","a33","a34","a1", "a3","a6",   
                               "a7","a8","a10","a12","a13","a14"))
head(test4) 
## 요인분석 
fit3 <- factanal(test4, factors=5, rostation="varimax", scores = "regression") 
print(fit3, cutoff=0.4, digit=3) # 0.4이상 / 3자리까지만 
e_value<- eigen(cor(test3)) 
e_value  

fit3$scores




#설문 사람당 독립변수에 대한 평균
IND1<-(a23+a24+a25+a29)/4
IND2<-(a20+a21+a22+a28+a30)/5
IND3<-(a6+a7+a13)/3
IND4<-(a26+a27)/2
IND5<-(a17+a18)/2
DP1<-(a41+a42+a43+a44+a45+a46+a48+a49)/8
DP2<-(a53+a54+a55)/2
DP3<-(a35+a36)/2
DP4<-(a47+a50+a51)/3
DP5<-(a37+a38)/2 

DT <- cbind(rule, IND1,IND2,IND3,IND4,IND5,DP1,DP2,DP3,DP4,DP5)

INSC1 <- fit3$scores[,1]
INSC2 <- fit3$scores[,2]
INSC3 <- fit3$scores[,3]
INSC4 <- fit3$scores[,4]
INSC5 <- fit3$scores[,5]

DNSC1 <- fit$scores[,1]
DNSC2 <- fit$scores[,2]
DNSC3 <- fit$scores[,3]
DNSC4 <- fit$scores[,4]
DNSC5 <- fit$scores[,5]
#
DT <- cbind(DT, INSC1,INSC2,INSC3,INSC4,INSC5,DNSC1,DNSC2,DNSC3,DNSC4,DNSC5)
head(DT) 
sen <- (a1+a2+a3+a4+a5)/5 
DT<-cbind(DT, sen) 
DT 
attach(DT) 
#min~1st / 1st~median / median~3rd / 3rd~max
summary(sen) 
# 중앙값으로 범위를 나눈다.
DT$sen_G[sen<2.800] <-1
DT$sen_G[sen>=2.800 &sen<3.200] <-2
DT$sen_G[sen>=3.200 &sen<3.600] <-3
DT$sen_G[sen>=3.600] <- 4
DT$sen_G