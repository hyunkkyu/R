rule<-read.csv(file.choose(), header =T) # 위에 있는 항목 이름을 표시할때 header = T 
#rule <- readLines(file.choose()) 
head(rule) 
attach(rule) 
a1 
table(a1) 
table(Gender) # 데이터가 없는 곳은 subset에서 제외한 후 다시 진행 
test <- subset(rule, select=c(,"a37","a38","a41","a42",   
                              "a43","a44","a45","a46","a47","a48","a49","a50", 
                              "a51","a53","a54","a55")) 
head(test) 
## 요인분석 
fit <- factanal(test, factors=5, rostation="varimax",score="regression") 
print(fit, cutoff=0.4, digit=3) # 0.4이상 / 3자리까지만 
e_value<- eigen(cor(test)) 
e_value # 결과값 맨 위에 $values 부분에서 1.0밑으로 내려가기전까지만 묶어서 한다. 
# a40과 같이 2개 이상의 요소에 걸릴 경우 한가지라도 0.6이상이라면 그것을 지키고 
# 둘다 0.6이 안넘는다면 지워야 한다. --> 설명력을 높이기 위해서 

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
fit3 <- factanal(test4, factors=5, rostation="varimax",score="regression") 
print(fit3, cutoff=0.4, digit=3) # 0.4이상 / 3자리까지만 
e_value<- eigen(cor(test3)) 
e_value  



IND1<-(a23+a24+a25+a29)/4
IND2<-(a20+a21+a22+a28+a30)/5
IND3<-(a6+a7+a13)/3

IND4<-(a26+a27)/2

IND5<-(a17+a18)/2
DP1<-(a41+a42+a43+a44+a45+a46+a48+a49)/8
DP2<-(a53+a54+a55)/3
DP3<-(a35+a36)/2
DP4<-(a47+a50+a51)/3
DP5<-(a37+a38)/2

DT<-cbind(rule, IND1,IND2,IND3,IND4,IND5,DP1,DP2,DP3,DP4,DP5)

head(DT)
#score 저장
INSC1<-fit3$scores[,1]
INSC2<-fit3$scores[,2]
INSC3<-fit3$scores[,3]
INSC4<-fit3$scores[,4]
INSC5<-fit3$scores[,5]
DPSC1<-fit$scores[,1]
DPSC2<-fit$scores[,2]
DPSC3<-fit$scores[,3]
DPSC4<-fit$scores[,4]

DPSC5<-fit$scores[,5]

DT<-cbind(DT,INSC1,INSC2,INSC3,INSC4,INSC5,DPSC1,DPSC2,DPSC3,DPSC4,DPSC5)
head(DT)
sen<-(a1+a2+a3+a4+a5)/5
DT<-cbind(DT,sen)
attach(DT)
summary(sen)

DT$sen_G[sen<2.800]<-1
DT$sen_G[2.800<=sen& sen<3.200]<-2
DT$sen_G[3.200<=sen& sen<3.600]<-3
DT$sen_G[3.600<=sen& sen<4.899]<-4
head(DT)
table(sen_G)

#crosstab
#1
#table(Gender,sen_G)
#2 margin 이용
Cros_T<-table(Gender,sen_G)
margin.table(Cros_T,1) #row sum
margin.table(Cros_T,2) #column sum
margin.table(Cros_T) #whole sum

chisq.test(Cros_T)

#더 편리한 방법
install.packages("gmodels")
library(gmodels)
CrossTable(Cros_T,expected = TRUE,prop.c = FALSE,prop.r = FALSE,prop.t = FALSE)

#교차분석 끝

#independent sample t test
#남녀 레저 선호도 


t.test(DP1~Gender, alt = "two.sided",var.eq=F)
t.test(DP2~Gender, alt = "two.sided",var.eq=F)
t.test(DP3~Gender, alt = "two.sided",var.eq=F)
t.test(DP4~Gender, alt = "two.sided",var.eq=T)
t.test(DP5~Gender, alt = "two.sided",var.eq=F)

t.test(IND1~Gender, alt = "two.sided",var.eq=F)
t.test(IND2~Gender, alt = "two.sided",var.eq=T)
t.test(IND3~Gender, alt = "two.sided",var.eq=T)
t.test(IND4~Gender, alt = "two.sided",var.ep=T)
t.test(IND5~Gender, alt = "two.sided",var.eq=F)

#양측검정 , 가설:차이가 있을 것이다.
#t값은 차이(순서에 따라 부호 결정), p가 0.05 보다 클 때는 평균차이가 없다.

#T검정 전 먼저 해야했던 일.->등분산 VAR테스트!
var.test(DP1~Gender)
#분산이 차이가 난다. ->t.test(DP1~Gender, alt = "two.sided",var.eq=F)로
var.test(DP2~Gender)
var.test(DP3~Gender)
var.test(DP4~Gender)
var.test(DP5~Gender)

var.test(IND1~Gender)
var.test(IND2~Gender)
var.test(IND3~Gender)
var.test(IND4~Gender)
var.test(IND5~Gender)


#paired t test
t.test(a1,a2,alt="two.sided")
#실험집단,비교집단 으로 비교한다.

install.packages("psych")
library(psych)
describeBy(IND1,sen_G)
describeBy(IND2,sen_G)
describeBy(IND3,sen_G)
describeBy(IND4,sen_G)
describeBy(IND5,sen_G)

describeBy(DP1,sen_G)
describeBy(DP2,sen_G)
describeBy(DP3,sen_G)
describeBy(DP4,sen_G)
describeBy(DP5,sen_G)
describeBy(DP5,Gender)
#one-way ANOVA
FV1<-aov(DP1~sen_G, data=DT)
summary(FV1)
#P값에(0.05이상) 의해 동호회 활동은 SEN_G그룹과 비교해 차이가 없다.
FV2<-aov(DP2~sen_G, data=DT)
summary(FV2)

FV3<-aov(DP3~sen_G, data=DT)

summary(FV3)

FV4<-aov(DP4~sen_G, data=DT)
summary(FV4)

FV5<-aov(DP5~sen_G, data=DT)
summary(FV5)

#Scheffe's Multiple Comparison
install.packages("doBy")
install.packages("agricolae")
library(doBy)
library(agricolae)
scheffe.test(FV3, "sen_G",alpha=0.05, console = TRUE)#a,b 차이
scheffe.test(FV4, "sen_G",alpha=0.05, console = TRUE)#a로 동일
scheffe.test(FV5, "sen_G",alpha=0.05, console = TRUE)#a,b차이

duncan.test(FV4,"sen_G",alpha=0.05, console = TRUE)# 같은 데이터를 사용해도 다른 결과가 나온다. 조금 더 민감하다는 뜻
duncan.test(FV5,"sen_G",alpha=0.05, console = TRUE)
#알파벳이 다른 것 들만 차이가 난다는 의미

#사후검증

#two-way ANOVA
#주효과 + 상호작용 효과
attach(DT)
FFV1<-aov(DP1~Gender+sen_G+Gender:sen_G)
summary(FFV1)
#두 개다 차이가 없다. p값>0.05
FFV2<-aov(DP2~Gender+sen_G+Gender*sen_G)
summary(FFV2)

FFV3<-aov(DP3~Gender+sen_G+Gender*sen_G)
summary(FFV3)

FFV4<-aov(DP4~Gender+sen_G+Gender*sen_G)
summary(FFV4)

FFV5<-aov(DP5~Gender+sen_G+Gender*sen_G)
summary(FFV5)

#correlation Analysis
cor(DP1,DP2)
COR_T<-subset(DT,select = c(DP1,DP2,DP3,DP4,DP5))
cor (COR_T,method = "pearson")

#scatter plot
plot(DP2,DP3)
#종속변수 끼리
cor(DP1,DP2)
COR_T<-subset(DT,select = c(IND1,IND2,IND3,IND4,IND5,DP1,DP2,DP3,DP4,DP5))
cor (COR_T,method = "pearson")
plot(DP2,IND1)
plot(DP3,IND1,pch=11)

#Multiple Regression 회귀분석
#


fit3$loadings
fit3$scores

