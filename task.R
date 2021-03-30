#清理当前工作空间
rm(list=ls())	

#使用路径方法，解决写死路径的问题
library(here)

#读取数据
data=read.csv(here("aug_train.csv"),header=T)  #读取csv格式的数据，并赋值给data
data_train=read.csv(here("aug_test.csv"),header=T)  #读取csv格式的数据，并赋值给data

#数据清理，删除雇佣id和城市id，对于数据分析没有意义
data=data[,-(1:2)]  #删除第1列到第2列的数据（-:代表删除（*，*）第1个*表示行，第2个*表示列）
data_train=data_train[,-(1:2)]

names(data)=c("city_dev","gender","expenience","university","edu_level","major","expenience_year","company_size","company_type","jobs","train_hours","target") #重新命名
names(data_train)=c("city_dev","gender","expenience","university","edu_level","major","expenience_year","company_size","company_type","jobs","train_hours","target") #重新命名


#构造一个函数 参数v-一列数据 功能--获取数据中的众数）
getmode <- function(v) { 
  uniqv <- unique(v) # 获取唯一值
  uniqv[which.max(tabulate(match(v, uniqv)))] # 得到众数
}

city_dev_avg=mean(data[,1])  #获取表中city_dev的平均数
gender_med=getmode(data[,2])  #获取表中gender的众数
expenience_med=getmode(data[,3])  #获取表中expenience的众数
university_med=getmode(data[,4])  #获取表中university的众数
edu_level_med=getmode(data[,5])  #获取表中edu_level的众数
major_med=getmode(data[,6])  #获取表中major的众数
expenience_year_med=getmode(data[,7])  #获取表中expenience_year的众数
# company_size_med=getmode(data[,8])  #获取表中company_size的众数，目前发现空字符串是最多的，这里就虚拟出一个other
company_size_med="Other" # 虚拟出other
company_type_med=getmode(data[,9])  #获取表中company_type的众数
jobs_med=getmode(data[,10])  #获取表中jobs的众数
train_hours_med=mean(data[,11])  #获取表中train_hours的平均数

# 数据处理
data[which(data$gender==""),"gender"] = gender_med # 为空的补上max值
data[which(data$expenience==""),"expenience"] = expenience_med # 为空的补上max值
data[which(data$university==""),"university"] = university_med # 为空的补上max值
data[which(data$edu_level==""),"edu_level"] = edu_level_med # 为空的补上max值
data[which(data$major==""),"major"] = major_med # 为空的补上max值
data[which(data$expenience_year==""),"expenience_year"] = expenience_year_med # 为空的补上max值
data[which(data$company_size==""),"company_size"] = company_size_med # 为空的补上max值
data[which(data$company_type==""),"company_type"] = company_type_med # 为空的补上max值
data[which(data$jobs==""),"jobs"] = jobs_med # 为空的补上max值
data$train_hours=scale(data$train_hours)

# 测试数据
data_train[which(data_train$gender==""),"gender"] = gender_med # 为空的补上max值
data_train[which(data_train$expenience==""),"expenience"] = expenience_med # 为空的补上max值
data_train[which(data_train$university==""),"university"] = university_med # 为空的补上max值
data_train[which(data_train$edu_level==""),"edu_level"] = edu_level_med # 为空的性别补上max值
data_train[which(data_train$major==""),"major"] = major_med # 为空的性别补上max值
data_train[which(data_train$expenience_year==""),"expenience_year"] = expenience_year_med # 为空的性别补上max值
data_train[which(data_train$company_size==""),"company_size"] = company_size_med # 为空的性别补上max值
data_train[which(data_train$company_type==""),"company_type"] = company_type_med # 为空的性别补上max值
data_train[which(data_train$jobs==""),"jobs"] = jobs_med # 为空的性别补上max值
data_train$train_hours=scale(data_train$train_hours)

# 查看序列数据情况
unique(data$gender) # "Male"   "Female" "Other" 
unique(data$expenience) #  "Has relevent experience" "No relevent experience" 
unique(data$university) # "no_enrollment"    "Full time course" "Part time course"
unique(data$edu_level) # "Graduate"       "Masters"        "High School"    ""               "Phd"            "Primary School"
unique(data$major) # "STEM"            "Business Degree" "Arts"            "Humanities"      "No Major"        "Other"      
sort(unique(data$expenience_year)) # "<1"  ">20" "1"   "10"  "11"  "12"  "13"  "14"  "15"  "16"  "17"  "18"  "19"  "2"   "20"  "3"   "4"   "5"   "6"   "7"   "8"   "9"  
unique(data$company_size) # "Others"    "50-99"     "<10"       "10000+"    "5000-9999" "1000-4999" "10/49"     "100-500"   "500-999" 
unique(data$company_type) # "Pvt Ltd"             "Funded Startup"      "Early Stage Startup" "Other"               "Public Sector"       "NGO"              
unique(data$jobs) # "1"     ">4"    "never" "4"     "3"     "2"    


# 统计函数
descrb = function(var){
  Z=data[,var]
  N=tapply(data$target,Z,length)
  MU=tapply(data$target,Z,mean)
  SD=tapply(data$target,Z,sd)
  MIN=tapply(data$target,Z,min)
  MED=tapply(data$target,Z,median)
  MAX=tapply(data$target,Z,max)
  out=cbind(N,MU,SD,MIN,MED,MAX)
  out
}

descrb("train_hours")

# 序列类型数据频数
# 性别 男性远远大于女性
barplot(table(data$gender),xlab="gender",main=NULL)

# 相关经验 有相关工作经验的比较多
barplot(table(data$expenience),xlab="expenience",main=NULL)

# 大学入学情况 很多都没有上大学
barplot(table(data$university),xlab="university",main=NULL)

# 教育水平
barplot(table(data$edu_level),xlab="edu_level",main=NULL)

# 专业
barplot(table(data$major),xlab="major",main=NULL)

# 工作经验，需要处理一下，具体看下面步骤
barplot(table(data$expenience_year),xlab="expenience year",main=NULL)

# 公司大小 绝大多数都是理工科
barplot(table(data$company_size),xlab="company size",main=NULL)

# 公司类型
barplot(table(data$company_type),xlab="company size",main=NULL)

# 工作更换次数
barplot(table(data$jobs),xlab="jobs",main=NULL)

# 数字类型数据情况
barplot(table(data$train_hours),xlab="jobs",main=NULL)

# 培训时长
descrb("gender")
descrb("expenience")
descrb("university")
descrb("edu_level")
descrb("major")
descrb("expenience_year")
descrb("company_size")
descrb("company_type")
descrb("jobs")
descrb("train_hours")


# 通过分析，可以考虑将工作经验做离散化处理
data[which(data$expenience_year=="<1"),"expenience_year"] = 1 # 1年以下工作经验归到1年
data[which(data$expenience_year==">20"),"expenience_year"] = 21 # 大于20用虚拟21来代替
# 排序查看数据分别
data$expenience_year=as.numeric(data$expenience_year)
barplot(table(sort(data$expenience_year)),xlab="expenience year",main=NULL)

# 按照1-5，5-10，10-20，20+ 四个段进行数据划分
data$expenience_year_new = 1 * (data$expenience_year<=5) + 2 * (data$expenience_year>5 & data$expenience_year<=10) + 3 * (data$expenience_year >10 & data$expenience_year <= 15) + 4 * (data$expenience_year >15 & data$expenience_year <= 20) + 5 * (data$expenience_year > 20)
barplot(table(sort(data$expenience_year_new)),xlab="expenience year",main=NULL)
descrb("expenience_year_new")

# 测试数据
data_train[which(data_train$expenience_year=="<1"),"expenience_year"] = 1 # 1年以下工作经验归到1年
data_train[which(data_train$expenience_year==">20"),"expenience_year"] = 21 # 大于20用虚拟21来代替
# 排序查看数据分别
data_train$expenience_year=as.numeric(data_train$expenience_year)
# 按照1-5，5-10，10-20，20+ 四个段进行数据划分
data_train$expenience_year_new = 1 * (data_train$expenience_year<=5) + 2 * (data_train$expenience_year>5 & data_train$expenience_year<=10) + 3 * (data_train$expenience_year >10 & data_train$expenience_year <= 15) + 4 * (data_train$expenience_year >15 & data_train$expenience_year <= 20) + 5 * (data_train$expenience_year > 20)


# 建立回归模型
# 空模型
model.empty=glm(target~1,family=binomial(link=logit), data=data)	
summary(model.empty)
# 全模型
model.full=glm(target~city_dev + as.factor(gender) + as.factor(expenience) + as.factor(university) + as.factor(edu_level) + as.factor(major) + as.factor(expenience_year_new) + as.factor(company_size) + as.factor(company_type) + as.factor(jobs) + train_hours,family=binomial(link=logit),data=data) 
summary(model.full)

#计算空模型和全模型的方差分析
anova(model.empty, model.full)							
21519-19464 # 似然函数，残差平方和
1-pchisq(2055, df=36) # P值为零，说明至少有一个变量起作用

library(car) #载入程序包car
# Type III SS 在软件里一般显示为Adjust SS，指的是，将p个变量纳入回归模型后，各个变量的额外贡献度（独立贡献度），
# 一般来说，各个变量的SS之和是小于SSR的，仅当各个变量完全不相关时，各个变量的SS的和才等于SSR。相应地，可以求出Type III r^{2}，即：
Anova(model.full, type="III") #对模型做三型方差分析

# 模型选择
# AIC模型
model.aic=step(model.full,trace=F)
summary(model.aic)
# BIC模型
ss=length(data[,1])
model.bic=step(model.full,trace=F,k=log(ss))
summary(model.bic)

# 加载ROC类似
library(pROC)
pred.full=predict(model.full,data=data)
pred.aic=predict(model.aic,data=data)
pred.bic=predict(model.bic,data=data)

roc.full=roc(data$target,pred.full)
roc.aic=roc(data$target,pred.aic)
roc.bic=roc(data$target,pred.bic)

# 查看3个模型的结果, 目前全模型效果最好 0.7827171 0.7824730 0.7795770
print(c(roc.full$auc,roc.aic$auc,roc.bic$auc))
par(mfrow=c(1,3))
plot(roc.full,main="Full model")
plot(roc.aic,main="AIC")
plot(roc.bic,main="BIC")


# 模型预测
par(mfrow=c(1,1))
data_train$company_size=as.factor(data_train$company_size)
p = predict(model.aic, data_train)  #利用模型glm1.a对数据a2进行预测
p = exp(p) / (1+exp(p))								#计算预测得到的概率
data_train$pred=1*(p>0.5)								#以0.5为阈值生成预测值
table(data_train[,c("target","pred")])								#计算预测值与真实值的2维频数表

ngrids=500									#设置格点数为500
TPR=rep(0,ngrids)								#为TPR(true positive ratio)赋初值
FPR=rep(0,ngrids)								#为FPR(false positive ratio)赋初值
for(i in 1:ngrids){
  p0=i/ngrids;									#选取阈值p0
  ST.true=data_train$target		 	#取出真实值并赋值给ST.true
  ST.pred=1*(p>p0)								#以p0为阈值生成预测值
  TPR[i]=sum(ST.pred*ST.true)/sum(ST.true)					#计算TPR
  FPR[i]=sum(ST.pred*(1-ST.true))/sum(1-ST.true)					#计算FPR
}
plot(FPR,TPR,type="l",col=2)							#画出FPR与TPR的散点图，即ROC曲线
points(c(0,1),c(0,1),type="l",lty=2)						#添加对角线


# 全模型 vs AIC vs BIC
p=matrix(0,length(data_train[,1]),3)							#生成矩阵，用于存储各模型的预测值
p[,1]=predict(model.full,data_train)							#利用全模型对数据进行预测
p[,2]=predict(model.aic,data_train)							#利用模型logit.aic对数据进行预测
p[,3]=predict(model.bic,data_train)							#利用模型logit.bic对数据进行预测
p[,c(1:3)]=exp(p[,c(1:3)])/(1+exp(p[,c(1:3)]))					#计算预测得到的概率

plot(c(0,1),c(0,1),type="l",main="FPR vs. TPR",xlab="FPR",ylab="TPR")		#画图，生成基本框架

FPR=rep(0,ngrids)								#为FPR赋初值
TPR=rep(0,ngrids)								#为TPR赋初值
for(k in 1:3){
  prob=p[,k]								#取出p中第K列的值，即第K个模型的预测概率
  for(i in 1:ngrids){
    p0=i/ngrids							#选取阈值
    ST.hat=1*(prob>p0)						#根据阈值生成预测值
    FPR[i]=sum((1-ST.true)*ST.hat)/sum(1-ST.true)			#计算FPR
    TPR[i]=sum(ST.true*ST.hat)/sum(ST.true)				#计算TPR
  }
  points(FPR,TPR,type="b",col=k,lty=k,pch=k)				#向图上添加第k个模型的TPR与FPR的散点图
}
legend(0.6,0.3,c("LOGIT FULL MODEL","LOGIT AIC MODEL", "LOGIT BIC MODEL"),lty=c(1:3),col=c(1:3),pch=c(1:3))	#为3个模型添加标示，区分3个模型





