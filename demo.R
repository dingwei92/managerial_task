#清理当前工作空间
rm(list=ls())	

#读取数据
data=read.csv("/Users/dingwei/研究生/管理统计/作业/aug_train.csv",header=T)  #读取csv格式的数据，并赋值给data

#数据清理
data=data[,-(1:2)]  #删除第1列到第2列的数据（-:代表删除（*，*）第1个*表示行，第2个*表示列）

names(data)=c("city_dev","gender","expenience","university","edu_level","major","expenience_year","company_size","company_type","jobs","train_hours") #重新命名

getmode <- function(v) { #构造一个函数 参数v-一列数据 功能--获取数据中的众数）
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

city_dev_avg=mean(data[,1])  #获取表中city_dev的平均数
gender_med=getmode(data[,2])  #获取表中gender的众数
expenience_med=getmode(data[,3])  #获取表中expenience的众数
university_med=getmode(data[,4])  #获取表中university的众数
edu_level_med=getmode(data[,5])  #获取表中edu_level的众数
major_med=getmode(data[,6])  #获取表中major的众数
expenience_year_med=getmode(data[,7])  #获取表中expenience_year的众数
# company_size_med=getmode(data[,8])  #获取表中company_size的众数
company_size_med="1000-4999"
company_type_med=getmode(data[,9])  #获取表中company_type的众数
jobs_med=getmode(data[,10])  #获取表中jobs的众数
train_hours_med=mean(data[,11])  #获取表中train_hours的平均数


count_li=0  #定义列数
for (i in data){ #遍历数据 将数据中的null值赋值
  count_li=count_li+1
  count_ha=1 #定义行数
  for(j in i){
    if(count_li==1 && (data[count_ha,count_li]==""||is.null(data[count_ha,count_li]))) {
      data[count_ha,count_li]=city_dev_avg  #给city_dev空值赋值
    }
    if(count_li==2 && (data[count_ha,count_li]==""||is.null(data[count_ha,count_li]))) {
      data[count_ha,count_li]=gender_med  #给gender_med空值赋值
    }
    if(count_li==3 && (data[count_ha,count_li]==""||is.null(data[count_ha,count_li]))) {
      data[count_ha,count_li]=expenience_med  #给expenience_med空值赋值
    }
    if(count_li==4 && (data[count_ha,count_li]==""||is.null(data[count_ha,count_li]))) {
      data[count_ha,count_li]=university_med  #给university_med空值赋值
    }
    if(count_li==5 && (data[count_ha,count_li]==""||is.null(data[count_ha,count_li]))) {
      data[count_ha,count_li]=edu_level_med  #给edu_level_med空值赋值
    }
    if(count_li==6 && (data[count_ha,count_li]==""||is.null(data[count_ha,count_li]))) {
      data[count_ha,count_li]=major_med  #给major_med空值赋值
    }
    if(count_li==7 && (data[count_ha,count_li]==""||is.null(data[count_ha,count_li]))) {
      data[count_ha,count_li]=expenience_year_med  #给expenience_year_med空值赋值
    }
    if(count_li==8 && (data[count_ha,count_li]==""||is.null(data[count_ha,count_li]))) {
      data[count_ha,count_li]=company_size_med  #给company_size_med空值赋值
    }
    if(count_li==9 && (data[count_ha,count_li]==""||is.null(data[count_ha,count_li]))) {
      data[count_ha,count_li]=company_type_med  #给company_type_med空值赋值
    }
    if(count_li==10 && (data[count_ha,count_li]==""||is.null(data[count_ha,count_li]))) {
      data[count_ha,count_li]=jobs_med  #给jobs_med空值赋值
    }
    if(count_li==11 && (data[count_ha,count_li]==""||is.null(data[count_ha,count_li]))) {
      data[count_ha,count_li]=train_hours_med  #给train_hours_med空值赋值
    }
    count_ha=count_ha+1
  }
}

unknown=-1 # 数据转换对应的数值变量
zero=0  
one=1
two=2
three=3
four=4
five=5
six=6
sever=7

count1_li=0  #定义列数
for (i in data){  #遍历数据 将数据数字化
  count1_li=count1_li+1
  count1_ha=1 #定义行数
  for(j in i){
    if(count1_li==2) { # gender列 将字符串类型变成数值
      if(data[count1_ha,count1_li]=="Male"){
        data[count1_ha,count1_li]=zero
      }else if(data[count1_ha,count1_li]=="Female"){
        data[count1_ha,count1_li]=one
      }else{
        data[count1_ha,count1_li]=unknown
      }
    }
    if(count1_li==3) {  # relevent_experience列 将字符串类型变成数值
      if(data[count1_ha,count1_li]=="No relevent experience"){
        data[count1_ha,count1_li]=zero
      }else if(data[count1_ha,count1_li]=="No relevent experience"){
        data[count1_ha,count1_li]=one
      }else if(data[count1_ha,count1_li]=="Has relevent experience"){
        data[count1_ha,count1_li]=two
      }else{
        data[count1_ha,count1_li]=unknown
      }
    }
    if(count1_li==4) {  # enrolled_university列 将字符串类型变成数值
      if(data[count1_ha,count1_li]=="no_enrollment"){
        data[count1_ha,count1_li]=zero
      }else if(data[count1_ha,count1_li]=="Part time course"){
        data[count1_ha,count1_li]=one
      }else if(data[count1_ha,count1_li]=="Full time course"){
        data[count1_ha,count1_li]=two
      }else{
        data[count1_ha,count1_li]=unknown
      }
    }
    if(count1_li==5) {  # education_level列 将字符串类型变成数值
      if(data[count1_ha,count1_li]=="Graduate"){
        data[count1_ha,count1_li]=zero
      }else if(data[count1_ha,count1_li]=="Masters"){
        data[count1_ha,count1_li]=one
      }else if(data[count1_ha,count1_li]=="Phd"){
        data[count1_ha,count1_li]=two
      }else if(data[count1_ha,count1_li]=="High School"){
        data[count1_ha,count1_li]=three
      }else if(data[count1_ha,count1_li]=="Primary School"){
        data[count1_ha,count1_li]=four
      }else{
        data[count1_ha,count1_li]=unknown
      }
    }
    if(count1_li==6) {  # major_discipline列 将字符串类型变成数值
      if(data[count1_ha,count1_li]=="STEM"){
        data[count1_ha,count1_li]=zero
      }else if(data[count1_ha,count1_li]=="Humanities"){
        data[count1_ha,count1_li]=one
      }else if(data[count1_ha,count1_li]=="Business Degree"){
        data[count1_ha,count1_li]=two
      }else if(data[count1_ha,count1_li]=="Arts"){
        data[count1_ha,count1_li]=three
      }else{
        data[count1_ha,count1_li]=unknown
      }
    }
    if(count1_li==7) {  # experience列 将字符串类型变成数值
      if(data[count1_ha,count1_li]=="1"){
        data[count1_ha,count1_li]=1
      }else if(data[count1_ha,count1_li]=="2"){
        data[count1_ha,count1_li]=2
      }else if(data[count1_ha,count1_li]=="3"){
        data[count1_ha,count1_li]=3
      }else if(data[count1_ha,count1_li]=="4"){
        data[count1_ha,count1_li]=4
      }else if(data[count1_ha,count1_li]=="5"){
        data[count1_ha,count1_li]=5
      }else if(data[count1_ha,count1_li]=="6"){
        data[count1_ha,count1_li]=6
      }else if(data[count1_ha,count1_li]=="7"){
        data[count1_ha,count1_li]=7
      }else if(data[count1_ha,count1_li]=="8"){
        data[count1_ha,count1_li]=8
      }else if(data[count1_ha,count1_li]=="9"){
        data[count1_ha,count1_li]=9
      }else if(data[count1_ha,count1_li]=="10"){
        data[count1_ha,count1_li]=10
      }else if(data[count1_ha,count1_li]=="11"){
        data[count1_ha,count1_li]=11
      }else if(data[count1_ha,count1_li]=="12"){
        data[count1_ha,count1_li]=12
      }else if(data[count1_ha,count1_li]=="13"){
        data[count1_ha,count1_li]=13
      }else if(data[count1_ha,count1_li]=="14"){
        data[count1_ha,count1_li]=14
      }else if(data[count1_ha,count1_li]=="15"){
        data[count1_ha,count1_li]=15
      }else if(data[count1_ha,count1_li]=="16"){
        data[count1_ha,count1_li]=16
      }else if(data[count1_ha,count1_li]=="17"){
        data[count1_ha,count1_li]=17
      }else if(data[count1_ha,count1_li]=="18"){
        data[count1_ha,count1_li]=18
      }else if(data[count1_ha,count1_li]=="19"){
        data[count1_ha,count1_li]=19
      }else if(data[count1_ha,count1_li]=="20"){
        data[count1_ha,count1_li]=20
      }else if(data[count1_ha,count1_li]==">20"){
        data[count1_ha,count1_li]=30
      }else if(data[count1_ha,count1_li]=="<1"){
        data[count1_ha,count1_li]=0
      }else{
        data[count1_ha,count1_li]=unknown
      }
    }
    if(count1_li==8) {  # company_size列 将字符串类型变成数值
      if(data[count1_ha,count1_li]=="<10"){
        data[count1_ha,count1_li]=zero
      }else if(data[count1_ha,count1_li]=="10-49"){
        data[count1_ha,count1_li]=one
      }else if(data[count1_ha,count1_li]=="50-99"){
        data[count1_ha,count1_li]=two
      }else if(data[count1_ha,count1_li]=="100-500"){
        data[count1_ha,count1_li]=three
      }else if(data[count1_ha,count1_li]=="500-999"){
        data[count1_ha,count1_li]=four
      }else if(data[count1_ha,count1_li]=="1000-4999"){
        data[count1_ha,count1_li]=five
      }else if(data[count1_ha,count1_li]=="5000-9999"){
        data[count1_ha,count1_li]=six
      }else if(data[count1_ha,count1_li]=="10000"){
        data[count1_ha,count1_li]=sever
      }else {
        data[count1_ha,count1_li]=unknown
      }
    }
    if(count1_li==9) {  # company_type列 将字符串类型变成数值
      if(data[count1_ha,count1_li]=="Pvt Ltd"){
        data[count1_ha,count1_li]=zero
      }else if(data[count1_ha,count1_li]=="Funded Startup"){
        data[count1_ha,count1_li]=one
      }else if(data[count1_ha,count1_li]=="Early Stage Startup"){
        data[count1_ha,count1_li]=two
      }else if(data[count1_ha,count1_li]=="Public Sector"){
        data[count1_ha,count1_li]=three
      }else if(data[count1_ha,count1_li]=="NGO"){
        data[count1_ha,count1_li]=four
      }else{
        data[count1_ha,count1_li]=unknown
      }
    }
    if(count1_li==10) {  # last_new_job列 将字符串类型变成数值
      if(data[count1_ha,count1_li]=="never"){
        data[count1_ha,count1_li]=zero
      }else if(data[count1_ha,count1_li]=="1"){
        data[count1_ha,count1_li]=one
      }else if(data[count1_ha,count1_li]=="2"){
        data[count1_ha,count1_li]=two
      }else if(data[count1_ha,count1_li]=="3"){
        data[count1_ha,count1_li]=three
      }else if(data[count1_ha,count1_li]=="4"){
        data[count1_ha,count1_li]=four
      }else if(data[count1_ha,count1_li]==">4"){
        data[count1_ha,count1_li]=five
      }else{
        data[count1_ha,count1_li]=unknown
      }
    }
    count1_ha=count1_ha+1 
  }
}
print(data)

