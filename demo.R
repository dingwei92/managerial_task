#清理当前工作空间
rm(list=ls())		
#读取csv格式的数据，并赋值给data
data=read.csv("/Users/dingwei/研究生/管理统计/作业/aug_train.csv",header=T)

#数据清理

#删除数据重复数据
unique(data)
#删除数据结构中值为空(即缺失值以NA表示)的数据
na.omit(data)
#清除字符型数据前后的空格
trim(data)
#重新命名
names(data)=c("city_dev","gender","expenience","university","edu_level","major","expenience_year","company_size","company_type","jobs","train_hours")
data[c(1:5),]

