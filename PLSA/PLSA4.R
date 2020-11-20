# BigHomework1
# 2020/11/18 Logan
# 潜在语义分析模型PLSA

# 清除变量
rm(list = ls())

# 加载包
library(stringr)
library(tm)
library(SnowballC)

# 加载数据
path<-"D:/Courses/Statistical Methods with Applications/Homework/BigHomework1/Task2-Corpus.csv"
data <- read.csv(path)
colnames(data) <- c("doc_id", "text") 

# 生成词频矩阵
# 利用TM包
data_corpus <- VCorpus(DataframeSource(data))
#inspect(data_corpus)
data_corpus <- tm_map(data_corpus, PlainTextDocument)#将reuters转化为纯文本文件，去除标签
data_corpus<- tm_map(data_corpus, stripWhitespace)#去掉空白
data_corpus<-tm_map(data_corpus,removeNumbers)#去除数字
data_corpus<-tm_map(data_corpus,removePunctuation)#消除标点
data_corpus<- tm_map(data_corpus, content_transformer(tolower))#转换为小写
#data_corpus<- tm_map(data_corpus, tolower)#转换为小写
data_corpus<- tm_map(data_corpus, removeWords, stopwords("english"))# 去掉停用词
data_corpus<- tm_map(data_corpus,stemDocument)
DTM <- DocumentTermMatrix(data_corpus) 

# inspect(DTM[1:5, 10000:10005]) # 查看原始DTM
DTM_rmspar <- removeSparseTerms(DTM, sparse=0.999)  # 去稀疏，可以去掉错词和生僻词
# inspect(DTM_rmspar[1:5, 5500:6505]) # 查看去稀疏后的DTM_rmspar
# DTM_standard <- as.data.frame(inspect(DTM)  # 受inpect查看元素数量限制，无法完成转换
# DTM_standard2 <- as.matrix(DTM)  # 会爆内存，所以后面手动转换
# findFreqTerms(DTM_rmspar[1,],1)  # 查看频次大于1的词数

# 从DocumentTermMatrix转化为matrix对象
# amazing 内存几乎没有明显增加
size_col=DTM_rmspar$ncol
size_row=DTM_rmspar$nrow
DTM_std=matrix(0, nrow=DTM_rmspar$nrow, ncol=DTM_rmspar$ncol) # DTM的矩阵格式
for(r in 1:DTM_rmspar$nrow){
    DTM_std[r,] = as.matrix(DTM_rmspar[r,])
}

save(DTM_std,DTM_rmspar,file="DTM_std.Rdata")
# 初始化参数

# # 调试用，用少量数据测试代码
# DTM_std_origin=DTM_std;
# DTM_std=DTM_std_origin[1:50,];
# DTM_std=DTM_std_origin;

load(file="DTM.Rdata")
# number.sample<-DTM_rmspar$nrow     # 样本数量
# number.words<-DTM_rmspar$ncol      # 词表数量
number.sample<-nrow(DTM_std)
number.words<-ncol(DTM_std)  
number.component<-4       # 主题数量
max.step<-50               # 循环步数




ey <- matrix(NA, nrow=number.sample, ncol=number.component);  #主题条件概率参数，共N*4
dy <- matrix(NA, nrow=number.sample, ncol=number.component);  #主题概率参数，共N*4

ep <- matrix(0, nrow=number.component, ncol=number.words); #主题单词概率，共4*W
ep[1,] <- runif(number.words, 0.0, 1.0);
ep[1,] <- ep[1,]/sum(ep[1,]);
ep[2,] <- runif(number.words, 0.0, 1.0);
ep[2,] <- ep[2,]/sum(ep[2,]);
ep[3,] <- runif(number.words, 0.0, 1.0);
ep[3,] <- ep[3,]/sum(ep[3,]);
ep[4,] <- runif(number.words, 0.0, 1.0);
ep[4,] <- ep[4,]/sum(ep[4,]);
el <- runif(number.component, 0.0, 1.0);       # 主题概率，共4个，即层次模型，第一层的概率
el <- el / sum(el);                            # 归一化
el
elpre <- c(0,0,0,0)
# a<- 3.665822e-325;a=0


el
for(k in 1:max.step){
    for(j in 1:number.component){
        for(s in 1:number.sample){
          # dy[s,j] <- prod(ep[j,]^DTM_std[s,]) * el[j];    # 给定单词，主题的概率,太小，需要取对数         
          # 概率为0的词
          rm_nan=DTM_std[s,]*log(ep[j,]);
          rm_nan[is.nan(rm_nan)]<-0; # 概率为0的词，会产生nan
          dy[s,j] <- sum(rm_nan) +log(el[j]);    #给定单词，主题的概率,取对数

        }
    }
    for(i in 1:number.sample){
        # 采用对数求和方法
        dy_m<-max(dy[i,]);   
        dy_temp<-exp(dy[i,]-dy_m);
        sy_temp<-log(sum(dy_temp))+dy_m;     # 和的对数   
        ey[i,]<-exp(dy[i,]-sy_temp)
    }
    for(j in 1:number.component){
        el[j] <- sum(ey[,j]) / number.sample;  #给定文章，主题的概率，lamda                
        ep[j,] <- colSums(DTM_std*ey[,j]) / sum(DTM_std*ey[,j]);  #给定主题，单词的概率       
        
    }    
    print(c(k,el)) # 打印循环次数和el 
    if(k>5&sum((elpre-el)^2)<1e-7){
        break;
    }
    elpre=el
}

# 保存结果
argpath=paste(gsub("[^0-9]","",date()),"ep.Rdata")
save(el,ep,ey,dy,file=argpath)


# 查看结果
el
z1=order(ep[1,],decreasing=TRUE)[1:30]
ep[1,z1];z1
z2=order(ep[2,],decreasing=TRUE)[1:30]
ep[2,z2];z2
z3=order(ep[3,],decreasing=TRUE)[1:30]
ep[3,z3];z3
z4=order(ep[4,],decreasing=TRUE)[1:30]
ep[4,z4];z4


inspect(DTM_rmspar[0,z1])
inspect(DTM_rmspar[0,z2])
inspect(DTM_rmspar[0,z3])
inspect(DTM_rmspar[0,z4])



