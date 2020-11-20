# BigHomework1
# 2020/11/18 Logan
# 潜在语义分析模型PLSA

# 清除变量
rm(list = ls())

# 加载包
library(stringr)
library(tm)
library(SnowballC)

load(file="DTM_std.Rdata")

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
