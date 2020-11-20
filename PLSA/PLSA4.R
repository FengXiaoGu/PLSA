# BigHomework1
# 2020/11/18 Logan
# Ǳ���������ģ��PLSA

# �������
rm(list = ls())

# ���ذ�
library(stringr)
library(tm)
library(SnowballC)

# ��������
path<-"D:/Courses/Statistical Methods with Applications/Homework/BigHomework1/Task2-Corpus.csv"
data <- read.csv(path)
colnames(data) <- c("doc_id", "text") 

# ���ɴ�Ƶ����
# ����TM��
data_corpus <- VCorpus(DataframeSource(data))
#inspect(data_corpus)
data_corpus <- tm_map(data_corpus, PlainTextDocument)#��reutersת��Ϊ���ı��ļ���ȥ����ǩ
data_corpus<- tm_map(data_corpus, stripWhitespace)#ȥ���հ�
data_corpus<-tm_map(data_corpus,removeNumbers)#ȥ������
data_corpus<-tm_map(data_corpus,removePunctuation)#�������
data_corpus<- tm_map(data_corpus, content_transformer(tolower))#ת��ΪСд
#data_corpus<- tm_map(data_corpus, tolower)#ת��ΪСд
data_corpus<- tm_map(data_corpus, removeWords, stopwords("english"))# ȥ��ͣ�ô�
data_corpus<- tm_map(data_corpus,stemDocument)
DTM <- DocumentTermMatrix(data_corpus) 

# inspect(DTM[1:5, 10000:10005]) # �鿴ԭʼDTM
DTM_rmspar <- removeSparseTerms(DTM, sparse=0.999)  # ȥϡ�裬����ȥ�����ʺ���Ƨ��
# inspect(DTM_rmspar[1:5, 5500:6505]) # �鿴ȥϡ����DTM_rmspar
# DTM_standard <- as.data.frame(inspect(DTM)  # ��inpect�鿴Ԫ���������ƣ��޷����ת��
# DTM_standard2 <- as.matrix(DTM)  # �ᱬ�ڴ棬���Ժ����ֶ�ת��
# findFreqTerms(DTM_rmspar[1,],1)  # �鿴Ƶ�δ���1�Ĵ���

# ��DocumentTermMatrixת��Ϊmatrix����
# amazing �ڴ漸��û����������
size_col=DTM_rmspar$ncol
size_row=DTM_rmspar$nrow
DTM_std=matrix(0, nrow=DTM_rmspar$nrow, ncol=DTM_rmspar$ncol) # DTM�ľ����ʽ
for(r in 1:DTM_rmspar$nrow){
    DTM_std[r,] = as.matrix(DTM_rmspar[r,])
}

save(DTM_std,DTM_rmspar,file="DTM_std.Rdata")
# ��ʼ������

# # �����ã����������ݲ��Դ���
# DTM_std_origin=DTM_std;
# DTM_std=DTM_std_origin[1:50,];
# DTM_std=DTM_std_origin;

load(file="DTM.Rdata")
# number.sample<-DTM_rmspar$nrow     # ��������
# number.words<-DTM_rmspar$ncol      # �ʱ�����
number.sample<-nrow(DTM_std)
number.words<-ncol(DTM_std)  
number.component<-4       # ��������
max.step<-50               # ѭ������




ey <- matrix(NA, nrow=number.sample, ncol=number.component);  #�����������ʲ�������N*4
dy <- matrix(NA, nrow=number.sample, ncol=number.component);  #������ʲ�������N*4

ep <- matrix(0, nrow=number.component, ncol=number.words); #���ⵥ�ʸ��ʣ���4*W
ep[1,] <- runif(number.words, 0.0, 1.0);
ep[1,] <- ep[1,]/sum(ep[1,]);
ep[2,] <- runif(number.words, 0.0, 1.0);
ep[2,] <- ep[2,]/sum(ep[2,]);
ep[3,] <- runif(number.words, 0.0, 1.0);
ep[3,] <- ep[3,]/sum(ep[3,]);
ep[4,] <- runif(number.words, 0.0, 1.0);
ep[4,] <- ep[4,]/sum(ep[4,]);
el <- runif(number.component, 0.0, 1.0);       # ������ʣ���4���������ģ�ͣ���һ��ĸ���
el <- el / sum(el);                            # ��һ��
el
elpre <- c(0,0,0,0)
# a<- 3.665822e-325;a=0


el
for(k in 1:max.step){
    for(j in 1:number.component){
        for(s in 1:number.sample){
          # dy[s,j] <- prod(ep[j,]^DTM_std[s,]) * el[j];    # �������ʣ�����ĸ���,̫С����Ҫȡ����         
          # ����Ϊ0�Ĵ�
          rm_nan=DTM_std[s,]*log(ep[j,]);
          rm_nan[is.nan(rm_nan)]<-0; # ����Ϊ0�Ĵʣ������nan
          dy[s,j] <- sum(rm_nan) +log(el[j]);    #�������ʣ�����ĸ���,ȡ����

        }
    }
    for(i in 1:number.sample){
        # ���ö�����ͷ���
        dy_m<-max(dy[i,]);   
        dy_temp<-exp(dy[i,]-dy_m);
        sy_temp<-log(sum(dy_temp))+dy_m;     # �͵Ķ���   
        ey[i,]<-exp(dy[i,]-sy_temp)
    }
    for(j in 1:number.component){
        el[j] <- sum(ey[,j]) / number.sample;  #�������£�����ĸ��ʣ�lamda                
        ep[j,] <- colSums(DTM_std*ey[,j]) / sum(DTM_std*ey[,j]);  #�������⣬���ʵĸ���       
        
    }    
    print(c(k,el)) # ��ӡѭ��������el 
    if(k>5&sum((elpre-el)^2)<1e-7){
        break;
    }
    elpre=el
}

# ������
argpath=paste(gsub("[^0-9]","",date()),"ep.Rdata")
save(el,ep,ey,dy,file=argpath)


# �鿴���
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


