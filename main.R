unit<-function(index)  {
index.uz<-matrix(data=0,nrow=nrow(index),ncol=1)
index.unit<-matrix(data=0,nrow=nrow(index), ncol=ncol(index))  
for (i in 1:ncol(index))   {
for (n in 1:nrow(index))        {
if(cor(index[,i],index[,1])<0)       {
   index.unit[n,i]<-(max(index[,i])-as.numeric(index[n,i]))/(max(index[,i])-min(index[,i]))
                                       }                         
  else {
   index.unit[n,i]<-(as.numeric(index[n,i])-min(index[,i]))/(max(index[,i])-min(index[,i]))
        }
                                 }
                          }

for(n in 1:nrow(index.unit)){
  index.uz[n,1]<-mean(index.unit[n,])
}
return(index.uz)
                        }


unit(index)

