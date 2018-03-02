#### Temp Usage & Parrallel Programming ####
#### New HuaToo ####
library(doParallel)
cl <- makeCluster(2);
registerDoParallel(cl);

begin.time<-proc.time()
n = 5000
foreach(i = 1:n) %dopar% invisible(sqrt(i));
#for(i in 1:n) invisible(sqrt(i))
#x<-foreach(i = 1:n) %do% sqrt(i);

proc.time() - begin.time



x <- foreach(a = 1:10, b= 21:30, .combine = 'c', .inorder = T) %dopar% { a + b}

#### Quick Sort ####
quick.sort <- function(x){
  
  
  
}

#### Temp Fixed ####
root = "/Volumes/Experiment_Data-2/A1c"
setwd(root)
replacer = "$Text Record Version"

original.list<-c();

for(i in 1:length(trial)){
  m = read.delim(trial[i], header=F, blank.lines.skip = F, stringsAsFactors = F);
  original.list<-c(original.list, as.character(m[1,]));
  cat("\rReading:",i,"/",length(trial)," Folder" )
}

View(matrix(original.list, ncol=1))

wrong.parse = trial[which(sapply(original.list, nchar) == 1)]


for(i in 1:length(wrong.parse)){
  writer_fix(wrong.parse[i]);
  cat("\rReading:",i,"/",length(wrong.parse)," Folder" )
}


writer_fix<-function(file, replacer = "$Text Record Version"){
  single <- read.delim(file, blank.lines.skip = F, stringsAsFactors = FALSE, header = F);
  print(single[1,]);
  single[1,] <- replacer;
  print(single[1,]);
  write.table(single[,1], file, quote = FALSE, row.names = FALSE, col.names = F);
}





#### ####
quickCheck<-function(m_data_set, crit = 10){
  m_ref_set<-matrix(rep(m_data_set[,1],each=dim(m_data_set)[2]), 
                     ncol = dim(m_data_set)[2], byrow = T);
  bias_table<- 100 * (m_data_set - m_ref_set) / m_ref_set;
  index_table <- ifelse(bias_table > crit, 1, 0);
  output_vec <- apply(index_table,2, sum);
  output_list <- list(
    Vec = output_vec,
    Sum = c(sum(output_vec[2:3]), sum(output_vec[4:5]), sum(output_vec[6:7]))
  );
  
  return(output_list);
}

mm<-matrix(scan(), ncol=7, byrow=T);
quickCheck(mm)

cv_meter <- matrix(NA, ncol=15, nrow = 2);

for(i in 1:ncol(mm)){
  index1<-which( (1:nrow(mm) %% 2) == 1)
  index2<-which( (1:nrow(mm) %% 2) == 0)
  cv1<-cv(mm[index1,i]);
  cv2<-cv(mm[index2,i]);
  cv_meter[1:2,i]<-c(cv1,cv2)
}

  View(cv_meter)
  
  
   temp.result <- lm(Ref~ I(1.08*Est) + Temp + I(1.08*Est*Temp/100))
   m_coef <- coef(temp.result)
   Z<-mapply(temp.est,mm[,1], mm[,2])
   mm[,3] <- Z
   mm[,4] <- mm[,3]/mm[,1]
   plot(mm[,2],mm[,4], xlim=c(15,32), col=color.gradient(mm[,1], colors = c("blue","red")))

  
  
  