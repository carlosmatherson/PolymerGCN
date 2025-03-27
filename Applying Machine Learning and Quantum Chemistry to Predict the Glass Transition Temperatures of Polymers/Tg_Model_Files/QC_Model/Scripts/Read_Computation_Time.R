rm(list = ls())      # Clear all variables  
graphics.off()    # Close graphics windows  
library(stringr)
library(plyr)

#home="C:/Users/khickey/Documents/Gauss_Polymers/Log_Files_Higher_Temp/"
home="C:/Users/khickey/Documents/Gauss_Polymers/SMILES_Batch/Log_Files_Simple_Test_nopop/"

# List log files from directory, extract chemical name
data=dir(path=paste(home,sep=""),recursive = TRUE)

data=data[grep(".log$",data)]
#data=data[grep("3M.log$",data)]
# Remove extension, leading j
data=gsub(".log","",data)
chemical=data

cpu_sum=0
job_num=0

# Loop through all log files in directory
for (i in 1:length(chemical)){
  
  #     Scan in each logfile
  fp <- paste(home, chemical[i], ".log", sep="")  
  
  logfile <- scan(file=fp, what=character(), sep="\n")
  
  last_10=logfile[c((length(logfile)-10):length(logfile))]
  
  jobs=grep("Job cpu time",last_10)
  job_days=as.numeric(gsub(" days","",str_extract(last_10[jobs],"[0-9.]+ days")))*24
  job_hours=as.numeric(gsub(" hours","",str_extract(last_10[jobs],"[0-9.]+ hours")))
  job_miuntes=as.numeric(gsub(" minutes","",str_extract(last_10[jobs],"[0-9.]+ minutes")))*(1/60)
  job_time=job_days+job_hours+job_miuntes
  if (length(job_time)<1) next
  print(paste0("Job cpu time: ",job_time," hours"))
  cpu_sum=cpu_sum+job_time
  job_num=job_num+1
  
  elapsed=grep("Elapsed time",last_10)
  elapsed_days=as.numeric(gsub(" days","",str_extract(last_10[elapsed],"[0-9.]+ days")))*24
  elapsed_hours=as.numeric(gsub(" hours","",str_extract(last_10[elapsed],"[0-9.]+ hours")))
  elapsed_miuntes=as.numeric(gsub(" minutes","",str_extract(last_10[elapsed],"[0-9.]+ minutes")))*(1/60)
  elapsed_time=elapsed_days+elapsed_hours+elapsed_miuntes
  
  #bracket for chemical for-loop
}
print(paste0("cpu hours: ",cpu_sum))
print(paste0("number of jobs: ",job_num))
print(paste0("average cpu hour: ",(cpu_sum/job_num)))