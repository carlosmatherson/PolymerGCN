rm(list = ls())      # Clear all variables  
graphics.off()    # Close graphics windows  
library(stringr)
library(plyr)
library(dplyr)

home="C:/Users/khickey/Documents/Gauss_Polymers/SMILES_Batch/Log_Files_SMILES_Batch_Fix/"

# List log files from directory, extract chemical name
data=dir(path=paste(home,sep=""))
#data="poly_ethylene_terephthalate_3M_total.log"
data=data[grep(".log$",data)]

# Remove extension, leading j
data=gsub(".log","",data)
#data=data[grep("1M$",data)]
chemical=data

#Build the data frame df
df=data.frame(chemical)
df$error=NA
df$Stat_Pt=NA
df$Jobs=NA
df$imag=NA
df$HF=NA
df$Gibbs_Gas=NA
df$Gibbs_Water=NA
df$Gibbs_Oct=NA
df$Enthalpy_Gas=NA
df$Enthalpy_Water=NA
df$Enthalpy_Oct=NA
df$Volume=NA
df$Radius=NA
df$Etherm=NA
df$Cv=NA
df$S=NA
df$qp=NA
df$qm=NA
df$alpha_avg=NA
df$quad_avg=NA
df$log_Kow=NA
df$kow_vol=NA
df$kow_rad=NA
df$amu=NA

# Loop through all log files in directory
for (i in 1:length(chemical)){
  
  #     Scan in each logfile
  fp <- paste(home, chemical[i], ".log", sep="")  
  
  logfile <- scan(file=fp, what=character(), sep="\n")
  
  last_10=logfile[c((length(logfile)-10):length(logfile))]
  
  # Index current chemical in data frame
  rindx=grep(paste("^",chemical[i],"$",sep=""),df$chemical)
  
  #check for Normal terminations, jobs
  norm=grep("Normal termination", logfile)
  jobs=grep("Job cpu",logfile)
  stat=grep("Stationary point found",logfile)
  if (length(stat)>0) df$Stat_Pt[rindx]="Yes"
  df$Jobs[rindx]=length(jobs)
  
  #  check if an Error termination is there
  lerr=grep("Error termination via",last_10)
  ntrerr=grep("Error termination in NtrErr:",last_10)
  wtime=grep("Walltime for job",last_10)
  if (length(lerr)>0 | length(ntrerr)>0){
    errortype=str_extract(last_10[lerr],"...\\.exe")
    if (length(ntrerr)>0) errortype="NtrErr"
    df$error[rindx]=gsub("\\.exe","",errortype)
    next
  }
  
  if (length(wtime)>0 && length(ntrerr)<1 && length(grep("Normal termination",last_10))<1){
    df$error[rindx]="odd"
    next
  }
  #  if (length(wtime)<1){
  if (max(norm)!=length(logfile) && max(lerr)!=length(logfile)){
    df$error[rindx]="DNF"
    next
  }
  # if (length(wtime)<1){
  #   df$error[rindx]="DNF"
  #   next
  # }
  
  ##### Split up jobs
  gas_opt_start=0
  gas_opt_end=0
  gas_freq_start=0
  gas_freq_end=0
  water_opt_start=0
  water_opt_end=0
  water_freq_start=0
  water_freq_end=0
  octanol_opt_start=0
  octanol_opt_end=0
  octanol_freq_start=0
  octanol_freq_end=0
  check=NA
  rout_id=grep("#",logfile)
  for (n in c(1:(length(rout_id)-1))){
    #    for (n in c(1:(5))){
    id=rout_id[n]
    id2=rout_id[n+1]
    
    templine=paste(logfile[id-1],logfile[id],logfile[id+1],logfile[id+2],sep="")
    templine2=paste(logfile[id2-1],logfile[id2],logfile[id2+1],logfile[id2+2],sep="")
    
    if (grepl("\\\\",templine)==T || grepl("\\\\",templine2)==F){
      #print(paste("1:",id))
      next
    }
    
    else if (grepl("pm6",templine)==T){
      #print(paste("2:",id))
      next
    }
    
    else if (grepl(" B3LYP/6-31G",templine)==T && grepl("SMD",templine)==F && grepl("opt",templine)==T){
      gas_opt_start=id
      gas_opt_end=id2
      check="gas"
      #print("klskskkks")
    }
    else if (grepl("B3LYP/6-31G",templine)==T && check=="gas" && grepl("opt",templine)==F){
      gas_freq_start=id
      gas_freq_end=id2
    }
    else if (grepl(" B3LYP/6-31G",templine)==T && grepl("Water",templine)==T && grepl("opt",templine)==T){
      water_opt_start=id
      water_opt_end=id2
      check="water"
    }
    else if (grepl("B3LYP/6-31G",templine)==T && check=="water" && grepl("opt",templine)==F){
      water_freq_start=id
      water_freq_end=id2
    }
    else if (grepl(" B3LYP/6-31G",templine)==T && grepl("Octanol",templine)==T && grepl("opt",templine)==T){
      octanol_opt_start=id
      octanol_opt_end=id2
      check="octanol"
    }
    else if (grepl("B3LYP/6-31G",templine)==T && check=="octanol" && grepl("opt",templine)==F){
      octanol_freq_start=id
      octanol_freq_end=id2
    }
    
  }
  
  # Index start of optimization and frequency archive sections
  opt=grep("\\\\FOpt\\\\",logfile)
  freq=grep("\\\\Freq\\\\",logfile)
  
  # Index start and end of all archive sections
  loc.start=grep("GINC",logfile)
  loc.end=grep("@",logfile)
  
  # Ensure equal # of start and end lines
  if(length(loc.start)!=length(loc.end))  next
  
  ####  Get imaginary frequencies
  
  # if (any(grepl(max(freq),loc.start))){
  #   endindx=grep("TRUE",grepl(max(freq),loc.start))
  #   df1=NULL
  #   for (j in max(freq):loc.end[endindx]){
  #     df1=paste(df1,logfile[j],sep="")      
  #   }
  #   df1=gsub(" ","",df1)
  #   
  #   imag=str_extract(df1,"NImag=.")
  #   imnum=as.numeric(gsub("NImag=","",imag))
  #   df$imag[rindx]=imnum
  #   if (imnum>0) next    
  # }
  #  f=freq[2]
  for (f in freq){
    if (any(grepl(f,loc.start))){
      
      endindx=grep("TRUE",grepl(f,loc.start))
      df1=NULL
      for (j in f:loc.end[endindx]){
        df1=paste(df1,logfile[j],sep="")      
      }
      df1=gsub(" ","",df1)
      
      imag=str_extract(df1,"NImag=.")
      imnum=as.numeric(gsub("NImag=","",imag))
      
      if (imnum==0){
        df$imag[rindx]=imnum
      }
      else if (f>gas_freq_start && f<gas_freq_end){
        freq_ind=grep(f,freq)
        imnum2=paste("G",freq_ind,"=",imnum)
        df$imag[rindx]=imnum2
      }
      else if (f>water_freq_start && f<water_freq_end){
        freq_ind=grep(f,freq)
        imnum2=paste("W",freq_ind,"=",imnum)
        df$imag[rindx]=imnum2
      }
      else if (f>octanol_freq_start && f<octanol_freq_end){
        freq_ind=grep(f,freq)
        imnum2=paste("O",freq_ind,"=",imnum)
        df$imag[rindx]=imnum2
      }
      
    }
  }
  ####  Get HF energies (Gas Phase)
  opt_gas=opt[which(between(opt,gas_opt_start,gas_opt_end))]
  if (any(grepl(opt_gas,loc.start))){
    endoptindx=grep("TRUE",grepl(opt_gas,loc.start))
    df2=NULL
    for (q in opt_gas:loc.end[endoptindx]){
      df2=paste(df2,logfile[q],sep="")      
    }
    df2=gsub(" ","",df2)
    
    hf=str_extract(df2,"\\\\HF=[^\\\\]*")
    hfnum=gsub("\\\\HF=","",hf)
    df$HF[rindx]=hfnum
  }
  
  #### Get Gibbs, enthalpy energies
  
  loc.gibbs=grep("Sum of electronic and thermal Free Energies=",logfile)
  
  gg_ind=which(between(loc.gibbs,gas_freq_start,gas_freq_end))
  wg_ind=which(between(loc.gibbs,water_freq_start,water_freq_end))
  og_ind=which(between(loc.gibbs,octanol_freq_start,octanol_freq_end))
  
  gg_gibbs=logfile[loc.gibbs[gg_ind]]
  gg_gibbs=gsub("Sum of electronic and thermal Free Energies=","",gg_gibbs)
  gg_gibbs=as.numeric(gsub(" ","",gg_gibbs))
  df$Gibbs_Gas[rindx]=gg_gibbs
  
  wg_gibbs=logfile[loc.gibbs[wg_ind]]
  wg_gibbs=gsub("Sum of electronic and thermal Free Energies=","",wg_gibbs)
  wg_gibbs=as.numeric(gsub(" ","",wg_gibbs))
  df$Gibbs_Water[rindx]=wg_gibbs
  
  og_gibbs=logfile[loc.gibbs[og_ind]]
  og_gibbs=gsub("Sum of electronic and thermal Free Energies=","",og_gibbs)
  og_gibbs=as.numeric(gsub(" ","",og_gibbs))
  df$Gibbs_Oct[rindx]=og_gibbs
  
  #enthalpy
  loc.enth=grep("Sum of electronic and thermal Enthalpies=",logfile)
  
  gg_ind=which(between(loc.enth,gas_freq_start,gas_freq_end))
  wg_ind=which(between(loc.enth,water_freq_start,water_freq_end))
  og_ind=which(between(loc.enth,octanol_freq_start,octanol_freq_end))
  
  gg_enth=logfile[loc.enth[gg_ind]]
  gg_enth=gsub("Sum of electronic and thermal Enthalpies=","",gg_enth)
  gg_enth=as.numeric(gsub(" ","",gg_enth))
  df$Enthalpy_Gas[rindx]=gg_enth
  
  wg_enth=logfile[loc.enth[wg_ind]]
  wg_enth=gsub("Sum of electronic and thermal Enthalpies=","",wg_enth)
  wg_enth=as.numeric(gsub(" ","",wg_enth))
  df$Enthalpy_Water[rindx]=wg_enth
  
  og_enth=logfile[loc.enth[og_ind]]
  og_enth=gsub("Sum of electronic and thermal Enthalpies=","",og_enth)
  og_enth=as.numeric(gsub(" ","",og_enth))
  df$Enthalpy_Oct[rindx]=og_enth
  
  #### Get Etherm, Cv, S
  loc.therm=grep("CV",logfile)[which(between(grep("CV",logfile),gas_freq_start,gas_freq_end))]
  therm=logfile[(loc.therm)+2]
  therm=gsub(" *Total +","",therm)
  therm=gsub(" +",",",therm)
  Eth=as.numeric(str_extract(therm,"^[0-9.]+"))
  cv=as.numeric(gsub(",","",str_extract(therm,",[0-9.]+,")))
  S=as.numeric(gsub(",","",str_extract(therm,",[0-9.]+$")))
  df$Etherm[rindx]=Eth
  df$Cv[rindx]=cv
  df$S[rindx]=S
  
  #### positive / negative charge on hydrogens
  loc.esp=max(grep("ESP charges:",logfile))
  loc.esp=loc.esp+2
  Echeck=TRUE
  qind=0
  hold=c()
  hold_neg=c()
  while (Echeck){
    if (grepl(" H ",logfile[(loc.esp)+qind])){
      hold=c(hold,as.numeric(str_extract(logfile[(loc.esp)+qind],"[0-9.-]+$")))
      hold_neg=c(hold_neg,as.numeric(str_extract(logfile[(loc.esp)+qind],"[0-9.-]+$")))
      qind=qind+1
    }
    else if (grepl("[0-9]  [A-Z]",logfile[(loc.esp)+qind])){
      hold_neg=c(hold_neg,as.numeric(str_extract(logfile[(loc.esp)+qind],"[0-9.-]+$")))
      qind=qind+1
    }
    else Echeck=FALSE
  }
  qp=max(hold)
  df$qp[rindx]=qp
  qm=min(hold_neg)
  df$qm[rindx]=qm
  
  ### alpha
  # loc.alphaxx=grep(" xx ",logfile)[max(which(between(grep(" xx ",logfile),gas_freq_start,gas_freq_end)))]
  # alphaxx=as.numeric(gsub("xx +","",gsub("D","E",str_extract(logfile[loc.alphaxx],"xx +[0-9.-D+]+"))))
  # alphayy=as.numeric(gsub("yy +","",gsub("D","E",str_extract(logfile[(loc.alphaxx+2)],"yy +[0-9.-D+]+"))))
  # alphazz=as.numeric(gsub("zz +","",gsub("D","E",str_extract(logfile[(loc.alphaxx+5)],"zz +[0-9.-D+]+"))))
  # alphaavg=mean(c(alphaxx,alphayy,alphazz))
  # df$alpha_avg[rindx]=alphaavg
  
  # alpha via Polar
  
  loc.alphaxx=grep("Exact polarizability:",logfile)[max(which(between(grep("Exact polarizability:",logfile),gas_freq_start,gas_freq_end)))]
  alpha_line=logfile[loc.alphaxx]
  nums=str_extract_all(alpha_line,"-?[0-9.]+")
  alphaxx=as.numeric(nums[[1]][1])
  alphayy=as.numeric(nums[[1]][3])
  alphazz=as.numeric(nums[[1]][6])
  alphaavg=mean(c(alphaxx,alphayy,alphazz))
  df$alpha_avg[rindx]=alphaavg
  
  ### Quadrupole moment
  loc.quad=grep("^ Quadrupole moment",logfile)[max(which(between(grep("^ Quadrupole moment",logfile),gas_opt_start,gas_opt_end)))]
  quadxx=as.numeric(gsub("XX= +","",str_extract(logfile[loc.quad+1],"XX= +[0-9.-]+")))
  quadyy=as.numeric(gsub("YY= +","",str_extract(logfile[loc.quad+1],"YY= +[0-9.-]+")))
  quadzz=as.numeric(gsub("ZZ= +","",str_extract(logfile[loc.quad+1],"ZZ= +[0-9.-]+")))
  df$quad_avg[rindx]=mean(c(quadxx,quadyy,quadzz))
  
  ### Molar Volume ml/mol
  volind=max(grep("Molar volume = ",logfile))
  vol=str_extract(logfile[volind],"\\( *[0-9.]+")
  vol=gsub("^[\\( ]+","",vol)
  df$Volume[rindx]=as.numeric(vol)
  
  ### Solute Radius
  radind=max(grep("Recommended a0 for SCRF calculation = ",logfile))
  rad=str_extract(logfile[radind],"\\( *[0-9.]+")
  rad=gsub("^[\\( ]+","",rad)
  df$Radius[rindx]=as.numeric(rad)
  
  ### Kow
  kow=-((df$Gibbs_Oct[rindx]-df$Gibbs_Water[rindx])*627.51)/(2.303*(1.988588e-3)*298.15)
  df$log_Kow[rindx]=kow
  df$kow_vol[rindx]=kow/df$Volume[rindx]
  df$kow_rad[rindx]=kow/df$Radius[rindx]
  
  # Molecular Mass
  amu_ind=grep("Molecular mass:",logfile)
  amu_text=logfile[amu_ind[1]]
  df$amu[rindx]=str_extract(amu_text,"[0-9.]+")
  
  
  #bracket for chemical for-loop
}
# save to a csv file
write.csv(df, file=paste(home,"Log_Output_SMILES_Batch_Fix_new.csv",sep=""), row.names=F)

dfg=df[grep(FALSE,is.na(df$Enthalpy)),]
dfg$chemical=gsub("_v[0-9]$","",dfg$chemical)

#write.csv(dfg, file="Log_CNNB_Good.csv", row.names=F)
