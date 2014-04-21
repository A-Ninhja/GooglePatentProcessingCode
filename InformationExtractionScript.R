# All the code included in this set of files is an original work by Canek Acosta. Copyright 2014.
print("All the code included in this set of files is an original work by Canek Acosta. Copyright 2014.")
# set the working directory to the folder containing extracted patent applications
setwd("C:/Users/Ck/Desktop/Patents")
# create the vectors that will contain the data for the data frame
applicationNumberCol = c()
filingDateCol=c()
statusCol=c()
allowanceDateCol=c()
prosecutionLengthCol=c()
rejectionsCol=c()
RCECol=c()
examinerInterviewsCol=c()
tranhistoryCol=c()
continuityCol=c()
foreignCol=c()
# loop through each folder in the directory
for(path in dir()){
  applicationNumber <- as.character(path)
  tranhistory <- 0
  continuity <- 0
  foreign <- 0
  rejections <- 0
  RCE <- 0
  examinerInterviews <- 0
  filingDate <- "NA"
  allowanceDate <- "NA"
  prosecutionLength <- "NA"
  status <- "Unknown or Pending"
  # loop through each file in each folder
  for(file in dir(path)){
    fullPath = file.path(path,file)
    # determine if it is a child patent application . . .
    if(any(grepl("continuity_data.tsv", fullPath))){
      continuity <- 1
    }
    # determine if it is this patent application claims foreign priority . . .
    if(any(grepl("foreign_priority.tsv", fullPath))){
      foreign <- 1
    }
    # fetch this patent applications transaction history, if available
    if(any(grepl("transaction_history.tsv", fullPath))){
      tranhistory <- 1
      data <- read.delim(fullPath)
      # get filing date
      filingDate <- as.Date(as.character(data$Date[length(data$Date)]), format = "%m-%d-%Y")
      # get number of rejections, RCEs, and examiner interviews
      for(i in data$Transaction.Description){
        if(i == "Non-Final Rejection" | i == "Final Rejection"){
          rejections = rejections + 1
        }
        if(i == "Request for Continued Examination (RCE)"){
          RCE <- RCE + 1
        }
        if(grepl("Examiner Interview", i) & !grepl("Mail", i)){
          examinerInterviews <- examinerInterviews + 1
        }
        # determine whether the patent application was abandoned . . .
        if(i == "Mail Abandonment for Failure to Respond to Office Action"){
          status <- "Abandoned"
        }
        # determine whether the patent application was allowed . . .
        if (i == "Mail Notice of Allowance" & status != "Allowed"){
          status <- "Allowed"
          allowanceDate <- as.Date(as.character(data$Date[data$Transaction.Description == "Mail Notice of Allowance"]), format = "%m-%d-%Y")
          prosecutionLength <- as.character(allowanceDate - filingDate)
        }
      }
    }
  }
  applicationNumberCol = append(applicationNumberCol, as.character(applicationNumber))
  filingDateCol=append(filingDateCol, as.Date(filingDate, format = "%m-%d-%Y"))
  statusCol=append(statusCol, as.character(status))
  allowanceDateCol=append(allowanceDateCol, as.Date(max(allowanceDate), format = "%m-%d-%Y"))
  prosecutionLengthCol=append(prosecutionLengthCol, as.numeric(max(prosecutionLength)))
  rejectionsCol=append(rejectionsCol, as.numeric(rejections))
  RCECol=append(RCECol, as.numeric(RCE))
  examinerInterviewsCol=append(examinerInterviewsCol, as.numeric(examinerInterviews))
  tranhistoryCol=append(tranhistoryCol, as.numeric(tranhistory))
  continuityCol=append(continuityCol, as.numeric(continuity))
  foreignCol=append(foreignCol, as.numeric(foreign))
}
df <- data.frame(applicationNumber=applicationNumberCol, filingDate=filingDateCol, status=statusCol, allowanceDate=allowanceDateCol, prosecutionLength=prosecutionLengthCol, rejections=rejectionsCol, RCE=RCECol, examinerInterviews=examinerInterviewsCol, tranhistory=tranhistoryCol, continuity=continuityCol, foreign=foreignCol)
write.table(df,file="patents.csv",quote=T,append=F,sep=",",eol = "\n", na = "NA", dec = ".", row.names = T,col.names = T)
