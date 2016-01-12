plot_series <- function(input, output, first_list, second_list)
{
  tc <- list()
  ta <- list()
  sc <- list()
  sa <- list()
  len <- length(first_list)
  seq <- 1:len
  for(i in 1:len){
    print(first_list[i]) 
    input_folder <- paste(input, first_list[i], sep="")
    file <- paste(input_folder, "total_throughput", sep="/")
    print(file)
    mydata <- read.table(file, skip=1)
    print(mydata)
    tc <- c(tc, mydata[1, "V2"])
    ta <- c(ta, mydata[1, "V7"])
    sc <- c(sc, mydata[2, "V2"])
    sa <- c(sa, mydata[2, "V7"])
  }
  tc <- as.numeric(tc)
  ta <- as.numeric(ta)
  sc <- as.numeric(sc)
  sa <- as.numeric(sa)
  print(paste("tttt",tc))
  print(paste("scis",sc))
  print(seq)
  plot(seq, tc, xlim=c(1,2), ylim=c(0,300), main="Throughput with different thread" las=1, ylab="throughput", type="o")
  segments(seq, tc-sc, seq, tc+sc)
  epsilon = 0.02
  segments(seq-epsilon,tc-sc,seq+epsilon,tc-sc)
  segments(seq-epsilon,tc+sc,seq+epsilon,tc+sc)
  abline(h = seq(0,250,by=50), v = 0, col = "gray60", lty=3)
}
