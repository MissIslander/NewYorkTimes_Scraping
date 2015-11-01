
## PURPOSE: ## idea: search articles with the names 'interactive' and save number for plot one and
##  url for finding the number of comments with the Community API
## HISTORY: Created BM 30.10.2015, cleaned 31.10.2015
## BACKGROUND: How to extract APIs from NYT API explanation page: http://developer.nytimes.com/docs/
##  Parts of the code for extraction of date from the pub_date result from http://web.stanford.edu/~cengel/cgi-bin/anthrospace/scraping-new-york-times-articles-with-r
##  registered on NYT for API number


#####################   PROGRAMM BEGINS  #########################################
## search all articles and save date as well as URL
library(RJSONIO)
library (RCurl)

## set parameters 
api <- "87fe802cb8c6358aa193a622a8419258:1:73347879 " #API key from NYT sign in
apic  <- "7422e5914838a8ab10edff52c349ccf:0:73347879" # API comments
q <- "interactive" # Query string, use + instead of space

records <- 1000 #number of results 
pageRange <- 0:(records/10-1)

# start in year 2000, unlikely real interactity before that date
# get data 
datd <- c()        #for the date
datd2 <- c()        #without 'interactive'
datn <- c()        #for the urlnames
datn2 <- c()        # without 'interactive'
ncomments <- c()   #for the number of comments in interactive
ncomments2  <- c()  #comments in same number of articles as we found interactive ones and on same dates as interactive ones to avoid bias for time
for (i in pageRange) {
  # concatenate search-URL for each page
  urid <- paste0("http://api.nytimes.com/svc/search/v2/articlesearch.json?q=", q, "&page=", i, "&begin_date=20000101&fl=pub_date&api-key=", api)
  urin <- paste0("http://api.nytimes.com/svc/search/v2/articlesearch.json?q=", q, "&page=", i, "&begin_date=20000101&fl=web_url&api-key=", api)
  dd <- getURL(urid)
  dn <- getURL(urin)
  resd <- fromJSON(dd,simplify = FALSE)
  resn <- fromJSON(dn,simplify = FALSE)
  datd <- append(datd, unlist(resd$response$docs))  # convert the dates to a vector and append
  datn <- append(datn, unlist(resn$response$docs))  # links append
  }

# loop over datn and count comments (I do not only select only parent comments on purpose because the ones with more 
# replies should count more here as it shows more involvment)
for (j in 1:records){
  uri <- paste0("http://api.nytimes.com/svc/community/v3/user-content/url.json?url=",datn[j],"&api-key=", apic)  
  du <- getURL(uri)
  resu <- fromJSON(du,simplify = FALSE)
  ncomments <- append(ncomments, unlist(resu$results$totalCommentsFound))  # append
  } 


# Reformat the dates to make plot and to find in not interactive ones
dat.conv <- strptime(datd, format="%Y-%m-%d")
daterange <- c(min(dat.conv), max(dat.conv))
dat.all <- seq(daterange[1], daterange[2], by="day") # all possible days

# aggregate counts for dates and put into a data frame
cts <- as.data.frame(table(datd))

dat.all <- strptime(dat.all, format="%Y-%m-%d")
freqs <- ifelse(as.character(dat.all) %in% as.character(strptime(cts$dat, format="%Y-%m-%d")), cts$Freq, 0)

# Make Plot 1 which shows number of use of word 'interactive' over time
png('Increase_Interactivity.png',width=6,height=2.5,units="in",res=1200)
plot (freqs, type="l", xaxt="n", main=paste("NYT articles which include word '",q,"'"), ylab="Number of articles", xlab="date")
axis(1, 1:length(freqs), dat.all)
dev.off()


#now compare to number of comments in articles without the word 'interactive' but 
#from the exact same dates and find same number of articles at these dates
datsearch = strtrim(strptime(cts$datd,format="%Y-%m-%d"),10)
datsearch = gsub("-", "_", datsearch)
for (dlop in 1:length(datsearch)) {
  urin2 <- paste0("http://api.nytimes.com/svc/search/v2/articlesearch.json?page=1&pub_date=",datsearch[dlop],"&fl=web_url&api-key=", api)
  dn2 <- getURL(urin2)
  resn2 <- fromJSON(dn2,simplify = FALSE)
  datadd <- unlist(resn2$response$docs)
  datn2 <- append(datn2, datadd[1:cts$Freq[dlop]])  # links append, but only as many as we also have strings in the interactive article search
 }
  
for (j in 1:records){
  uri2 <- paste0("http://api.nytimes.com/svc/community/v3/user-content/url.json?url=",datn2[j],"&api-key=", apic)  
  du2 <- getURL(uri2)
  resu2 <- fromJSON(du2,simplify = FALSE)
  ncomments2 <- append(ncomments2, unlist(resu2$results$totalCommentsFound))  # append
} 


# show ncomments (interactive) and ncomments2 (not interact) as boxplots to see the distribution
# Write Percent of Articles with comments above the boxplot
png('Comments_Per_Article_1000.png',width=3.25,height=3.25,units="in",res=1200)
boxplot(ncomments[ncomments >0],ncomments2[ncomments2 > 0], main="Comments per article",
        ylab="Number of comments if comment",names=c('Interactive','Arbitrary')) 
dev.off()

# Limitation: Some articles have no comments, and we do not know whether comments are turned off
# on these articles which is often the case for interactive articles or whether they are not interesting
nco = 100*(sum(ncomments > 0))/length(ncomments)
nco2 = 100*(sum(ncomments2 > 0))/length(ncomments2)


