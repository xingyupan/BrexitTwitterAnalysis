###############################
###### packages ##################
##############################
require(tm) # text mining package -- for document text matrix etc
require(stringr) # for string processing

require(dplyr)

require(syuzhet) # sentiment analysis pkg

require(wordcloud) 
require(plotly) # plotting pkgs

##############################################
##### file path and additional functions #####
##############################################
setwd('g:\\misc\\twitter_brexit')

source("twitter_brexit_custom_funcs.r")

files<-list.files()
tweet_files <- files[grepl('csv',files)]


leave_tags<-'(leave)|(freedom)|(strongerout)'
remain_tags<-'(remain)|(stay)|(strongerin)'

x<-NULL
#### combing twitter stream to 1 single file
for (i in c(1:length(tweet_files))) {
    dx<-read.csv(tweet_files[i], stringsAsFactors = F)
    Encoding(dx$user_loc)<- "UTF-8"
    Encoding(dx$user_loc2)<-'UTF-8'
    Encoding(dx$text) <-"UTF-8"
    Encoding(dx$hashtags) <-"UTF-8"
    x<-rbind(x, dx)
}

x_euref= grepl("[Ee][Uu][Rr][Ee][Ff]", x$hashtags) | grepl("[Ee][Uu][Rr][Ee][Ff]", x$text)
x_all<-x
x = x_all[x_euref,]
summary(x_euref)
### create some new variables
    x$loc = str_c(x$user_loc, x$user_loc2, sep=" ")
    x$country= detect_country(x$loc)
    x$created_at = clean_time(x$created_at)
    x$tweet_time_bin = cut(x$created_at, breaks = '5 mins')
    x$user_hist = clean_time(x$user_hist)
    
    #pop_tweet= sort(table(x$text), decreasing=T) # this line takes forever...
    
    ##################################
    #### Detect/Drop campaign IDs ####
    ##################################
    
    ## Classify accounts that's less than 1 month old as campaign accounts 
    ## Also, identify accounts which doesn't have user history.
    ## Drop tweets from such accounts ##
    short_hist = (x$user_hist > as.POSIXlt(strptime("May 23 2016", "%b %d %Y", tz="GMT")))
    t = table(short_hist)
    
    #sprintf("Starting time: %s GMT; Streaming elapsed time: %.2f mins", 
    #        x$created_at[1],
    #        x$created_at %>% tail(1) %>% difftime(x$created_at[1],unit="mins"))
   
    x = x[!is.na(short_hist) & !short_hist,]
    x_clean = clean_tweet(x$text)
    x$is_rt = x_clean$rt
    x$is_reply = x_clean$reply
    x$quote = x_clean$quote
    x$url = x_clean$urls
    x$cleaned_text = x_clean$cleaned
    
    x$tweet_time_bin = droplevels(x$tweet_time_bin)
    barplot(height=table(x$tweet_time_bin), xlab="Time", ylab="# tweets", main="Streaming Volume", col='blue')
    
######## Take a look at the data ####
    
    sprintf("percentage of new account: %.2f", t[2]/t[1]*100)
    #### Original or Retweets ####

    allquoted= x$quote
    sprintf('Time: from %s to %s', x$created_at[1], tail(x$created_at,1))
    sprintf("Percentage of retweets: %.2f", 100*(x$is_rt %>% table())[2]/length(x$is_rt))
    sprintf("Percentage of unique tweets: %.2f", 100*length(unique(x$text))/length(x$text))

    #### Twitter celebs: Who is Who ####
    ## save the frequent mention/ retweet ids so we can look into it later
    fr_rt = allquoted[x$is_rt] %>% table() %>% sort(decreasing=T)
    sprintf("Most frequently retweeted twitterIDs: %s", names(fr_rt)[1:10] %>% str_c(collapse=", "))
    
    fr_mt = allquoted %>% table() %>% sort(decreasing=T)
    sprintf("Most frequently mentioned twitterIDs: %s", (fr_mt%>% names())[1:10] %>% str_c(collapse=", "))
    
    pal2 = brewer.pal(8, "Dark2")
    png('fre_retweet_screennames_new.png', width=12, height=8, units='in',res=300)
    wordcloud(names(fr_rt), fr_rt, min.freq = 10, 
              random.order=F, rot.per=.15, colors=pal2)
    dev.off()
    
    #fr_mt_wc= wordcloud(names(fr_mt), fr_mt, min.freq = 2)
    #fr_url = table(x$url) %>% sort(decreasing=T) # not a good idea as urls get truncated at the end.
    
    ##### hashtags ####
    alltags<- decompose_tags(x$hashtags)
    tag_table <- table(alltags)
    tag_table <- sort(tag_table, decreasing=T) ; length(tag_table) 
    fq_tag_table <- tag_table[tag_table>1] ; length(fq_tag_table)
    sprintf("Top 10 tags: %s", str_c(names(fq_tag_table)[1:10], collapse = ', '))
    
    ##### location ####
    ctr_table = table(x$country) %>% sort(decreasing=T)
    city = detect_city(x$user_loc)
    city_table <- table(city) %>% sort(decreasing=T)
    print(ctr_table)
    print(city_table[-1])
    loc_table = table(x$user_loc)
    loc_table = sort(loc_table, decreasing = T); length(loc_table)
    fq_loc = loc_table[loc_table>1 & names(loc_table)!=""]; length(fq_loc)
    print(fq_loc[1:10])

    #### focus on tweets from UK ####
    #### build a corpus ####
    tt <-x$cleaned_text[x$country=="UK"]

    tt_u <- unique(tt)
    tt_u = Corpus(VectorSource(tt_u))
    ttu <- tm_map(tt_u, tolower)
    ttu <- tm_map(ttu, removePunctuation)
    ttu <- tm_map(ttu, removeNumbers)
    ttu <- tm_map(ttu, stemDocument)
    mystopwords = c(stopwords("en"),"rt", "http","https","htt", "will", "youre", 
                    "lets", "make", "made", "get","https\u0085", "just", "dont", 
                    "today", "tomorrow", "now")
    ttu <- tm_map(ttu, removeWords, mystopwords)
    ttu <- tm_map(ttu, stripWhitespace)
    ttu <- tm_map(ttu, PlainTextDocument)
    tu_dtm <- DocumentTermMatrix(ttu)
    tu_dtm <- removeSparseTerms(tu_dtm, .9997)
    tf <-colSums(as.matrix(tu_dtm))
    tf <- sort(tf, decreasing = T) 
    tf <- tf[!names(tf) %in% c("dont","today","tomorrow", "now", "can", "one",
                               "cant", "say", "says")]
    
    png('uk_tweets.png', width=6, height=6, units="in", res=300)
    wordcloud(names(tf), tf, random.order=F, scale=c(4,0.5), colors=pal2, max.words = 500)
    dev.off()
    
    #### find associations for politician names, political issues, and emotions ####
    politician = findAssocs(tu_dtm, c("boris","farage","cameron"),0.08)
    issue = findAssocs(tu_dtm, c("democracy", "immigration","economy"),0.08)
    emotion = findAssocs(tu_dtm, c("fear","liar","anger","disaster","calm"),0.08)
    
    library(ggplot2)
    names<- names(politician$boris)
    values <- politician$boris
    df_boris <- data.frame(term=names,value=values)
    png('boris.png')
    ggplot(df_boris, 
           aes(x = names, y = values)) + 
        geom_bar(stat = "identity", fill="red") + xlab("Terms") +
        ylab("Associations with Boris") + coord_flip()
    dev.off()
    
    names<- names(politician$farage)
    values <- politician$farage
    df_farage <- data.frame(term=names,value=values)
    png('farage.png')
    ggplot(df_farage, 
           aes(x = names, y = values)) + 
        geom_bar(stat = "identity", fill="red") + xlab("Terms") +
        ylab("Associations with Farage") + coord_flip()
    dev.off()
    
    names<- names(politician$cameron)
    values <- politician$cameron
    df_cameron <- data.frame(term=names,value=values)
    png('cameron.png')
    ggplot(df_cameron, 
           aes(x = names, y = values)) + 
        geom_bar(stat = "identity", fill="blue") + xlab("Terms") +
        ylab("Associations with Cameron") + coord_flip()
    dev.off()
    tx_sent <- tx %>% enc2native() %>% get_sentiment(method="afinn")
    tx_sent2 <- tx %>% enc2native() %>% get_sentiment()
    
    tx_sent_bi <-tx_sent
    tx_sent_bi[tx_sent>0]=1
    tx_sent_bi[tx_sent<0]=-1
    tx_sent_bi2 <-tx_sent2
    tx_sent_bi2[tx_sent2>0]=1
    tx_sent_bi2[tx_sent2<0]=-1
    tx_sent_s = tx_sent_bi+tx_sent_bi2
    
    #### a twitter poll for EURef? ####
    ## Easiest idea -- a poll using hashtags:
    
    poll1<-tag_poll(remain_tags, leave_tags, x$hashtags)
    poll2<-poll1[x$country=="UK"]
    
    ## sentiment by poll
    plot_ly(y=tx_sent[poll2=="leave"], type="box", name="Leave") %>%
        add_trace(y=tx_sent[poll2=="remain"], type="box", name="Remain") %>%
        add_trace(y=tx_sent[poll2=="unknown"], type="box", name="Unclear") %>%
        layout(xaxis=list(title="Hashtag Poll", font=list(size=12)), 
               yaxis=list(title='Tweet Sentiment'), font=list(size=12))
    
    write.table(tx[tx_sent<(-10)],'most_negative_tweets.txt')
    write.table(tx[tx_sent>8],'most_positive_tweets.txt')
    
    ## poll results by sentiment
    print(table(poll1))
    fq_leave <- x$hashtags[poll1=="leave"] %>%
        table() %>% sort(decreasing=T) %>% head(5)
    fq_remain <- x$hashtags[poll1=="remain"] %>%
        table() %>% sort(decreasing=T) %>% head(5)
    fq_unclear <- x$hashtags[poll1=="unknown"] %>%
        table() %>% sort(decreasing=T) %>% head(5)
    sprintf("Top Leave tags: %s", str_c(names(fq_leave), collapse=", "))
    sprintf("Top Remain tags: %s", str_c(names(fq_remain), collapse = ", "))
    sprintf("Top Unclear tags: %s", str_c(names(fq_unclear), collapse= ",  "))
    
    k= x$tweet_time_bin
    #k = droplevels(k)
    t_poll = table(k, poll1)

    raw_tag_poll=plot_ly(x=unique(k), y= t_poll[,1], type="bar", 
                         marker=list(color= 'red'),name="Leave") %>%
        add_trace(x= unique(k), y= t_poll[,3], type="bar", 
                         marker=list(color= 'blue'),name ="Remain") %>% 
        add_trace(x= unique(k), y= t_poll[,4], type="bar", 
                  marker=list(color= 'orange'), name ="Unclear") %>%
        add_trace(x= unique(k), y= t_poll[,2], type="bar",
                  marker=list(color= 'grey'), name ="No tag")%>%
        layout(yaxis=list(title='Count'),
               xaxis=list(title='Time'),
               barmode="stack")
    raw_tag_poll
    t_poll_pc= prop.table(t_poll, margin=1)
 
    pc_tag_poll=plot_ly(x=rownames(t_poll_pc), y= t_poll_pc[,1], type="bar", 
                        marker=list(color= 'red'),name="Leave")%>%
        add_trace(x= rownames(t_poll_pc), y= t_poll_pc[,3], type="bar", 
                  marker=list(color= 'blue'),name ="Remain") %>% 
        add_trace(x= rownames(t_poll_pc), y= t_poll_pc[,4], type="bar", 
                  marker=list(color= 'yellow'), name ="Unclear") %>%
        add_trace(x= rownames(t_poll_pc), y= t_poll_pc[,2], type="bar",
                  marker=list(color= 'grey'), name ="No tag")%>%
        layout(title="Twitter Hashtag Poll acoss time",
               yaxis=list(title='Proportion of tweets per category'),
               xaxis=list(title='Time'),
               barmode="stack")
    pc_tag_poll
    
    ## tag poll by country
    poll_ctr = table(x$country, poll1)
    
    ## tag poll by city
    poll_city = table(city, poll1)
    poll_city = poll_city[-1,]
    poll_city = prop.table(poll_city, margin=1)
    city_tag_poll=plot_ly(x=rownames(poll_city), y= poll_city[,1], type="bar", 
                marker=list(color= 'red'),name="Leave")%>%
        add_trace(x=rownames(poll_city), y= poll_city[,3], type="bar", 
                  marker=list(color= 'blue'),name ="Remain") %>% 
        layout(title="Twitter Hashtag Poll across UK cities",
               yaxis=list(title='Proportion'),
               xaxis=list(title='Time'),
               barmode="relative")
    city_tag_poll
    

    
## A more complicated way: categorizing tweets into Remain & Leave campaigns.



