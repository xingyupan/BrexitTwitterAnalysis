
decompose_tags <- function(tags) {
    #############################################################
    ### take hashtags fields and decompose into separate tags ###
    #############################################################
    tags <- strsplit(tags,';')
    tags <- unlist(tags)
    tags <- tolower(tags)
    return (tags)
}
tag_poll <-function(remain_pattern, leave_patten, tags) {
    ##################################################
    ## conduct a "Poll" using the hashtag of tweets ##
    ##################################################
    count =0
    pollout<-NULL ### Output values: remain, leave, unknown, notag
    for (tag in tags) {
        # breakdown string into multiple tags, and change them to all lowercases
        count = count +1
        x = decompose_tags(tag)
        
        # if tags contain remain pattern, then vote remain
        # if tags contain leave pattern, then vote leave
        # if both or neither, then unknown
        remain_flag = F; leave_flag=F # init
        remain_flag = grepl(remain_pattern, x)
        leave_flag = grepl(leave_patten, x)
        if (sum(remain_flag)>sum(leave_flag)) {
            pollout<-c(pollout, "remain")
        } else if (sum(leave_flag)>sum(remain_flag)) {
            pollout <- c(pollout,"leave") 
        } else if (length(x)==0) {
            pollout <- c(pollout, "notag")
        } else if ((sum(remain_flag)== sum(leave_flag)) & sum(remain_flag)>0 ){
            pollout <- c(pollout, "notag")
        } else        {
            pollout <- c(pollout, "unknown")
        }
    }
    pollout
}
clean_time <- function(timestamps) {
    ### format twitter timestamp to R class and enable extracting of time components ###
    ### All timestamps from twitter API are in GMT ###
    timestamps %>% 
        strptime("%a %b %d %H:%M:%S +0000 %Y", tz='GMT') %>%
        as.POSIXlt()
}

clean_tweet <- function(tweets, user_id) {
    ## for each tweet, detect whether it is an original post, a reply, or a retweet ##
    ## if retweet or reply, find out who the original twweet was from, eliminate the 
    tweets <- enc2native(tweets)
    rt_pattern = "RT"
    scr_name_pattern = "@[[:alnum:]]+"
    url_pattern = '(([^(:| )]+)://)(([^/ ]+)[/]?)+'
    tag_pattern = '#[A-Za-z]+'

    is_rt= (substr(tweets, 1,2) %in% rt_pattern)
    out_id = str_extract(tweets, scr_name_pattern) %>% gsub(pattern="[@:]", replacement="")
    is_reply = (!is_rt) & (!is.na(out_id))
    out_url = str_extract(tweets, url_pattern)
    
    cleaned = gsub(url_pattern, "", tweets)
    cleaned = gsub(scr_name_pattern, "", cleaned)
    cleaned = gsub(tag_pattern, "", cleaned)
    cleaned = gsub("\\\\[[:alnum:]]+","", cleaned)
    cleaned = gsub('[<][[:alnum:]]+[>]',"",cleaned)
    
    
    return(list(rt=is_rt, reply=is_reply, quote=out_id, urls=out_url, cleaned = cleaned))
}
detect_country <- function(loc) {
    # most tweets are from UK
    # UK codes come in a variety
    # the regular expr need to handle instances:
    # UK & United Kingdom; England, Ireland, Wales
    # Or a particular city name in England (London, Yorkshire)
    uk<-'([Uu](nited )?[Kk](ingom)?)|([Ee]ngland)|([Ii]reland)|([Ww]ales)|([Ss]cotland)|([Gg]reat [Bb]ritain)|[Uu][.][Kk]'
    us<-'(USA)|(America)|(US)|([Uu]nited [Ss]tates)|([Uu][.][Ss])'
    uk_cities <- c("[Ll]ondon|Manchester|Birmingham|Glasgow|Liverpool|Leeds|Sheffield|Edinburgh|Bristol|Cardiff|Newcastle|York|Liverpool|Essex|Yorkshire")
    us_cities <- c("New York|Washington|DC|Chicago|Los Angeles|Silicon Valley|San Francisco|Boston|NY")
    loc_out <-rep("Other", length(loc))
    loc_out[grepl("EU|[Ee]urope", loc)]="Other EU"
    loc_out[grepl(uk, loc)] ="UK"
    loc_out[grepl(us, loc)] ="USA"
    loc_out[grepl(uk_cities,loc)] = "UK"
    loc_out[grepl(us_cities,loc)] = "USA"
    ## Other English speaking countries ##
    loc_out[grepl("[Cc]anada", loc)] ="Canada"
    loc_out[grepl("[Aa]ustralia", loc)] = 'Australia'
    loc_out[grepl("[Ii]ndia", loc)] ='India'
    loc_out[grepl("[Gg]ermany", loc)] ="Germany"
    loc_out[grepl("[Ss]witzerland", loc)]="Swizerland"
    loc_out[grep("[Ii]tal(ia|y)", loc)]="Italy"
    loc_out[loc==""]='Unknown'
    return(loc_out)
}

detect_city <- function(locs) {
    uk_cities <- c("London|Manchester|Birmingham|Glasgow|Liverpool|Leeds|Sheffield|Edinburgh|Bristol|Cardiff|Newcastle|Yorkshire|Liverpool|Essex")
    cities <- str_extract(locs, uk_cities)
    cities[is.na(cities)]=""
    cities
}

preprocessing <- function(clean_tweet){
    out=NULL
    for (t in clean_tweet) {
        t = MC_tokenizer(t)
        t = tolower(t)
        t = t[! t %in% c(stopwords("en"), letters, "rt", "htt","http")]
        t = stemDocument(t)
        t = str_c(t, collapse = " ")
        out = c(out, t)
    }
    out
}
