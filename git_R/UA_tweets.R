library(twitteR)
library(plyr)
library(stringr)
library(wordcloud)
library(tm)
library(dplyr)

api_key <- "PqJTcQPYNI6YdAqucQxdFxNxp"
api_secret <- "H4Be2PSidOdJwE7KZJjBOc8R5hUd9gCnFdeTeLxScnanJdSclu"
access_token <- "713469705697275904-XgLL2qDDM6Oa5EBxGuWo8SFX6JxWptm"
access_token_secret <- "137YdPfO9xmhjPDZyjIvYKtA5DgOkf1z6LhPQxQD1jy8H"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

#UAtweets <-c(searchTwitter('@unitedairlines',n=2000,since='2016-03-24'),searchTwitter('@united',n=2000,since='2016-03-24'),searchTwitter('united airlines',n=2000,since='2016-03-24'))
UAtweets <-c(searchTwitter('@unitedairlines',n=1000,since=as.character(Sys.Date() - 1)),searchTwitter('@united',n=2000,since=as.character(Sys.Date() - 1)))

#UAtweets.cln = laply(UAtweets, function(t) t$getText() )   #one-dimensional array = vector

#remove duplicate tweets
UAtweets.txt <- sapply( unlist( UAtweets ) , function(x) `$`( x , "text" ) )
length(UAtweets.txt)
#typeof(UAtweets.txt)
#is.vector(UAtweets.txt)

UAtweets.cln <- UAtweets.txt[!duplicated( UAtweets.txt )]   #this does the same: UAtweets.cln <- unique(UAtweets.txt)
length(UAtweets.cln)

#UAtweets.cln=str_replace_all(UAtweets.cln,"(^[:graph:])", "x")   #remove non graphical characters in vector with regex. THIS DOESN'T WORK CORRECTLY...don't know why

hu.liu.pos = scan('C:/Users/u298739/Downloads/positive-words.txt',
                  what='character', comment.char=';')
hu.liu.neg = scan('C:/Users/u298739/Downloads/negative-words.txt',
                  what='character', comment.char=';')

pos.words = c(hu.liu.pos, 'stroopwafel', 'waffle')
neg.words = c(hu.liu.neg, 'wtf', 'wait', 'waiting','epicfail', 'mechanical', 'UnitedFail', 'late', 'stuck', 'unitedisadump', 'unitedidiots', 'custservfail', 'overbook', 'oversell')


  
  score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
  {
    require(plyr)
    require(stringr)
    
    # we got a vector of sentences. plyr will handle a list
    # or a vector as an "l" for us
    # we want a simple array of scores back, so we use
    # "l" + "a" + "ply" = "laply":
    scores = laply(sentences, function(sentence,pos.words, neg.words) {
      
      # clean up sentences with R's regex-driven global substitute, gsub():
      sentence = gsub('[[:punct:]]', '', sentence)
      sentence = gsub('[[:cntrl:]]', '', sentence)
      sentence = gsub('\\d+', '', sentence)
      # and convert to lower case:
      sentence = tolower(sentence)
      
      # split into words. str_split is in the stringr package
      word.list = str_split(sentence, '\\s+')
      # sometimes a list() is one level of hierarchy too much
      words = unlist(word.list)
      
      # compare our words to the dictionaries of positive & negative terms
      pos.matches = match(words, pos.words)
      neg.matches = match(words, neg.words)
      
      # match() returns the position of the matched term or NA
      # we just want a TRUE/FALSE:
      pos.matches = !is.na(pos.matches)
      neg.matches = !is.na(neg.matches)
      
      # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
      score = sum(pos.matches) - sum(neg.matches)
      
      return(score)
    }, pos.words, neg.words, .progress=.progress )
    
    scores.df = data.frame(score=scores, text=sentences)
    return(scores.df)
  }
  
  
  
  sanityck <- c("You're awesome!", "I hate you!", "flights are delayed again", "stroopwafel")
  result_ck <-score.sentiment(sanityck, pos.words, neg.words)
  
  
  result <- score.sentiment(UAtweets.cln, pos.words, neg.words)
  result
  