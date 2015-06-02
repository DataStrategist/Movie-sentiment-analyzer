## Interesting packages

## ------------------------------------------------------------------------
library(syuzhet)
library(stringr)
library(tm)
library(wordcloud)
library(ggplot2)
library(xtable)

###--------- Choices -------------------------------
# Amit:
#   Rambo
#   Predator
#   Die Hard
#   Machete
#   The last dragon
#   
# Laure:
#   When Harry Met Sally
#   Bridget Jones
#   Breakfast at Tiffany's
#   Casablanca
#   Notting Hill

###--------- Load in -------------------------------
pathF <- "K:/Dropbox/Dropbox/My projects/Movie - sentiment analyzer/Movie scripts/"
pathO <- "K:/Dropbox/Dropbox/My projects/Movie - sentiment analyzer/web/Output/"
filesList = list.files(pathF)

generalStats <- data.frame(0,0,0,0)
names(generalStats) <- c("Words","Length","Rate")
iii<-0
naam.list <-NULL


for (i in 1:length(filesList)){
  
  ###--------------- CALCULATE STUFF ------------
  naam <- gsub(".txt","",filesList[i])
  naam.list <-rbind(naam.list,naam)
  iii <- iii+1 
  ## fix name for tifanie
  if (naam=="Breakfast at Tiffanys") naam <- "Breakfast at Tiffany's"
  
  ## Read in
  sentences.df <- get_sentences(get_text_as_string(
    paste(pathF,filesList[i],sep="/")))
  ou <- removeWords(sentences.df, stopwords("english"))  
  
  ## Sentiment analysis 
  feelings.df <- get_sentiment(sentences.df, method="bing")
  
  
  ###--------------- PLOT STUFF ------------
  ###-------- Basic stats
  Werds <- str_count(get_text_as_string(
    paste(pathF,filesList[i],sep="/"))," ")
  Sentences <- length(sentences.df)
#   Length <- ggplot2::movies$length[naam==ggplot2::movies$title]
# hell with this, neither this nor grep work.. movie names are too weird
# what with the ellipsis and quotes and subtext... forget it! Cheat mode:
  moobieLengths=c(115,97,102,131,107,124,107,92,109,96)
  Length=moobieLengths[i]
  WordRate <- Werds/Length
  generalStats[iii,] <- data.frame(Sentences,Length,WordRate)
  
#   print.xtable(xtable(generalStats, type="html", 
#                file=paste(pathO,naam,"-stats.html", sep="")))
# cat(naam, Werds,Length, WordRate)
  
  
  ###--------- Wordclouds -------------------------------
  # man colour
  # wordcloud(ou, scale=c(3,0.5), max.words=50, colors=brewer.pal(8,"PuOr"))
  ##woman colour, use this for all... pff.
  png(width=250, height=250, file=paste(pathO,naam,"-wordcloud.png", sep=""))
    wordcloud(ou, scale=c(3,0.5), max.words=50, colors=brewer.pal(8,"Set2"))
  dev.off()
  
  ## ---- Plot Line -----------------------------------------------------
  percent_vals <- get_percentage_values(feelings.df,bins=moobieLengths[i])
  png(width=725, height=725, file=paste(pathO,naam,"-perc.png", sep=""))
  plot(
    percent_vals, 
    type="l", 
    main="Plot trajectory by minute", 
    xlab = "Narrative Time (min)", 
    ylab= "Emotional Valence", 
    col="red"
  )
  dev.off()
  
  ## ---- Transformed -----------------------------------------------------
  ft_values <- get_transformed_values(
    feelings.df, 
    low_pass_size = 3, 
    x_reverse_len = moobieLengths[i],
    scale_vals = TRUE,
    scale_range = FALSE
  )
  png(width=725, height=725, file=paste(pathO,naam,"-tran.png", sep=""))
  plot(
    ft_values, 
    type ="h", 
    main =" using Transformed Values", 
    xlab = "Narrative Time (min)s", 
    ylab = "Emotional Valence", 
    col = "red"
  )
  dev.off()
  
  ## ---- Feelings-------------------------------------------------------
  
  nrc_data <- get_nrc_sentiment(sentences.df)
  colors <- c("red", "blue", "khaki", "yellow",
              "purple","black","orange", "white")
  feelings.v = colSums(prop.table(nrc_data[, 1:8]))
  colors <- colors[order(feelings.v)]
  feelings.v <- feelings.v[order(feelings.v)]
  feelings.v <- feelings.v *100
  fff = data.frame(feelings=rownames(data.frame(feelings.v)),
                   value=as.vector(feelings.v))
    
  ggplot(fff,aes(x=feelings,y=value)) +
    geom_bar(stat="identity", color="black",
             fill=c("red", "blue", "khaki", "yellow",
                    "purple","black","orange", "white")) + 
    coord_flip()  + theme(axis.text = element_text(size = rel(1.5))) + 
    ggtitle(paste("Prevailing emotions for",naam))+ylab("%")
    
    ggsave(filename=paste(pathO,naam,"-feelings.png", sep=""))

  ####---------- Sentence Drawings -------------------
  all <- data.frame(sentance=sentences.df, sentiment=feelings.df,length=nchar(sentences.df))
  df<- data.frame(x=0,y=0)
  a <- 0
  
  for (ii in 2:nrow(all)){
    if (a==4) a=0
    if (a==0){
      df[ii,1]=df[ii-1,1]+all$length[ii]
      df[ii,2]=df[ii-1,2]
    } else if (a==1){
      df[ii,1]=df[ii-1,1]
      df[ii,2]=df[ii-1,2]+all$length[ii]
    } else if (a==2){
      df[ii,1]=df[ii-1,1]-all$length[ii]
      df[ii,2]=df[ii-1,2]
    } else if (a==3){
      df[ii,1]=df[ii-1,1]
      df[ii,2]=df[ii-1,2]-all$length[ii]
    }
      a=a+1
  }
  
  ####### -------- Add color
  ### Try fancie coloring if you want... but it don't really work.
  # x=c(0,0,3,4)
  # y=C(0,1,1,4)
  # z=c("red","grey", "blue", "red")
  
  ## Select colors
  # library(choosecolor)
  # MyPal <- palette.picker(n=2)
  # color.df <- data.frame(val=sort(unique(feelings.df)),
  #                        color=Mypal(length(unique(feelings.df))),stringsAsFactors =FALSE)
  # color.df[color.df$val==0,2] <- "grey"
  # for (i in 1:nrow(all)){
  #   all$colors[i] <- as.character(color.df[match(all$sentiment[i],color.df$val),2])
  # }
  
  all$colors <- all$sentiment
  all$colors <- gsub("-.+","red",all$colors)
  all$colors <- gsub("0","grey",all$colors)
  all$colors <- gsub("\\d+","green",all$colors)
  
  ## construct final df
  df <- data.frame(x=df$x,y=df$y,col=all$colors)

  ## plot!

  xlim=c(min(df$x)-5,max(df$x)+5)
  ylim=c(min(df$y)-5,max(df$y)+5)
  png(width=725, height=725, file=paste(pathO,naam,"-sDrawing.png", sep=""))  
    plot(xlim,ylim,type="n",xlab="",ylab="",yaxt="n",xaxt="n")
    for (j in 1:nrow(df)){
      lines(df[c(j,j+1),1],df[c(j,j+1),2],col=as.character(df[j+1,3]))
    }
    points(0,0,pch=20)    
    points(x=df[nrow(df),1],y=df[nrow(df),2],pch=20, col="blue")
  dev.off()


  ###------------ Emo Mo --------------------
  ## take into consideration the prior emotional valence.
  
  df.momentum<- data.frame(x=0,y=0)
  
  # For each sentence, add to plot:
  #   TO X: 5
  #   TO Y: sentence length * Sentiment (Or just the sentiment)
  # Yeah, I miss the last sentance... oh boohoo... ;)
  for (ii in 2:nrow(all)){
    df.momentum[ii,1] <- df.momentum[ii-1,1] + 5
    df.momentum[ii,2] <- df.momentum[ii-1,2] + 
#       all$length[ii] * all$sentiment[ii]  
      all$sentiment[ii] # or each line is just the sentiment
  }
  
  ## construct final df
  df.mom <- data.frame(x=df.momentum$x,y=df.momentum$y,col=all$colors)
  
  ## plot!
  xlim=c(min(df.mom$x)-5,max(df.mom$x)+5)
  ylim=c(min(df.mom$y)-5,max(df.mom$y)+5)
  png(width=725, height=725, file=paste(pathO,naam,"-emoMo.png", sep=""))  
  plot(xlim,ylim,type="n",ylab="Emotional valence",xlab="Sentence number")
  for (j in 1:nrow(df.mom)){
    lines(df.mom[c(j,j+1),1],df.mom[c(j,j+1),2],col=as.character(df.mom[j+1,3]))
  }
  points(0,0,pch=20)
  points(x=df.mom[nrow(df.mom),1],y=df.mom[nrow(df.mom),2],pch=20, col="blue")
  dev.off()

  if (iii==10){
    generalStats$gender <- c("female","female","female","male","male","female","male","male","male","female")
    generalStats$Name <- naam.list                        
    ggplot(generalStats,aes(x=Words,y=Length,size=Rate,
                            label=Name,color=factor(gender))) +
      geom_point() + 
      labs(title="Movie length vs # words",
           x="Number of words (used spaces as proxy)",
           y="Movie length (min)",
           color="Movie genre",
           size="Speech rate (words/min)") +
      geom_text(aes(label=Name),vjust=-.5)+ 
      scale_size(range=c(3,6))
    ggsave(filename=paste(pathO,"Movie pace.png", sep=""))
  }
} #-------------------------------- loop end




## Emomo example:
e <- c(
 "I love you.",
 "aw... I love you too!",
 "I hate you, you're destroying my life.",
 "... what?",
 "Just kidding! You're awesome.",
 "Lol... you so crazy!")
e.sentiment <- get_sentiment(e, method="bing")
png(width=725, height=325, file=paste(pathO,"emoMo example.png", sep="")) 
par(mfrow=c(1,2))
  plot(e.sentiment,type="l")
abline(lsfit(1:6, e.sentiment),col="red",lwd=2)
abline(h = 0, v = 0, col = "gray60")
title("Normal sentiment")
df.mom <- data.frame(x=0,y=0)

for (ii in 2:length(e)+1){
  df.mom[ii,1] <- df.mom[ii-1,1] + 1
  df.mom[ii,2] <- df.mom[ii-1,2] + e.sentiment[ii] 
}
## plot!
xlim=c(min(df.mom$x),max(df.mom$x))
ylim=c(min(df.mom$y),max(df.mom$y))
 
plot(xlim,ylim,type="n",ylab="Emotional valence",xlab="Sentence number")
for (j in 1:nrow(df.mom)){
  lines(df.mom[c(j,j+1),1],df.mom[c(j,j+1),2])
}
abline(lsfit(1:6, df.mom[2]),col="red",lwd=2)
abline(h = 0, v = 0, col = "gray60")
title("Emotional Momentum (EmoMo)")
dev.off()





