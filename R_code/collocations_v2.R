model <- udpipe_load_model("english-ud-2.0-170801.udpipe")

stopwords <- c("a", "about", "above", "above", "across", "after", "afterwards", "again", "against", "all", "almost", "alone", "along", "already", "also","although","always","am","among", "amongst", "amoungst", "amount",  "an", "and", "another", "any","anyhow","anyone","anything","anyway", "anywhere", "are", "around", "as",  "at", "back","be","became", "because","become","becomes", "becoming", "been", "before", "beforehand", "behind", "being", "below", "beside", "besides", "best", "between", "beyond", "bill", "both", "bottom","but", "by", "call", "can", "cannot", "cant", "co", "con", "could", "couldnt", "cry", "de", "describe", "detail", "do", "done", "down", "due", "during", "each", "eg", "eight", "either", "eleven","else", "elsewhere", "empty", "enough", "etc", "even", "ever", "every", "everyone", "everything", "everywhere", "except", "few", "fifteen", "fify", "fill", "find", "fire", "first", "five", "for", "former", "formerly", "forty", "found", "four", "from", "front", "full", "further", "get", "give", "go", "had", "has", "hasnt", "have", "he", "hence", "her", "here", "hereafter", "hereby", "herein", "hereupon", "hers", "herself", "him", "himself", "his", "how", "however", "hundred", "ie", "if", "in", "inc", "indeed", "interest", "into", "is", "it", "its", "itself", "keep", "last", "latter", "latterly", "least", "less", "love","ltd", "made", "many", "may", "me", "meanwhile", "might", "mill", "mine", "more", "moreover", "most", "mostly", "move", "much", "must", "my", "myself", "name", "namely", "neither", "never", "nevertheless", "next", "nine", "no", "nobody", "none", "noone", "nor", "not", "nothing", "now", "nowhere", "of", "off", "often", "on", "once", "one", "only", "onto", "or", "other", "others", "otherwise", "our", "ours", "ourselves", "out", "over", "own","part", "per", "perhaps", "please", "put", "rather", "review" ,"re", "same", "say", "see", "seem", "seemed", "seeming", "seems", "serious", "several", "she", "should", "show", "side", "since", "sincere", "six", "sixty", "so", "some", "somehow", "someone", "something", "sometime", "sometimes", "somewhere", "still", "such", "system", "take", "ten", "than", "that", "the", "their", "them", "themselves", "then", "thence", "there", "thereafter", "thereby", "therefore", "therein", "thereupon", "these", "they", "thickv", "thin", "third", "this", "those", "though", "three", "through", "throughout", "thru", "thus", "to", "together", "too", "top", "toward", "towards", "twelve", "twenty", "two", "un", "under", "until", "use", "up", "upon", "us", "very", "via", "was", "we", "well", "were", "what", "whatever", "when", "whence", "whenever", "where", "whereafter", "whereas", "whereby", "wherein", "whereupon", "wherever", "whether", "which", "while", "whither", "who", "whoever", "whole", "whom", "whose", "why", "will", "with", "within", "without", "would", "yet", "you", "your", "yours", "yourself", "yourselves", "the", "try", "make", "want", "way", "lot", "need", "good", "great", "hate", "feature", "update", "game")

ProcessApp <- function(app_df, app_name){
  parsed <- udpipe_annotate(object = model, x = app_df$content, doc_id = app_df$review_id, parser="none")
  parsed <- as.data.frame(parsed)
  parsed <- parsed[,c("doc_id", "sentence_id", "sentence", "token_id", "token", "lemma", "upos")]
  parsed$lemma <- tolower(parsed$lemma)
  
  DT <- as.data.table(parsed)
  
  grouped <- DT[, list(filtered_review = paste(lemma, collapse=" ")), by=c("doc_id", "sentence_id")]
  grouped$filtered_review <- sapply(grouped$filtered_review, str_replace_all, pattern="\\s+(?=\\p{Punct})", replacement="")
  
  
  filtered_DT <- DT[upos %in% c("NOUN", "VERB", "ADJ") & !lemma %in% c("app", app_name, stopwords)]
  collocations <- collocation(as.data.frame(filtered_DT), term = "lemma", 
              group = c("doc_id", "sentence_id"), ngram_max = 2, 
              n_min = 3)
  collocations$pos_right <- parsed[match(collocations$right, parsed$lemma), "upos"]
  collocations$pos_left <- parsed[match(collocations$left, parsed$lemma), "upos"]
  collocations <- filter(collocations, !(pos_right == "ADJ" & (pos_left == "ADJ" | pos_left == "VERB")))
  collocations <- filter(collocations, !(pos_right == "VERB" & pos_left == "VERB"))
  collocations <- filter(collocations, !(right == left))
  

  
  Segment <- function(corpus, collocation) {
    word1 <- unlist(strsplit(collocation, " "))[1]
    word2 <- unlist(strsplit(collocation, " "))[2]
    #regex_builder <- paste(word1, "([a-z]+ ){0,1}", word2, sep = "")
    regex_builder <- paste("\\b",word1,"(?:\\W+\\w+){0,1}\\W+",word2, "\\b", sep = "")
    seg <- corpus_segment(x = corpus, pattern= regex_builder, valuetype = "regex")
    docs <- seg$documents
    if (nrow(docs) == 0){
      docs <- data.frame(texts = "NA", document = "NA", docid = "NA", segid = "NA", pattern = "NA") 
    } else {
    
      docs$pattern <- collocation
      colnames(docs) <- c("texts", "document", "docid", "segid", "pattern")
    }
    return(docs)
  }
  
  sentences <- DT[,list(sentence = paste(token, collapse=" ")), by=c("doc_id", "sentence_id")]
  sentences$sentence <- sapply(sentences$sentence, str_replace_all, pattern="\\s+(?=\\p{Punct})", replacement="")
  

  preprocessed.corpus <- corpus(grouped$filtered_review)
  tagged <- lapply(collocations$keyword, Segment, corpus = preprocessed.corpus)
  if(length(tagged) == 0){tags <- data.frame()} else{
    tags <- rbind.fill(tagged)[,c("docid", "pattern")]
    tags <- filter(tags, tags$pattern != "NA")
    tags <- unique(tags)
    names(tags) <- c("parsed_row_id", "feature")
    tags$app <- app_name
    tags$review_id <- sentences$doc_id[as.integer(tags$parsed_row_id)]
    tags$sentence <- sentences$sentence[as.integer(tags$parsed_row_id)]
    tags <- tags[,-1]
    
    feature_count <- tags %>%
      group_by(feature, review_id) %>%
      summarise() %>%
      count(feature) %>%
      ungroup() %>%
      filter(n>2) %>%
      arrange(desc(n))
    
    for (row in seq(1:nrow(feature_count))){
      most_freq_feature <- feature_count$feature[row]
      reverse <- paste(rev(unlist(str_split(most_freq_feature, " "))), collapse = " ")
      index_reverse <- match(reverse,feature_count$feature)
      if(is.na(index_reverse)) next
      feature_count$feature[index_reverse] <- most_freq_feature
    }
    feature_count <- aggregate(n ~ feature, feature_count, sum) %>%
      arrange(desc(n))
    
    for (row in seq(1:nrow(feature_count))){
      most_freq_feature <- feature_count$feature[row]
      distances <- sapply(feature_count$feature, adist, most_freq_feature)
      index_similar <- match(1, distances)
      if(is.na(index_similar)) next
      feature_count$feature[index_similar] <- most_freq_feature
    }
    feature_count <- aggregate(n ~ feature, feature_count, sum) %>%
      arrange(desc(n))
  
  tags <- tags %>%
    filter(feature %in% feature_count$feature) %>%
    unique()}
  return(tags)
  
}
GroupAllCollocations <- function(df) {
  
  unique_patterns <- unique(df[,c("feature", "review_id")])
  patterns <- unique(unique_patterns$feature)
  
  pattern_freq <- data.table(t(table(patterns)))
  pattern_freq <- pattern_freq[,c("patterns", "N")][order(-rank(N))]
    
  comb <- t(combn(as.character(pattern_freq$patterns), 2))
  comb <- as.data.frame(comb, stringsAsFactors = FALSE)
  
 
  
  cosine_sim <- function(comb){
    lists <- apply(comb, 1, function(x) list(list(text = x[1]), list(text = x[2])))
    req <- httr::POST(url = "http://api.cortical.io:80/rest/compare/bulk?retina_name=en_associative",
                      body = (lists), encode = "json")
    unlist <- unlist(httr::content(req))
    return(as.numeric(unlist[names(unlist) == "cosineSimilarity"]))
  }
  
  replace_df <- data.frame()
  try <- try(cosine_sim(comb))
  if (!inherits(try, "try-error")){df <- df}
  else{
    comb$cosine_score <-try
    ref <- comb
    df <- data.table(df)
    i <- 1
    rows <- nrow(ref)
    while(rows >0 ){
      from <- ref[i, "V2"]
      to <- ref[i, "V1"]
      cosine_score <- ref[i, "cosine_score"]
      
      if (cosine_score >= 0.60 #| context_score >= 0.5
          ){
        replace_df <- rbind(replace_df, data.frame(from = from, to = to))
        ref <- ref[!(ref$V1 == from | ref$V2 == from),]
      } else {
        ref <- ref[-1,]
      }
      rows <- nrow(ref)
    }
  
    index1 <- match(df$feature, replace_df$from)
    df$feature[!is.na(index1)] <- as.character(replace_df$to[na.omit(index1)])
    df <- unique(df)}
    
    
    return(df)
}


ConstructFinalDF <- function(df, full_df) {
  join <- join(df, full_df, by = c("review_id", "app"), type = "left")
  no_feature <- full_df[!full_df$review_id %in% join$review_id,]
  no_feature$feature <- "none"
  no_feature$sentence <- "none"
  final <- rbind(join, no_feature)
  final <- unique(final)
  final <- plyr::rename(final, replace = c("content" = "review", "feature" = "pattern"))
  final <- mutate(final, review_sentiment = case_when(
     rating == 1 ~ -2,
     rating == 2 ~ -1,
     rating == 3 ~ 0,
     rating == 4 ~ 1,
     rating == 5 ~ 2))
  final <- final %>%
    select(app, review_id, pattern, review, rating, sentence,  review_sentiment, date) %>%
    arrange(review_id) %>%
    group_by(app, review_id, pattern, review, rating,  review_sentiment, date) %>%
    summarize(sentence = paste(sentence, collapse = "(...)\n"))
  
  return(final)
}


