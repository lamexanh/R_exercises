
################################################################################ 
#################### Statistical Programming Languages 2021  ##################
#################### Programming Tasks                       ##################
#################### Author: Trang Le Thi (616071)           ##################
################################################################################


# 4a) To_b_language:

install.packages("stringr")
library(stringr)


transform <- function(word) {
  # find the indices and lengths of the vowel in 1 word
  matches <- gregexpr("([aieouAIEOU]*[yY])$|([aieouAIEOU]*[yY])[^aieouAIEOU]|([aieouAIEOU]+)", word)
  matches_indices <- matches[[1]]   # each word creat the list with length = 1. within the list, it has the matches_indices and lengths 
  matches_lengths <- attr(matches_indices, "match.length")
  
  # if there is no vowel in the word
  
  if (matches_indices[1] == -1) {
    return(word)
  }
  
  # Initially, creat an empty result, and then update the result
  # last_index is used to check the process of updation
  # index and length show the position and length of vowel part in the word
  
  
  last_index = 1
  result = ""
  
  for (i in 1:length(matches_indices)) {
    index <- matches_indices[i]
    length <- matches_lengths[i]
    
    # Update the result by appending the part from last_index to current index (not include current index)
    if (last_index < index) {
      result <- paste(result, substr(word, last_index, index - 1), sep="")
    }
    
    # show the vowel(s), given current index and length
    vowels <- substr(word, index, index + length - 1)
    
    # in case [yY] followed by a consonant, I remove the consonant (before doubling this vowel(s))
    
    if (str_detect(substr(word, index, index + length - 1),".*[yY][^oaiueAIEOU]$")) {
      
      length <- length - 1
      vowels <- substr(word, index, index + length - 1)
    }
    
    # update the result by appending the vowel(s)
    result <- paste(result, vowels, "b", vowels, sep="")
    
    # then update the last_index

    last_index = index + length

  # insert the last part of the word (if the word ends with consonant(s))
  if ((i == length(matches_indices) & last_index <= nchar(word))) { 
    result = paste(result, substr(word, last_index, nchar(word)), sep="")
  }
  
  }
  return(result)
}

to_b_language <- function(text) {
  #sanity:
  if(!is.character(text)) stop ("Text must be of type 'character'.")
  
  words <- unlist(str_split(text, boundary("word")))
  if(all(str_detect(words,"[^a-zA-Z]+"))) stop ("b_text contains no words to be transferred.") #(only contain improper word)
  words <- gsub("(.*[^a-zA-Z]+.*)", "", words) # (to clear improper words away)
  words <- words[words != ""] # to keep only proper words
  
  
  
  result <- c()
  for(i in 1:length(words)) {
    result[i] <- transform(words[i])
  }
  
  #to convert all letter (except for the first letter) to lowercase letters
  result <- paste(substring(result, 1,1), tolower(substring(result, 2)), sep="") 
  
  return(result)
}

### example tests:

text<- c("B-Language w45gsgsdf r34tw5gs is FUNNun!", " Joyful FFgfhhgj, dgfhfgIYIUYjfhjy quoay hoaOIYEOTWEy yby tung hoanh ngang doc huy hoang huynh khuong quanh quyen luyen","Without knowing the rules, you hardly understand it.", "coiyon, coiyly, joy, joyful, toy, eye, you", "security repay accompanying by very, any tyler many ways Your Styling/Headers Looney")

to_b_language(text)


text3 <- c("HeLLo", "wORLD!")
to_b_language(text3)

to_b_language("4ever young")
to_b_language(1:10)
to_b_language("4ever")


########################################################

# 4b) From_b_language: 

library(stringr)

from_b_language <- function(b_text){
  
  #sanity:
  
  if(!is.character(b_text)) stop ("Text must be of type 'character'.")
  
  b_text <- unlist(str_split(b_text, boundary("word")))
  if(all(str_detect(b_text,"[^a-zA-Z]+"))) stop ("b_text contains no words to be transferred.") #(only contain improper word)
  b_text <- gsub("(.*[^a-zA-Z]+.*)", "", b_text) # (to clear improper words away)
  b_text <- b_text[b_text != ""] # to only keep proper words
  
  # where consecutive vowels are followed by the letter "b" and repeated, I replace them by these consecutive vowels
 
   result <- gsub("([aeiuoyAEIUOY]+)b\\1", "\\1", b_text)
  
  #to convert all letter (except for the first letter) to lowercase letters
  result <- paste(substring(result, 1,1), tolower(substring(result, 2)), sep="") 
  
  return(result)
}

### example tests:


b_text <- c ("325345r", "HebeLLobo", "wORLD!", "Aban ibibibis ibis aba bibird bubut ibin BLabanguabuagebe ibit meabeans ibis")

from_b_language(b_text)

from_b_language("4evevever")
from_b_language(1:10)


from_b_language(to_b_language(text))






