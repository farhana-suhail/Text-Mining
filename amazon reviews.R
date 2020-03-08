install.packages("stringr")
library(stringr)

install.packages("utils")
library(utils)

require(stringr)||install.packages("stringr"); library(stringr)  
require(utils)||install.packages("utils"); library(utils)  

amazon <- function(url,   #Amazon review URL. first click on show all or top review
                   n)     #Number of pages to extract
{
  text_page=character(0)  #define blank file
  
  pb <- txtProgressBar() #define progress bar
  url = unlist(str_split(url,"&"))[1]
  
  for (i in 0:n) {   #loop for url
    
    p=i*10
    e= "/ref=cm_cr_arp_d_paging_btm_next_2?pageNumber=2"
    url0 = paste(url,e,p,sep = "")  #create amazon url in correct format
    
    text = readLines(url0) #Read URL
    
    text_start = grep("<span class=\"a-size-base\">",text)  #review start marker
    
    text_stop = grep("<div class=\"a-expander-header a-expander-partial-collapse-header readMore\">",text)  #review end marker
    
    
    setTxtProgressBar(pb, i)  #print progress bar
    
    if (length(text_start) == 0) break  #check for loop termination, i.e valid page found
    
    for(j in 1:length(text_start))  #Consolidate all reviews
    {
      text_temp = paste(paste(text[(text_start[j]+1):(text_stop[j])]),collapse=" ")
      #text_temp = paste(merge(text(text_start[j]+1):(text_stop[j])),collapse = "")
      text_page = c(text_page,text_temp)
      plot.new()
    }
    #Sys.sleep(1)
  }
  text_page = gsub("<.*?>","",text_page)  #regex for removing HTML characher
  text_page = gsub("^\\s+|\\s+$","",text_page)  #regex for removing leading and trailing white space
  return(text_page)
}
url = "https://www.amazon.in/product-reviews/B01LXMHNMQ"
samsung = amazon(url,10)
length(samsung)

samsung1 <- as.data.frame(samsung)
samsung1 <- write.table(samsung1, 'samsung1.txt')
getwd()
