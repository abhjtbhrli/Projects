# Code to scrape data about Indian football's state associations registered under AIFF
# Abhijit Bharali

# Write a function to scrape data from every state association of AIFF

scrapeAiffSfa<-function(sfa_url){
  # load required libraries
  library(rvest)
  library(stringr)
  pg<-sfa_url
  scr<-read_html(pg)
  
  # scrape data
  a<-as_tibble(str_replace_all((scr%>%html_nodes(".col-6 .blk")%>%
                                  html_text()%>%
                                  as.character()),
                               "[\t\n]",""))
  
  a1<-cbind(as_tibble(gsub('\\d','', a$value)),
            as_tibble(as.numeric(gsub('\\D','', a$value))))
  
  colnames(a1)<-c("category","count")
  a1$stateassoc<-a1$category
  return(a1)
} 

# Create a dataframe containing the state association urls (Manual work since I couldn't manage to scrape the urls from the webpage)

sfa_url<-data.frame(state.ut=c("Manipur","Andaman & Nicobar","Andhra Pradesh",
                               "Arunachal Pradesh","Assam","Bihar","Chandigarh",
                               "Chattisgarh","Dadra and Nagar Haveli",
                               "Daman and Diu","Odisha","Delhi","Goa","Gujarat",
                               "Haryana","Himachal Pradesh","West Bengal",
                               "Jammu & Kashmir","Jharkhand","Karnataka",
                               "Kerala","Lakshadweep","Madhya Pradesh","Meghalaya",
                               "Mizoram","Nagaland","Pondicherry","Punjab",
                               "Rajasthan","Sikkim","Tamil Nadu","Telangana",
                               "Tripura","Uttar Pradesh","Uttarakhand","Maharashtra"),
                    sfa_url=c("https://www.the-aiff.com/association/497",
                              "https://www.the-aiff.com/association/521",
                              "https://www.the-aiff.com/association/522",
                              "https://www.the-aiff.com/association/504",
                              "https://www.the-aiff.com/association/498",
                              "https://www.the-aiff.com/association/499",
                              "https://www.the-aiff.com/association/500",
                              "https://www.the-aiff.com/association/501",
                              "https://www.the-aiff.com/association/534",
                              "https://www.the-aiff.com/association/502",
                              "https://www.the-aiff.com/association/503",
                              "https://www.the-aiff.com/association/491",
                              "https://www.the-aiff.com/association/492",
                              "https://www.the-aiff.com/association/505",
                              "https://www.the-aiff.com/association/506",
                              "https://www.the-aiff.com/association/507",
                              "https://www.the-aiff.com/association/494",
                              "https://www.the-aiff.com/association/508",
                              "https://www.the-aiff.com/association/509",
                              "https://www.the-aiff.com/association/493",
                              "https://www.the-aiff.com/association/495",
                              "https://www.the-aiff.com/association/560",
                              "https://www.the-aiff.com/association/510",
                              "https://www.the-aiff.com/association/511",
                              "https://www.the-aiff.com/association/512",
                              "https://www.the-aiff.com/association/513",
                              "https://www.the-aiff.com/association/514",
                              "https://www.the-aiff.com/association/496",
                              "https://www.the-aiff.com/association/516",
                              "https://www.the-aiff.com/association/517",
                              "https://www.the-aiff.com/association/518",
                              "https://www.the-aiff.com/association/551",
                              "https://www.the-aiff.com/association/520",
                              "https://www.the-aiff.com/association/523",
                              "https://www.the-aiff.com/association/524",
                              "https://www.the-aiff.com/association/519"))

# Create an empty dataframe with 3 columns (hard coded column names)
x<-data.frame(category=character(),
              count=integer(),
              stateassoc=character())

# Populate empty dataframe
for (i in 1:nrow(sfa_url)) {
  y<-scrapeAiffSfa(sfa_url$sfa_url[i])
  for (j in 1:nrow(y)) {
    y$stateassoc[j]=sfa_url$state.ut[i]
  }
  x<-rbind(x,y)
}

# Transform dataframe to wide format
x_tr<-x%>%spread(category,count)

# Time to make vizzes...
