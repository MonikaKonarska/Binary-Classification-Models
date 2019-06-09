*Let's get more information about the borrowers.*
-------------------------------------------------

From data LC (from 2013-01-01 to 2015-06-30) selected samples of
variable `empl_title`. Created three datasets (the sample of all
borrowers, the sample of borrowers who didn't pay off loan and the
sample of borrowers who paid off).

Here the some examples of employment title.

    ##   [1] "business development"                  
    ##   [2] "Center Director"                       
    ##   [3] "Mountain Manager"                      
    ##   [4] "EAP Counselor"                         
    ##   [5] "Specialist"                            
    ##   [6] "TEAM LEADER"                           
    ##   [7] "Patient Financial Services"            
    ##   [8] "Gary School Corporation"               
    ##   [9] "CNC operator"                          
    ##  [10] "Engineer"                              
    ##  [11] "owner"                                 
    ##  [12] "Cloud Software Engineer"               
    ##  [13] ""                                      
    ##  [14] "Administrator"                         
    ##  [15] "Waikiki Beach Resort & Spa (Marriott)" 
    ##  [16] "Branch Manager"                        
    ##  [17] "City of NC"                            
    ##  [18] "planes moving and storage"             
    ##  [19] "State Compensation Insurance Fund"     
    ##  [20] "Chrysler LLC"                          
    ##  [21] "Specialties"                           
    ##  [22] "Leasing & Management Company, Inc."    
    ##  [23] "Infuscience"                           
    ##  [24] "Registered Nurse"                      
    ##  [25] "Tongue & Cheek"                        
    ##  [26] "admistrative assistant"                
    ##  [27] "welder"                                
    ##  [28] "Communication Sergeant"                
    ##  [29] "Police communication technician"       
    ##  [30] "Sr programmer"                         
    ##  [31] "optician"                              
    ##  [32] "Providence St. Joseph Medical Center"  
    ##  [33] "Rustic Inn"                            
    ##  [34] "agent"                                 
    ##  [35] "stonebridge com"                       
    ##  [36] "clerk"                                 
    ##  [37] "Envoy Mortgage"                        
    ##  [38] "Key Systems"                           
    ##  [39] "Program Manager"                       
    ##  [40] "Director"                              
    ##  [41] "security"                              
    ##  [42] "Senior Captain"                        
    ##  [43] "Prestonwood Baptist Church"            
    ##  [44] "Medical Laboratory Scientist"          
    ##  [45] "Lakewood Health Care"                  
    ##  [46] "pre trial interviewer"                 
    ##  [47] "Director Plant Opertions"              
    ##  [48] "Shift Supervisor"                      
    ##  [49] "Conagra Foods"                         
    ##  [50] "server"                                
    ##  [51] "executive chef"                        
    ##  [52] "Kettle Foods Inc."                     
    ##  [53] "Sales Representative"                  
    ##  [54] "Maintenance technician"                
    ##  [55] "Aon Hewitt"                            
    ##  [56] "Business Director"                     
    ##  [57] "Operations / Assistant"                
    ##  [58] "Brand"                                 
    ##  [59] "Omni Logistics, Inc"                   
    ##  [60] "Lead Mobile Designer"                  
    ##  [61] "RN supervisor"                         
    ##  [62] "manager"                               
    ##  [63] "US Department of Labor - OFCCP"        
    ##  [64] ""                                      
    ##  [65] "Myers and Stauffer LC"                 
    ##  [66] "physicians assistants for neurosurgery"
    ##  [67] "UTC Aerospace Systems"                 
    ##  [68] "correction officer"                    
    ##  [69] "Infection Preventionist"               
    ##  [70] "locksmith"                             
    ##  [71] "Attorney"                              
    ##  [72] "assembler"                             
    ##  [73] ""                                      
    ##  [74] "Financial Administrator"               
    ##  [75] "Occupational Therapists"               
    ##  [76] "Pressman"                              
    ##  [77] "NC Dept. of Transportation"            
    ##  [78] "Electronic Integrated Mechanic/WG-12"  
    ##  [79] "ace insurance co"                      
    ##  [80] "Benefits Actuary"                      
    ##  [81] "sports producer/ photographer"         
    ##  [82] ""                                      
    ##  [83] ""                                      
    ##  [84] "Corrections Officer"                   
    ##  [85] "Bermo Ind Inc"                         
    ##  [86] "Medical Social Worker"                 
    ##  [87] "Walgreens"                             
    ##  [88] "County of Santa Barbara"               
    ##  [89] "Columbia College Chicago"              
    ##  [90] "Manager"                               
    ##  [91] "UCLA Radiological Sciences"            
    ##  [92] "Sr. Field Ops"                         
    ##  [93] ""                                      
    ##  [94] "Roper St. Francis Hospital"            
    ##  [95] "set lighting technician"               
    ##  [96] "general clerk"                         
    ##  [97] "Lindenwood University"                 
    ##  [98] "PHYSICAL THERAPIST"                    
    ##  [99] "HASCO INC."                            
    ## [100] "Field Service Supervisor"

### **Word Cloud of borrowers employment**

### Converting the text file into a Corpus

    docsBorrowersAll <- Corpus(VectorSource(borrowesAll))
    docsBorrowersDefault <- Corpus(VectorSource(borrowersWithDefault))
    docsBorrowersWithoutDefault <- Corpus(VectorSource(borrowersWithoutDefault))

### Text cleaning

Used function `tm_map()` from the `tm` package for processing text. It
is some operations on the text:

-   strip unnecessary white space,  
-   convert everything to lower case,  
-   remove numbers and punctuation with the removeNumbers and
    removePunctuation arguments.

<!-- -->

    borrowesAll_tm <- tm_map(docsBorrowersAll, stripWhitespace)
    borrowesAll_tm <- tm_map(borrowesAll_tm, tolower)
    borrowesAll_tm <- tm_map(borrowesAll_tm, removeNumbers)
    borrowesAll_tm <- tm_map(borrowesAll_tm, removePunctuation)
    borrowesAll_tm <- tm_map(borrowesAll_tm, removeWords, stopwords('english'))

    docsBorrowersDefault_tm <- tm_map(docsBorrowersDefault, stripWhitespace)
    docsBorrowersDefault_tm <- tm_map(docsBorrowersDefault_tm, tolower)
    docsBorrowersDefault_tm <- tm_map(docsBorrowersDefault_tm, removeNumbers)
    docsBorrowersDefault_tm <- tm_map(docsBorrowersDefault_tm, removePunctuation)
    docsBorrowersDefault_tm <- tm_map(docsBorrowersDefault_tm, removeWords, stopwords('english'))

    docsBorrowersWithoutDefault_tm <- tm_map(docsBorrowersWithoutDefault, stripWhitespace)
    docsBorrowersWithoutDefault_tm <- tm_map(docsBorrowersWithoutDefault_tm, tolower)
    docsBorrowersWithoutDefault_tm <- tm_map(docsBorrowersWithoutDefault_tm, removeNumbers)
    docsBorrowersWithoutDefault_tm <- tm_map(docsBorrowersWithoutDefault_tm, removePunctuation)
    docsBorrowersWithoutDefault_tm <- tm_map(docsBorrowersWithoutDefault_tm, removeWords, stopwords('english'))

##### Creating word clouds

    wordcloud (docsBorrowersWithoutDefault_tm,
               scale=c(5,0.5),
               max.words=200,
               random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, 'Dark2'))

![](who_are_the_borrowers_files/figure-markdown_strict/clouds-1.png)

    wordcloud (docsBorrowersDefault_tm,
               scale=c(5,0.5),
               max.words=200,
               random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, 'Dark2'))

![](who_are_the_borrowers_files/figure-markdown_strict/clouds-2.png)

    wordcloud (borrowesAll_tm,
               scale=c(5,0.5),
               max.words=200,
               random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, 'Dark2'))

![](who_are_the_borrowers_files/figure-markdown_strict/clouds-3.png)
