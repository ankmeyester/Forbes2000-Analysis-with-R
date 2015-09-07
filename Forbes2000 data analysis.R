
#---------Forbes2000 dataset evaluation-------------

#Loading package with Forbes2000 dataset
require(HSAUR2)

#Assigning data to a new variable
x <- Forbes2000

#Top 3 companies in each category wrt to sales

        #Table view
        by(x[order(x$sales,decreasing = TRUE),],x$category,head,3)

        #Only company names
        by(x[order(x$sales,decreasing = TRUE),2],x$category,head,3)

#Top 3 companies in each category wrt to profits

        #Table view
        by(x[order(x$profits,decreasing = TRUE),],x$category,head,3)
        
        #Only company names
        by(x[order(x$profits,decreasing = TRUE),2],x$category,head,3)

#----------------------------------------------------------------------------
#Top 3 companies in each country wrt to assets

        #Table view
        by(x[order(x$assets,decreasing = TRUE),],x$country,head,3)
        
        #Only company names
        by(x[order(x$assets,decreasing = TRUE),2],x$country,head,3)
        

#Top 3 companies in each country wrt to market value
        
        #Table view
        by(x[order(x$marketvalue,decreasing = TRUE),],x$country,head,3)
        
        #Only company names
        by(x[order(x$marketvalue,decreasing = TRUE),2],x$country,head,3)
        
#------------------------------------------------------------------------------       
#How many companies will it take in any 2 country other than USA to :
#        
#        a. Match the combined asset size (+/-10%) of top 2 companies in USA.
#------------------------------------------------------------------------------

#Output Data Frame contains all valid countries (not just two) with required information:

        #Assigning data to a new variable
        x <- Forbes2000
        
        #Taking total asset of top two USA companies
        USassetSum <- sum(head(x[x$country=="United States",],2)$assets)
        
        #Creating Asset Size range
        USAL <- USassetSum - (USassetSum*0.1)
        USAU <- USassetSum + (USassetSum*0.1)

        #Extracting levels and removing USA from the level list
        CountryLevel <- levels(x[,3])
        CountryLevel <- CountryLevel[CountryLevel!="United States"]
        
        #Data frame to fill the required data into with first column names "Assets"
        df <- data.frame(Assets=numeric())
        
        #Looping through the levels
        for (i in CountryLevel){
            
            #Taking asset total per country
            allCountryAssets <- sum(x[x$country==i,]$assets)
            
            #Adding into the empty data frame
            df[i,1] <- allCountryAssets
            
            #Comparing the asset total per country to lower end of USassetSum range
            df[i,2] <- ifelse(allCountryAssets>=USAL, TRUE, FALSE)
            
            #Ordering according to asset size per country (highest to lowest)
            df <- df[order(df$Assets,decreasing=TRUE),]
            
            #Assigning main data to y as per country
            y <- x[x$country==i,]
            
            #Ordering y data set by asset size (highest to lowest)
            y <- y[order(y$asset,decreasing=TRUE),]$assets
            
            #Initializing variables in order to count companies
            AssetSize <- 0
            count <- 0
            
            #Looping over Asset vector per country sorted highest to lowest
            for(n in seq(along=y)){
                    
                   #If initial value crosses USassetSum, skip it
                   if( (AssetSize + y[n]) > USAU ){next}
                    
                    #Adding asset size for valid companies
                    AssetSize <- AssetSize + y[n]
                    
                    #Adding number of companies provided condition is met
                    count <- count + 1
                    
                  #Stop adding to count when upper limit has been met
                  if (AssetSize > USAL & AssetSize < USAU){
                          break
                  }
            }
            
            #Add Count as a new column
            df[i,3] <- count
            
            #Add Asset size as a new column
            df[i,4] <- AssetSize
            
            #For all the countries which did not meet the criteria, records are removed
            if (df[i,2]==FALSE){
                  df <- df[!rownames(df)==i,]
            }
        
           }

           #Removing first two columns as it's not required in final output
           df <- df[,-c(1,2)]
           
           #Create visible range column in table and also name columns
           df[,3] <- paste(USAL,USAU,sep=" to ")
           names(df)[c(1:3)] <- c("Total Companies","Combined Asset Size","Required Within Range")
           
           #Please refer to df data frame as output file with all required information


#------------------------------------------------------------------------------
#How many companies have the nearly same range (+/- 5%) of  profit ratio 
#(profits/sales) :
#       In each category
#       In each country
#       In each country and category.
#------------------------------------------------------------------------------

#I was concerned about the validity of this problem statement.
#Regardless, I have tried my best to give a clean solution.
           
        
        #Assigning data to a new variable
        x <- Forbes2000
           
        #Removing NAs from profits
        x <- x[!is.na(x$profits),]
        
        #Adding column for Profit Ratio and sorting according to profit ratio
        x$profitRatio <- x$profits / x$sales
        a <- x[order(x$profitRatio),]
        
        #Extracting Levels of Category and Country
        lcat <- levels(a$category)
        lcou <- levels(a$country)
        
        #Loop to find companies with same profit ratio range per category
        for (i in 1:length(lcat)) {
                
                #Assigning previous data frame to a temporary data frame
                temp <- a[a$category == lcat[i],]
                
                #With profitRatio sorted data, compare nth data to (n+1)the data and record positions
                d <- which(temp$profitRatio[1:(length(temp$profitRatio) - 1)] == temp$profitRatio[2:length(temp$profitRatio)])
                
                #If no similarity is obtained, skip to next category
                if (length(d) == 0) {
                        next
                }
                
                #Consider the n+1 positions also
                e <- d + 1
                
                #combining all positions into one vector
                f <- c(d,e)
                
                #Considering only unique positions in case of repetition and sort the position vector
                f <- unique(f)
                f <- sort(f)
                
                #As per position, assign the values in data frame to new data frame
                temp2 <- temp[f,]
                
                #Print category and values per category
                #(Note: categories without any repeated ProfitRatio will be omitted)
                print(lcat[i])
                print(temp2)
                
        }
        
        #Loop to find companies with same profit ratio range per country
        
        #Similar functioning as previous loop
        for (i in 1:length(lcou)) {
                temp <- a[a$country == lcou[i],]
                if (length(temp[,1]) == 1 | length(temp[,1]) == 0) {next}
                d <- which(temp$profitRatio[1:(length(temp$profitRatio) - 1)] == temp$profitRatio[2:length(temp$profitRatio)])
                if (length(d) == 0) {next}
                e <- d + 1
                f <- c(d,e)
                f <- unique(f)
                f <- sort(f)
                temp3 <- temp[f,]
                print(lcou[i])
                print(temp3)
        }
        
        #Loop to find companies with same profit ratio range per country and category
        
        #Similar functioning as first loop with addition of initial country loop
        for (i in 1:length(lcou)) {
                tempp <- a[a$country == lcou[i],]
                
                for (i in 1:length(lcat)) {
                        temp <- tempp[tempp$category == lcat[i],]
                        if (length(temp[,1]) == 1 | length(temp[,1]) == 0) {next}
                        d <- which(temp$profitRatio[1:(length(temp$profitRatio) - 1)] == temp$profitRatio[2:length(temp$profitRatio)])
                        if (length(d) == 0) {next}
                        e <- d + 1
                        f <- c(d,e)
                        f <- unique(f)
                        f <- sort(f)
                        temp4 <- temp[f,]
                        
                        print(temp4)
                }
        }
        
        
#----------------------End Of Code-----------------------------------------------------
        