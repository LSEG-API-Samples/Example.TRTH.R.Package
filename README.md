# Example-RTH-R-Package
This example package bundles together code from [Tick History in R language](https://developers.refinitiv.com/en/article-catalog/article/tick-history-in-r-language-part-1) with several improvements.

## Store session token
The user simply call the `login` once and the package will store the session token for all other request.
```r
login("dss_username","dss_password")
```

## Historical Search and Historical Chain Resolution
Search historical instrument and search list of identifier constituents of a chain ric.
```r
historical_search("TRI.N","1996-01-01","2018-01-01",identifier_type = "Ric",results_by = "Entity")
historical_chain("0#.DJI","1996-01-01","2018-01-01")
```

## Standard Extraction
As described in the [article part 2](https://developers.refinitiv.com/en/article-catalog/article/tick-history-in-r-language-part-2)
But unlike the article, the user can specify how many pages they wish to request.
```r
user_packages() #One page
user_packages(3) #Three pages
user_packages("all") #All pages
```

## On-demand Extraction
Include Time and Sale, Market Depth, Intraday, and Raw extractions.
The package will automatically check and retrieve the report.
If not specified, the report will be saved on temp folder.
```r
fid <- c("Trade - Price","Trade - Volume","Trade - Exchange Time")
cond <- tas_condition("Range","2018-09-18","2018-09-19")
a <- tas_request("TRI.N",fid,cond)
b <- read.csv(a)
```
