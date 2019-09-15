library(RCurl)
library(XML)

source('util.R')

LATEST_AID <- 1347131 # As of 10/14/2019
AID_SEQ <- 1:LATEST_AID

MIN_TOTAL <- 8000
MIN_RATIO <- 0.05

RESULTS_FILE <- 'results.csv'

PUG_URL <- 'https://pubchem.ncbi.nlm.nih.gov/rest/pug/assay/aid/%s/summary/XML'

for(aid in AID_SEQ){
  
  url <- sprintf(PUG_URL, aid)
  res <- getURL(url)
  doc <- xmlParse(res)
  root <- xmlRoot(doc)
  
  SIDAll <- as.numeric(xmluValue.safe(xmlElementsByTagName(root, 'SIDCountAll', recursive=T)))
  CIDAll <- as.numeric(xmluValue.safe(xmlElementsByTagName(root, 'CIDCountAll', recursive=T)))
  
  if(SIDAll >= MIN_TOTAL){
    SIDActive <- as.numeric(xmluValue.safe(xmlElementsByTagName(root, 'SIDCountActive', recursive=T)))
    SIDInactive <- as.numeric(xmluValue.safe(xmlElementsByTagName(root, 'SIDCountInactive', recursive=T)))
    SIDRatioActive <- SIDActive / SIDAll
    SIDRatioInactive <- SIDInactive / SIDAll
    
    if(SIDRatioActive >= MIN_RATIO && SIDInactive >= MIN_RATIO){
      write.csv(data.frame(AID=aid,Total=CIDAll,Active=CIDActive,Inactive=CIDInactive), file=RESULTS_FILE, append=T)
      print(sprintf('Added AID %s: SIDS: Total(%s) Active(%s) Inactive(%s)', aid, SIDAll, SIDActive, SIDInactive))
    }else{
      print(sprintf('Skipping AID %s: SIDS: Total(%s) Active(%s) Inactive(%s)', aid, SIDAll, SIDActive, SIDInactive))
    }
  }else if(CIDAll >= MIN_TOTAL){
    CIDActive <- as.numeric(xmluValue.safe(xmlElementsByTagName(root, 'CIDCountActive', recursive=T)))
    CIDInactive <- as.numeric(xmluValue.safe(xmlElementsByTagName(root, 'CIDCountInactive', recursive=T)))
    CIDRatioActive <- CIDActive / CIDAll
    CIDRatioInactive <- CIDInactive / CIDAll
    if(CIDRatioActive >= MIN_RATIO && CIDRatioInactive >= MIN_RATIO){
      write.csv(data.frame(AID=aid,Total=CIDAll,Active=CIDActive,Inactive=CIDInactive), file=RESULTS_FILE, append=T) 
      print(sprintf('Added AID %s: CIDS: Total(%s) Active(%s) Inactive(%s)', aid, CIDAll, CIDActive, CIDInactive))
    }else{
      print(sprintf('Skipping AID %s: CIDS: Total(%s) Active(%s) Inactive(%s)', aid, CIDAll, CIDActive, CIDInactive))
    }
  }else{
    print(sprintf('Skipping AID %s: SIDS(%s) CIDS(%s)', aid, SIDAll, CIDAll))  
  }
}

