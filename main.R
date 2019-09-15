library(RCurl)
library(XML)

source('util.R')

LATEST_AID <- 1347131 # As of 10/14/2019
AID_SEQ <- 1:LATEST_AID

MIN_TOTAL <- 8000
MIN_FRAC <- 0.05

RESULTS_FILE <- 'results.csv'

PUG_URL <- 'https://pubchem.ncbi.nlm.nih.gov/rest/pug/assay/aid/%s/summary/XML'

if(!file.exists(RESULTS_FILE)) write.table(c('AID','Total','Active','Inactive'), sep=',', row.names=FALSE, col.names=FALSE)

for(aid in AID_SEQ){
  url <- sprintf(PUG_URL, aid)
  res <- getURL(url)
  doc <- xmlParse(res)
  root <- xmlRoot(doc)
  
  SIDAll <- as.numeric(xmluValue.safe(xmlElementsByTagName(root, 'SIDCountAll', recursive=T)))
  SIDActive <- as.numeric(xmluValue.safe(xmlElementsByTagName(root, 'SIDCountActive', recursive=T)))
  SIDInactive <- as.numeric(xmluValue.safe(xmlElementsByTagName(root, 'SIDCountInactive', recursive=T)))
  SIDFracActive <- SIDActive / SIDAll
  SIDFracInactive <- SIDInactive / SIDAll
  
  CIDAll <- as.numeric(xmluValue.safe(xmlElementsByTagName(root, 'CIDCountAll', recursive=T)))
  CIDActive <- as.numeric(xmluValue.safe(xmlElementsByTagName(root, 'CIDCountActive', recursive=T)))
  CIDInactive <- as.numeric(xmluValue.safe(xmlElementsByTagName(root, 'CIDCountInactive', recursive=T)))
  CIDFracActive <- CIDActive / CIDAll
  CIDFracInactive <- CIDInactive / CIDAll
  
  if(SIDAll >= MIN_TOTAL){
    if(SIDFracActive >= MIN_FRAC && SIDInactive >= MIN_FRAC){
      write.table(data.frame(AID=aid,Total=CIDAll,Active=CIDActive,Inactive=CIDInactive), file=RESULTS_FILE, append=T, sep=',', row.names=FALSE, col.names=FALSE)
      print(sprintf('Added AID %s: SIDs: Total(%s) Active(%s) Inactive(%s)', aid, SIDAll, SIDActive, SIDInactive))
    }else{
      print(sprintf('Skipping AID %s: SIDs: Total(%s) Active(%s) Inactive(%s)', aid, SIDAll, SIDActive, SIDInactive))
    }
  }else if(CIDAll >= MIN_TOTAL){
    if(CIDFracActive >= MIN_FRAC && CIDFracInactive >= MIN_FRAC){
      write.table(data.frame(AID=aid,Total=CIDAll,Active=CIDActive,Inactive=CIDInactive), file=RESULTS_FILE, append=T, sep=',', row.names=FALSE, col.names=FALSE)
      print(sprintf('Added AID %s: CIDs: Total(%s) Active(%s) Inactive(%s)', aid, CIDAll, CIDActive, CIDInactive))
    }else{
      print(sprintf('Skipping AID %s: CIDs: Total(%s) Active(%s) Inactive(%s)', aid, CIDAll, CIDActive, CIDInactive))
    }
  }else{
    print(sprintf('Skipping AID %s: SIDs(%s) CIDs(%s)', aid, SIDAll, CIDAll))  
  }
}

