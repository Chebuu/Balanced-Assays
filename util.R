xmluValue.safe <- function(xmlElement, default=FALSE){
  isSingular <- length(xmlElement) == 1
  hasChildren <- ifelse(isSingular, length(xmlChildren(xmlElement[[1]])) > 1, FALSE)
  OK <- isSingular && !hasChildren
  ifelse(OK, xmlValue(xmlElement[[1]]), default)
}

