
#' Iterative Proportional fitting - Generate sample integers and weights using spatial microsim book
#'
#' @param x 
#'
#' @return xint
#' @export
#'
#' @examples
int_trs <- function(x){
  # For generalisation purpose, x becomes a vector
  xv <- as.vector(x) # allows trs to work on matrices
  xint <- floor(xv) # integer part of the weight
  r <- xv - xint # decimal part of the weight
  def <- round(sum(r)) # the deficit population
  # the weights be 'topped up' (+ 1 applied)
  topup <- sample(length(x), size = def, prob = r)
  xint[topup] <- xint[topup] + 1
  dim(xint) <- dim(x)
  dimnames(xint) <- dimnames(x)
  xint
}


#' Iterative Proportional fitting - prepare cons matrix of census tables for use in ipf process
#'
#' @param cons 
#'
#' @return 
#' @export
#'
#' @examples
ipf_consprep <- function(cons) {
  # check internal consistency of constraints
  check <-cons %>% 
    mutate(label = gsub("[$]", "", label),
           label = gsub("ndnslabel", "",label),
           label = gsub("ndns", "", label)) %>% 
    filter(sex!="all") %>% 
    separate(label, into=c("sex","agelower","ageupper","nssec"), sep="_")
  
  cons1 <- cons %>% filter(sex!="all")
  sum(cons1$Sheffield)
  cons2 <- cons %>% filter(sex=="all")
  sum(cons2$Sheffield)
  
  # label and transpose constraint
  constraint <- cons$label # Define the category names, these will be the constraints within the ipf
  length <- length(names(cons))
  cons <- t(cons[,5:length]) # Select data with number of participants in each constraint
  colnames(cons)<-constraint # Add label to the numbers in each constraint
  colnames(cons)
  colnames(cons) <- sub("[$]", "", colnames(cons))
  colnames(cons) <- sub("ndnslabel", "", colnames(cons))
  colnames(cons) <- sub("ndnsethgrp5", "", colnames(cons))
  return(cons)
}

#' Iterative Proportional fitting - prepare ind matrix of ndns labels for use in ipf process
#'
#' @param NDNS
#'
#' @return ind 
#' ind is a matrix of 0-1 binary indicators of the individual characteristics that will be contrained in the ipf process
#' @export
#'
#' @examples
ipf_ndnsprep <- function(NDNS) {
  
  ndns <- NDNS %>% 
    dplyr::select(nssec8, ageband, Sex, ethgrp5) %>% 
    drop_na() %>% 
    mutate(ethgrp5 = as.factor(ethgrp5),
           # nssec8 = as.character(nssec8),
           # Sex = as.character(Sex),
           # ageband = as.character(ageband),
      label = paste(Sex, ageband, nssec8, sep="_"))
  
  # Generate binary matrix of the individual characteristics
  
  # AGE SEX NSSEC
  cat_label <- model.matrix(~ ndns$label - 1)
  
  # ETHNICITY
  cat_eth <- model.matrix(~ ndns$ethgrp5 - 1)
  
  # QUALIFICATIONS
#  cat_qual <- model.matrix(~ NDNS$qual - 1)
  
  # Bind all matrices together
#  ind <- cbind(cat_label , cat_eth , cat_qual)
  ind <- cbind(cat_label ,cat_eth )
  colnames(ind) <- sub("[$]", "", colnames(ind))
  colnames(ind) <- sub("ndnslabel", "", colnames(ind))
  colnames(ind) <- sub("ndnsethgrp5", "", colnames(ind))
  colnames(ind)
  return(ind)
}

#' Iterative Proportional fitting - run the ipf process
#'
#' @param NDNS cons
#'
#' @return weights
#' iweights is a matrix of sample weights for the NDNS dataset to run different national and local populations
#' @export
#'
#' @examples
ipf_run <- function(NDNS, cons, ind) {
  rowSums(ind) #Sum of the rows
  dim(ind)     # these should match the sample size and number of categories
  ind_cat <- as.data.frame(ind)
  ind_cat <- ind_cat[ , order(names(ind_cat))]
  cons <- data.frame(cons)
  cons <- cons[ , order(names(cons))]
  colnames(cons) == names(ind_cat)
  colnames(cons)
  colnames(ind_cat)
  names(ind_cat) == colnames(cons)
  colnames(cons) == names(ind_cat)
  
  ind_agg <- colSums(ind)  # Sum the columns to see how many individuals fall in each category. 
  notes_table <- rbind(cons[1:2,] , ind_agg) # View distributions across two populations. This can be used for error checking.
  
  n_ind <- nrow(NDNS) # Set up the parameters needed in the ipfp function. This sets how many individuals are in data set.
  cons <- apply(cons, 2, as.numeric)  # The object needs to be numeric within the procedure
  ind_t <- t(ind) # Need to transpose ind for the function to work
  x0 <- rep(1, n_ind) # create an initial vector of 1's for ipfp function
  weights <- matrix(data = 1, nrow = nrow(NDNS), ncol = nrow(NDNS)) # create a matrix to store the ipfp weights
  
  
  # The Iterative Proportional fitting procedure
  
  weights <- apply(cons, MARGIN = 1, FUN = 
                     function(x) ipfp(x, ind_t, x0, maxit = 500, v=T)) # apply the method multiple times for the different zones (MARGIN tells R to iterate through the rows in cons, rather than the columns)
  
  # Check that the weights imply similar number of people in each constraint
  
  # ind_agg <- t(apply(weights, 2, function(x) colSums(x * ind))) # Sum the number of people in each group post-procedure
  # colnames(ind_agg) <- colnames(cons) # make the column names equal
  # notes_table<-rbind(notes_table, ind_agg[1 , ])  # Bind together the weights with with the census data to evaluate fit
  
  return(weights)
}

#' Iterative Proportional fitting 
#'
#' @param NDNS cons
#'
#' @return weights
#' iweights is a matrix of sample weights for the NDNS dataset to run different national and local populations
#' @export
#'
#' @examples
#' 
ipf_weights <- function(census_constraints, NDNS) {
  cons <- ipf_consprep(census_contraints)
  ind <- ipf_ndnsprep(NDNS)
  weights <- ipf_run(NDNS, cons, ind)
  colnames(weights)<-colnames(census_contraints[5:354])
  return(weights)
}
