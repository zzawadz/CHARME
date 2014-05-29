## Tests for functions from Utils.R

########## Tests for createModelsList ############

# expect error for unproper list
expect_error(createModelsList(1,2))

expect_is({
  a = garchSpec()
  b = garchSpec()
  createModelsList(a,b)
}, "list")


########## Tests for createTransMatrix ############

sapply(1:1000, function(i){
expect_equal(
{ 
  prob = runif(1)
  dim = runif(1,min = 1,max = 20)
  CHARME:::checkTransMatrix(createTransMatrix(dim  = dim, prob = prob))
}, NULL)
  return(invisible())
})



