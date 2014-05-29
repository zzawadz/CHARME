### Tests for functions which check user input (transition matrix etc)
require(testthat)

################ Transition matrix ###################

# Throws error when rowsums are not equal 1
expect_error({
  x = cbind(c(1,1), c(1,1))
  CHARME:::checkTransMatrix(x)
})

# Thorws error when matrix is not square
expect_error({
  x = cbind(c(1,1))
  CHARME:::checkTransMatrix(x)
})

# Throws error when marix have noetgive entries
expect_error({
  CHARME:::checkTransMatrix(diag(2)-2)
})

# Expect succes when proper matrix is specified
expect_equal({
  x = cbind(diag(2))
  CHARME:::checkTransMatrix(x)
},NULL)



################ Models list ###################

# Models list is ok
expect_equal({
spec1 = garchSpec(model = list(omega=0.10, mu= 0, ar = 0.7))
spec2 = garchSpec(model = list(omega=0.10, mu= 0, ar = 0.1))
spec_list = list(spec1, spec2)

CHARME:::checkModelsList(spec_list)
},NULL)

# models list is not valid - expect error!
expect_error({
  spec1 = garchSpec(model = list(omega=0.10, mu= 0, ar = 0.7))
  spec_list = list(spec1, 1)
  
  CHARME:::checkModelsList(spec_list)
})
