
giveFeedBack <- function(cond, pass_message = "check passed", fail_message = "check failed"){
  if(cond){
    tagList(
      tags$p(pass_message , style = "color:green;")
    )
  } else {
    tagList(
      tags$p(fail_message , style = "color:red;")
    )
  }
}
