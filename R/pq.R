example.peerquiz = function() {
  setwd("D:/libraries/peerquiz")
  file = "budget.yaml"
  app = eventsApp()
  pq = import.yaml(file=file)
  pq = init.peerquiz(pq=pq)

  sols = c(list(pq$solution), pq$wrongsols)
  sols = lapply(sols,sol.source.to.secure.html)
  ui = peerquiz.guess.sol.ui(sols, pq=pq)

  ui = peerquiz.input.ui(pq)


  app$ui = fluidPage(
    # Add a CSS class for red text colour
    inlineCSS(list(
      .bgempty = "background: white",
      .bg1 = "background:  #FFD700",
      .bg2 = "background:  #D0D0D0",
      .bg3 = "background: ##CD7F32"
    )),
    withMathJax(ui)
  )
  viewApp()

  #view.html(ui=ui)
}

init.peerquiz = function(yaml=NULL,pq=NULL, id = NULL) {
  restore.point("init.peerquiz")
  if (is.null(pq)) {
    pq = yaml.load(yaml)
  }
  pq$question_html = md2html(pq$question)
  pq$id = paste0("pq_",pq$name)
  pq$ace_id = paste0("ace_",pq$id)
  pq$preview_id = paste0("preview_",pq$id)
  pq$acetabset_id = paste0("acetabset_",pq$id)
  pq$ace_btn_id = paste0("ace_",pq$id)
  pq$sol_div_id = paste0("sol_div_",1:8,"_",pq$id)
  pq$num.sol.click = 2

  pq$cur = new.env()
  if (is.null(pq$ace_lines)) {
    nsol = NROW(sep.lines(pq$solution))
    pq$ace_lines = max(nsol+2, nsol*1.3)
  }

  pq = init.pq.form(pq)
  if (!is.null(pq$funs)) {
    txt = sep.lines(pq$funs)

    code = unlist(find.rmd.chunks(txt, add.code=TRUE)$code)
    if (length(code)>0) {
      call = parse(text=code)
      env = new.env()
      eval(call, env)
      pq = c(pq,as.list(env))
    }
  }


  pq
}

sol.source.to.secure.html = function(sol) {
  restore.point("sol.source.to.secure.html")
  # Properly escape to prevent
  # an XSS attack
  html = htmltools::htmlEscape(sol)
  # Still we want to preserve line breaks
  html = gsub("\n","<br>\n", html, fixed=TRUE)
  html
}

peerquiz.guess.sol.ui = function(sols=cur$sols,pq, cur=pq$cur, num.cols=2) {
  restore.point("peerquiz.input.ui")
  cur$sols = sols

  divs = lapply(seq_along(sols), quiz.sol.div, pq=pq)
  is.left = seq_along(divs)%%2 == 1
  left = divs[is.left]
  right = divs[!is.left]
  if (length(right)<length(left)) right[[length(left)]] = ""

  str = paste0('<tr><td valign="top" style="border: 0px solid #000000">',left,'</td><td valign="top" style="border: 0px solid #000000">',right,"</td></tr>")
  tab = paste0('<table  style="width: 100%; border-collapse:collapse;"><col width="50%"><col width="50%">', paste0(str, collapse="\n"),"</table>")

  ui = withMathJax(HTML(tab))
  ui
}

quiz.sol.div = function(sol.num=1, pq, cur=pq$cur) {
  restore.point("quiz.sol.div")

  sol = cur$sols[[sol.num]]
  id = pq$sol_div_id[[sol.num]]

  ui = div(id = id,style="margin:5px; border: 1px solid #000000; padding:10px;",
    p(HTML(sol))
  )
  #jsclickHandler(id, click.quiz.sol, sol.num=sol.num, pq=pq)
  #jsclickHandler(id, my.fun, sol.num=sol.num, pq=pq)
  as.character(ui)
}

my.fun = function(...) {
  myargs = list(...)
  restore.point("my.fun")
}

click.quiz.sol = function(id,sol.num=NULL,pq=NULL,...) {
  restore.point("click.quiz.sol")
  cur = pq$cur


  if (sol.num %in% cur$sol.clicked) {
    for (i in seq_along(cur$sol.clicked)) {
      rsol.num = cur$sol.clicked[[i]]
      rid = pq$sol_div_id[[rsol.num]]
      shinyjs::removeClass(id = rid, paste0("bg",i))
    }
    cur$sol.clicked = NULL
    return()
  }


  if (length(cur$sol.clicked) >= pq$num.sol.click) {
    for (i in seq_along(cur$sol.clicked)) {
      rsol.num = cur$sol.clicked[[i]]
      rid = pq$sol_div_id[[rsol.num]]
      shinyjs::removeClass(id = rid, paste0("bg",i))
    }
    cur$sol.clicked = sol.num
  } else {
    cur$sol.clicked = c(cur$sol.clicked,sol.num)
  }
  rank = length(cur$sol.clicked)
  shinyjs::addClass(id = id, paste0("bg",rank))

  cat("I was clicked!")
}

