get.pq.dir = function(...) {
  pq.opts()$pq.dir
}

get.pq.userid = function(pq) {
  pq$userid
}

pq.opts = function(app=getApp()) {
  restore.point("pq.opts")
  if (!is.null(app[["pq.opts"]]))
    return(app$pq.opts)

  pq.opts = getOption("peerquiz.options")
  if (is.null(pq.opts)) {
    pq.opts = init.pq.opts()
    set.pq.opts(pq.opts)
  }
  pq.opts
}

set.pq.opts = function(pq.opts = init.pq.opts(), app=getApp()) {
  if (!is.null(app)) {
    app$pq.opts = pq.opts
  }
  options(pq.opts=pq.opts)
  invisible(pq.opts)
}

init.pq.opts = function(pq.dir = file.path(getwd(),"pqa")) {
  nlist(pq.dir)
}

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

load.pq = function(yaml.file=NULL, pq.file = NULL, yaml=NULL,...) {
  restore.point("load.pq")
  if (!is.null(pq.file)) {
    return(readRDS(pq.file))
  }

  pq = import.yaml(file=yaml.file, text=yaml)

  init.peerquiz(pq=pq)
}

init.peerquiz = function(yaml=NULL,pq=NULL, id = NULL, userid="JonDoe", pq.dir = get.pq.dir()) {
  restore.point("init.peerquiz")
  if (is.null(pq)) {
    yaml = merge.lines(yaml)
    pq = yaml.load(yaml)
  }
  pq$question_html = md2html(pq$question)
  pq$id = paste0(pq$name)
  pq$userid = userid
  pq$ns = NS(paste0("pq_",pq$id))
  pq$sol_div_id = paste0("sol_div_",1:8,"_",pq$id)
  pq$num.sol.click = 2
  pq$lang = first.non.null(pq$lang, "en")
  pq$str = pq_strings(pq$lang)

  if (is.null(pq$ace_lines)) {
    pq$ace_lines = 10
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
