example.peerquiz = function() {
  setwd("D:/libraries/peerquiz")
  file = "budget.yaml"
  pq = load.pq(file)
  adf = load.pq.answers(pq=pq)


  app = eventsApp()

  ui = ans$answer.ui[[1]]
  ans = adf
  pgu = new.pgu(pq=pq, ans=ans)
  set.pgu(pgu)

  ui = peerquiz.guess.sol.ui(ans, pq=pq)

  www.path = system.file("www",package="peerquiz")

  ns = pq$ns
  app$ui = fluidPage(
    singleton(tags$head(includeScript(file.path(www.path,"clickrank.js")))),
    singleton(tags$head(includeCSS(file.path(www.path,"clickrank.css")))),
    withMathJax(ui),
    uiOutput(ns("ranking"))
  )
  appInitHandler(function(...) {
    callJS("newClickRank",id=pq$id,div_ids=pgu$ans.div.id,max_ranked=3)
  })
  eventHandler("clickRankChange",id=pq$id,function(ranked,max_ranked, num_ranked, ...) {
    restore.point("cr.clickRankChange")
    ns = pq$ns
    ranked = unlist(ranked)
    if (length(ranked)>0) {
      ranked = ranked+1
      if (num_ranked == pgu$num.ans-1) {
        ranked = unique(c(ranked,1:pgu$num.ans))
      }
    }
    cat("\nRanking:",paste0(ranked, collapse=", "))
    if (length(ranked)==0) {
      str = pq$str$not_yet_ranked
    } else {
      str = paste0(seq_along(ranked), ": ",pq$str$Answer," ", ranked,collapse="<br>")
    }
    ranking.ui = tagList(
      h4(pq$str$your_ranking,":"),
      p(HTML(str))
    )
    setUI(ns("ranking"), ranking.ui)
  })
  viewApp()

  #view.html(ui=ui)
}

new.pgu = function(pq,ans=NULL,num.ans = NROW(ans),...) {
  pgu = as.environment(list(id=pq$id, ans=ans, num.ans=num.ans, ans.div.id = paste0("ans-div-id-",seq_len(NROW(ans)),"-",pq$id)))
}

set.pgu = function(pgu, app=getApp()) {
  if (is.null(app[["pgu.li"]]))
    app$pgu.li = list()

  app$pgu.li[[pgu$id]] = pgu
}

get.pgu = function(pq=NULL,id = pq$id, app=getApp()){
  if (is.null(app[["pgu.li"]]))
    app$pgu.li = list()
  if (is.null(app$pgu.li[[id]]))
    app$pgu.li[[id]] = new.pgu(pq=pq)
  app$pgu.li[[id]]
}

peerquiz.guess.sol.ui = function(ans=pgu$ans,pq, pgu=get.pgu(pq), num.cols=2) {
  restore.point("peerquiz.guess.sol.ui")
  pgu$ans = ans

  divs = lapply(seq_len(NROW(ans)), quiz.ans.div, pq=pq,pgu=pgu)
  is.left = seq_along(divs)%%2 == 1
  left = divs[is.left]
  right = divs[!is.left]
  if (length(right)<length(left)) right[[length(left)]] = ""

  str = paste0('<tr><td valign="top" style="border: 0px solid #000000">',left,'</td><td valign="top" style="border: 0px solid #000000">',right,"</td></tr>")
  tab = paste0('<table  style="width: 100%; border-collapse:collapse;"><col width="50%"><col width="50%">', paste0(str, collapse="\n"),"</table>")

  ui = withMathJax(tagList(
    HTML(pq$question_html),
    h4(pq$str$proposed_answers),
    HTML(tab)
  ))
  ui
}

quiz.ans.div = function(ans.num=1, pq, pgu=get.pgu(pq)) {
  restore.point("quiz.ans.div")

  ans = pgu$ans[ans.num,]
  id = pgu$ans.div.id[[ans.num]]

  ui = div(id = id,style="margin:5px; border: 1px solid #000000; padding:10px;", class="clickable",
    tags$h4(pq$str$Answer, ans.num),
    ans$answer.ui[[1]]
  )

  as.character(ui)
}
