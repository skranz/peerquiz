example.peerquiz = function() {
  setwd("D:/libraries/peerquiz")
  file = "budget.yaml"
  app = eventsApp()
  pq = import.yaml(file=file)
  pq = init.peerquiz(pq=pq)
  ui = peerquiz.input.ui(pq)

  app$ui = fluidPage(
    withMathJax(ui)
  )
  viewApp(app)

  #view.html(ui=ui)
}



peerquiz.input.ui = function(pq) {
  restore.point("peerquiz.input.ui")


  ui = tagList(
    p(HTML(pq$question_html)),
    tabsetPanel(type="pills",id = pq$acetabset_id,
      tabPanel(title = "Edit", value = "edit",
        pq[["form.ui"]],
        aceEditor(outputId = pq$ace_id,value = pq$template,mode = "text",showLineNumbers = FALSE,wordWrap = TRUE,height = 12*pq$ace_lines+20)
      ),
      tabPanel(title = "Preview", value = "preview",
        uiOutput(pq$preview_id)
      )
    ),
    actionButton(pq$ace_btn_id,"Ok")
  )
  changeHandler(pq$acetabset_id,pq=pq, function(value, app,pq, ...) {
    if (value=="preview") {
      sol = getInputValue(pq$ace_id)
      if (pq$has.form) {
        res = get.form.values(pq$form, show.alerts=TRUE)
        if (!res$ok) {
          updateTabsetPanel(app$session,pq$acetabset_id,"edit")
          return()
        }
        values = res$values
      } else {
        values = NULL
      }

      restore.point("to.preview.2")
      sol.ui = withMathJax(HTML(sol.source.to.secure.html(sol)))
      if (!is.null(pq$render.answer.fun)) {
        ui = pq$render.answer.fun(pq=pq, values=values, sol=sol, sol.ui=sol.ui)
      } else {
        ui = wellPanel(sol.ui)
      }

      setUI(pq$preview_id, ui)
    }
  })
  ui
}

init.pq.form = function(pq, lang=first.none.null(pq$lang,"en")) {
  restore.point("init.pq.form")

  pq$has.form = !is.null(pq[["fields"]])
  if (!pq$has.form) {
    pq$form = pq$form.ui = NULL
    return(pq)
  }

  pq$form.prefix = paste0(pq$id,"-")


  if (is.null(pq[["inputform"]])) {
    form = pq["fields"]
    form$prefix = pq$form.prefix
    form.ui = form.ui.simple(form=form,add.submit = FALSE,lang=lang, prefix)
  } else {
    form = pq["fields"]
    form$widget.as.character=form$form.control.class=FALSE
    form$prefix =pq$form.prefix
    set.form(form)
    rmd = pq[["inputform"]]
    cr = compile.rmd(text = rmd,out.type = "shiny")
    ui = render.compiled.rmd(cr,out.type = "shiny")
    form.ui = ui
  }

  pq$form = form
  pq$form.ui = form.ui
  pq
}
