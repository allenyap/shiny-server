library(shiny)
require(openxlsx)
library(xtable)
require(DT)

#shinyserver
server<-function(input,output){
  
  #產生進貨進貨在Stock_China/Taiwan_Monthly的column數
  purchase<-c();ship<-c()
  for(i in c(1:31)){
    purchase[i]<-2*i-1
    ship[i]<-2*i}
  
  #讀取進出貨的各個表格
  
  
  Stock<-read.xlsx("/home/allen/Documents/Taiwan/Taiwan.xlsx")
  Monthly<-"/home/allen/Documents/Taiwan/Stock_Taiwan_Monthly.xlsx"
  
  
  k<-c()
  for(i in c(1:24)){
    if(length(k)==0){
      
      k[1]<-i;v<-(i+1)/2
      data<-read.xlsx(Monthly,sheet=v)
      begin<-c(which((names(data)=="Number")|(names(data)=="Name")|(names(data)=="Inv0")))
      data<-data[,-begin]
      end<-which(names(data)=="delta")
      if(i==1){acc<-matrix(NA,nrow=length(rownames(Stock)),ncol=12)}
      else{acc<-matrix(acc,nrow=length(rownames(Stock)),ncol=12)}
      acc[,v]<-data[,end]
      data<-data[,c(1:end-1)]
      
      if(i==1){m<-array(NA,dim=c(dim(data)[1],31,24))}
      else{m<-array(m,dim=c(dim(data)[1],31,24))}
      m[,,i]<-as.numeric(as.matrix(data[,purchase]))
      colnames(m)<-c(1:31)
    }
    
    else{
      
      k<-c();v<-i/2
      data<-read.xlsx(Monthly,sheet=v)
      begin<-c(which((names(data)=="Number")|(names(data)=="Name")|(names(data)=="Inv0")))
      data<-data[,-begin]
      end<-which(names(data)=="delta")
      data<-data[,c(1:end-1)]
      
      m<-array(m,dim=c(dim(data)[1],31,24))
      m[,,i]<-as.numeric(as.matrix(data[,ship]))
      colnames(m)<-c(1:31)
    }
  }
  
  output$S0<-renderDT(datatable(Stock,rownames=F,selection="none",options = list(lengthMenu = list(c(10, 20, -1), c('10', '20', 'All')),pageLength = 10)))
  output$tableA<-renderDT(datatable(Stock[grep("^A",Stock$Number),],rownames=F,selection="none",options=list(paging=F)))
  output$tableC<-renderDT(datatable(Stock[grep("^C",Stock$Number),],rownames=F,selection="none",options=list(paging=F)))
  output$tableD<-renderDT(datatable(Stock[grep("^D",Stock$Number),],rownames=F,selection="none",options=list(paging=F)))
  output$tableE<-renderDT(datatable(Stock[grep("^E",Stock$Number),],rownames=F,selection="none",options=list(paging=F)))
  output$tableG<-renderDT(datatable(Stock[grep("^G",Stock$Number),],rownames=F,selection="none",options=list(paging=F)))
  output$tableI<-renderDT(datatable(Stock[grep("^I",Stock$Number),],rownames=F,selection="none",options=list(paging=F)))
  output$tableZ<-renderDT(datatable(Stock[grep("^Z",Stock$Number),],rownames=F,selection="none",options=list(paging=F)))
  
  
  output$inv1<-renderTable(xtable(m[,,1]));output$inv2<-renderTable(xtable(m[,,2]))
  output$inv3<-renderTable(xtable(m[,,3]));output$inv4<-renderTable(xtable(m[,,4]))
  output$inv5<-renderTable(xtable(m[,,5]));output$inv6<-renderTable(xtable(m[,,6]))
  output$inv7<-renderTable(xtable(m[,,7]));output$inv8<-renderTable(xtable(m[,,8]))
  output$inv9<-renderTable(xtable(m[,,9]));output$inv10<-renderTable(xtable(m[,,10]))
  output$inv11<-renderTable(xtable(m[,,11]));output$inv12<-renderTable(xtable(m[,,12]))
  output$inv13<-renderTable(xtable(m[,,13]));output$inv14<-renderTable(xtable(m[,,14]))
  output$inv15<-renderTable(xtable(m[,,15]));output$inv16<-renderTable(xtable(m[,,16]))
  output$inv17<-renderTable(xtable(m[,,17]));output$inv18<-renderTable(xtable(m[,,18]))
  output$inv19<-renderTable(xtable(m[,,19]));output$inv20<-renderTable(xtable(m[,,20]))
  output$inv21<-renderTable(xtable(m[,,21]));output$inv22<-renderTable(xtable(m[,,22]))
  output$inv23<-renderTable(xtable(m[,,23]));output$inv24<-renderTable(xtable(m[,,24]))
  
  output$tableacc<-renderDT({
    #s為初始存貨數量
    s<-as.numeric(Stock[,"S0"])
    #利用條件判斷，月份為2-12月時將存貨變動根據input變數指定月份進行累積加總
    if(input$month>1){
      acc_delta<-rowSums(acc[,c(1:input$month)])
    }
    #當月份為1月時，因為rowSums()的x維度問題，將累積變動設定為0
    else{acc_delta<-0}
    #將初始數量加上累積變動則為指定累積月份之期末存貨
    Stock[,"S0"]<-s+acc_delta
    datatable(Stock,rownames=F,selection="none",options = list(lengthMenu = list(c(10, 20, -1), c('10', '20', 'All')),pageLength = 10))})
  
  output$search_purchase<-renderDT({
    #讀取陣列中奇數的資料
    data<-m[,,2*(as.numeric(input$result_p))-1]
    #將編號以及品名和資料結合
    monthly_result<-data.frame(Stock[,c(1,2)],data)
    #根據input的編號找出商品，將該商品資料放入result變數中
    result<-monthly_result[which(monthly_result[,1]==input$select_p),]
    names(result)<-c("Number","Name",c(1:31))
    #根據input選擇是否只要單一日期
    if(input$dateornot_p)
    {result<-result[,c(1,2,as.numeric(input$date_p)+2)]}
    datatable(result,rownames=F,options=list(paging=F,searching=F))
  })
  
  output$search_ship<-renderDT({
    data<-m[,,2*(as.numeric(input$result_s))]
    monthly_result<-data.frame(Stock[,c(1,2)],data)
    result<-monthly_result[which(monthly_result[,1]==input$select_s),]
    names(result)<-c("Number","Name",c(1:31))
    if(input$dateornot_s)
    {result<-result[,c(1,2,as.numeric(input$date_s)+2)]}
    datatable(result,rownames=F,options=list(paging=F,searching=F))
  })
  S<-matrix(as.numeric(as.matrix(Stock[,c(3,5,6)])),nrow=length(rownames(Stock)),ncol=3)
  SS<-data.frame(Stock[,c(1,2)],S)
  D<-datatable(SS,filter=list(position = 'top', clear = TRUE, plain = FALSE),rownames=F,selection = 'none',options=list(autoWidth=T,columnDefs=list(list(width="100px",targets="_all"))))
  output$table<- renderDT(D,options = list(pageLength = 5))
  
}

Stock<-read.xlsx("/home/allen/Documents/Taiwan/Taiwan.xlsx")
Monthly<-"/home/allen/Documents/Taiwan/Stock_Taiwan_Monthly.xlsx"

ui<-fluidPage(navbarPage("Stock",
                         navbarMenu("Category",
                                    tabPanel("Stock in the very begging",
                                             fluidPage(
                                               DTOutput("S0")
                                             )
                                    ),
                                    tabPanel(("A"),DTOutput("tableA")),
                                    tabPanel(("C"),DTOutput("tableC")),
                                    tabPanel(("D"),DTOutput("tableD")),
                                    tabPanel(("E"),DTOutput("tableE")),
                                    tabPanel(("G"),DTOutput("tableG")),
                                    tabPanel(("I"),DTOutput("tableI")),
                                    tabPanel(("Z"),DTOutput("tableZ"))
                         ),
                         tabPanel(("Acc Stock"),
                                  fluidPage(headerPanel("Acc Stock in each month"),
                                            selectInput("month","月份",choices=as.character(c(1:12)),selected="1")),
                                  DTOutput("tableacc")
                         ),
                         tabPanel(("purchasing goods record"),
                                  fluidPage(headerPanel("Purchasing"),
                                            selectInput("select_p","Number",choices=as.character(Stock[,1],selected=Stock[,1][1])),
                                            selectInput("result_p","MOnth",choices=as.character(c(1:12)),selected="1"),
                                            checkboxInput("dateornot_p",label=c("Date or not?"),value=F),
                                            conditionalPanel(condition="input.dateornot_p==true",
                                                             selectInput("date_p","Date",choices=as.character(c(1:31)),selected="1")
                                            ),
                                            DTOutput("search_purchase")
                                  )
                         ),
                         tabPanel(("shipping goods record"),
                                  fluidPage(headerPanel("shipping"),
                                            selectInput("select_s","Number",choices=as.character(Stock[,1],selected=Stock[,1][1])),
                                            selectInput("result_s","Month",choices=as.character(c(1:12)),selected="1"),
                                            checkboxInput("dateornot_s",label=c("Date or not?"),value=F),
                                            conditionalPanel(condition="input.dateornot_s==true",
                                                             selectInput("date_s","Date",choices=as.character(c(1:31)),selected="1")
                                            ),
                                            DTOutput("search_ship")
                                  )
                         ),
                         navbarMenu("purchasing file",
                                    tabPanel(("Jan"),tableOutput("inv1")),
                                    tabPanel(("Feb"),tableOutput("inv3")),
                                    tabPanel(("Mar"),tableOutput("inv5")),
                                    tabPanel(("Apr"),tableOutput("inv7")),
                                    tabPanel(("May"),tableOutput("inv9")),
                                    tabPanel(("Jun"),tableOutput("inv11")),
                                    tabPanel(("Jul"),tableOutput("inv13")),
                                    tabPanel(("Aug"),tableOutput("inv15")),
                                    tabPanel(("Sep"),tableOutput("inv17")),
                                    tabPanel(("Oct"),tableOutput("inv19")),
                                    tabPanel(("Nov"),tableOutput("inv21")),
                                    tabPanel(("Dec"),tableOutput("inv23"))
                         ),
                         navbarMenu("shipping file",
                                    tabPanel(("Jan"),tableOutput("inv2")),
                                    tabPanel(("Feb"),tableOutput("inv4")),
                                    tabPanel(("Mar"),tableOutput("inv6")),
                                    tabPanel(("Apr"),tableOutput("inv8")),
                                    tabPanel(("May"),tableOutput("inv10")),
                                    tabPanel(("Jun"),tableOutput("inv12")),
                                    tabPanel(("Jul"),tableOutput("inv14")),
                                    tabPanel(("Aug"),tableOutput("inv16")),
                                    tabPanel(("Sep"),tableOutput("inv18")),
                                    tabPanel(("Oct"),tableOutput("inv20")),
                                    tabPanel(("Nov"),tableOutput("inv22")),
                                    tabPanel(("Dec"),tableOutput("inv24"))
                         ),
                         tabPanel(("test"),
                                  fluidPage(
                                    fluidRow(
                                      column(10,DTOutput("table"))                                      )
                                  )
                         )
)
)

shinyApp(ui=ui,server=server)