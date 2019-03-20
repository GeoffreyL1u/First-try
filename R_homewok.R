#性质1：行列互换，行列式不变。
m1=matrix(1:9,3,3,byrow=TRUE)
m21=seq(1,3,1)
m22=seq(4,6,1)
m23=seq(7,9,1)
m2=rbind(m22,m21,m23)
det(m2)
det(m1)#显然此处m1的行列式等于m2的行列式。

#性质2；行列式中如果某一行是两组数的和，那么
#这个行列式就等于两个行列式的和，这两个行列式
#除该行外，都相等。
m1=matrix(2:10,3,3,byrow=TRUE)
a=list()
a[[1]]=c(1,3,3)
a[[2]]=c(4,3,4)
a[[3]]=c(2,3,4)
a[[4]]=c(8,9,10)
m2=rbind(a[[3]],a[[1]],a[[4]])
m3=rbind(a[[3]],a[[2]],a[[4]])
if(det(m1)==det(m2)+det(m3))
  print("1")#显然二者相等

#性质3：对换行列式中两行的位置，行列式反号。
m1=matrix(2:10,3,3,byrow=TRUE)
a=list()
a[[1]]=c(2,3,4)
a[[2]]=c(5,6,7)
a[[3]]=c(8,9,10)
m2=rbind(a[[2]],a[[1]],a[[3]])
if(det(m1)+det(m2)==0)
  print("yes")#显然性质成立

#性质4：行列式中一行的公因式可以提出去。
m1=matrix(2:10,3,3,byrow=TRUE)
a=list()
a[[1]]=c(2,3,4)
a[[2]]=c(5,6,7)
a[[3]]=c(8,9,10)
a[[4]]=c(20,30,40)
m2=rbind(a[[4]],a[[2]],a[[3]])
if(det(m1)==10*det(m2))
  print("yes")#验证完毕

#性质5：把一行的倍数加到另一行，行列式不变。
m1=matrix(2:10,3,3,byrow=TRUE)
a=list()
a[[1]]=c(2,3,4)
a[[2]]=c(5,6,7)
a[[3]]=c(8,9,10)
a[[4]]=c(4,6,8)
m2=rbind(a[[1]],a[[2]]+a[[4]],a[[3]])
if(det(m2)==det(m1))
  print("yes")#验证完毕。

#性质6：秩（A+B）<=秩（A）+秩（B）
m1=matrix(2:10,3,3,byrow=TRUE)
a=list()
a[[1]]=c(1,3,3)
a[[2]]=c(4,3,4)
a[[3]]=c(2,3,4)
a[[4]]=c(8,9,10)
m2=rbind(a[[3]],a[[1]],a[[4]])
m3=rbind(a[[3]],a[[2]],a[[4]])
if(qr(m1)$rank<=qr(m2)$rank+qr(m3)$rank)
  print("yes")#验证完毕

#性质7：（A+B）'=A'+B'
m1=matrix(2:10,3,3,byrow=TRUE)
a=list()
a[[1]]=c(2,1,2)
a[[2]]=c(3,4,5)
a[[3]]=c(8,7,6)
a[[4]]=c(0,2,2)
a[[5]]=c(2,2,2)
a[[6]]=c(0,2,4)
m2=rbind(a[[1]],a[[2]],a[[3]])
m3=rbind(a[[4]],a[[5]],a[[6]])
m11=t(m1)
m22=t(m2)
m33=t(m3)
if(m11==m22+m33)
  print("yes")#验证完毕

#性质8：(AB)'=A'B'
m1=matrix(1:9,3,3,byrow=TRUE)
m2=matrix(2:10,3,3,byrow=TRUE)
m3=t(m1%*%m2)
if(m3==t(m1)%*t%(m2))
  print("true")#验证完毕

#性质9：(kA')=kA'
m1=matrix(1:9,3,3,byrow=TRUE)
if(t(10*m1)==10*t(m1))
  print("true")#验证完毕

#性质10：AB的行列式等于A的行列式与B的行列式的积。
m1=matrix(2:10,3,3,byrow=TRUE)
m2=matrix(2:10,3,3,byrow=TRUE)
m3=m1*m2
m1
m2
m3#可验证得性质

#性质11：秩AB<=min[秩（A），秩（B）]
m1=matrix(2:2,3,3,byrow=TRUE)
m2=matrix(1:9,3,3,byrow=TRUE)
m3=m1%*%m2
if(qr(m3)$rank<=qr(m1)$rank)
  print("true")
if(qr(m3)$rank<=qr(m2)$rank)
  print("true")#验证完毕

#性质12：A+B=B+A
m1=matrix(1:9,3,3,byrow=TRUE)
m2=matrix(2:10,3,3,byrow=TRUE)
if(m1+m2==m2+m1)
  print("TRUE")#验证完毕

#性质13：A（B+C）=AB+AC
m1=matrix(1:9,3,3,byrow=TRUE)
m2=matrix(2:10,3,3,byrow=TRUE)
m3=matrix(3:11,3,3,byrow=TRUE)
if(m1%*%(m2+m3)==(m1%*%m2+m1%*%m3))
  print("TRUE")#验证完毕

#性质14矩阵乘法不适合交换律
m1=matrix(1:9,3,3,byrow=TRUE)
m2=matrix(2:10,3,3,byrow=TRUE)
if(m1%*%m2!=m2%*%m1)
  print("TRUE")#验证完毕

#性质15：如果行列式中有两行相等，则行列式为0
a=list()
a[[1]]=c(1,2,3)
a[[2]]=c(7,8,9)
m2=rbind(a[[1]],a[[1]],a[[2]])
if(det(m2)==0)
  print("yes")#验证完毕

#性质16：A的转置的逆等于A的逆的转置
m1=matrix(1:9,3,3,byrow=TRUE)
m2=matrix(2:10,3,3,byrow=TRUE)
m3=m1*m2
t(solve(m3))
solve(t(m3))#可见性质成立

#性质17：AB的逆等于B的逆乘A的逆
m1=matrix(1:9,3,3,byrow=TRUE)
m2=matrix(2:10,3,3,byrow=TRUE)
m3=m1*m2
m4=m3*m1
solve(m4%*%m3)
solve(m3)%*%solve(m4)#验证完毕

#性质18：矩阵的逆的行列式式矩阵的行列式的倒数
m1=matrix(1:9,3,3,byrow=TRUE)
m2=matrix(2:10,3,3,byrow=TRUE)
m3=m1*m2
det(solve(m3))*det(m3)#验证完毕

#性质19：可逆矩阵满秩
m1=matrix(1:9,3,3,byrow=TRUE)
m2=matrix(2:10,3,3,byrow=TRUE)
m3=m1*m2
solve(m3)
qr(m3)$rank#验证完毕

#性质20：三角形矩阵行列式为其主对角线上的乘积
a=list()
a[[1]]=c(1,0,0)
a[[2]]=c(2,4,0)
a[[3]]=c(3,4,6)
m1=rbind(a[[1]],a[[2]],a[[3]])
det(m1)






