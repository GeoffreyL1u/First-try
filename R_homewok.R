#����1�����л���������ʽ���䡣
m1=matrix(1:9,3,3,byrow=TRUE)
m21=seq(1,3,1)
m22=seq(4,6,1)
m23=seq(7,9,1)
m2=rbind(m22,m21,m23)
det(m2)
det(m1)#��Ȼ�˴�m1������ʽ����m2������ʽ��

#����2������ʽ�����ĳһ�����������ĺͣ���ô
#�������ʽ�͵�����������ʽ�ĺͣ�����������ʽ
#�������⣬����ȡ�
m1=matrix(2:10,3,3,byrow=TRUE)
a=list()
a[[1]]=c(1,3,3)
a[[2]]=c(4,3,4)
a[[3]]=c(2,3,4)
a[[4]]=c(8,9,10)
m2=rbind(a[[3]],a[[1]],a[[4]])
m3=rbind(a[[3]],a[[2]],a[[4]])
if(det(m1)==det(m2)+det(m3))
  print("1")#��Ȼ�������

#����3���Ի�����ʽ�����е�λ�ã�����ʽ���š�
m1=matrix(2:10,3,3,byrow=TRUE)
a=list()
a[[1]]=c(2,3,4)
a[[2]]=c(5,6,7)
a[[3]]=c(8,9,10)
m2=rbind(a[[2]],a[[1]],a[[3]])
if(det(m1)+det(m2)==0)
  print("yes")#��Ȼ���ʳ���

#����4������ʽ��һ�еĹ���ʽ�������ȥ��
m1=matrix(2:10,3,3,byrow=TRUE)
a=list()
a[[1]]=c(2,3,4)
a[[2]]=c(5,6,7)
a[[3]]=c(8,9,10)
a[[4]]=c(20,30,40)
m2=rbind(a[[4]],a[[2]],a[[3]])
if(det(m1)==10*det(m2))
  print("yes")#��֤���

#����5����һ�еı����ӵ���һ�У�����ʽ���䡣
m1=matrix(2:10,3,3,byrow=TRUE)
a=list()
a[[1]]=c(2,3,4)
a[[2]]=c(5,6,7)
a[[3]]=c(8,9,10)
a[[4]]=c(4,6,8)
m2=rbind(a[[1]],a[[2]]+a[[4]],a[[3]])
if(det(m2)==det(m1))
  print("yes")#��֤��ϡ�

#����6���ȣ�A+B��<=�ȣ�A��+�ȣ�B��
m1=matrix(2:10,3,3,byrow=TRUE)
a=list()
a[[1]]=c(1,3,3)
a[[2]]=c(4,3,4)
a[[3]]=c(2,3,4)
a[[4]]=c(8,9,10)
m2=rbind(a[[3]],a[[1]],a[[4]])
m3=rbind(a[[3]],a[[2]],a[[4]])
if(qr(m1)$rank<=qr(m2)$rank+qr(m3)$rank)
  print("yes")#��֤���

#����7����A+B��'=A'+B'
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
  print("yes")#��֤���

#����8��(AB)'=A'B'
m1=matrix(1:9,3,3,byrow=TRUE)
m2=matrix(2:10,3,3,byrow=TRUE)
m3=t(m1%*%m2)
if(m3==t(m1)%*t%(m2))
  print("true")#��֤���

#����9��(kA')=kA'
m1=matrix(1:9,3,3,byrow=TRUE)
if(t(10*m1)==10*t(m1))
  print("true")#��֤���

#����10��AB������ʽ����A������ʽ��B������ʽ�Ļ���
m1=matrix(2:10,3,3,byrow=TRUE)
m2=matrix(2:10,3,3,byrow=TRUE)
m3=m1*m2
m1
m2
m3#����֤������

#����11����AB<=min[�ȣ�A�����ȣ�B��]
m1=matrix(2:2,3,3,byrow=TRUE)
m2=matrix(1:9,3,3,byrow=TRUE)
m3=m1%*%m2
if(qr(m3)$rank<=qr(m1)$rank)
  print("true")
if(qr(m3)$rank<=qr(m2)$rank)
  print("true")#��֤���

#����12��A+B=B+A
m1=matrix(1:9,3,3,byrow=TRUE)
m2=matrix(2:10,3,3,byrow=TRUE)
if(m1+m2==m2+m1)
  print("TRUE")#��֤���

#����13��A��B+C��=AB+AC
m1=matrix(1:9,3,3,byrow=TRUE)
m2=matrix(2:10,3,3,byrow=TRUE)
m3=matrix(3:11,3,3,byrow=TRUE)
if(m1%*%(m2+m3)==(m1%*%m2+m1%*%m3))
  print("TRUE")#��֤���

#����14����˷����ʺϽ�����
m1=matrix(1:9,3,3,byrow=TRUE)
m2=matrix(2:10,3,3,byrow=TRUE)
if(m1%*%m2!=m2%*%m1)
  print("TRUE")#��֤���

#����15���������ʽ����������ȣ�������ʽΪ0
a=list()
a[[1]]=c(1,2,3)
a[[2]]=c(7,8,9)
m2=rbind(a[[1]],a[[1]],a[[2]])
if(det(m2)==0)
  print("yes")#��֤���

#����16��A��ת�õ������A�����ת��
m1=matrix(1:9,3,3,byrow=TRUE)
m2=matrix(2:10,3,3,byrow=TRUE)
m3=m1*m2
t(solve(m3))
solve(t(m3))#�ɼ����ʳ���

#����17��AB�������B�����A����
m1=matrix(1:9,3,3,byrow=TRUE)
m2=matrix(2:10,3,3,byrow=TRUE)
m3=m1*m2
m4=m3*m1
solve(m4%*%m3)
solve(m3)%*%solve(m4)#��֤���

#����18��������������ʽʽ���������ʽ�ĵ���
m1=matrix(1:9,3,3,byrow=TRUE)
m2=matrix(2:10,3,3,byrow=TRUE)
m3=m1*m2
det(solve(m3))*det(m3)#��֤���

#����19�������������
m1=matrix(1:9,3,3,byrow=TRUE)
m2=matrix(2:10,3,3,byrow=TRUE)
m3=m1*m2
solve(m3)
qr(m3)$rank#��֤���

#����20�������ξ�������ʽΪ�����Խ����ϵĳ˻�
a=list()
a[[1]]=c(1,0,0)
a[[2]]=c(2,4,0)
a[[3]]=c(3,4,6)
m1=rbind(a[[1]],a[[2]],a[[3]])
det(m1)





