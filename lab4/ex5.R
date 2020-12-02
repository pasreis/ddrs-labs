# kleinrock approximation
C=256000
lp=1000
l = c(215,64,128,128)
mi = C/lp
w1 = 1/(mi-l[1])+1/(mi-l[1])+1/(mi-l[1])
w2 = 1/(mi-(l[2]+l[3]))+1/(mi-(l[2]+l[3]))
w3 = 1/(mi-(l[2]+l[3]))+1/(mi-(l[2]+l[3]))+1/(mi-(l[2]))
w4 = 1/(mi-l[4])
w = sum(l[1]/sum(l)*w1,l[2]/sum(l)*w2,l[3]/sum(l)*w3,l[4]/sum(l)*w4)
print(paste("W1=",w1*1000,"ms"))
print(paste("W2=",w2*1000,"ms"))
print(paste("W3=",w3*1000,"ms"))
print(paste("W4=",w4*1000,"ms"))
print(paste("W=",w*1000,"ms"))
