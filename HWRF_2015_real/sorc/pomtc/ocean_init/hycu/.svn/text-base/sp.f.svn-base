      subroutine sp(x,y,r,z,kk,jj,kkm,jjm)
      dimension x(kkm),y(kkm),r(jjm),z(jjm),
     *p(500),pm(500),h(500),a(500),b(500),c(500),d(500),
     *v(500),q(500),r1(500)
      do 200 k=2,kk
  200 h(k)=x(k)-x(k-1)
      pm(1)=0.
      pm(kk)=0.
      k1=kk-1
      do 201 k=2,k1
      c1=h(k)
      c2=h(k+1)
      b(k)=2.
      c(k)=c2/(c1+c2)
      a(k)=1-c(k)
  201 d(k)=6.*((y(k+1)-y(k))/c2-(y(k)-y(k-1))/c1)/(c1+c2)
c
      q(1)=0.
      v(1)=0.
      do 202 k=2,k1
      p(k)=a(k)*q(k-1)+b(k)
      q(k)=-c(k)/p(k)
      v(k)=(d(k)-a(k)*v(k-1))/p(k)
  202 continue
c
      do 203 k=2,k1
      i=kk+1-k
  203 pm(i)=q(i)*pm(i+1)+v(i)
      k=2
c
      do 204 j=1,jj
      e=z(j)
      f=abs(e-x(k-1))
      g=abs(x(k)-x(k-1))*1.e-6
      if(f.lt.g) goto 207
  205 if((e-x(k-1))*(e-x(k))) 207,207,206
  206 if(k.eq.kk) goto 207
      k=k+1
      goto 205
c
  207 a1=pm(k-1)
      a2=pm(k)
      b1=y(k-1)
      b2=y(k)
      c1=x(k)-e
      c2=e-x(k-1)
      c3=h(k)
c
      r(j)=(a1*(c1**3)+a2*(c2**3)+(b1*6.-a1*c3*c3)
     **c1+(b2*6.-a2*c3*c3)*c2)/(6.*c3)
      r1(j)=(-a1*c1*c1*0.5+a2*c2*c2*0.5+b2-b1)/c3-
     *(a2-a1)*c3/6.
  204 continue
      return
      end
