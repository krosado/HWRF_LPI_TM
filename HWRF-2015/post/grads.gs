'open grads_nc.ctl'
'define sp=mag(u10m,v10m)'
'define vmax=max(max(sp,x=1,x=260),y=1,y=470)'
*'define vmax=max(max(sp,x=1,x=190),y=1,y=350)'
'd vmax'
result1=sublin(result,1)
temp1=subwrd(result1,4)
time_series=temp1
rc=write('out.txt',time_series,append)
'define pmin=min(min(slp/100,x=1,x=260),y=1,y=470)'
*'define pmin=min(min(slp/100,x=1,x=190),y=1,y=350)'
'd pmin'
result2=sublin(result,1)
temp2=subwrd(result,4)
time_serie=temp2
rc=write('outp.txt',time_serie,append)
