function [w,x,y]=w100(cdf,itime);
% W100 calculates bottom velocity at 1 m above bottom from ECOM.cdf files
%     Method: law-of-the-wall profile is assumed
%function [w,x,y]=w100(cdf,itime);
%
[sigma]=mcvgt(cdf,'sigma');
n=length(sigma);
[w,x,y]=ksliceuv(cdf,itime,n-1);
[cd,x,y]=kslice(cdf,'cd',itime);
[depth,x,y]=kslice(cdf,'depth');
zr=depth*.5*(sigma(n-1)-sigma(n));
ustar = sqrt(cd).*abs(w);
z0 = cdtoz0(cd,zr);
ur = (ustar/0.4).*log(ones(size(zr))./z0);
w=w.*(ur)./(abs(w)+eps);
ind=find(depth==-99999.);
w(ind)=w(ind)*nan;
