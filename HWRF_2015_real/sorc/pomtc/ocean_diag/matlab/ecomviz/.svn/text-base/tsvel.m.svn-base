function [w,jd,z]=tsvel(cdf,sta,z);
% TSVEL Reads velocity time-series from TSEPIC style files.
% 
% USAGE:
% [w,jd,z]=tsvel(cdf,sta);    % gets velocity at all levels
%
% [w,jd]=tsvel(cdf,sta,depths);    % gets velocity at specified z values
% i.e. [w,jd]=tsvel(cdf,sta,[-2 -10 -24]);
%
[t]=mcvgt(cdf,'time');
nt=length(t);
%
[stations]=mcvgt(cdf,'stations');
nsta=length(stations);
%
[sigma]=mcvgt(cdf,'sigma');
nsigma=length(sigma);
sigma(nsigma+1)=-1;
%  velocity exists at center of two sigma surfaces, so average
sigma=0.5*(sigma(1:nsigma)+sigma(2:nsigma+1));
if(sta>nsta),
  disp('invalid station');
  return
end
%
corner=[0 0 0 sta-1];
edges=[nt, nsigma, 1, 1];
u=mcvgt(cdf,'u',corner,edges);
v=mcvgt(cdf,'v',corner,edges);
w=u+i*v;
[m,n]=size(w);
% if w is 2d, we need to fliplr, since data is stored with 
%  depth increasing upward in tsepic.cdf
%  (in tsepic.cdf, index 0 = sea bed!)
if(m~=1),
  w=fliplr(w);
end
depth=mcvgt(cdf,'depth');
depth=depth(sta)*sigma;
if(nargin>2),
  m=length(z);
  if(min(z)<min(depth)), disp('requested level below data!'),return,end
  if(max(z)>max(depth)), disp('requested level above data!'),return,end
  for k=1:m,
    lev2=max(find(depth>z(k)));
    lev1=lev2+1;
    frac=(z(k)-depth(lev1))/(depth(lev2)-depth(lev1));
    wmod(:,k)=w(:,lev1)+frac*(w(:,lev2)-w(:,lev1));
  end
  w=wmod;
else
  z=depth;
end
%
base_date=zeros(1,6);
base_date(1:3)=mcagt(cdf,'global','base_date');
jd0=julian(base_date);
jd=jd0+t/(3600*24);
