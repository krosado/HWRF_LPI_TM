function [jd]=ecomtime(cdf)
% function [jd]=ecomtime(cdf)
% returns Julian time vector from ECOMSI.CDF type file
%
mexcdf('setopts',0);
ncid=mexcdf('open',cdf,'nowrite');
[nam,nt]=mexcdf('diminq',ncid,'time');
base_date=[0 0 0 0 0 0];
base_date(1:3)=mexcdf('attget',ncid,'global','base_date');
t=mexcdf('varget',ncid,'time',0,nt);
jd0=julian(base_date);
jd=jd0+t;
mexcdf('close',ncid);
