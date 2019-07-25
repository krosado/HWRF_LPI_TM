function [hlat,hlon]=readhwrftrak(opth,snam,stid,sdat)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Author: Richard M. Yablonsky, URI   %%%
%%% Last Update: 10/10/2014             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

hwrff=[opth,'/',snam,stid,'.',sdat,'.trak.hwrf.3hourly.dat']
hwrfall=csvread(hwrff);
hwrffhr=hwrfall(:,6);
hwrflat=hwrfall(:,7)/10;
hwrflon=-hwrfall(:,8)/10;

if stid(3) == 's' || stid(3) == 'p' || stid(3) == 'x' || stid(3) == 'u'
  hwrflat=-hwrflat; % Southern Hemisphere
end

if stid(3) == 'w' || stid(3) == 'a' || stid(3) == 's' || stid(3) == 'p' || stid(3) == 'o' || stid(3) == 'b' || stid(3) == 'u' || stid(3) == 't'
  hwrflon=-hwrflon; % Eastern Hemisphere
end

hlat=hwrflat(hwrffhr(1:end-1)~=hwrffhr(2:end));
asiz=size(hlat);
hlat(asiz+1)=hwrflat(end);

hlon=hwrflon(hwrffhr(1:end-1)~=hwrffhr(2:end));
asiz=size(hlon);
hlon(asiz+1)=hwrflon(end);
