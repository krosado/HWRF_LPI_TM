function plothmpitmp(opth,spth,snam,stid,sdat,mphs,n,hlat,hlon,k,i,cl,prtd1)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Author: Richard M. Yablonsky, URI   %%%
%%% Last Update: 12/2/2014              %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

load coast.dat; load conus.dat; load state.dat;
zz=[5 15 25 35 45 55 65 77.5 92.5 110 135 175 250 375 550 775 1100 1550 2100 2800 3700 4850 5500];

mtim=num2str(n,'%4.4i')

fld1='t'
fld2='u'
fld3='v'

if mphs == 'COUPLD'
  phsd='runwrf';
  trkend=n*prtd1*8+1;
else
  phsd=['pom/output/OCEAN/',mphs];
  trkend=1;
end

file=[spth,'/',sdat,'/',upper(stid),'/',phsd,'/',upper(snam),'.',mtim,'.nc']

x=nc_varget(file,'east_e');  x=x';
y=nc_varget(file,'north_e'); y=y';

nc=mexnc('open',file,'nowrite');
  [t xx yy]=horiz_section(file,fld1,1,-zz(k));
  [u xx yy]=horiz_section(file,fld2,1,-zz(k));
  [v xx yy]=horiz_section(file,fld3,1,-zz(k));
mexnc('close',nc);

t(t==0)=nan; t([1,end],:)=nan; t(:,[1,end])=nan;        
u(u==0)=nan; u([1,end],:)=nan; u(:,[1,end])=nan; 
v(v==0)=nan; v([1,end],:)=nan; v(:,[1,end])=nan; 

pcolor(x,y,t); hold on;
shading interp;

plot(hlon(1:2:end),hlat(1:2:end),'k--','LineWidth',1.5);
plot(hlon(1:2:trkend),hlat(1:2:trkend),'ko-','LineWidth',2);
plot(hlon(1:8:trkend),hlat(1:8:trkend),'wo','LineWidth',2);

if i ~= 0
  quiver(x(1:6*i:end,1:6*i:end),y(1:6*i:end,1:6*i:end), ...
         u(1:6*i:end,1:6*i:end),v(1:6*i:end,1:6*i:end),0,'k');
  xlim([hlon(max(trkend-6*(i-1),1))-i*5 hlon(max(trkend-6*(i-1),1))+i*5]);
  ylim([hlat(max(trkend-6*(i-1),1))-i*5 hlat(max(trkend-6*(i-1),1))+i*5]);
  caxis(cl);
  colorbar('location','eastoutside')
else
  xlim([min(min(x)) max(max(x))]);
  ylim([min(min(y)) max(max(y))]);
  caxis([9 31]);
  colorbar('location','southoutside')
end

plot(coast(:,1),coast(:,2),'color','k','LineWidth',1.5);
plot(conus(:,1),conus(:,2),'color','k','LineWidth',1.5);
plot(state(:,1),state(:,2),'color','k','LineWidth',1.5);

xlabel(['longitude']);
ylabel(['latitude']);

ns=num2str(n*prtd1*24); zzs=num2str(zz(k)); is=num2str(i*10);
output=[opth,'/',snam,stid,'.',sdat,'.plothwrftmp.',mphs,'.hour',ns,'.depth',zzs,'m.zoom',is,'.png']
print('-dpng',output); hold off;
