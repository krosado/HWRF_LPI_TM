function plotssttimeseries(opth,spth,snam,stid,sdat,n)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Author: Richard M. Yablonsky, URI   %%%
%%% Last Update: 12/5/2014              %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

wrad=num2str(n,'%3.3i')
phsd='runwrf';
file=[spth,'/',sdat,'/',upper(stid),'/',phsd,'/sst',wrad,'km.dat']
%%file=[spth,'/sst',wrad,'km.dat'] % For development only
tfile=load(file);

fhr=tfile(:,1);
avgsst=tfile(:,4);
minsst=tfile(:,5);
maxsst=tfile(:,6);
avgsstd1=tfile(:,7);
minsstd1=tfile(:,8);
maxsstd1=tfile(:,9);
avgsstd2=tfile(:,10);
minsstd2=tfile(:,11);
maxsstd2=tfile(:,12);

figure(1)
hold on;
plot(fhr,avgsst,'g-','LineWidth',2)
plot(fhr,minsst,'b-','LineWidth',2)
plot(fhr,maxsst,'r-','LineWidth',2)
xlim([0 126])
ylim([21 31])
set(gca,'XTick',[0:12:126])
set(gca,'YTick',[21:1:31])
xlabel(['Forecast Hour']);
ylabel(['SST (\circC) within ',wrad,' km radius of storm center']);
legend('AVG SST','MIN SST','MAX SST',3)
output=[opth,'/sst.',wrad,'km.png']
print('-dpng',output); hold off;

figure(2)
hold on;
plot(fhr,avgsstd1,'g-','LineWidth',2)
plot(fhr,minsstd1,'b-','LineWidth',2)
plot(fhr,maxsstd1,'r-','LineWidth',2)
xlim([0 126])
ylim([-5 5])
set(gca,'XTick',[0:12:126])
set(gca,'YTick',[-5:1:5])
xlabel(['Forecast Hour']);
ylabel(['SST difference from phase 1 (\circC) within ',wrad,' km radius of storm center']);
legend('AVG SSTD1','MIN SSTD1','MAX SSTD1',2)
output=[opth,'/sstd1.',wrad,'km.png']
print('-dpng',output); hold off;

figure(3)
hold on;
plot(fhr,avgsstd2,'g-','LineWidth',2)
plot(fhr,minsstd2,'b-','LineWidth',2)
plot(fhr,maxsstd2,'r-','LineWidth',2)
xlim([0 126])
ylim([-5 5])
set(gca,'XTick',[0:12:126])
set(gca,'YTick',[-5:1:5])
xlabel(['Forecast Hour']);
ylabel(['SST difference from phase 2 (\circC) within ',wrad,' km radius of storm center']);
legend('AVG SSTD2','MIN SSTD2','MAX SSTD2',2)
output=[opth,'/sstd2.',wrad,'km.png']
print('-dpng',output); hold off;

close all;
