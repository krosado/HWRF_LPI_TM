function [d,x,y,i,j,xx,yy,h]=readgrid(modgridfile);
%  READGRID reads the ECOM "model_grid" file
%
%  Usage:  [d,x,y]=readgrid(modgridfile);
%  Usage:  [d,x,y,i,j,xx,yy]=readgrid(modgridfile);
%
%  where d = depth grid array
%        x = array of x locations of grid centers
%        y = array of y locations of grid centers
%
%        i,j,xx,yy,h = column vectors of i,j,x,y,h from model_grid
%
%  Examples:   [d,x,y]=readgrid('model_grid'); 
 
fid=fopen(modgridfile);

% read two comments lines at beginning of model_grid

titl=fgetl(fid);
comment2=fgetl(fid);

nsigma=fscanf(fid,'%d',1); %number of sigma layers
sigma=fscanf(fid,'%f',nsigma); %number of sigma layers
% I don't know why I need the following line, but I seem
% to need it:
crap=fgetl(fid);

comment3=fgetl(fid);  %comment line

%read grid size
n=fscanf(fid,'%d',2);
im=n(1);
jm=n(2);

nt=im*jm;

data=fscanf(fid,'%f',[10,nt]);
data=data';
fclose(fid);

land=-99999;

i=data(:,1);
j=data(:,2);
h=data(:,5);
xx=data(:,9);
yy=data(:,10);

for k=1:nt;
  ii=i(k);
  jj=j(k);
  d(ii,jj)=h(k);
  x(ii,jj)=xx(k);
  y(ii,jj)=yy(k);
end
ind=find(d==-99999.);
d(ind)=d(ind)*nan;           
