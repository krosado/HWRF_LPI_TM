function [vel,height,heightlim] = profvel(file,itime,i,j)
%
% profvel:         computes a velocity profile at cell (i,j) from a 
%                  *.nc file from pom2k
%
% Usage: [vel,height,heightlim] = profvel(file,itime,i,j)
%
% where: vel ..... complex 2-D section of horizontal velocity vectors, with 
%                  components in east and north
%        height .. vertical coordinate
%        heightlim . limits of height for plotting
%
%        file .... the name of the netCDF file
%        itime ... the time index (first output = 1, etc.)
%        i ....... i-index of cell
%        j ....... j-index of cell
%
% Acknowledgement: this function draws heavily on the "omviz" package
% written by Rich Signell (USGS)
%
% Initial version, JRH 11/12/2001
% Sign of rot corrected, JRH 19/12/2001
%
if (nargin ~= 4 | nargout ~= 3)
  help profvel;
  return
end
%
% Turn off warnings from netCDF:
%
mexnc('setopts',0);
%
% Open netCDF file:
%
ncid=mexnc('open',file,'nowrite');
%
if(ncid==-1)
  disp(['File ' file ' not found'])
  return
end
%
[name,nx]=mexnc('diminq',ncid,'x');
[name,ny]=mexnc('diminq',ncid,'y');
[name,nz]=mexnc('diminq',ncid,'z');
%
% Get rotation angle:
%
rot=mexnc('varget',ncid,'rot',[j-1 i-1],[1 1],1);
%
% Get depth and surface elevation:
%
h=mexnc('varget',ncid,'h',[j-1 i-1],[1 1],1);
elb=mexnc('varget',ncid,'elb',[(itime-1) j-1 i-1],[1 1 1],1);
%
% Get sigma:
%
sigma=mexnc('varget',ncid,'zz',[0],[nz],1);
%
% Get profile at given time level:
%
if i == 1
%
  u=mexnc('varget',ncid,'u',[itime-1 0 j-1 1],[1 nz-1 1 1],1);
%  
elseif i == nx
%
  u=mexnc('varget',ncid,'u',[itime-1 0 j-1 nx-1],[1 nz-1 1 1],1);
%
else
%
  u=mexnc('varget',ncid,'u',[itime-1 0 j-1 i-1],[1 nz-1 1 2],1);
  u=(u(1,1,:)+u(2,1,:))/2;
%
end
%
if j == 1
%
  v=mexnc('varget',ncid,'v',[itime-1 0 j i-1],[1 nz-1 1 1],1);
%  
elseif j == ny
%
  v=mexnc('varget',ncid,'v',[itime-1 0 ny-1 i-1],[1 nz-1 1 1],1);
%
else
%
  v=mexnc('varget',ncid,'v',[itime-1 0 j-1 i-1],[1 nz-1 2 1],1);
  v=(v(1,1,:)+v(1,2,:))/2;
%
end
%
% Generate height:
%
height=elb+sigma*(h+elb);
height=reshape(height(1,1:nz-1),nz-1,1);
heightlim=[-h ; elb];
%
% Rotate into east/north components:
%
uv=u+sqrt(-1)*v;
vel=uv.*exp(sqrt(-1)*rot);
vel=reshape(vel,nz-1,1);
%
% Close file:
%
mexnc('close',ncid);
