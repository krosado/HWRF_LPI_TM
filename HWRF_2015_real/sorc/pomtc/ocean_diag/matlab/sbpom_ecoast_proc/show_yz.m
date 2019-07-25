clear, close('all')
% Shows vertical sections of a 3-D fields along j-index
% Original y-sigma coordinates are interpolated into y-z
file='../out/ecoast.0010.nc'; % path and name of the netCDF file
isec=281; % j-index along which the section is taken

%% Read data
nc=mexnc('open',file,'nowrite');
h=mexnc('varget',nc,'h',[0 0],[-1 -1],1);
mexnc('close',nc);
[T,x,z]=isection(file,'t',1,isec);
[S,x,z]=isection(file,'s',1,isec);
[V,x,z]=isection(file,'v',1,isec);
[U,x,z]=isection(file,'u',1,isec);

%% Figure
[nx,nz]=size(x);
x=x(1:nx-1,1:nz-1)/1000;
z=z(1:nx-1,1:nz-1);

figure
contourf(x,z,T);colorbar
xlabel('X-direction (km)')
ylabel('Depth (m)')

figure
contourf(x,z,S);colorbar
xlabel('X-direction (km)')
ylabel('Depth (m)')

figure
contourf(x,z,U);colorbar
xlabel('X-direction (km)')
ylabel('Depth (m)')

figure
contourf(x,z,V);colorbar
xlabel('X-direction (km)')
ylabel('Depth (m)')
