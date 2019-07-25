%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Run HWRF/MPIPOMTC ocean diagnostics %%%
%%% Author: Richard M. Yablonsky, URI   %%%
%%% Last Update: 12/2/2014              %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clear all; close all; clc;
mpth='/lfs3/projects/hur-uri/Richard.Yablonsky/MPI_POMTC_20140930/sorc/pomtc/ocean_diag/matlab' % Current directory
opth=[mpth,'/pngs']                                                                             % Path for diagnostic output
%spth='/lfs3/projects/hur-uri/Richard.Yablonsky/pytmp/MPI_POMTC_20140930'                        % Path to HWRF run $(WORKhwrf)
spth='/lfs1/projects/dtc-hurr/Ligia.Bernardet/pytmp/rocHWRF'                                    % Path to HWRF run $(WORKhwrf)
snam='edouard'                                                                                  % Storm name
stid='06l'                                                                                      % Storm ID & basin identifier
sdat='2014091218'                                                                               % Storm cycle (YYYYMMDDHH)
prtd1=1                                           
addpath(genpath(mpth));

%%%% Load HWRF/MPIPOMTC forecast track %%%%
% Make sure to run prephmpitrak.sh first! %
[hlat,hlon]=readhwrftrak(opth,snam,stid,sdat);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Plot ocean temperature and current vectors %%%

k=1           % Depth level (choose 1-23; corresponds to "zz" depths in plothmpitmp.m
cl=[22 30]    % Color bar limits (in deg C)
mphs='COUPLD' % Ocean model phase (COUPLD, PHASE1, PHASE2)

for n=0:1/prtd1:5/prtd1 % Note run lengths (days): COUPLD = 5, PHASE1 = 2, PHASE2 = 3
  for i=0:4
    plothmpitmp(opth,spth,snam,stid,sdat,mphs,n,hlat,hlon,k,i,cl,prtd1)
  end
end

k=8           % Depth level (choose 1-23; corresponds to "zz" depths in plothmpitmp.m
cl=[15 29]    % Color bar limits (in deg C)
mphs='COUPLD' % Ocean model phase (COUPLD, PHASE1, PHASE2)

for n=0:1/prtd1:5/prtd1 % Note run lengths (days): COUPLD = 5, PHASE1 = 2, PHASE2 = 3
  for i=0:4
    plothmpitmp(opth,spth,snam,stid,sdat,mphs,n,hlat,hlon,k,i,cl,prtd1)
  end
end
