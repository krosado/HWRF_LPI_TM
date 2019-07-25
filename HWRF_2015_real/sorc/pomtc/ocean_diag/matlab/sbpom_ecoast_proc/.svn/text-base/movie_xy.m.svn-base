clear, close('all')
% Movie of a horizontal section
id='ecoast'; % run in
field1='t'; %  name of the first netCDF variable
                %  (elb, rho, t, s, w, wr, clock, counter)
field2='vel'; %  name of the second netCDF variable
              %  (tran, vel)
nsub=8; % number to subsample for quiver
depth=-10; % depth (negative) relative to surface (meters)
maxvel=.2; % maximum value for field2


%% Output files
filelist=dir(['../out/',id,'.0*']);


% Model grid
file=['../out/',filelist(1).name];
mexnc('setopts',0); % turn off warnings from netcdf
nc=mexnc('open',file,'nowrite');
z=mexnc('varget',nc,'z',0,-1,1);
h=mexnc('varget',nc,'h',[0,0],[-1,-1],1);
dx=mexnc('varget',nc,'dx',[0,0],[-1,-1],1);
dy=mexnc('varget',nc,'dy',[0,0],[-1,-1],1);
fsm=mexnc('varget',nc,'fsm',[0,0],[-1,-1],1);
th=mexnc('varget',nc,'rot',[0,0],[-1,-1],1);
mexnc('close',nc);
x=cumsum(dx)/1e3;
y=cumsum(dy,2)/1e3;


%% Read data
for m=2:length(filelist)
    file=['../out/',filelist(m).name];
    disp(file)
    nc=mexnc('open',file,'nowrite');
    time=mexnc('varget',nc,'time',0,-1);
    % filed 1
    switch field1
        case {'elb'}
            s=mexnc('varget',nc,field1,[0,0,0],[-1,-1,-1],1);
            s(fsm==0)=nan;
        case {'rho','t','s','w','wr','u','v'}
            [s,xx,yy]=horiz_section(file,field1,1,depth);
            clear('xx','yy')
    end
    % field2
    switch field2
        case {'tran'}
            u=mexnc('varget',nc,'uab',[0 0 0],[-1 -1 -1],1);
            v=mexnc('varget',nc,'vab',[0 0 0],[-1 -1 -1],1);
            u(fsm==0)=nan;
            v(fsm==0)=nan;
            w=u+1i*v;
        case {'vel'}
            [w,xx,yy]=horiz_sectionvel(file,1,depth);
            clear('xx','yy')
    end
    mexnc('close',nc);

    % exclude boundary values   
    s([1,end],:)=nan;
    s(:,[1,end])=nan;

    % figure
    pslice(x,y,s);
    hold on
    xlabel('X-direction (km)')
    ylabel('Y-direction (km)')
    set(gca,'dataaspectratio',[1 1 1])
    w=w.*(cos(th)+1i*sin(th));
    fac=max(max(abs(w)));
    if(fac==0), fac=eps; end
    psliceuv(x,y,w/fac,nsub,10*fac/maxvel,'k');
    contour(x,y,h,[100,1000:1000:3000],'k');
    title(num2str(time));
    pause(1)
end
