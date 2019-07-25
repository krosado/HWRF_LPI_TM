clear, close('all')
% movie of transport and elevation
id='ecoast'; % run id
nsub=8; % number to subsample for quiver
maxvel=0.1; % maximum value for trasnport


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
    ele=mexnc('varget',nc,'elb',[0,0,0],[-1,-1,-1],1);
    ele(fsm==0)=nan;
    u=mexnc('varget',nc,'uab',[0 0 0],[-1 -1 -1],1);
    v=mexnc('varget',nc,'vab',[0 0 0],[-1 -1 -1],1);
    u(fsm==0)=nan;
    v(fsm==0)=nan;
    tran=(u+i*v);
    mexnc('close',nc);

    % figure
    clf
    pslice(x,y,ele);
    hold on
    xlabel('X-direction (km)')
    ylabel('Y-direction (km)')
    set(gca,'dataaspectratio',[1 1 1])
    tran=tran.*(cos(th)+1i*sin(th));
    fac=max(max(abs(tran)));
    if(fac==0), fac=eps; end
    psliceuv(x,y,tran/fac,nsub,10*fac/maxvel,'k');
    contour(x,y,h,[1000:1000:3000],'k');
    title(['day ',num2str(time)]);
    pause(1)

    % total transport (Sv) at the open boundaries
    ww=h.*tran;
    [nx,ny]=size(x);
    a(1)=time;
    a(2)=nansum(real(ww(nx-1,:)).*dy(nx,:));
    a(3)=nansum(imag(ww(:,ny-1)).*dx(:,ny));
    a(4)=nansum(imag(ww(:,2)).*dx(:,2));
    disp(num2str([a(1),a(2:4)/1e6]))
end
