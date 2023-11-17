% Script to visualise satellite attitudes, i.e. the orientation of satellite
% body frame axes in terrestrial (ECEF) or celestial (TOD) reference frame.
% 
% To use this script:
% 1. Uncomment `debugAttitude();` line in `doDebugs()` in debug.cpp;
% 2. Customise the testing example in `debugAttitude()` (see inline comments);
% 3. Recompile and run the pea, with terminal output redirected to a file;
% 4. Copy the debugging output between `Debugging satellite attitude:` and
%    `Starting to process epochs...` to a file as the input file;
% 5. Config (see `%% Config` section) and run this script and the animation
%    will be saved to the specified GIF file.


%% Do some cleaning
clear all;
close all;
clc;


%% Constants
RE = 6371000;                   % mean radius of the Earth (m)
wE = rad2deg(7.2921151467E-5);  % mean angular velocity of the Earth (degree)


%% Config
refFrame    = 'ECEF';                                % reference frame to plot in {'TOD', 'ECEF'}
scaleEarth  = 1;                                    % plot scale of the Earth
scaleAxis   = 5e6;                                  % plot scale of body frame axes
scaleTime   = 10;                                   % time scale of animation (1.0: normal speed, 2.0: twice as fast, 0.5: half speed, etc.)
fileInput   = 'orbexDebugDes/G01_2023240.att';      % input attitude debugging info (outputted by debugAttitude() in Ginan)
fileOutput  = [fileInput, '_', refFrame, '.test.gif'];   % output GIF file
textTitle   = 'G01';                                % plot title


%% Read input data
data = load(fileInput);

t  = data(:, 1: 2);     % GPS time {GPS week, sec of week}
r  = data(:, 3: 5);     % sat pos in ECEF (m)
eX = data(:, 6: 8);     % unit vector of X axis of sat body frame in ECEF
eY = data(:, 9:11);     % unit vector of Y axis of sat body frame in ECEF
eZ = data(:,12:14);     % unit vector of Z axis of sat body frame in ECEF
eS = data(:,24:26);     % unit vector of Sun pos in ECEF

% Convert ECEF to TOD (Ture-of-Date) coordinates (polar motion not considered)
if strcmp(refFrame, 'TOD')
    for i = 1:length(t)
        theta = wE*t(i,2);      % Earth rotation angle
        Rz = rotz(theta)';      % rotation matrix (transposed)
        
        r (i,:) = r (i,:)*Rz;
        eX(i,:) = eX(i,:)*Rz;
        eY(i,:) = eY(i,:)*Rz;
        eZ(i,:) = eZ(i,:)*Rz;
        eS(i,:) = eS(i,:)*Rz;
    end
elseif ~strcmp(refFrame, 'ECEF')
    disp('Unsupported reference frame!');
    return;
end


%% Plot first frame
dX = eX*scaleAxis;
dY = eY*scaleAxis;
dZ = eZ*scaleAxis;
dS = eS*scaleAxis;

X = r + dX;
Y = r + dY;
Z = r + dZ;
S = r + dS;

figure('unit', 'normalized', 'position', [0, 0, 1, 1]);
axes('position', [0, 0, 0.75, 0.9]);
view(135, 30);
hold on;
axis equal;
axis off;
box off;

upper = max([r; X; Y; Z; S]);
lower = min([r; X; Y; Z; S]);
xlim([lower(1), upper(1)]);
ylim([lower(2), upper(2)]);
zlim([lower(3), upper(3)]);

% Plot the Earth
RE = RE*scaleEarth;

load topo;
axesm('globe', 'galtitude', RE);
earth = meshm(topo, topolegend, [], RE);
demcmap(topo);

if strcmp(refFrame, 'TOD')
    rotate(earth, [0, 0, 1], theta, [0, 0, 0]);
end

% Plot sat & orbit
orbit = plot3(r(1,1), r(1,2), r(1,3), 'c');
sat   = scatter3(r(1,1), r(1,2), r(1,3), 'filled');
radial= plot3([0; r(1,1)], [0; r(1,2)], [0; r(1,3)], 'm--');

% % Plot terrestrial/celestial frame
% quiver3(0, 0, 0, RE*2, 0, 0, 'r');
% quiver3(0, 0, 0, 0, RE*2, 0, 'g');
% quiver3(0, 0, 0, 0, 0, RE*2, 'b');
% 
% text(RE*2, 0, 0, ['X_{', refFrame, '}']);
% text(0, RE*2, 0, ['Y_{', refFrame, '}']);
% text(0, 0, RE*2, ['Z_{', refFrame, '}']);

% Plot unit vectors of sat body frame axes & Sun pos
axisX = quiver3(r(1,1), r(1,2), r(1,3), dX(1,1), dX(1,2), dX(1,3), 'r');
axisY = quiver3(r(1,1), r(1,2), r(1,3), dY(1,1), dY(1,2), dY(1,3), 'g');
axisZ = quiver3(r(1,1), r(1,2), r(1,3), dZ(1,1), dZ(1,2), dZ(1,3), 'b');
eSun  = quiver3(r(1,1), r(1,2), r(1,3), dS(1,1), dS(1,2), dS(1,3), 'y');

textX = text(X(1,1), X(1,2), X(1,3), 'X_b', 'horizontalalignment', 'left');
textY = text(Y(1,1), Y(1,2), Y(1,3), 'Y_b', 'horizontalalignment', 'left');
textZ = text(Z(1,1), Z(1,2), Z(1,3), 'Z_b', 'horizontalalignment', 'left');
textS = text(S(1,1), S(1,2), S(1,3), 'eSun');

% Title & dashboard
title = title(textTitle);

eR(1,:) = r(1,:)/norm(r(1,:));
info  = sprintf('GPS Week: %d\nSec of Week: %8.1f\nSat Pos: %12.3f %12.3f %12.3f\neSun: %9.6f %9.6f %9.6f\neX_b: %9.6f %9.6f %9.6f\neY_b: %9.6f %9.6f %9.6f\neZ_b: %9.6f %9.6f %9.6f\neZ_b*eSat = %9.6f\neY_b*eSun = %9.6f', ...
    t (1,1), t (1,2), ...
    r (1,1), r (1,2), r (1,3), ...
    eS(1,1), eS(1,2), eS(1,3), ...
    eX(1,1), eX(1,2), eX(1,3), ...
    eY(1,1), eY(1,2), eY(1,3), ...
    eZ(1,1), eZ(1,2), eZ(1,3), ...
    dot(eZ(1,:), eR(1,:)), ...
    dot(eY(1,:), eS(1,:)));
dash  = annotation('textbox', [0.7, 0.8, 0, 0], 'string', info, 'fitboxtotext', 'on');

% Pause and save GIF file
delayTime = 1/scaleTime;
pause(delayTime);

frame = getframe(gcf);
im = frame2im(frame);
[imind, cm] = rgb2ind(im, 256);
imwrite(imind, cm, fileOutput, 'gif', 'loopcount', inf, 'delaytime', delayTime);


%% Update plot to create animation
for i = 2:length(t)
    
    % Rotate the Earth
    if strcmp(refFrame, 'TOD')
        dt = t(i,2) - t(i-1,2);
        theta = wE*dt;
        rotate(earth, [0, 0, 1], theta, [0, 0, 0]);
    end
    
    % Update sat & orbit
    set(orbit, 'xdata', r(1:i,1), 'ydata', r(1:i,2), 'zdata', r(1:i,3));
    set(sat,   'xdata', r(i,1),   'ydata', r(i,2),   'zdata', r(i,3));
    set(radial,'xdata', [0; r(i,1)], 'ydata', [0; r(i,2)], 'zdata', [0; r(i,3)]);
    
    % Update unit vectors of sat body frame axes & Sun pos
    set(axisX, 'xdata', r(i,1), 'ydata', r(i,2), 'zdata', r(i,3), 'udata', dX(i,1), 'vdata', dX(i,2), 'wdata', dX(i,3));
    set(axisY, 'xdata', r(i,1), 'ydata', r(i,2), 'zdata', r(i,3), 'udata', dY(i,1), 'vdata', dY(i,2), 'wdata', dY(i,3));
    set(axisZ, 'xdata', r(i,1), 'ydata', r(i,2), 'zdata', r(i,3), 'udata', dZ(i,1), 'vdata', dZ(i,2), 'wdata', dZ(i,3));
    set(eSun,  'xdata', r(i,1), 'ydata', r(i,2), 'zdata', r(i,3), 'udata', dS(i,1), 'vdata', dS(i,2), 'wdata', dS(i,3));
    
    set(textX, 'position', [X(i,1), X(i,2), X(i,3)]);
    set(textY, 'position', [Y(i,1), Y(i,2), Y(i,3)]);
    set(textZ, 'position', [Z(i,1), Z(i,2), Z(i,3)]);
    set(textS, 'position', [S(i,1), S(i,2), S(i,3)]);
    
    % Update dashboard
    eR(i,:) = r(i,:)/norm(r(i,:));
    info  = sprintf('GPS Week: %d\nSec of Week: %8.1f\nSat Pos: %12.3f %12.3f %12.3f\neSun: %9.6f %9.6f %9.6f\neX_b: %9.6f %9.6f %9.6f\neY_b: %9.6f %9.6f %9.6f\neZ_b: %9.6f %9.6f %9.6f\neZ_b*eSat = %9.6f\neY_b*eSun = %9.6f', ...
        t (i,1), t (i,2), ...
        r (i,1), r (i,2), r (i,3), ...
        eS(i,1), eS(i,2), eS(i,3), ...
        eX(i,1), eX(i,2), eX(i,3), ...
        eY(i,1), eY(i,2), eY(i,3), ...
        eZ(i,1), eZ(i,2), eZ(i,3), ...
        dot(eZ(i,:), eR(i,:)), ...
        dot(eY(i,:), eS(i,:)));
    set(dash, 'string', info);
    
    % Pause and save GIF file
    pause(delayTime);
    
    frame = getframe(gcf);
    im = frame2im(frame);
    [imind, cm] = rgb2ind(im, 256);
    imwrite(imind, cm, fileOutput, 'gif', 'writemode', 'append', 'delaytime', delayTime);
end
